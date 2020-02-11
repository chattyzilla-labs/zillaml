module Mdb = Lmdb_bindings;
module type Flags = Mdb.Flags;
module Bigstring = Bigstringaf;

exception Not_found = Not_found;
exception Exists = Mdb.Exists;
exception Map_full = Mdb.Map_full;
exception Error = Mdb.Error;

type perm('a) =
  | Ro: perm([ | `Read])
  | Rw: perm([ | `Read | `Write]);

let version = Mdb.version;

let pp_error = (fmt, i) => Format.fprintf(fmt, "%s@.", Mdb.strerror(i));

module Env = {
  type t = Mdb.env;

  /* exception Assert of (t * string) */

  module Flags = Mdb.EnvFlags;

  let create =
      (
        type p,
        perm: perm(p),
        ~max_readers=?,
        ~map_size=?,
        ~max_maps=?,
        ~flags=Flags.none,
        ~mode=0o644,
        path,
      ) => {
    let flags =
      switch (perm) {
      | Rw => flags
      | Ro => Flags.(flags + read_only)
      };

    let env = Mdb.env_create();
    try({
      let opt_iter = f =>
        fun
        | None => ()
        | Some(x) => f(x);

      opt_iter(Mdb.env_set_mapsize(env), map_size);
      opt_iter(Mdb.env_set_maxdbs(env), max_maps);
      opt_iter(Mdb.env_set_maxreaders(env), max_readers);
      /* Mdb.env_set_assert env (fun env s -> raise (Assert (env,s))) ; */
      Mdb.env_open(env, path, flags, mode);
      env;
    }) {
    | Error(_) as exn =>
      Mdb.env_close(env);
      raise(exn);
    };
  };

  let close = Mdb.env_close;

  let copy = (~compact=false, db, s) => {
    let flag = if (compact) {Mdb.CopyFlags.compact} else {Mdb.CopyFlags.none};
    Mdb.env_copy(db, s, flag);
  };

  let copyfd = (~compact=false, env, fd: Unix.file_descr) => {
    let flag = if (compact) {Mdb.CopyFlags.compact} else {Mdb.CopyFlags.none};
    Mdb.env_copyfd(env, fd, flag);
  };

  let set_flags = Mdb.env_set_flags;
  let flags = Mdb.env_get_flags;

  let set_map_size = Mdb.env_set_mapsize;

  let path = Mdb.env_get_path;
  let sync = (~force=false, env) => Mdb.env_sync(env, force);

  let fd = Mdb.env_get_fd;

  let max_readers = Mdb.env_get_maxreaders;

  let max_keysize = Mdb.env_get_maxkeysize;

  let reader_list = env => {
    let x = ref([]);
    assert(
      Mdb.reader_list(
        env,
        s => {
          x := [s, ...x^];
          0;
        },
      )
      == 0,
    );
    x^;
  };

  let reader_check = Mdb.reader_check;

  let stat = Mdb.env_stat;
  let info = Mdb.env_info;
};

module Txn = {
  type t(-'perm) = Mdb.txn constraint 'perm = [< | `Read | `Write];

  exception Abort(Obj.t);

  let env = Mdb.txn_env;

  let abort = txn => raise(Abort(Obj.repr(txn)));
  let go = (type p, perm: perm(p), ~txn as parent=?, env, f) => {
    let flags =
      switch (perm) {
      | Rw => Env.Flags.none
      | Ro => Env.Flags.read_only
      };

    let txn = Mdb.txn_begin(env, parent, flags);
    switch (f(txn)) {
    | result =>
      Mdb.txn_commit(txn);
      Some(result);
    /* In case txn_commit fails with MDB_MAP_FULL or MDB_BAD_TXN, the txn has
     * been aborted by txn_commit. In those cases don't catch the exception. */
    | exception (Abort(t)) when t === Obj.repr(txn) || parent == None =>
      Mdb.txn_abort(txn);
      None;
    | exception exn =>
      /*let bt = Printexc.get_raw_backtrace () in*/
      Mdb.txn_abort(txn);
      raise(exn);
    };
  };
  /*Printexc.raise_with_backtrace exn bt - since OCaml 4.05 */

  /* Used internally for trivial functions, not exported. */
  let trivial = (perm, ~txn=?, e, f) =>
    switch (txn) {
    | Some(txn) =>
      if (e !== env(txn)) {
        /* Cave: this error is not caught by lmdb */
        invalid_arg("Lmdb: transaction from wrong environment.");
      } else {
        f(txn);
      }
    | None =>
      switch (go(perm, e, f)) {
      | None => assert(false)
      | Some(x) => x
      }
    };
};

module Conv = {
  type bigstring = Bigstring.t;

  module Flags = Mdb.DbiFlags;

  type t('a) = {
    flags: Flags.t,
    serialise: (int => Bigstring.t, 'a) => Bigstring.t,
    deserialise: Bigstring.t => 'a,
  };

  let make = (~flags=Flags.none, ~serialise, ~deserialise, ()) => {
    flags,
    deserialise,
    serialise,
  };

  let serialise = ({serialise, _}) => serialise;
  let deserialise = ({deserialise, _}) => deserialise;
  let flags = ({flags, _}) => flags;

  let is_int_size = n => n == Mdb.sizeof_int || n == Mdb.sizeof_size_t;

  let overflow = Invalid_argument("Lmdb: Integer out of bounds");

  let int32_be = {
    flags:
      if (Sys.big_endian && is_int_size(4)) {
        Flags.(integer_key + integer_dup + dup_fixed);
      } else {
        Flags.(dup_fixed);
      },
    serialise: (alloc, x) => {
      let a = alloc(4);
      Bigstring.set_int32_be(a, 0, x);
      a;
    },
    deserialise: a => Bigstring.get_int32_be(a, 0),
  };

  let int32_le = {
    flags:
      if (!Sys.big_endian && is_int_size(4)) {
        Flags.(integer_key + integer_dup + dup_fixed);
      } else {
        Flags.(reverse_key + reverse_dup + dup_fixed);
      },
    serialise: (alloc, x) => {
      let a = alloc(4);
      Bigstring.set_int32_le(a, 0, x);
      a;
    },
    deserialise: a => Bigstring.get_int32_le(a, 0),
  };

  let int32_as_int = ({flags, deserialise, serialise}) => {
    flags,
    serialise:
      if (Sys.int_size <= 32) {
        (alloc, i) => serialise(alloc) @@ Int32.of_int(i);
      } else {
        (alloc, i) => {
          let ix = Int32.of_int(i);
          if (Int32.to_int(ix) == i) {
            serialise(alloc, ix);
          } else {
            raise(overflow);
          };
        };
      },
    deserialise:
      if (Sys.int_size >= 32) {
        a => deserialise(a) |> Int32.to_int;
      } else {
        a => {
          let ix = deserialise(a);
          let i = Int32.to_int(ix);
          if (Int32.of_int(i) == ix) {
            i;
          } else {
            raise(overflow);
          };
        };
      },
  };

  let int32_be_as_int = int32_as_int(int32_be);
  let int32_le_as_int = int32_as_int(int32_le);

  let int64_be = {
    flags:
      if (Sys.big_endian && is_int_size(8)) {
        Flags.(integer_key + integer_dup + dup_fixed);
      } else {
        Flags.(dup_fixed);
      },
    serialise: (alloc, x) => {
      let a = alloc(8);
      Bigstring.set_int64_be(a, 0, x);
      a;
    },
    deserialise: a => Bigstring.get_int64_be(a, 0),
  };

  let int64_le = {
    flags:
      if (!Sys.big_endian && is_int_size(8)) {
        Flags.(integer_key + integer_dup + dup_fixed);
      } else {
        Flags.(reverse_key + reverse_dup + dup_fixed);
      },
    serialise: (alloc, x) => {
      let a = alloc(8);
      Bigstring.set_int64_le(a, 0, x);
      a;
    },
    deserialise: a => Bigstring.get_int64_le(a, 0),
  };

  let int64_as_int = ({flags, deserialise, serialise}) => {
    flags,
    serialise: (alloc, i) => serialise(alloc) @@ Int64.of_int(i),
    deserialise:
      if (Sys.int_size >= 64) {
        a => deserialise(a) |> Int64.to_int;
      } else {
        a => {
          let ix = deserialise(a);
          let i = Int64.to_int(ix);
          if (Int64.of_int(i) == ix) {
            i;
          } else {
            raise(overflow);
          };
        };
      },
  };

  let int64_be_as_int = int64_as_int(int64_be);
  let int64_le_as_int = int64_as_int(int64_le);

  let string = {
    flags: Flags.none,
    serialise: (alloc, s) => {
      let len = String.length(s);
      let a = alloc(len);
      Bigstring.blit_from_string(s, ~src_off=0, a, ~dst_off=0, ~len);
      a;
    },
    deserialise: a =>
      Bigstring.substring(a, ~off=0, ~len=Bigstring.length(a)),
  };

  let bigstring = {
    flags: Flags.none,
    serialise: (_, b) => b,
    deserialise: b => b,
  };
};

module Map = {
  type t('k, 'v, -'dup) = {
    env: Env.t,
    mutable dbi: Mdb.dbi,
    flags: Mdb.DbiFlags.t,
    key: Conv.t('k),
    value: Conv.t('v),
  }
  constraint 'dup = [< | `Dup | `Uni];

  let env = ({env, _}) => env;

  type card('a) =
    | Nodup: card([ | `Uni])
    | Dup: card([ | `Dup | `Uni]);

  let create =
      (
        type dup,
        type key,
        type value,
        perm: perm('openperm),
        dup: card(dup as 'dup),
        ~key: Conv.t(key),
        ~value: Conv.t(value),
        ~txn: option(Txn.t('openperm))=?,
        ~name: option(string)=?,
        env: Env.t,
      )
      : t(key, value, 'dup) => {
    let create_of_perm = (type p, perm: perm(p)) =>
      switch (perm) {
      | Ro => Conv.Flags.none
      | Rw => Conv.Flags.create
      };

    let flags =
      Conv.Flags.(
        create_of_perm(perm)
        + key.flags
        * (reverse_key + integer_key)
        + (
          switch (dup) {
          | Nodup => Conv.Flags.none
          | Dup when name == None =>
            invalid_arg(
              "Lmdb.Map.create: The unnamed map does not support duplicates",
            )
          | Dup =>
            dup_sort + value.flags * (dup_fixed + integer_dup + reverse_dup)
          }
        )
      );

    let (dbi, flags) =
      Txn.trivial(perm, ~txn?, env) @@
      (
        txn => {
          let dbi = Mdb.dbi_open(txn, name, flags);
          let flags' = Mdb.dbi_flags(txn, dbi);
          if (!Conv.Flags.(eq(unset(create, flags), flags'))) {
            Mdb.dbi_close(env, dbi);
            Printf.sprintf(
              "Lmdb.Map.create: While opening %s got flags %0#x, but expected %0#x\n",
              switch (name) {
              | None => "<unnamed>"
              | Some(name) => name
              },
              Conv.Flags.to_int(flags'),
              Conv.Flags.to_int(flags),
            )
            |> invalid_arg;
          };
          (dbi, flags);
        }
      );

    let db_t = {env, dbi, flags, key, value};
    Gc.finalise(
      ({env, dbi, _}) =>
        if (dbi !== Mdb.invalid_dbi) {
          Mdb.dbi_close(env, dbi);
        },
      db_t,
    );
    db_t;
  };

  let create = (dup, ~key, ~value, ~txn=?, ~name=?, env) =>
    create(Rw, dup, ~key, ~value, ~txn?, ~name?, env)
  and open_existing = (dup, ~key, ~value, ~txn=?, ~name=?, env) =>
    create(Ro, dup, ~key, ~value, ~txn?, ~name?, env);

  let stat = (~txn=?, {env, dbi, _}) =>
    Txn.trivial(Ro, ~txn?, env) @@ (txn => Mdb.dbi_stat(txn, dbi));

  let _flags = (~txn=?, {env, dbi, _}) =>
    Txn.trivial(Ro, env, ~txn?) @@ (txn => Mdb.dbi_flags(txn, dbi));

  let drop = (~txn=?, ~delete=false, {dbi, env, _} as map) => {
    if (delete) {
      map.dbi = Mdb.invalid_dbi;
    };
    Txn.trivial(Rw, ~txn?, env) @@ (txn => Mdb.drop(txn, dbi, delete));
  };

  let get = (map, ~txn=?, k) =>
    Txn.trivial(Ro, ~txn?, map.env) @@
    (
      txn =>
        Mdb.get(txn, map.dbi, map.key.serialise(Bigstring.create, k))
        |> map.value.deserialise
    );

  module Flags = Mdb.PutFlags;

  let put_raw_key = (map, ~txn=?, ~flags=Flags.none, ka, v) =>
    if (Conv.Flags.(test(dup_sort, map.flags))) {
      let va = map.value.serialise(Bigstring.create, v);
      Txn.trivial(Rw, ~txn?, map.env) @@
      (txn => Mdb.put(txn, map.dbi, ka, va, flags));
    } else {
      Txn.trivial(Rw, ~txn?, map.env) @@
      (
        txn => {
          let va_opt = ref(Mdb.Block_option.none);
          let alloc = len => {
            if (Mdb.Block_option.is_some(va_opt^)) {
              invalid_arg(
                "Lmdb: converting function tried to allocate twice.",
              );
            };
            let va = Mdb.put_reserve(txn, map.dbi, ka, len, flags);
            va_opt := Mdb.Block_option.some(va);
            va;
          };

          let va = map.value.serialise(alloc, v);
          if (Mdb.Block_option.is_some(va_opt^)) {
            if (Mdb.Block_option.get_unsafe(va_opt^) !== va) {
              invalid_arg(
                "Lmdb: converting function allocated, but returned different buffer.",
              );
            };
          } else {
            Mdb.put(txn, map.dbi, ka, va, flags);
          };
        }
      );
    };

  let add = (map, ~txn=?, ~flags=Flags.none, k, v) => {
    let flags =
      if (Conv.Flags.(test(dup_sort, map.flags))) {
        flags;
      } else {
        Flags.(flags + no_overwrite);
      };

    let ka = map.key.serialise(Bigstring.create, k);
    put_raw_key(map, ~txn?, ~flags, ka, v);
  };

  let set = (map, ~txn=?, ~flags=?, k, v) => {
    let ka = map.key.serialise(Bigstring.create, k);
    if (Conv.Flags.(test(dup_sort, map.flags))) {
      Txn.trivial(Rw, ~txn?, map.env) @@
      (
        txn => {
          try(Mdb.del(txn, map.dbi, ka, Mdb.Block_option.none)) {
          | Not_found => ()
          };
          put_raw_key(map, ~txn, ~flags?, ka, v);
        }
      );
    } else {
      put_raw_key(map, ~txn?, ~flags?, ka, v);
    };
  };

  let remove = (map, ~txn=?, ~value as v=?, k) => {
    let key = map.key
    and value = map.value;
    let ka = key.serialise(Bigstring.create, k);
    let va =
      switch (v) {
      | None => Mdb.Block_option.none
      | Some(v) =>
        Mdb.Block_option.some @@ value.serialise(Bigstring.create, v)
      };

    Txn.trivial(Rw, ~txn?, map.env) @@ (txn => Mdb.del(txn, map.dbi, ka, va));
  };

  let compare_key = (map, ~txn=?, x, y) => {
    let key = map.key;
    let xa = key.serialise(Bigstring.create, x);
    let ya = key.serialise(Bigstring.create, y);
    Txn.trivial(Ro, ~txn?, map.env) @@ (txn => Mdb.cmp(txn, map.dbi, xa, ya));
  };

  let compare_val = (map, ~txn=?) => {
    if (!Conv.Flags.(test(dup_sort, map.flags))) {
      invalid_arg("Lmdb: elements are only comparable in a dup_sort map");
    };
    let value = map.value;
    (x, y) => {
      let xa = value.serialise(Bigstring.create, x);
      let ya = value.serialise(Bigstring.create, y);
      Txn.trivial(Ro, ~txn?, map.env) @@
      (txn => Mdb.dcmp(txn, map.dbi, xa, ya));
    };
  };

  let compare = compare_key;
};

module Cursor = {
  module Ops = Mdb.Ops;

  module Flags = Mdb.PutFlags;

  type t('k, 'v, -'perm, -'dup) = {
    cursor: Mdb.cursor,
    map: Map.t('k, 'v, 'dup),
  }
  constraint 'dup = [< | `Dup | `Uni]
  constraint 'perm = [< | `Read | `Write];

  let go = (perm, ~txn=?, map: Map.t(_), f) =>
    Txn.trivial(perm, map.env, ~txn?) @@
    (
      t => {
        let cursor = {cursor: Mdb.cursor_open(t, map.dbi), map};

        switch (f(cursor)) {
        | result =>
          Mdb.cursor_close(cursor.cursor);
          result;
        | exception exn =>
          /*let bt = Printexc.get_raw_backtrace () in*/
          Mdb.cursor_close(cursor.cursor);
          raise(exn);
        };
      }
    );
  /*Printexc.raise_with_backtrace exn bt - since OCaml 4.05 */

  /* Used internally for trivial functions, not exported. */
  let trivial = (perm, ~cursor=?, map: Map.t(_), f) =>
    switch ((cursor: option(t(_)))) {
    | Some(cursor) =>
      if (cursor.map !== map) {
        invalid_arg("Lmdb.Cursor.fold: Got cursor for wrong map");
      };
      f(cursor);
    | None => go(perm, map, f)
    };

  let seek = ({cursor, map}, k) => {
    let key = map.key
    and value = map.value;
    let ka = key.serialise(Bigstring.create, k);
    let (ka', va) =
      Mdb.cursor_get(
        cursor,
        Mdb.Block_option.some(ka),
        Mdb.Block_option.none,
        Ops.set,
      );

    assert(ka' == ka);
    (k, value.deserialise(va));
  };

  let get = (cursor, k) => snd @@ seek(cursor, k);

  let seek_range = ({cursor, map}, k) => {
    let key = map.key
    and value = map.value;
    let (ka, va) =
      Mdb.cursor_get(
        cursor,
        Mdb.Block_option.some(key.serialise(Bigstring.create, k)),
        Mdb.Block_option.none,
        Ops.set_range,
      );

    (key.deserialise(ka), value.deserialise(va));
  };

  let get_prim = (op, {cursor, map}) => {
    let key = map.key
    and value = map.value;
    let (ka, va) =
      Mdb.cursor_get(
        cursor,
        Mdb.Block_option.none,
        Mdb.Block_option.none,
        op,
      );

    (key.deserialise(ka), value.deserialise(va));
  };

  let current = c => get_prim(Ops.get_current, c);
  let first = c => get_prim(Ops.first, c);
  let last = c => get_prim(Ops.last, c);
  let next = c => get_prim(Ops.next, c);
  let prev = c => get_prim(Ops.prev, c);
  let next_nodup = c => get_prim(Ops.next_nodup, c);
  let prev_nodup = c => get_prim(Ops.prev_nodup, c);

  let count = ({cursor, _}) => Mdb.cursor_count(cursor);

  let seek_dup = ({cursor, map}, k, v) => {
    let key = map.key
    and value = map.value;
    let ka = key.serialise(Bigstring.create, k);
    let va = value.serialise(Bigstring.create, v);
    let (ka', va') =
      Mdb.cursor_get(
        cursor,
        Mdb.Block_option.some(ka),
        Mdb.Block_option.some(va),
        Ops.get_both,
      );

    assert(ka' == ka);
    assert(va' == va);
  };

  let seek_range_dup = ({cursor, map}, k, v) => {
    let key = map.key
    and value = map.value;
    let (ka, va) =
      Mdb.cursor_get(
        cursor,
        Mdb.Block_option.some(key.serialise(Bigstring.create, k)),
        Mdb.Block_option.some(value.serialise(Bigstring.create, v)),
        Ops.get_both_range,
      );

    (key.deserialise(ka), value.deserialise(va));
  };

  let get_dup_prim = (op, {cursor, map}) => {
    let value = map.value;
    let (_, va) =
      Mdb.cursor_get(
        cursor,
        Mdb.Block_option.none,
        Mdb.Block_option.none,
        op,
      );

    value.deserialise(va);
  };

  let first_dup = c => get_dup_prim(Ops.first_dup, c);
  let last_dup = c => get_dup_prim(Ops.last_dup, c);
  let next_dup = c => get_dup_prim(Ops.next_dup, c);
  let prev_dup = c => get_dup_prim(Ops.prev_dup, c);

  let cursor_none = cursor =>
    Mdb.cursor_get(
      cursor.cursor,
      Mdb.Block_option.none,
      Mdb.Block_option.none,
    );

  let get_values_multiple = (cursor, len) => {
    let value = cursor.map.value;
    assert(Conv.Flags.(test(dup_fixed, cursor.map.flags)));
    let (_, first) = cursor_none(cursor, Ops.first_dup);
    let size = Bigstring.length(first);
    let values = Array.make(len, Obj.magic());
    let (_, buf) = cursor_none(cursor, Ops.get_multiple);
    let rec convert = (buf, off, i) =>
      if (off + size <= Bigstring.length(buf)) {
        values[i] = value.deserialise @@ Bigstring.sub(buf, ~off, ~len=size);
        convert(buf, off + size, i + 1);
      } else {
        assert(off == Bigstring.length(buf));
        i;
      };

    let i = convert(buf, 0, 0);
    let rec loop = i =>
      switch (
        try(Some(cursor_none(cursor, Ops.next_multiple))) {
        | Not_found => None
        }
      ) {
      | None => i
      | Some((_, buf)) => loop(convert(buf, 0, i))
      };

    let i = loop(i);
    assert(i == len);
    values;
  };

  let get_values_from_first = (cursor, first) =>
    if (!Conv.Flags.(test(dup_sort, cursor.map.flags))) {
      [|first|];
    } else {
      let len = Mdb.cursor_count(cursor.cursor);
      if (len > 1 && Conv.Flags.(test(dup_sort + dup_fixed, cursor.map.flags))) {
        get_values_multiple(cursor, len);
      } else {
        let values = Array.make(len, first);
        for (i in 1 to len - 1) {
          values[i] = next_dup(cursor);
        };
        values;
      };
    };

  let get_values_from_last = (cursor, last) =>
    if (!Conv.Flags.(test(dup_sort, cursor.map.flags))) {
      [|last|];
    } else {
      let len = Mdb.cursor_count(cursor.cursor);
      if (len > 1 && Conv.Flags.(test(dup_sort + dup_fixed, cursor.map.flags))) {
        let values = get_values_multiple(cursor, len);
        cursor_none(cursor, Ops.first_dup) |> ignore;
        values;
      } else {
        let values = Array.make(len, last);
        for (i in len - 2 downto 0) {
          values[i] = prev_dup(cursor);
        };
        values;
      };
    };

  let get_all = (cursor, k) => {
    let first = get(cursor, k);
    get_values_from_first(cursor, first);
  };

  let all_prim_from_first = (cursor, f) => {
    let (key, first) = f(cursor);
    (key, get_values_from_first(cursor, first));
  };
  let all_prim_from_last = (cursor, f) => {
    let (key, first) = f(cursor);
    (key, get_values_from_last(cursor, first));
  };

  let first_all = c => all_prim_from_first(c, first);
  let next_all = c => all_prim_from_first(c, next_nodup);
  let last_all = c => all_prim_from_last(c, last);
  let prev_all = c => all_prim_from_last(c, prev_nodup);
  let seek_all = (c, k) => all_prim_from_first(c, c => seek(c, k));
  let seek_range_all = (c, k) =>
    all_prim_from_first(c, c => seek_range(c, k));
  let current_all = c => {
    first_dup(c) |> ignore;
    all_prim_from_first(c, current);
  };

  let put_raw_key = ({cursor, map}, ~flags, ka, v) => {
    let value = map.value;
    if (Conv.Flags.(test(dup_sort, map.flags))) {
      let va = value.serialise(Bigstring.create, v);
      Mdb.cursor_put(cursor, ka, va, flags);
    } else {
      let va_opt = ref(Mdb.Block_option.none);
      let alloc = len => {
        if (Mdb.Block_option.is_some(va_opt^)) {
          invalid_arg("Lmdb: converting function tried to allocate twice.");
        };
        va_opt :=
          Mdb.Block_option.some @@
          Mdb.cursor_put_reserve(cursor, ka, len, flags);
        Mdb.Block_option.get_unsafe(va_opt^);
      };

      let va = value.serialise(alloc, v);
      if (Mdb.Block_option.is_some(va_opt^)) {
        if (Mdb.Block_option.get_unsafe(va_opt^) !== va) {
          invalid_arg(
            "Lmdb: converting function allocated, but returned different buffer.",
          );
        };
      } else {
        Mdb.cursor_put(cursor, ka, va, flags);
      };
    };
  };

  let set = ({cursor, map}, ~flags=Flags.none, k, v) => {
    let ka = map.key.serialise(Bigstring.create, k);
    if (Conv.Flags.(test(dup_sort, map.flags))) {
      switch (
        Mdb.cursor_get(
          cursor,
          Mdb.Block_option.some(ka),
          Mdb.Block_option.none,
          Ops.set,
        )
      ) {
      | exception Not_found => ()
      | (_, _) => Mdb.cursor_del(cursor, Flags.no_dup_data)
      };
    };
    let va = map.value.serialise(Bigstring.create, v);
    Mdb.cursor_put(cursor, ka, va, flags);
  };

  let add = (cursor, ~flags=Flags.none, k, v) => {
    let flags =
      if (Conv.Flags.(test(dup_sort, cursor.map.flags))) {
        flags;
      } else {
        Flags.(flags + no_overwrite);
      };

    let ka = cursor.map.key.serialise(Bigstring.create, k);
    put_raw_key(cursor, ~flags, ka, v);
  };

  let remove = (~all=false, cursor) =>
    Mdb.cursor_del(
      cursor.cursor,
      if (all) {Flags.no_dup_data} else {Flags.none},
    );

  let replace = (cursor, v) => {
    /* mdb_put mdb_current is supposed to replace the current _value_.
     * LMDB API documentation says the current key needs to be passed, too.
     * So first get the raw current key to pass it back in. */
    let (ka, _) =
      Mdb.cursor_get(
        cursor.cursor,
        Mdb.Block_option.none,
        Mdb.Block_option.none,
        Ops.get_current,
      );

    put_raw_key(cursor, ~flags=Flags.current, ka, v);
  };

  let fold_prim = (init, step, ~cursor=?, ~f, acc, map) => {
    let fold = cursor =>
      switch (init(cursor)) {
      | exception Not_found => acc
      | (key, value) =>
        let acc = f(acc, key, value);
        let rec loop = acc =>
          switch (step(cursor)) {
          | exception Not_found => acc
          | (key, value) =>
            let acc = f(acc, key, value);
            loop(acc);
          };
        loop(acc);
      };

    trivial(Ro, map, ~cursor?, fold);
  };

  let fold_left = (~cursor=?, ~f, acc, map) =>
    fold_prim(first, next, ~cursor?, ~f, acc, map);

  let fold_right = (~cursor=?, ~f, map, acc) => {
    let f = (acc, key, values) => f(key, values, acc);
    fold_prim(last, prev, ~cursor?, ~f, acc, map);
  };

  let iter = (~cursor=?, ~f, map) =>
    fold_left(~cursor?, (), map, ~f=(_acc, key, value) => f(key, value));

  let iter_rev = (~cursor=?, ~f, map) =>
    fold_right(~cursor?, map, (), ~f=(key, value, _acc) => f(key, value));

  let fold_prim_all = (init, step, get_all, ~cursor=?, ~f, acc, map) => {
    let fold = cursor =>
      switch (init(cursor)) {
      | exception Not_found => acc
      | (key, first) =>
        let values = get_all(cursor, first);
        let acc = f(acc, key, values);
        let rec loop = acc =>
          switch (step(cursor)) {
          | exception Not_found => acc
          | (key, first) =>
            let values = get_all(cursor, first);
            let acc = f(acc, key, values);
            loop(acc);
          };
        loop(acc);
      };

    trivial(Ro, ~cursor?, map, fold);
  };

  let fold_left_all = (~cursor=?, ~f, acc, map) =>
    fold_prim_all(
      first,
      next_nodup,
      get_values_from_first,
      ~cursor?,
      ~f,
      acc,
      map,
    );

  let fold_right_all = (~cursor=?, ~f, map, acc) => {
    let f = (acc, key, values) => f(key, values, acc);
    fold_prim_all(
      last,
      prev_nodup,
      get_values_from_last,
      ~cursor?,
      ~f,
      acc,
      map,
    );
  };

  let iter_all = (~cursor=?, ~f, map) =>
    fold_left_all(~cursor?, (), map, ~f=((), key, values) => f(key, values));

  let iter_rev_all = (~cursor=?, ~f, map) =>
    fold_right_all(~cursor?, map, (), ~f=(key, values, ()) =>
      f(key, values)
    );
};
