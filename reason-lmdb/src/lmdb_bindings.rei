/** Raw bindings for LMDB. */;

type bigstring =
  Bigarray.Array1.t(char, Bigarray.int8_unsigned_elt, Bigarray.c_layout);

let version: (string, int, int, int);

/** {2 Exceptions} */;

exception Exists;
exception Map_full;
exception Error(int);
external strerror: int => string = "mdbs_strerror";

/** {2 Flags} */;

/** Operations on sets of flags. */

module type Flags = {
  /** The type of a set of flags */

  type t;

  /** [a + b] is the {e union} of flag sets [a] and [b].
      This corresponds to a bitwise {e or} on C bitfields. */
  external (+): (t, t) => t = "%orint";

  /** [a * b] is the intersection of flag sets a and b.
      This corresponds to a bitwise {e and} on C bitfields. */
  external ( * ): (t, t) => t = "%andint";

  /** [test a b] is [true] only if [a] is a subset of [b].
      This corresponds to [a & b == a] for C bitfields. */

  let test: (t, t) => bool;

  /** [unset a b] removes flags [a] from flag set [b].
      This corresponds to [a & ~b] for C bitfields. */

  let unset: (t, t) => t;

  /** [eq a b] The equals relation. */ external eq: (t, t) => bool = "%equal";

  external of_int: int => t = "%identity";
  external to_int: t => int = "%identity";

  /** [none] The empty set of flags. */

  let none: t;
};

module Flags: Flags;

/** {2 Environment} */;

type env;
module EnvFlags: {
  include Flags;
  let fixed_map: t;
  /** Create the environment not in an existing directory,
          but create the data file with exactly the filename given to {!Env.create}.
          The lock file will have "-lock" appended.
      */

  let no_subdir: t;

  let no_sync: t;
  let read_only: t;
  let no_meta_sync: t;
  let write_map: t;
  let map_async: t;
  let no_tls: t;
  let no_lock: t;
  let no_read_ahead: t;
  let no_mem_init: t;
};
module CopyFlags: {
  include Flags;
  let compact: t;
};
external env_create: unit => env = "mdbs_env_create";
external env_open: (env, string, EnvFlags.t, int) => unit = "mdbs_env_open";
external env_close: env => unit = "mdbs_env_close";
external env_set_mapsize: (env, int) => unit = "mdbs_env_set_mapsize";
external env_set_maxdbs: (env, int) => unit = "mdbs_env_set_maxdbs";
external env_set_maxreaders: (env, int) => unit = "mdbs_env_set_maxreaders";
external env_copy: (env, string, CopyFlags.t) => unit = "mdbs_env_copy2";
external env_copyfd: (env, Unix.file_descr, CopyFlags.t) => unit =
  "mdbs_env_copyfd2";
external env_set_flags: (env, EnvFlags.t, bool) => unit = "mdbs_env_set_flags";
external env_get_flags: env => EnvFlags.t = "mdbs_env_get_flags";
external env_get_path: env => string = "mdbs_env_get_path";
external env_get_fd: env => Unix.file_descr = "mdbs_env_get_fd";
external env_sync: (env, bool) => unit = "mdbs_env_sync";
external env_get_maxreaders: env => int = "mdbs_env_get_maxreaders";
external env_get_maxkeysize: env => int = "mdbs_env_get_maxkeysize";
external reader_list: (env, string => int) => int = "mdbs_reader_list";
external reader_check: env => int = "mdbs_reader_check";
type stat = {
  psize: int,
  depth: int,
  branch_pages: int,
  leaf_pages: int,
  overflow_pages: int,
  entries: int,
};
external env_stat: env => stat = "mdbs_env_stat";
type envinfo = {
  /** To recover the actual address this integer needs to be shifted to the
      left by one bit. A 31 bit integer may overflow. */
  map_addr: int,
  map_size: int,
  last_pgno: int,
  last_txnid: int,
  max_readers: int,
  num_readers: int,
};
external env_info: env => envinfo = "mdbs_env_info";

/** {2 Transaction} */;

type txn;
external txn_env: txn => env = "mdbs_txn_env";
external txn_begin: (env, option(txn), EnvFlags.t) => txn = "mdbs_txn_begin";
external txn_commit: txn => unit = "mdbs_txn_commit";
external txn_abort: txn => unit = "mdbs_txn_abort";

/** {2 Dbi} */;

type dbi;
let invalid_dbi: dbi;
module DbiFlags: {
  include Flags;
  let reverse_key: t;
  let dup_sort: t;
  let integer_key: t;
  let dup_fixed: t;
  let integer_dup: t;
  let reverse_dup: t;
  let create: t;
};
module PutFlags: {
  include Flags;
  /** Raise {!exception: Exists} if the key already exists no matter whether the map
        supports duplicates.
    */

  let no_overwrite: t;

  /** Only for maps supporting duplicates: Don't add the value to an already
        existing key if this value is already part of this key.
    */

  let no_dup_data: t;

  /** Add a key that is greater than any existing key.
        Used to efficiently add sorted data.
    */

  let append: t;

  /** Add value to key that is greater than any existing value of this key.
        Used to efficiently add sorted values to a key.
    */

  let append_dup: t;

  let current: t;
  let reserve: t;
  let multiple: t;
};
module Block_option: {
  type t(+'a);
  let none: t('a);
  external some_unsafe: 'a => t('a) = "%identity";
  external get_unsafe: t('a) => 'a = "%identity";
  let is_some: 'a => bool;
  let is_none: 'a => bool;
  let some: 'a => t('a);
  let get_exn: t('a) => 'a;
};
external dbi_open: (txn, option(string), DbiFlags.t) => dbi = "mdbs_dbi_open";
external dbi_close: (env, dbi) => unit = "mdbs_dbi_close";
external dbi_flags: (txn, dbi) => DbiFlags.t = "mdbs_dbi_flags";
external dbi_stat: (txn, dbi) => stat = "mdbs_stat";
external drop: (txn, dbi, bool) => unit = "mdbs_drop";
external get: (txn, dbi, bigstring) => bigstring = "mdbs_get";
external put: (txn, dbi, bigstring, bigstring, PutFlags.t) => unit =
  "mdbs_put";
external put_reserve: (txn, dbi, bigstring, int, PutFlags.t) => bigstring =
  "mdbs_put";
external del: (txn, dbi, bigstring, Block_option.t(bigstring)) => unit =
  "mdbs_del";
external cmp: (txn, dbi, bigstring, bigstring) => int = "mdbs_cmp";
external dcmp: (txn, dbi, bigstring, bigstring) => int = "mdbs_dcmp";

/** {2 Cursor} */;

type cursor;
module Ops: {
  type t;
  let first: t;
  let first_dup: t;
  let get_both: t;
  let get_both_range: t;
  let get_current: t;
  let get_multiple: t;
  let last: t;
  let last_dup: t;
  let next: t;
  let next_dup: t;
  let next_multiple: t;
  let next_nodup: t;
  let prev: t;
  let prev_dup: t;
  /* let prev_multiple  = prev_multiple - only since lmdb 0.9.19 */
  let prev_nodup: t;
  let set: t;
  let set_key: t;
  let set_range: t;
};
external cursor_open: (txn, dbi) => cursor = "mdbs_cursor_open";
external cursor_close: cursor => unit = "mdbs_cursor_close";
external cursor_put: (cursor, bigstring, bigstring, PutFlags.t) => unit =
  "mdbs_cursor_put";
external cursor_put_reserve: (cursor, bigstring, int, PutFlags.t) => bigstring =
  "mdbs_cursor_put";
external cursor_del: (cursor, PutFlags.t) => unit = "mdbs_cursor_del";
external cursor_get:
  (cursor, Block_option.t(bigstring), Block_option.t(bigstring), Ops.t) =>
  (bigstring, bigstring) =
  "mdbs_cursor_get";
external cursor_count: cursor => int = "mdbs_cursor_count";

/** {2 Internal} */;

let sizeof_int: int;
let sizeof_size_t: int;
