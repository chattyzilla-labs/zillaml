/** High level bindings for LMDB. */;

/** The {{:http://www.lmdb.tech/doc/}LMDB} database
    is a fast in-file key-value store that supports ACID transactions.

    These bindings attempt to expose a typesafe yet low-overhead API.

    First, an environment must be opened using {!Env.create}:

    {[let env = Env.(create Rw ~flags:Flags.no_subdir "mydb") ]}

    Now the data file [mydb] and lock file [mydb-lock] have been created
    in the current directory.

    One environment may contain multiple named and one unnamed key-value stores.
    They are called {e databases} in the
    {{:http://www.lmdb.tech/doc/starting.html}LMDB documentation}, but called
    {e maps} in these OCaml bindings.

    A single [('key, 'value, [< `Read | `Write], [< `Dup | `Uni ])] {!type: Map.t}
    is a key-value store mapping OCaml values of type ['key] to values of
    type ['value].
    Multiple values per key are supported on request.

    Using {!Map}, we can open the unnamed map and add our first value:
{[
let map = Map.open_existing Nodup ~key:Conv.string ~value:Conv.string env in
Map.add map "Bactrian camel" "Elegant and beautiful animal with two humps."
]}

    {{!Txn}Transactions} and {{!Cursor}Iterators} are also available.
*/;

/** {2 Raw bindings} */;

module Mdb = Lmdb_bindings;

/** {2 Permissions} */;

/** This library uses [[< `Read | `Write ]] phantom types to encode the
    read/write permissions of transactions and cursors. The following values
    are used to request read-only or read-write permissions on environments,
    transactions and cursors.
*/

type perm('a) =
  | Ro: perm([ | `Read])
  | Rw: perm([ | `Read | `Write]);

/** {2 Database} */;

/** Collection of maps stored in a single memory-mapped file. */

module Env: {
  type t;

  module Flags = Mdb.EnvFlags;

  /** [create perm path] creates an environment with {!Ro} or {!Rw} permissions
      with {e data} and {e lock} files in the already existing directory [path].
      If no separate directory is desired, {!Flags.no_subdir} can be passed.

      The returned handle is not garbage collected and should be closed
      explicitely to free locks and prevent corruption on async environments.

      @param map_size Size of the memory map. Limited by the virtual address space.
      @param max_readers Maximum number of threads/reader slots.
      @param max_maps Maximum number of named maps.
      @param mode The UNIX permissions to set on created files and semaphores. Default is [0o644].
  */

  let create:
    (
      perm(_),
      ~max_readers: int=?,
      ~map_size: int=?,
      ~max_maps: int=?,
      ~flags: Flags.t=?,
      ~mode: int=?,
      string
    ) =>
    t;

  let sync: (~force: bool=?, t) => unit;

  let close: t => unit;

  let copy: (~compact: bool=?, t, string) => unit;

  let copyfd: (~compact: bool=?, t, Unix.file_descr) => unit;

  let set_flags: (t, Flags.t, bool) => unit;

  let flags: t => Flags.t;

  let set_map_size: (t, int) => unit;

  let path: t => string;

  let fd: t => Unix.file_descr;

  let stat: t => Mdb.stat;

  let info: t => Mdb.envinfo;

  let max_readers: t => int;

  let max_keysize: t => int;

  let reader_list: t => list(string);

  let reader_check: t => int;
};

/** Series of operations on an environment performed atomically. */

module Txn: {
  /** A transaction handle. A transaction may be read-only or read-write. */

  type t(-'perm) constraint 'perm = [< | `Read | `Write];

  /** [go perm env f]
      runs a transaction with [perm] read/write permissions in [env].

      The function [f txn] will receive the transaction handle. All changes to
      the environment [env] done using the transaction handle will be persisted
      to the environment only when [f] returns. After [f] returned, the
      transaction handle is invalid and should therefore not be leaked outside
      [f].

      @return [None] if the transaction was aborted with [abort], and [Some _] otherwise.
      @param txn Create a child transaction to [txn].
      This is not supported on an [env] with {!Env.Flags.write_map}.

      Here is an example incrementing a value atomically:
{[
go rw env begin fun txn ->
  let v = Map.get ~txn k in
  Map.add ~txn k (v+1) ;
  v
end
]}
  */

  let go:
    (perm('perm), ~txn: t('perm)=?, Env.t, t('perm) => 'a) => option('a);

  /** [abort txn] aborts transaction [txn] and the current [go] function,
      which will return [None].
  */

  let abort: t(_) => _;

  /** [env txn] returns the environment of [txn] */

  let env: t('perm) => Env.t;
};

/** Converters to and from the internal representation of keys and values.
    A converter contains serialising and deserialising functions as well as
    the flags applied when the converter is used in a map.
*/

module Conv: {
  /** {2 Types } */;

  type t('a);

  /** Bigstrings are used to transfer the raw serialised data into and out of
      the database. They may point directly to a memory-mapped region of the
      database file. */

  type bigstring =
    Bigarray.Array1.t(char, Bigarray.int8_unsigned_elt, Bigarray.c_layout);

  /** Flags describing the (sorting) properties of keys and values of a map.

      See the LMDB documentation for the meaning of these flags.

      You probably won't need those flags since the converters provided in
      {!Conv} will already make appropriate use of these flags.
  */

  module Flags = Lmdb_bindings.DbiFlags;

  /** {2 Constructor and accessors} */;

  /** [make ~serialise ~deserialise]
      creates a converter from a serialising and a deserialising function

      @param serialise [serialise alloc x]
        {e may} call [alloc len] {e once} to allocate a [bigstring] of size [len].
        It then {e must} fill the serialised data of [x] into this [bigstring]
        and return {e exactly this} bigstring. If [serialise] didn't call [alloc] it may
        return any [bigstring].
        [alloc] may return uninitialised memory. It is therefore recommended
        that [serialise] overwrites all allocated memory to avoid leaking possibly
        sensitive memory content into the database.

        If [serialise] calls [alloc] the library may utilise the [MDB_RESERVE]
        interface when appropriate to avoid calls to [malloc] and [memcpy].

      @param deserialise
        The passed {!bigstring} is only valid as long as the current transaction.
        It is therefore strongly recommended not to leak it out of [deserialise].

      @param flags Flags to be set on a map using this converter.

        Depending on the use of a converter as {e key} or {e value}
        {!Map.create} and {!Map.open_existing} will select the correct set of
        flags: [_key] flags will be used for keys and [_dup] flags will be
        used for values on maps supporting duplicates.

  */

  let make:
    (
      ~flags: Flags.t=?,
      ~serialise: (int => bigstring, 'a) => bigstring,
      ~deserialise: bigstring => 'a,
      unit
    ) =>
    t('a);

  let serialise: (t('a), int => bigstring, 'a) => bigstring;
  let deserialise: (t('a), bigstring) => 'a;
  let flags: t(_) => Flags.t;

  /** {2 Predefined converters } */;

  /** {3 Strings } */;

  /** The [bigstring] converter returns bigstrings as returned by the lmdb
      backend. These bigstrings point into the environment memory-map and
      are therefore only guaranteed to be valid until the transaction ends.
      If you need longer-lived values then use the [string] converter, make a copy
      or write a custom converter.
  */

  let bigstring: t(bigstring);

  /** The [string] converter simply copies the raw database content from / to
      OCaml strings. */

  let string: t(string);

  /** {3 Integers } */;

  /** The integer converters will make use of {! Flags.t} as
      appropriate so that integers are sorted in ascending order irrespective
      of machine endianness.
  */;

  let int32_be: t(Int32.t);
  let int64_be: t(Int64.t);
  let int32_le: t(Int32.t);
  let int64_le: t(Int64.t);

  /** For convenience, the [_as_int] converters convert the internal integer
      representation to and from [int].
      @raise Invalid_argument [Invalid_argument "Lmdb: Integer out of bounds"]
  */;

  let int32_be_as_int: t(int);
  let int64_be_as_int: t(int);
  let int32_le_as_int: t(int);
  let int64_le_as_int: t(int);
};

/** Key-value maps. */

module Map: {
  /** A handle for a map from keys of type ['key] to values of type ['value].
      The map may support only a single value per key ([[ `Dup ]])
      or multiple values per key ([[ `Dup | `Uni ]]). */

  type t('key, 'value, -'dup)
  constraint 'perm = [< | `Read | `Write]
  constraint 'dup = [< | `Dup | `Uni];

  type card('a) =
    | Nodup: card([ | `Uni])
    | Dup: card([ | `Dup | `Uni]);

  /** [create dup ~key ~value env]
      open (and possibly create) a map in the environment [env].

      [dup] may be {!Dup} or {!Nodup}, specifying whether the map supports
      multiple values per key.

      Only a single transaction may call this function at a time.
      This transaction needs to finish before any other transaction may call
      this function.

      @param name if omitted the unnamed map will be opened. Otherwise make
      sure that {! Env.create} was called with a large enough [~max_maps].
      @param key Converter for keys
      @param value Converter for values
      @raise Invalid_argument if an existing map doesn't support duplicates,
      but duplicates where requested.
  */

  let create:
    (
      card([< | `Dup | `Uni] as 'dup),
      ~key: Conv.t('key),
      ~value: Conv.t('value),
      ~txn: Txn.t([> | `Read | `Write])=?,
      ~name: string=?,
      Env.t
    ) =>
    t('key, 'value, 'dup);

  /** [open_existing env] is like [create], but only opens already existing maps.
      @raise Not_found if the map doesn't exist.
  */

  let open_existing:
    (
      card([< | `Dup | `Uni] as 'dup),
      ~key: Conv.t('key),
      ~value: Conv.t('value),
      ~txn: Txn.t([> | `Read])=?,
      ~name: string=?,
      Env.t
    ) =>
    t('key, 'value, 'dup);

  /** [env map] returns the environment of [map]. */

  let env: t(_) => Env.t;

  /** [get map key] returns the first value associated to [key].
      @raise Not_found if the key is not in the map.
  */

  let get: (t('key, 'value, _), ~txn: Txn.t([> | `Read])=?, 'key) => 'value;

  module Flags = Lmdb_bindings.PutFlags;

  /** [add map key value] adds [value] to [key].

      For a map not supporting duplicates an existing value is overwritten. For
      a map supporting duplicates the value is added to the key. This is the
      same as [overwrite] for duplicate maps, but
      [overwrite ~flags:Flags.no_overwrite] for non-duplicate maps.

      @param flags {!Flags}
      @raise Exists on maps not supporting duplicates if the key already exists.
      @raise Exists if key is already bound to [value] and {!
      Map.Flags.no_dup_data} was passed.
  */

  let add:
    (
      t('key, 'value, _),
      ~txn: Txn.t([> | `Write])=?,
      ~flags: Flags.t=?,
      'key,
      'value
    ) =>
    unit;

  /** [set map key value] sets binding of [key] to [value].

      Values of an already existing key are silently overwritten.

      @param flags {!Flags}
  */

  let set:
    (
      t('key, 'value, _),
      ~txn: Txn.t([> | `Write])=?,
      ~flags: Flags.t=?,
      'key,
      'value
    ) =>
    unit;

  /** [remove map key] removes [key] from [map].

      @param value Only the specified value is removed.
      If not provided, all the values of [key] and [key] itself are removed.

      @raise Not_found if the key is not in the map.
  */

  let remove:
    (
      t('key, 'value, _),
      ~txn: Txn.t([> | `Write])=?,
      ~value: 'value=?,
      'key
    ) =>
    unit;

  /** {2 Misc} */;

  let stat: (~txn: Txn.t([> | `Read])=?, t('key, 'value, _)) => Mdb.stat;

  /** [drop ?delete map] Empties [map].
      @param delete If [true] [map] is also deleted from the environment
      and the handle [map] invalidated. */

  let drop:
    (~txn: Txn.t([> | `Write])=?, ~delete: bool=?, t('key, 'value, _)) =>
    unit;

  /** [compare_key map ?txn a b]
     Compares [a] and [b] as if they were keys in [map]. */

  let compare_key:
    (t('key, 'value, _), ~txn: Txn.t([> | `Read])=?, 'key, 'key) => int;

  /** [compare map ?txn a b] Same as [compare_key]. */

  let compare:
    (t('key, 'value, _), ~txn: Txn.t([> | `Read])=?, 'key, 'key) => int;

  /** [compare_val map ?txn a b]
     Compares [a] and [b] as if they were values in a [dup_sort] [map]. */

  let compare_val:
    (
      t('key, 'value, [> | `Dup]),
      ~txn: Txn.t([> | `Read])=?,
      'value,
      'value
    ) =>
    int;
};

/** Iterators over maps. */

module Cursor: {
  /** A cursor allows to iterate manually on the map.
      Every cursor implicitely uses a transaction.
  */;

  /** A cursor inherits two phantom types: the [[< `Read | `Write ]] permissions
      from the transaction and the [[< `Dup | `Uni ]] support from the map.
  */

  type t('key, 'value, -'perm, -'dup)
  constraint 'perm = [< | `Read | `Write]
  constraint 'dup = [< | `Dup | `Uni];

  /** [go perm map ?txn f] makes a cursor in the transaction [txn] using the
      function [f cursor].

      The function [f] will receive the [cursor].
      A cursor can only be created and used inside a transaction.
      The cursor inherits the permissions of the transaction.
      The cursor should not be leaked outside of [f].

      Here is an example that returns the first 5 elements of a [map]:
      {[
go ro map begin fun c ->
let h = first c in
let rec aux i =
  if i < 5 then next c :: aux (i+1)
  else []
in
h :: aux 1
end
      ]}

      @param txn if omitted a transient transaction will implicitely be
      created before calling [f] and be committed after [f] returns.
  */

  let go:
    (
      perm('perm),
      ~txn: Txn.t('perm)=?,
      Map.t('key, 'value, 'dup),
      t('key, 'value, 'perm, 'dup) => 'a
    ) =>
    'a;

  /** {2 Modification} */;

  module Flags = Lmdb_bindings.PutFlags;

  /** [add cursor key value] adds [value] to [key] and moves the cursor to
      its position.

      For a map not supporting duplicates an existing value is overwritten. For
      a map supporting duplicates the value is added to the key. This is the
      same as [overwrite] for duplicate maps, but
      [overwrite ~flags:Flags.no_overwrite] for non-duplicate maps.

      @param flags {!Flags}
      @raise Exists on maps not supporting duplicates if the key already exists.
      @raise Exists if [key] is already bound to [value] and
      {! Cursor.Flags.no_dup_data} was passed.
  */

  let add:
    (
      t('key, 'value, [> | `Read | `Write], _),
      ~flags: Flags.t=?,
      'key,
      'value
    ) =>
    unit;

  /** [set cursor key value] sets binding of [key] to [value].
      and moves the cursor to its position.

      Values of an already existing key are silently overwritten.

      @param flags {!Flags}
  */

  let set: (t('key, 'value, _, _), ~flags: Flags.t=?, 'key, 'value) => unit;

  /** [replace cursor value] replace the current value by [value]. */

  let replace: (t('key, 'value, [> | `Read | `Write], _), 'value) => unit;

  /** [remove cursor] removes the current binding.
      @param all If [true] removes all values associated to the current key.
      Default is [false].
  */

  let remove:
    (~all: bool=?, t('key, 'value, [> | `Read | `Write], _)) => unit;

  /** {2 Reading} */;

  /** [current cursor] returns key and value at the position of the cursor. */

  let current: t('key, 'value, [> | `Read], _) => ('key, 'value);

  /** [current_all cursor] moves the cursor to the {e last} value of the
      {e current} key. Returns key and all values of the current key.
  */

  let current_all:
    t('key, 'value, [> | `Read], [> | `Dup]) => ('key, array('value));

  /** [count cursor] returns the number of values bound to the current key. */

  let count: t('key, 'value, [> | `Read], [> | `Dup]) => int;

  /** {3 Seeking} */;

  /** [get cursor key] moves the cursor to the {e first} value of [key]. */

  let get: (t('key, 'value, [> | `Read], _), 'key) => 'value;

  /** [get_all cursor key] moves the cursor to the {e last} value of [key].
      Returns all values of [key].
  */

  let get_all:
    (t('key, 'value, [> | `Read], [> | `Dup]), 'key) => array('value);

  /** [seek cursor key] moves the cursor to the first value of [key]. */

  let seek: (t('key, 'value, [> | `Read], _), 'key) => ('key, 'value);

  /** [seek_all cursor key]
      moves the cursor to the {e last} value of [key].
      Returns all values of [key].
  */

  let seek_all:
    (t('key, 'value, [> | `Read], [> | `Dup]), 'key) =>
    ('key, array('value));

  /** [seek_range cursor key] moves the cursor to the {e first} value of the
      first key greater than or equal to [key].
  */

  let seek_range: (t('key, 'value, [> | `Read], _), 'key) => ('key, 'value);

  /** [seek_range_all cursor key] moves the cursor to the {e last} value of the
      first key greater than or equal to [key]. Returns all values of this key.
  */

  let seek_range_all:
    (t('key, 'value, [> | `Read], [> | `Dup]), 'key) =>
    ('key, array('value));

  /** [seek_dup cursor key value] moves the cursor to [value] of [key]. */

  let seek_dup:
    (t('key, 'value, [> | `Read], [> | `Dup]), 'key, 'value) => unit;

  /** [seek_range_dup cursor key value] moves the cursor to the first value greater
      than or equal to [value] of the first key greater than or equal to [key].
  */

  let seek_range_dup:
    (t('key, 'value, [> | `Read], [> | `Dup]), 'key, 'value) =>
    ('key, 'value);

  /** {3 Moving} */;

  /** {4 Moving over all key-value pairs } */;

  /** [first cursor] moves the cursor to the {e first} value of the first key. */

  let first: t('key, 'value, [> | `Read], _) => ('key, 'value);

  /** [last cursor] moves the cursor to the {e last} value of the last key. */

  let last: t('key, 'value, [> | `Read], _) => ('key, 'value);

  /** [next cursor] moves the cursor to the next key-value pair.
      This may be the {e next value} of the {e current key} or the
      {e first value} of the {e next key}.
  */

  let next: t('key, 'value, [> | `Read], _) => ('key, 'value);

  /** [prev cursor] moves the cursor to the previous key-value pair.
      This may be the {e previous value} of the {e current key} or the
      {e last value} of the {e previous key}.
  */

  let prev: t('key, 'value, [> | `Read], _) => ('key, 'value);

  /** {4 Moving to neighboring keys } */;

  /** [next_nodup cursor]
      moves the cursor to the {e first} value of the next key.
  */

  let next_nodup: t('key, 'value, [> | `Read], _) => ('key, 'value);

  /** [prev_nodup cursor]
      moves the cursor to the {e last} value of the previous key.
  */

  let prev_nodup: t('key, 'value, [> | `Read], _) => ('key, 'value);

  /** {4 Moving over duplicates of a single key } */;

  /** [first_dup cursor] moves the cursor to the first {e value} of the current key. */

  let first_dup: t('key, 'value, [> | `Read], [> | `Dup]) => 'value;

  /** [last_dup cursor] moves the cursor to the last {e value} of the current key. */

  let last_dup: t('key, 'value, [> | `Read], [> | `Dup]) => 'value;

  /** [next_dup cursor] moves the cursor to the next value of the current key.
      @raise Not_found if the cursor is already on the last value of the current key.
  */

  let next_dup: t('key, 'value, [> | `Read], [> | `Dup]) => 'value;

  /** [prev_dup cursor] moves the cursor to the previous value of the current key.
      @raise Not_found if the cursor is already on the first value of the current key.
  */

  let prev_dup: t('key, 'value, [> | `Read], [> | `Dup]) => 'value;

  /** {4 Moving over keys getting all duplicates } */;

  /** [first_all cursor]
      moves the cursor to the {e last} value of the first key.
      Returns all values of the first key.
  */

  let first_all:
    t('key, 'value, [> | `Read], [> | `Dup]) => ('key, array('value));

  /** [last_all cursor]
      moves the cursor to the {e first} value of the last key.
      Returns all values of the {e last} key.
  */

  let last_all:
    t('key, 'value, [> | `Read], [> | `Dup]) => ('key, array('value));

  /** [next_all cursor]
      moves the cursor to the {e last} value of the next key.
      Returns all values of the next key.
  */

  let next_all:
    t('key, 'value, [> | `Read], [> | `Dup]) => ('key, array('value));

  /** [prev_all cursor]
      moves the cursor to the {e first} value of the previous key.
      Returns all values of the previous key.
  */

  let prev_all:
    t('key, 'value, [> | `Read], [> | `Dup]) => ('key, array('value));

  /** {2 Convenient Iterators} */;

  /** Call [f] once for each key-value pair.
      Will call [f] multiple times with the same key for duplicates */;

  let iter:
    (
      ~cursor: t('key, 'value, [> | `Read], 'dup)=?,
      ~f: ('key, 'value) => unit,
      Map.t('key, 'value, 'dup)
    ) =>
    unit;

  let iter_rev:
    (
      ~cursor: t('key, 'value, [> | `Read] as 'perm, 'dup)=?,
      ~f: ('key, 'value) => unit,
      Map.t('key, 'value, 'dup)
    ) =>
    unit;

  let fold_left:
    (
      ~cursor: t('key, 'value, [> | `Read], 'dup)=?,
      ~f: ('a, 'key, 'value) => 'a,
      'a,
      Map.t('key, 'value, 'dup)
    ) =>
    'a;

  let fold_right:
    (
      ~cursor: t('key, 'value, [> | `Read], 'dup)=?,
      ~f: ('key, 'value, 'a) => 'a,
      Map.t('key, 'value, 'dup),
      'a
    ) =>
    'a;

  /** Call [f] once for each key passing the key and {e all} associated values. */;

  let iter_all:
    (
      ~cursor: t('key, 'value, [> | `Read], 'dup)=?,
      ~f: ('key, array('value)) => unit,
      Map.t('key, 'value, [> | `Dup] as 'dup)
    ) =>
    unit;

  let iter_rev_all:
    (
      ~cursor: t('key, 'value, [> | `Read] as 'perm, 'dup)=?,
      ~f: ('key, array('value)) => unit,
      Map.t('key, 'value, [> | `Dup] as 'dup)
    ) =>
    unit;

  let fold_left_all:
    (
      ~cursor: t('key, 'value, [> | `Read], 'dup)=?,
      ~f: ('a, 'key, array('value)) => 'a,
      'a,
      Map.t('key, 'value, [> | `Dup] as 'dup)
    ) =>
    'a;

  let fold_right_all:
    (
      ~cursor: t('key, 'value, [> | `Read], 'dup)=?,
      ~f: ('key, array('value), 'a) => 'a,
      Map.t('key, 'value, [> | `Dup] as 'dup),
      'a
    ) =>
    'a;
};

/** {2 Error reporting} */;

/** Raised when adding an already existing key to a [`Uni] map or
    adding an an already existing value with {! Map.Flags.no_dup_data} to a
    key of a [`Dup] map.

    Also raised when trying to [add ~flags:Flags.append(_dup)] non-sorted data.
*/

exception Exists;

/** Raised when searching for non-existing key */

exception Not_found;

/** Raised when memory map is full */

exception Map_full;

/** Other errors are reported with [Invalid_arg s] or [Error n]. */

exception Error(int);

/** [pp_error Format.std_formatter e] prepares a human-readable description
    of the given error code [n] raised via [Error n].
*/

let pp_error: (Format.formatter, int) => unit;

/** [(name, major, minor, patch)] */

let version: (string, int, int, int);
