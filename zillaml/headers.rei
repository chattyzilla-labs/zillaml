type t;

type name = string;
type value = string;

let empty: t;

let of_list: list((name, value)) => t;
let of_rev_list: list((name, value)) => t;
let to_list: t => list((name, value));
let to_rev_list: t => list((name, value));

let add: (t, name, value) => t;
let add_unless_exists: (t, name, value) => t;
let add_list: (t, list((name, value))) => t;
let add_multi: (t, list((name, list(value)))) => t;

let remove: (t, name) => t;
let replace: (t, name, value) => t;

let mem: (t, name) => bool;
let get: (t, name) => option(value);
let get_exn: (t, name) => value;
let get_multi: (t, name) => list(value);

let iter: (~f: (name, value) => unit, t) => unit;
let fold: (~f: (name, value, 'a) => 'a, ~init: 'a, t) => 'a;

let to_string: t => string;
[@ocaml.toplevel_printer]
let pp_hum: (Format.formatter, t) => unit;