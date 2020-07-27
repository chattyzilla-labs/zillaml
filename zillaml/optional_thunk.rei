type t;

let none: t;
let some: (unit => unit) => t;

let is_none: t => bool;
let is_some: t => bool;

let call_if_some: t => unit;
let unchecked_value: (t, unit) => unit;
