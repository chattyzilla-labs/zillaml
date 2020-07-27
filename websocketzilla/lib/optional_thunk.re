type t = unit => unit;

let none = Sys.opaque_identity(() => ());
let some = f => {
  if (f === none) {
    failwith(
      "Optional_thunk: this function is not representable as a some value",
    );
  };
  f;
};

let is_none = t => t === none;
let is_some = t => !is_none(t);
let call_if_some = t => t();
let unchecked_value = t => t;
