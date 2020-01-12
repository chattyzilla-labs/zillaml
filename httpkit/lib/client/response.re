module type S = {
  type io('a);

  let body:
    ((Zillaml.Response.t, Zillaml.Body.t([ | `read]))) =>
    io(result(string, [> | `Reading_error]));
};

module Make = (M: S) : (S with type io('a) = M.io('a)) => {
  type io('a) = M.io('a);

  let body = M.body;
};
