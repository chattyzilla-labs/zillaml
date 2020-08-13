open Async;

type routeResponseT = {
  status: Zillaml.Status.t,
  headers: option(list((Zillaml.Headers.name, Zillaml.Headers.value))),
  body: string,
};

type routeFn =
  (Zillaml.Request.t, string) => Deferred.t(option(routeResponseT));

let createResponse = (~status, ~headers=?, body) =>
  Deferred.return(Some({status, headers, body}));

type requestHandler =
  (list(string), Zillaml.Request.t, string) =>
  Deferred.t(option(routeResponseT));

type routerList = list(requestHandler);

let rec run_router = (routers: routerList, path, req, body) => {
  switch (routers) {
  | [head, ...tail] =>
    switch%bind (head(path, req, body)) {
    | Some(_) as res => Deferred.return(res)
    | None => run_router(tail, path, req, body)
    }
  | [] => Deferred.return(None)
  };
};

let check_req_path = (~path, req: Zillaml.Request.t) => {
  Uri.of_string(req.target)
  |> Uri.path
  |> String.split_on_char('?')
  |> List.hd
  |> String.equal(path);
};

let get_path = (req: Zillaml.Request.t) =>
  Uri.of_string(req.target)
  |> Uri.path
  |> String.split_on_char('/')
  |> List.tl;
let get_query = (req: Zillaml.Request.t) =>
  Uri.of_string(req.target) |> Uri.query;

let create_router = (routerFn, req: Zillaml.Request.t, body_) => {
  let body =
    switch (body_) {
    | Some(str) => str
    | None => ""
    };
  let path =
    Uri.of_string(req.target)
    |> Uri.path
    |> String.split_on_char('/')
    |> List.tl;
  routerFn(path, req, body);
};
