open Core;
open Async;
open Zillaml_httpkit;
open Zillaml_httpkit_async.Server;


module Common = {
    let req_logger = (req: Zillaml.Request.t) => {
      let {Unix.tm_hour, tm_min, tm_sec, _} = Unix.time() |> Unix.localtime;
      let time = Printf.sprintf("%d:%d:%d", tm_hour, tm_min, tm_sec);
      let meth = req.meth |> Zillaml.Method.to_string;
      let path = req.target;
      Logs.info(m => m("%s — %s %s", time, meth, path));
    };
  };

let error_handler:
  (
    Async_unix__.Unix_syscalls.Socket.Address.Inet.t,
    ~request: Zillaml.Request.t=?,
    Zillaml.Server_connection.error,
    Zillaml.Headers.t => Zillaml.Body.t([ | `write])
  ) =>
  unit =
  (_client, ~request as req=?, _err, get) => {
    let err =
      switch (_err) {
      | `Bad_gateway => "Bad gateway"
      | `Bad_request => "Bad request"
      | `Internal_server_error => "Internal_server_error"
      | `Exn(exn) => Exn.to_string(exn)
      };
    Logs.err(m => m("Something went wrong! %s", err));
    ();
  };

  let on_start = (server, port) => {
    Fmt_tty.setup_std_outputs();
    Logs.set_level(Some(Logs.App));
    Logs.set_reporter(Logs_fmt.reporter());
    print_endline(Util.hello());
    print_endline(Util.running(port));
    Deferred.forever(
      (),
      () =>
        Clock.after(Time.Span.(of_sec(0.5)))
        >>| (() => Log.Global.printf("conns: %d", Tcp.Server.num_connections(server)))
    );
  };

  let ping_router: Router.requestHandler = (path, req, body) => {
    switch (path) {
      | ["ping"] => Router.createResponse(~status=`OK, {|{"success": true}|})
      | _ => Deferred.return(None)
    };
  };

let router = Router.run_router([ping_router]) |> Router.create_router;

module Websocket_interface: Http.Websocket_interface = {
  type server_to_client = string;
  type client_to_server = string;
  let server_to_client_of_string =  str => str;
  let string_of_server_to_client = str => str;
  let client_to_server_of_string=  str => str;
  let string_of_client_to_server =  str => str;
};



module Server = Http.Make_server_with_ws(Websocket_interface);

let socket_server = ((port, accepts), ()) => Server.start(
    ~port,
    ~on_start,
    ~max_accepts_per_batch=accepts,
    ~req_logger=Some(Common.req_logger),
    ~http_router=router, 
    ~check_for_websocket_request= (req) => Deferred.return(Websocket_async.upgrade_present(req.headers) && Websocket_async.default_ws_path(req)),
    ~on_ws_connect = ws => {
      { Websocket_async.on_message: (msg) => ws.send(msg |> Websocket_interface.string_of_client_to_server |> Websocket_interface.server_to_client_of_string), on_close: () => ()}
    },
    ~state=None
);

let server = ((port, accepts), ()) => Http.create_server(
    ~port,
    ~on_start=on_start,
    ~max_accepts_per_batch=accepts,
    ~error_handler,
    ~router,
    ~reqlogger=Some(Common.req_logger)
);

module Message = {
  [@deriving (sexp, bin_io, compare)]
  type t = {
    text: string,
    sender: string
  };
};


module MessageTopicServer = Pubsubzilla.Exchange.MakeExchange(Pubsubzilla.TopicRouterExchange.Make(Message));

// let pubsub_server = Pubsubzilla.Server.socket_server;