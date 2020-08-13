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

let socket_server = ((port, accepts), ()) => Http.create_server_with_web_sockets(
    ~port,
    ~on_start=on_start,
    ~max_accepts_per_batch=accepts,
    ~check_for_websocket_request=(req) => Deferred.return(Websocket_async.upgrade_present(req.headers) && Websocket_async.default_ws_path(req)),
    ~on_ws_connect=(ws) => {
      { Websocket_async.on_message: (msg) => ws.send(msg), on_close: () => ()}
    },
    ~http_router=router, 
    ~reqlogger=Some(Common.req_logger)
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