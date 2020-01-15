open Core;
open Async;
open Zillaml_async;
open Zillaml_httpkit_async.Server;

let on_start = (port) => {
  Fmt_tty.setup_std_outputs();
  Logs.set_level(Some(Logs.App));
  Logs.set_reporter(Logs_fmt.reporter());
  print_endline(Util.hello());
  print_endline(Util.running(port));
};

let socket_server = ((port, accepts), ()) => Http.create_socket_server(
    ~port,
    ~on_start=on_start,
    ~max_accepts_per_batch=accepts,
    ~check_request=(req) => Deferred.return(Websocket_async.upgrade_present(req.headers) && Websocket_async.default_ws_path(req)),
    ~on_connect=(ws) => {
      { Websocket_async.on_message: (msg) => ws.send(msg), on_close: () => ()}
    }
);

