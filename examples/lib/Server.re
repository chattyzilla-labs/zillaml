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

  let on_start = (port) => {
    Fmt_tty.setup_std_outputs();
    Logs.set_level(Some(Logs.App));
    Logs.set_reporter(Logs_fmt.reporter());
    print_endline(Util.hello());
    print_endline(Util.running(port));
  };

  let ping_router: Router.requestHandler = (path, req, body) => {
    switch (path) {
      | ["ping"] => Router.createResponse(~status=`OK, {|{"success": true}|})
      | _ => Deferred.return(None)
    };
  };

let router = Router.run_router([ping_router]) |> Router.create_router;


// let onConnected = ({ reader, writer, app_to_ws, ws_to_app, close_finished, request }: Websocket_async.t) => {
//     let read_loop = Websocket_async.Event_Listeners.loop_pipe_reader(app_to_ws, ~onClientMessage=(msg)=> Pipe.write(ws_to_app, "ClientMessage: " ++ msg));


//     let finally = () => {
//         Pipe.close(ws_to_app);
//         Pipe.close_read(app_to_ws);
//         let%bind () = Writer.close(writer)
//         and () = Reader.close(reader)
//         let%bind (_reason, str, _info) = close_finished()
//         print_endline(str);
//         Deferred.unit;
//       };

//       Monitor.protect(
//         ~finally,
//         () => {
//             Deferred.any([
//                 read_loop,
//                 Pipe.closed(ws_to_app),
//                 Pipe.closed(app_to_ws),
//             ])
//         },
//       );

// };

let socket_server = ((port, accepts), ()) => Http.create_socket_server(
    ~port,
    ~on_start=on_start,
    ~max_accepts_per_batch=accepts,
    ~check_request=(req) => Deferred.return(Websocket_async.upgrade_present(req.headers) && Websocket_async.default_ws_path(req)),
);

let server = ((port, accepts), ()) => Http.create_server(
    ~port,
    ~on_start=on_start,
    ~max_accepts_per_batch=accepts,
    ~error_handler,
    ~router,
    ~reqlogger=Some(Common.req_logger)
);

