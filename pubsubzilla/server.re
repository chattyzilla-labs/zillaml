open Core;
open Async;

open Zillaml_httpkit_async.Server;
open Websocket_async;

let on_start = (server, port) => {
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

let socket_server = ((port, accepts), ()) => {
  let { Pubsub.exchange_table, on_connect } = Pubsub.create_broker();
  let on_connect = ws => {
    let send = (`Message(topic, msg)) => {
      let payload =
        Printf.sprintf(
          "{\"topic\": \"%s\", \"payload\": \"%s\"}",
          Pubsub.string_of_topic(topic),
          msg,
        );
      ws.send(payload);
    };
    let conn = Pubsub.create_conn(ws.id, send, Pubsub.get_exchange(ws.path));
    let {Pubsub.on_action, on_close} = on_connect(conn);
    {Websocket_async.on_message: msg => on_action(Pubsub.parse_msg(msg)), on_close};
  };
  Http.create_socket_server(
    ~port,
    ~on_start,
    ~max_accepts_per_batch=accepts,
    ~check_request=
      req =>
        Deferred.return(
          Websocket_async.upgrade_present(req.headers)
          && Router.get_path(req)
          |> Pubsub.valid_path,
        ),
    ~on_connect,
  );
}
