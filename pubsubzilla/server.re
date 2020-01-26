open Core;
open Async;
open Zillaml_async;
open Zillaml_httpkit_async.Server;
open Websocket_async;

open Pubsub;

let exchange_table =
  Hashtbl.create((module String), ~growth_allowed=true, ~size=500);

let on_connect = ws => {
  // on connect is when we create/add connection to an exchange
  let exchange_name = add_exchange(exchange_table, get_exchange(ws.path));
  let exchange = Hashtbl.find_exn(exchange_table, exchange_name);
  let conn = create_conn_from_ws(ws);
  Hashtbl.set(exchange.connections, ~key=conn.id, ~data=conn);
  let topics = Hashtbl.create((module String), ~growth_allowed=true);
  // on message is where we take care of subscibing connections to topics, publishing to topics, and unsubscribing to topics
  let on_message = msg => {
    switch (parse_msg(msg)) {
    | Subscribe(topic) =>
      switch (Hashtbl.find(topics, string_of_topic(topic))) {
      | None =>
        subscribe(exchange, conn, topic);
        Hashtbl.set(topics, ~key=string_of_topic(topic), ~data=topic);
      | Some(_) => ()
      }
    // TODO add validation to make sure this is a valid topic ie no # or *
    | Publish(payload) => publish(exchange, payload)
    | Unsubscribe(topic) =>
      unsubscribe(exchange, conn, topic);
      Hashtbl.remove(topics, string_of_topic(topic));
    };
  };
  // on close is where we take care of clean-up task for the connection including unsubscribing to topics
  let on_close = () => {
    Hashtbl.iter(topics, ~f=topic => unsubscribe(exchange, conn, topic));
    Hashtbl.remove(exchange.connections, conn.id);
    /* check connections is empty and remove from exchange tabbl*/
    switch (Hashtbl.is_empty(exchange.connections)) {
    | false => ()
    | true => Hashtbl.remove(exchange_table, exchange_name)
    };
  };

  {Websocket_async.on_message, on_close};
};

let on_start = port => {
  Logs.set_level(Some(Logs.App));
  Logs.set_reporter(Logs_fmt.reporter());
  print_endline(Util.hello());
  print_endline(Util.running(port));
};

let socket_server = ((port, accepts), ()) =>
  Http.create_socket_server(
    ~port,
    ~on_start,
    ~max_accepts_per_batch=accepts,
    ~check_request=
      req =>
        Deferred.return(
          Websocket_async.upgrade_present(req.headers)
          && Router.get_path(req)
          |> valid_path,
        ),
    ~on_connect,
  );