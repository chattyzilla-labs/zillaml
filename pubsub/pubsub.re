/**
 * Important! do not interact with the global hashtables outside of on_connect, on_message, and on_close
 * so that all operations remain thread safe. To test if the current runnning thread is holding the async lock
 * you can run Thread_safe.am_holding_async_lock(). It will return true if you are currently runningin the context of the async event loop
 */

open Core;
open Async;
open Zillaml_async;
open Zillaml_httpkit_async.Server;
open Websocket_async;
open Yojson;
open Yojson.Basic.Util;

type word = Hashtag | Star | Word(string);

type topic = list(word);

type action = | Publish((topic, string)) | Subscribe(topic) | Unsubscribe(topic);

type topic_node = {
  id: int32,
  edge_count: int,
  binding_count: int
}

type topic_edge = {
  parent: int32,
  child: option(int32),
  word
}

type exchange = {
  root_node: int32,
  topic_node: Core.Hashtbl.t(int32, topic_node),
  topic_edge: Core.Hashtbl.t(int32, Core.Hashtbl.t(string, topic_edge)),
  bindings: Core.Hashtbl.t(int32, Core.Hashtbl.t(string, list(int32))),
  connections: Core.Hashtbl.t(int32, ws_connection)
};

let create_node = (~edge_count=0, ~binding_count=0, id) =>
  {
  id,
  edge_count,
  binding_count,
  };

let create_edge = (~child=None, parent, word) =>
  {
    parent,
    child,
    word
  };

  let string_of_word = fun
    | Hashtag => "#"
    | Star => "*"
    | Word(str) => str;

  let string_of_topic = fun
    | [] => ""
    | [hd, ...rest] => List.fold(
      rest,
      ~init=string_of_word(hd),
      ~f=(a, b) => a ++ "." ++ string_of_word(b)
    );

module Topic_Parser = {
  open Angstrom;

  let str_to_word =
    fun
    | "#" => Hashtag
    | "*" => Star
    | str => Word(str);

  let is_dot =
    fun | '.' => true | _ => false;
  let dot = skip_while(is_dot)
  let word = take_while1(
    fun
    | '.' => false
    | _ => true
  ) >>| str_to_word <* dot

  let extract_topic = {
    let rec go = (acc) => lift((x) => [x, ...acc], word) >>= go <|> return(acc |> List.rev);
    word >>= ((init) => go([init]));
  }

  let extract_topic = (str) =>
    switch (parse_string(extract_topic, str)) {
    | Ok(v) => v
    | Error(msg) => failwith(msg)
    };
}


// let ws_connection_table = Hashtbl.create(module Int32);
// let exchange_table = Hashtbl.create(module String);
// let topic_node_table = Hashtbl.create(module Int32);
// let topic_edge_table = Hashtbl.create(module Int32);
// let bindings_table = Hashtbl.create(module Int32);

let parse_msg = raw => {
  let json = Basic.from_string(raw);
  let action = json |> member("action") |> to_string;
  let topic = json |> member("topic") |> to_string |> Topic_Parser.extract_topic;
  switch action {
  | "Publish" =>
    let msg =  json |> member("payload") |> to_string;
    Publish((topic, msg))
  | "Subscribe" => Subscribe(topic)
  | "Unsubscibe" => Unsubscribe(topic)
  | unknown => failwith(unknown)
  };
};

let get_exchange  = fun
  | ["exchange", exchange] => exchange
  | _ => failwith("a ws connection got past the url validation step")
  ;

let valid_path =
  fun
  | ["exchange", _] => true
  | _ => false;

 let random_int32 = () => Random.int32(Int32.max_value);

/**
 * note: each binding must belong to a single exchange, so a clinet that wants to connect to multiple exchanges must create multiple
 * 1. unction to add a subscription
 * 2. function to remove a subscription : list(sting) =>  Hashtbl.key(Core.String.t)
 * 3. function to determine if a branch has no active subscriptions and can be removed.
 * 4. function to remove a branch
 * 5. function to retreive bindings based on published topic
 * 6. type that holds all hash tables types for an exchange
 * 7. hash table that holds all exchanges
 * 8. function to create exchange if it does not exist
 * 9. function to remove an exchange
 */

let exchange_table = Hashtbl.create((module String), ~growth_allowed=true, ~size=500);

let add_exchange =  (name) => {
  switch (Hashtbl.find(exchange_table, name)) {
  | Some(_exchange) => name
  | None =>
    let root_node_id = random_int32();
    let root = create_node(root_node_id);
    let topic_node = Hashtbl.create((module Int32), ~growth_allowed=true, ~size=500);
    Hashtbl.set(topic_node, ~key=root_node_id, ~data=root);
    let data = {
      root_node: root_node_id,
      topic_node,
      topic_edge: Hashtbl.create((module Int32), ~growth_allowed=true, ~size=1000),
      bindings: Hashtbl.create((module Int32), ~growth_allowed=true, ~size=500),
      connections: Hashtbl.create((module Int32), ~growth_allowed=true, ~size=500)
    };
    Hashtbl.set(data.topic_edge, ~key=root.id, ~data=Hashtbl.create((module String), ~growth_allowed=true));
    Hashtbl.set(data.bindings, ~key=root.id, ~data=Hashtbl.create((module String), ~growth_allowed=true));
    Hashtbl.set(exchange_table, ~key=name, ~data);
    name
  };
};

let subscribe = (exchange, ws: ws_connection, topic) => {
  open Hashtbl;
  let rec aux = (lst, edge) => switch (lst) {
  | [] => ()
  | [hd] =>
    let word_str = string_of_word(hd);
    switch (edge.child) {
    | Some(id) =>
      let node =  find_exn(exchange.topic_node, id);
      let node_edges = find_exn(exchange.topic_edge, id);
      switch (find(node_edges, word_str)) {
      | Some(_edge) =>
        set(exchange.topic_node, ~key=id, ~data={...node, binding_count: node.binding_count + 1 });
        change(
          find_exn(exchange.bindings, id),
          string_of_topic(topic),
          fun | Some(lst) => Some([ws.id, ...lst]) | None => Some([ws.id])
        );
      | None =>
        set(node_edges, ~key=word_str, ~data=create_edge(id, hd));
        set(
          exchange.topic_node,
          ~key=id,
          ~data={
            ...node,
            edge_count: node.edge_count + 1,
            binding_count: node.binding_count + 1
          }
        );
        change(
          find_exn(exchange.bindings, id),
          string_of_topic(topic),
          fun | Some(lst) => Some([ws.id, ...lst]) | None => Some([ws.id])
        );
      };
    | None =>
      let node = create_node(~edge_count=1, ~binding_count=1, random_int32())
      set(exchange.topic_node, ~key=node.id, ~data=node);
      change(
        find_exn(exchange.topic_edge, edge.parent),
        string_of_word(edge.word),
        fun | Some(e) => Some({...e, child: Some(node.id)}) | None => failwith("edge not found but logic should guarentee existence")
      );
      let edge_hash = Hashtbl.create((module String), ~growth_allowed=true);
      set(edge_hash, ~key=word_str, ~data=create_edge(node.id, hd))
      set(exchange.topic_edge, ~key=node.id, ~data=edge_hash);
      let binding_hash = Hashtbl.create((module String), ~growth_allowed=true);
      set(binding_hash, ~key=string_of_topic(topic), ~data=[ws.id]);
      set(
        exchange.bindings,
        ~key=node.id,
        ~data=binding_hash
      );
    };
  | [hd, ...rest] =>
    let word_str = string_of_word(hd);
    switch (edge.child) {
    | Some(id) => (find_exn(exchange.topic_edge, id) |> find_exn(_, word_str)) |> aux(rest);
    | None =>
      let node = create_node(~edge_count=1, ~binding_count=1, random_int32());
      set(exchange.topic_node, ~key=node.id, ~data=node);
      change(
        find_exn(exchange.topic_edge, edge.parent),
        string_of_word(edge.word),
        fun | Some(e) => Some({...e, child: Some(node.id)}) | None => failwith("edge not found but logic should guarentee existence")
      );
      let edge_hash = Hashtbl.create((module String), ~growth_allowed=true);
      let edge = create_edge(node.id, hd);
      set(edge_hash, ~key=word_str, ~data=edge);
      set(exchange.topic_edge, ~key=node.id, ~data=edge_hash);
      aux(rest, edge);
    };
  };
  switch(topic){
  | [] => ()
  | [hd, ...rest] =>
    let node =  find_exn(exchange.topic_node, exchange.root_node);
    let node_edges = find_exn(exchange.topic_edge, exchange.root_node);
    switch (find(node_edges, string_of_word(hd))) {
    | Some(edge) =>
      if(List.is_empty(rest)){
        set(exchange.topic_node, ~key=exchange.root_node, ~data={...node, binding_count: node.binding_count + 1 });
        change(
          find_exn(exchange.bindings, exchange.root_node),
          string_of_topic(topic),
          fun | Some(lst) => Some([ws.id, ...lst]) | None => Some([ws.id])
        );
      } else {
        aux(rest, edge);
      };

    | None =>
      let edge = create_edge(exchange.root_node, hd);
      set(node_edges, ~key=string_of_word(hd), ~data=edge);
      if(List.is_empty(rest)){
        set(
          exchange.topic_node,
          ~key=exchange.root_node,
          ~data={
            ...node,
            edge_count: node.edge_count + 1,
            binding_count: node.binding_count + 1
          }
        );
      change(
          find_exn(exchange.bindings, exchange.root_node),
          string_of_topic(topic),
          fun | Some(lst) => Some([ws.id, ...lst]) | None => Some([ws.id])
        );
      } else {
        set(
          exchange.topic_node,
          ~key=exchange.root_node,
          ~data={
            ...node,
            edge_count: node.edge_count + 1,
          }
        );
        aux(rest, edge);
      }
    };
  }
};

let unsubscribe = (exchange, ws, topic) => {
  ()
};

let publish = (exchange, ws, (topic, msg)) => {
  ()
};

let on_connect = (ws) => {
      // on connect is when we create/add connection to an exchange
      let exchange_name = add_exchange(get_exchange(ws.path));
      let exchange = Hashtbl.find_exn(exchange_table, exchange_name);
      Hashtbl.set(exchange.connections, ~key=ws.id, ~data=ws);
      let topics = Hashtbl.create((module String), ~growth_allowed=true, ~size=128);
      // on message is where we take care of subscibing connections to topics, publishing to topics, and unsubscribing to topics
      let on_message = msg => {
        switch(parse_msg(msg)){
          | Subscribe(topic) =>
            subscribe(exchange, ws, topic);
            Hashtbl.set(topics, ~key=string_of_topic(topic), ~data=true);
          | Publish(payload) => publish(exchange, ws, payload)
          | Unsubscribe(topic) => unsubscribe(exchange, ws, topic)
        }
      };
      // on close is where we take care of clean-up task for the connection including unsubscribing to topics
      let on_close = () => ();

      { Websocket_async.on_message, on_close }
};

let on_start = (port) => {
  Logs.set_level(Some(Logs.App));
  Logs.set_reporter(Logs_fmt.reporter());
  print_endline(Util.hello());
  print_endline(Util.running(port));
};

let socket_server = ((port, accepts), ()) => Http.create_socket_server(
    ~port,
    ~on_start=on_start,
    ~max_accepts_per_batch=accepts,
    ~check_request=(req) => Deferred.return(
      Websocket_async.upgrade_present(req.headers)
      && Router.get_path(req) |> valid_path
    ),
    ~on_connect=on_connect
);

