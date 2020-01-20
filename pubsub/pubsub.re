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
  id: int32,
  parent: int32,
  child: option(int32),
  word
}

type exchange = {
  root_node: int32,
  topic_node: Core.Hashtbl.t(int32, topic_node),
  topic_edge: Core.Hashtbl.t(int32, Core.Hashtbl.t(string, topic_edge)),
  bindings: Core.Hashtbl.t(int32, Core.Hashtbl.t(int32, list(int32))),
  connections: Core.Hashtbl.t(int32, ws_connection)
};
let random_int32 = () => Random.int32(Int32.max_value);
let create_node = (~edge_count=0, ~binding_count=0, id) =>
  {
  id,
  edge_count,
  binding_count,
  };

let create_edge = (~child=None, parent, word) =>
  {
    id: random_int32(),
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

let exchange_table = Hashtbl.create((module String), ~growth_allowed=true, ~size=500);

let get_node_and_edges = (exchange, node_id) => {
  open Hashtbl;
  switch (find(exchange.topic_node, node_id)) {
  | Some(node) => switch (find(exchange.topic_edge, node_id)) {
    | Some(edgetbl) => Some((node, edgetbl))
    | None => None
    };
  | None => None
  };
};

let get_leaf = (exchange, topic) => {
  open Hashtbl;
  let rec aux = (topic', node: topic_node) =>
    switch (topic') {
    | [] => failwith("we should not get an empty topic list")
    | [hd, ...rest] =>
      let edge =  find_exn(find_exn(exchange.topic_edge, node.id), string_of_word(hd))
      switch(List.is_empty(rest)){
      | true => (node, edge)
      | false =>
        let child_id = Option.value_exn(edge.child);
        aux(rest, find_exn(exchange.topic_node, child_id))
      }
    };
  aux(topic, find_exn(exchange.topic_node, exchange.root_node));
}

let get_edge_by_word = (tbl, word: string) =>
  Hashtbl.find(tbl, word)

let get_bindings_tbl_exn = (exchange, node_id) =>
  Hashtbl.find_exn(exchange.bindings, node_id)

let get_bindings_tbl = (exchange, node_id) =>
  Hashtbl.find(exchange.bindings, node_id)

let get_bindings_by_edge_id = (tbl, id: int32) =>
  Hashtbl.find(tbl, id)

let get_bindings_by_edge_id_exn = (tbl, id: int32) =>
  Hashtbl.find_exn(tbl, id)

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
    Hashtbl.set(data.bindings, ~key=root.id, ~data=Hashtbl.create((module Int32), ~growth_allowed=true));
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
      | Some(edge) =>
        set(exchange.topic_node, ~key=id, ~data={...node, binding_count: node.binding_count + 1 });
        change(
          find_exn(exchange.bindings, id),
          edge.id,
          fun | Some(lst) => Some([ws.id, ...lst]) | None => Some([ws.id])
        );
      | None =>
        let new_edge = create_edge(id, hd);
        set(node_edges, ~key=word_str, ~data=new_edge);
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
          new_edge.id,
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
      let new_edge = create_edge(node.id, hd);
      set(edge_hash, ~key=word_str, ~data=new_edge)
      set(exchange.topic_edge, ~key=node.id, ~data=edge_hash);
      let binding_hash = Hashtbl.create((module Int32), ~growth_allowed=true);
      set(binding_hash, ~key=new_edge.id, ~data=[ws.id]);
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
          edge.id,
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
          edge.id,
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

let unsubscribe = (exchange, ws: ws_connection, topic) => {
  open Hashtbl;

  let (node, edge) = get_leaf(exchange, topic);
  change(
    find_exn(exchange.bindings, node.id),
    edge.id,
    ~f=fun | None => None | Some(lst) => Some(List.filter(lst, ~f=id => Int32.(id != ws.id)))
  );
  let bindings_list =  find_exn(find_exn(exchange.bindings, node.id), edge.id);
  let remove_edge = List.is_empty(bindings_list);
  if(remove_edge){
    remove(find_exn(exchange.bindings, node.id), edge.id);
    remove(find_exn(exchange.topic_edge, node.id), string_of_word(edge.word));
  };
  let updated_node = { ...node, edge_count: remove_edge ? node.edge_count - 1 : node.edge_count, binding_count: node.binding_count - 1 };

  let rec aux = (topic) => {
    switch (topic) {
    | [] => ()
    | _lst => {
      let (node, edge) = get_leaf(exchange, topic);
      // need to consider bindingcount and edge count
      switch(node.binding_count){
        | 0 =>
          remove(exchange.topic_node, node.id);
          remove(exchange.topic_edge, node.id);
        | _count =>
          switch(find(find_exn(exchange.bindings, node.id), edge.id)){
          | None =>
            remove(find_exn(exchange.topic_edge, node.id), string_of_word(edge.word))
          | Some(_) => ()
          }
        };
       if(List.length(topic) > 1) {
        let topic' = List.rev(topic) |> List.tl_exn |> List.rev
        aux(topic');
      };
    }
    };
  };
  switch(updated_node.edge_count){
  | 0 when Int32.(updated_node.id != exchange.root_node) => {
     remove(exchange.topic_node, node.id);
      remove(exchange.topic_edge, node.id);
      if(List.length(topic) > 1) {
        let topic' = List.rev(topic) |> List.tl_exn |> List.rev
        aux(topic');
      };
  }
  | _int =>
    set(
      exchange.topic_node,
      ~key=updated_node.id,
      ~data=updated_node
    )
  }
};

let publish = (exchange, (topic, msg)) => {
  let edges_to_ids = edges => List.map(
    edges,
    ((node_id, edge_id)) =>
      get_bindings_by_edge_id_exn(
        get_bindings_tbl_exn(exchange, node_id),
        edge_id
      )
  ) |> List.concat_no_order;
  let identity = fun | None => None | Some(_) as v => v;
  let rec aux = (lst, nodes, edges) => switch(lst){
    | [] => edges_to_ids(edges)
    | [hd, ...rest] =>
      switch (List.is_empty(nodes)) {
      | true => edges_to_ids(edges)
      | false =>
        let word = string_of_word(hd);
        let lastword = List.is_empty(rest);
        let (nodes, newedges) = List.map(
          nodes,
          ~f=(id) => switch (get_node_and_edges(exchange, id)) {
          | None => [None]
          | Some((node, edgetbl)) =>
            let exact = switch(get_edge_by_word(edgetbl, word)){
              | None => None
              | Some(edge) =>
                switch (lastword) {
                | true => Some((None, Some((node.id, edge.id))))
                | false =>
                  switch (edge.child) {
                  | Some(id) => Some((Some(id), None))
                  | None => None
                  };
                };
            };
            let star = switch(get_edge_by_word(edgetbl, "*")){
              | None => None
              | Some(edge) =>
                switch (lastword) {
                | true => Some((None, Some((node.id, edge.id))))
                | false =>
                  switch (edge.child) {
                  | Some(id) => Some((Some(id), None))
                  | None => None
                  };
                };
            };
            let hashtag = switch (get_edge_by_word(edgetbl, "#")) {
              | None => None
              | Some(edge) =>
                switch (lastword) {
                | true => Some((None, Some((node.id, edge.id))))
                | false =>
                  switch(get_bindings_by_edge_id(get_bindings_tbl_exn(exchange, node.id), edge.id)){
                    | None =>
                      switch (edge.child) {
                      | Some(_id) => Some((Some(node.id), None))
                      | None => None
                      };
                    | Some(_bindings) =>
                      switch (edge.child) {
                      | Some(_id) => Some((Some(node.id), Some((node.id, edge.id))))
                      | None => Some((None, Some((node.id, edge.id))))
                      };
                  }

                };
            };
            [exact, star, hashtag]
          }
        )
        |> List.concat_no_order
        |> List.filter_map(~f=identity)
        |> List.unzip
        |> (
          ((a, b)) => (
            List.filter_map(a, ~f=identity),
            List.filter_map(b, ~f=identity)
          )
        );
        aux(rest, nodes, List.unordered_append(edges, newedges))
      };
  };
  let payload = Printf.sprintf(
    "{\"topic\": \"%s\", \"payload\": \"%s\"}",
    string_of_topic(topic),
    msg
  );
  List.iter(
      aux(topic, [exchange.root_node], [])
      |> List.dedup_and_sort(~compare=Int32.compare),
      id => Hashtbl.find_exn(exchange.connections, id).send(payload)
  );
};

let on_connect = (ws) => {
      // on connect is when we create/add connection to an exchange
      let exchange_name = add_exchange(get_exchange(ws.path));
      let exchange = Hashtbl.find_exn(exchange_table, exchange_name);
      Hashtbl.set(exchange.connections, ~key=ws.id, ~data=ws);
      let topics = Hashtbl.create((module String), ~growth_allowed=true);
      // on message is where we take care of subscibing connections to topics, publishing to topics, and unsubscribing to topics
      let on_message = msg => {
        switch(parse_msg(msg)){
          | Subscribe(topic) =>
            switch(Hashtbl.find(topics, string_of_topic(topic))){
              | None =>
                subscribe(exchange, ws, topic);
                Hashtbl.set(topics, ~key=string_of_topic(topic), ~data=topic);
              | Some(_) => ()
            }

            // add validation to make sure this is a valid topic ie no # or *
          | Publish(payload) => publish(exchange, payload)
          | Unsubscribe(topic) =>
            unsubscribe(exchange, ws, topic)
            Hashtbl.remove(topics, string_of_topic(topic))
        }
      };
      // on close is where we take care of clean-up task for the connection including unsubscribing to topics
      let on_close = () => {
        Hashtbl.iter(topics, ~f=(topic) => unsubscribe(exchange, ws, topic))
        Hashtbl.remove(exchange.connections, ws.id);
        // check connections is empty and remove from exchange tabble
      };

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

