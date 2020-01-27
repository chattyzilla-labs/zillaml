/**
 * Important! do not interact with the global hashtables outside of on_connect, on_message, and on_close
 * so that all operations remain thread safe. To test if the current runnning thread is holding the async lock
 * you can run Thread_safe.am_holding_async_lock(). It will return true if you are currently runningin the context of the async event loop
 *
 * This application works as a finite state machine so you will notice a lot of unsafe hash tbl operations
 * because the logic assumes the existence of data in some hashtbls. The state of the application would otherwise be invalid if the operations threw an exception
 *
 * TODO: refactor core code into module, abstract away connection so it is not coupled to a ws connection so this code can be reused for pubsub client, snapshots,
 * subscription/unsubscribe and publishing confirmations, retry sending msgs if no confirmation by subscription
 */
open Core;
open Zillaml_httpkit_async.Server;
open Websocket_async;
open Yojson;
open Yojson.Basic.Util;

type word =
  | Hashtag
  | Star
  | Word(string);

type topic = list(word);

type action =
  | Publish((topic, string))
  | Subscribe(topic)
  | Unsubscribe(topic);

// TODO: set up so code takes advantage of phantom types for all the ids
type topic_node = {
  id: int32,
  edge_count: int,
  binding_count: int,
};

type topic_edge = {
  id: int32,
  parent: int32,
  child: option(int32),
  word,
};

// this type serves as a common interface for client and server implementation
// this allows client code to use the same exchange management code as server.
// Client can use this to reuse the same connection for all subscriptions to a topic in an exchange.
// send on client side would be a callback onMessage function implemented by a subscription
type connection = {
  id: int32,
  send: string => unit
};

/**
 *  conceptually the group of hashes that make up an exchange represents a tree
 *  ( e(w) / ) reps a topic_edge of word w,
 *  (_) represents no next node <edge.child == None>,
 *  ([c]) list of connection id
 *  (rn) root node,
 *  (n) non root node
 *
 *                          Tree
 *          _________________________________________
 *                           rn
 *               [c] <- e(w) / \ e(w) -> [c]
 *                         _   n
 *                 [c] <- e(w) / \ e(w) -> [c]
 *                           n   _
 *               c] <- e(w) /
 *                        _
 *
 *  This was chosen with performance of application, and application metrics code in mind.
 *  Also for flexibility in gathering metrics.
 */

type exchange = {
  name: string,

  root_node: int32,
  /**
   * hash map that links node id to topic node
   * node_id -> topic_node
   */
  topic_node: Core.Hashtbl.t(int32, topic_node),
  /**
   * hash map thath links a node id to a hash map of the string representation of a word to a topic edge
   * node_id -> word -> topic_edge
  */
  topic_edge: Core.Hashtbl.t(int32, Core.Hashtbl.t(string, topic_edge)),
  /**
   * hash map that links a node id to a hash map that links a topic edge id to a list of connection ids
   * node_id -> edge_id -> list(connection_id)
   */
  bindings: Core.Hashtbl.t(int32, Core.Hashtbl.t(int32, list(int32))),
  /**
   * hash map that links a connection id to a connection
   * connection_id -> connection
   */
  connections: Core.Hashtbl.t(int32, connection),
};

let random_int32 = () => Random.int32(Int32.max_value);

let create_node = (~edge_count=0, ~binding_count=0, id) => {
  id,
  edge_count,
  binding_count,
};

let create_edge = (~child=None, parent, word) => {
  id: random_int32(),
  parent,
  child,
  word,
};

let create_conn_from_ws = (ws: ws_connection) => {id: ws.id, send: ws.send};

let string_of_word =
  fun
  | Hashtag => "#"
  | Star => "*"
  | Word(str) => str;

let string_of_topic =
  fun
  | [] => ""
  | [hd, ...rest] =>
    List.fold(rest, ~init=string_of_word(hd), ~f=(a, b) =>
      a ++ "." ++ string_of_word(b)
    );

module Topic_Parser = {
  open Angstrom;

  let str_to_word =
    fun
    | "#" => Hashtag
    | "*" => Star
    | str => Word(str);

  let is_dot =
    fun
    | '.' => true
    | _ => false;

  let dot = skip_while(is_dot);

  let word =
    take_while1(
      fun
      | '.' => false
      | _ => true,
    )
    >>| str_to_word
    <* dot;

  let extract_topic = {
    let rec go = acc =>
      lift(x => [x, ...acc], word) >>= go <|> return(acc |> List.rev);
    word >>= (init => go([init]));
  };

  let extract_topic = str =>
    switch (parse_string(extract_topic, str)) {
    | Ok(v) => v
    | Error(msg) => failwith(msg)
    };
};

let parse_msg = raw => {
  let json = Basic.from_string(raw);
  let action = json |> member("action") |> to_string;
  let topic =
    json |> member("topic") |> to_string |> Topic_Parser.extract_topic;
  switch (action) {
  | "Publish" =>
    let msg = json |> member("payload") |> to_string;
    Publish((topic, msg));
  | "Subscribe" => Subscribe(topic)
  | "Unsubscibe" => Unsubscribe(topic)
  | unknown => failwith(unknown)
  };
};

let get_exchange =
  fun
  | ["exchange", exchange] => exchange
  | _ => failwith("a ws connection got past the url validation step");

let valid_path =
  fun
  | ["exchange", _] => true
  | _ => false;

let random_int32 = () => Random.int32(Int32.max_value);

let get_node_and_edges = (exchange, node_id) => {
  Hashtbl.(
    switch (find(exchange.topic_node, node_id)) {
    | Some(node) =>
      switch (find(exchange.topic_edge, node_id)) {
      | Some(edgetbl) => Some((node, edgetbl))
      | None => None
      }
    | None => None
    }
  );
};

let get_leaf = (exchange, topic) => {
  open Hashtbl;
  let rec aux = (topic', node: topic_node) =>
    switch (topic') {
    | [] => failwith("we should not get an empty topic list")
    | [hd, ...rest] =>
      let edge =
        find_exn(
          find_exn(exchange.topic_edge, node.id),
          string_of_word(hd),
        );
      List.is_empty(rest)
        ? (node, edge)
        : {
          let child_id = Option.value_exn(edge.child);
          aux(rest, find_exn(exchange.topic_node, child_id));
        };
    };
  aux(topic, find_exn(exchange.topic_node, exchange.root_node));
};

let get_edge_by_word = (tbl, word: string) => Hashtbl.find(tbl, word);

let get_bindings_tbl_exn = (exchange, node_id) =>
  Hashtbl.find_exn(exchange.bindings, node_id);

let get_bindings_tbl = (exchange, node_id) =>
  Hashtbl.find(exchange.bindings, node_id);

let get_bindings_by_edge_id = (tbl, id: int32) => Hashtbl.find(tbl, id);

let get_bindings_by_edge_id_exn = (tbl, id: int32) =>
  Hashtbl.find_exn(tbl, id);

let create_exchange = (name) => {
  let root_node_id = random_int32();
  let root = create_node(root_node_id);
  let topic_node =
    Hashtbl.create((module Int32), ~growth_allowed=true, ~size=500);
  Hashtbl.set(topic_node, ~key=root_node_id, ~data=root);
   let data = {
     name,
     root_node: root_node_id,
     topic_node,
     topic_edge:
       Hashtbl.create((module Int32), ~growth_allowed=true, ~size=1000),
     bindings: Hashtbl.create((module Int32), ~growth_allowed=true, ~size=500),
     connections:
       Hashtbl.create((module Int32), ~growth_allowed=true, ~size=500),
   };
   Hashtbl.set(
     data.topic_edge,
     ~key=root.id,
     ~data=Hashtbl.create((module String), ~growth_allowed=true),
   );
   Hashtbl.set(
     data.bindings,
     ~key=root.id,
     ~data=Hashtbl.create((module Int32), ~growth_allowed=true),
   );
   data;
};

let add_exchange = (exchange_table, name) => {
  switch (Hashtbl.find(exchange_table, name)) {
  | Some(exchange) => exchange
  | None =>
    let root_node_id = random_int32();
    let root = create_node(root_node_id);
    let topic_node =
      Hashtbl.create((module Int32), ~growth_allowed=true, ~size=500);
    Hashtbl.set(topic_node, ~key=root_node_id, ~data=root);
    let data = create_exchange(name);
    Hashtbl.set(exchange_table, ~key=name, ~data);
    data;
  };
};

let subscribe = (exchange, conn: connection, topic) => {
  open Hashtbl;
  let rec aux = (lst, edge) =>
    switch (lst) {
    | [] => ()
    | [hd] =>
      let word_str = string_of_word(hd);
      switch (edge.child) {
      | Some(id) =>
        let node = find_exn(exchange.topic_node, id);
        let node_edges = find_exn(exchange.topic_edge, id);
        switch (find(node_edges, word_str)) {
        | Some(edge) =>
          set(
            exchange.topic_node,
            ~key=id,
            ~data={...node, binding_count: node.binding_count + 1},
          );
          change(
            find_exn(exchange.bindings, id),
            edge.id,
            fun
            | Some(lst) => Some([conn.id, ...lst])
            | None => Some([conn.id]),
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
              binding_count: node.binding_count + 1,
            },
          );
          change(
            find_exn(exchange.bindings, id),
            new_edge.id,
            fun
            | Some(lst) => Some([conn.id, ...lst])
            | None => Some([conn.id]),
          );
        };
      | None =>
        let node =
          create_node(~edge_count=1, ~binding_count=1, random_int32());
        set(exchange.topic_node, ~key=node.id, ~data=node);
        change(
          find_exn(exchange.topic_edge, edge.parent),
          string_of_word(edge.word),
          fun
          | Some(e) => Some({...e, child: Some(node.id)})
          | None =>
            failwith("edge not found but logic should guarentee existence"),
        );
        let edge_hash = Hashtbl.create((module String), ~growth_allowed=true);
        let new_edge = create_edge(node.id, hd);
        set(edge_hash, ~key=word_str, ~data=new_edge);
        set(exchange.topic_edge, ~key=node.id, ~data=edge_hash);
        let binding_hash =
          Hashtbl.create((module Int32), ~growth_allowed=true);
        set(binding_hash, ~key=new_edge.id, ~data=[conn.id]);
        set(exchange.bindings, ~key=node.id, ~data=binding_hash);
      };
    | [hd, ...rest] =>
      let word_str = string_of_word(hd);
      switch (edge.child) {
      | Some(id) =>
        find_exn(exchange.topic_edge, id)
        |> find_exn(_, word_str)
        |> aux(rest)
      | None =>
        let node =
          create_node(~edge_count=1, ~binding_count=1, random_int32());
        set(exchange.topic_node, ~key=node.id, ~data=node);
        change(
          find_exn(exchange.topic_edge, edge.parent),
          string_of_word(edge.word),
          fun
          | Some(e) => Some({...e, child: Some(node.id)})
          | None =>
            failwith("edge not found but logic should guarentee existence"),
        );
        let edge_hash = Hashtbl.create((module String), ~growth_allowed=true);
        let edge = create_edge(node.id, hd);
        set(edge_hash, ~key=word_str, ~data=edge);
        set(exchange.topic_edge, ~key=node.id, ~data=edge_hash);
        aux(rest, edge);
      };
    };
  switch (topic) {
  | [] => ()
  | [hd, ...rest] =>
    let node = find_exn(exchange.topic_node, exchange.root_node);
    let node_edges = find_exn(exchange.topic_edge, exchange.root_node);
    switch (find(node_edges, string_of_word(hd))) {
    | Some(edge) =>
      if (List.is_empty(rest)) {
        set(
          exchange.topic_node,
          ~key=exchange.root_node,
          ~data={...node, binding_count: node.binding_count + 1},
        );
        change(
          find_exn(exchange.bindings, exchange.root_node),
          edge.id,
          fun
          | Some(lst) => Some([conn.id, ...lst])
          | None => Some([conn.id]),
        );
      } else {
        aux(rest, edge);
      };

    | None =>
      let edge = create_edge(exchange.root_node, hd);
      set(node_edges, ~key=string_of_word(hd), ~data=edge);
      if (List.is_empty(rest)) {
        set(
          exchange.topic_node,
          ~key=exchange.root_node,
          ~data={
            ...node,
            edge_count: node.edge_count + 1,
            binding_count: node.binding_count + 1,
          },
        );
        change(
          find_exn(exchange.bindings, exchange.root_node),
          edge.id,
          fun
          | Some(lst) => Some([conn.id, ...lst])
          | None => Some([conn.id]),
        );
      } else {
        set(
          exchange.topic_node,
          ~key=exchange.root_node,
          ~data={...node, edge_count: node.edge_count + 1},
        );
        aux(rest, edge);
      };
    };
  };
};

let unsubscribe = (exchange, conn: connection, topic) => {
  open Hashtbl;

  let (node, edge) = get_leaf(exchange, topic);
  let is_root = Int32.(node.id == exchange.root_node);
  // find bindings list and remove connection
  change(
    find_exn(exchange.bindings, node.id),
    edge.id,
    ~f=
      fun
      | None => None
      | Some(lst) => Some(List.filter(lst, ~f=id => Int32.(id != conn.id))),
  );
  let bindings_list =
    find_exn(find_exn(exchange.bindings, node.id), edge.id);
  let remove_edge = List.is_empty(bindings_list);
  // if bindings list is empty remove edge hash
  if (remove_edge) {
    remove(find_exn(exchange.bindings, node.id), edge.id);
    remove(
      find_exn(exchange.topic_edge, node.id),
      string_of_word(edge.word),
    );
    /* if node bindings hash is empty and node is not root  remove node hash*/
    if (is_empty(find_exn(exchange.bindings, node.id)) && !is_root) {
      remove(exchange.bindings, node.id);
    };
  };
  // update node update edge count when edge is removed and this is the root node. If not root this is done later
  let updated_node = {
    ...node,
    edge_count: remove_edge && is_root ? node.edge_count - 1 : node.edge_count,
    binding_count: node.binding_count - 1,
  };
  set(exchange.topic_node, ~key=updated_node.id, ~data=updated_node);
  let rec aux = (node: topic_node, edge: topic_edge, topic) => {
    switch (topic) {
    | [] => ()
    | _lst =>
      let has_bindings =
        switch (find(exchange.bindings, node.id)) {
        | None => false
        | Some(hash) =>
          switch (find(hash, edge.id)) {
          | Some(_lst) => true
          | None => false
          }
        };
      /* if edge has bindings we are done */
      has_bindings
        ? ()
        : {
          remove(
            find_exn(exchange.topic_edge, node.id),
            string_of_word(edge.word),
          );
          let updated_node = {...node, edge_count: node.edge_count - 1};
          // substract edge_count from node
          set(exchange.topic_node, ~key=updated_node.id, ~data=updated_node);
          /* if node still has edges or is root we are done*/
          switch (
            is_empty(find_exn(exchange.topic_edge, node.id))
            && Int32.(node.id != exchange.root_node)
          ) {
          | false => ()
          | true =>
            // remove node hash in topic edge
            remove(exchange.topic_edge, node.id);
            let topic' = List.rev(topic) |> List.tl_exn |> List.rev;
            // get next leaf
            let (node, edge) = get_leaf(exchange, topic');
            // set the edge child to None and recursivly call aux
            set(
              find_exn(exchange.topic_edge, node.id),
              ~key=string_of_word(edge.word),
              ~data={...edge, child: None},
            );
            aux(node, edge, topic');
          };
        };
    };
  };

  /* if edge has node we are done */
  switch (edge.child) {
  | Some(_id) => ()
  | None =>
    // if root node done
    if (!is_root) {
      let topic' = List.rev(topic) |> List.tl_exn |> List.rev;
      aux(node, edge, topic');
    }
  };
};

let publish = (exchange, (topic, msg)) => {
  let edges_to_ids = edges =>
    List.map(edges, ((node_id, edge_id)) =>
      get_bindings_by_edge_id_exn(
        get_bindings_tbl_exn(exchange, node_id),
        edge_id,
      )
    )
    |> List.concat_no_order;
  let identity =
    fun
    | None => None
    | Some(_) as v => v;
  let rec aux = (lst, nodes, edges) =>
    switch (lst) {
    | [] => edges_to_ids(edges)
    | [hd, ...rest] =>
      List.is_empty(nodes)
        ? edges_to_ids(edges)
        : {
          let word = string_of_word(hd);
          let lastword = List.is_empty(rest);
          let (nodes, newedges) =
            List.map(nodes, ~f=id =>
              switch (get_node_and_edges(exchange, id)) {
              | None => [None]
              | Some((node, edgetbl)) =>
                let exact =
                  switch (get_edge_by_word(edgetbl, word)) {
                  | None => None
                  | Some(edge) =>
                    lastword
                      ? Some((None, Some((node.id, edge.id))))
                      : (
                        switch (edge.child) {
                        | Some(id) => Some((Some(id), None))
                        | None => None
                        }
                      )
                  };
                let star =
                  switch (get_edge_by_word(edgetbl, "*")) {
                  | None => None
                  | Some(edge) =>
                    lastword
                      ? Some((None, Some((node.id, edge.id))))
                      : (
                        switch (edge.child) {
                        | Some(id) => Some((Some(id), None))
                        | None => None
                        }
                      )
                  };
                let hashtag =
                  switch (get_edge_by_word(edgetbl, "#")) {
                  | None => None
                  | Some(edge) =>
                    lastword
                      ? Some((None, Some((node.id, edge.id))))
                      : (
                        switch (
                          get_bindings_by_edge_id(
                            get_bindings_tbl_exn(exchange, node.id),
                            edge.id,
                          )
                        ) {
                        | None =>
                          switch (edge.child) {
                          | Some(_id) => Some((Some(node.id), None))
                          | None => None
                          }
                        | Some(_bindings) =>
                          switch (edge.child) {
                          | Some(_id) =>
                            Some((Some(node.id), Some((node.id, edge.id))))
                          | None => Some((None, Some((node.id, edge.id))))
                          }
                        }
                      )
                  };
                [exact, star, hashtag];
              }
            )
            |> List.concat_no_order
            |> List.filter_map(~f=identity)
            |> List.unzip
            |> (
              ((a, b)) => (
                List.filter_map(a, ~f=identity),
                List.filter_map(b, ~f=identity),
              )
            );
          aux(rest, nodes, List.unordered_append(edges, newedges));
        }
    };
  let payload =
    Printf.sprintf(
      "{\"topic\": \"%s\", \"payload\": \"%s\"}",
      string_of_topic(topic),
      msg,
    );
  List.iter(
    aux(topic, [exchange.root_node], [])
    |> List.dedup_and_sort(~compare=Int32.compare),
    id =>
    Hashtbl.find_exn(exchange.connections, id).send(payload)
  );
};
