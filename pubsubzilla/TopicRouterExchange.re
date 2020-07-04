open Core;
open Async;
open Exchange;

module Make = (Message: Message) => {
  module Topic = {
    module Directory = {
      [@deriving (sexp, hash, bin_io, compare, yojson)]
      type t =
        | Hashtag
        | Star
        | Word(string);
    };

    [@deriving (sexp, bin_io, compare)]
    type t = list(Directory.t);

    let string_of_directory =
      fun
      | Directory.Hashtag => "#"
      | Star => "*"
      | Word(str) => str;

    let to_string =
      fun
      | [] => ""
      | [hd, ...rest] =>
        List.fold(rest, ~init=string_of_directory(hd), ~f=(a, b) =>
          a ++ "/" ++ string_of_directory(b)
        );
  };

  module Message = Message;

  type t = {
    node_type,
    children: Core.Hashtbl.t(Topic.Directory.t, t),
    mutable subscribers: list(Pipe.Writer.t(Message.t)),
    mutable last_message: option(Message.t),
  }
  and node_type =
    | Root
    | Leaf(t, Topic.Directory.t);

  type topic_meta = {
    subscriber_count: int,
    last_message: option(Message.t),
  };

  let version = 1;

  let create = () => {
    node_type: Root,
    children:
      Hashtbl.create(
        (module Topic.Directory),
        ~growth_allowed=true,
        ~size=500,
      ),
    subscribers: [],
    last_message: None,
  };

  /*
     if hash table is empty
     and subscribers list is empty
     and current node is not the root
     remove directory from parent hash table recall aux function with parent node
   */
  let rec clean_unused_nodes = node => {
    switch (
      Hashtbl.is_empty(node.children) && List.length(node.subscribers) == 0
    ) {
    | false => ()
    | true =>
      switch (node.node_type) {
      | Root => ()
      | Leaf(parent, dir) =>
        Hashtbl.remove(parent.children, dir);
        clean_unused_nodes(parent);
      }
    };
  };

  let rec get_leaf = (root, topic) => {
    switch (topic) {
    | [] => Some(root)
    | [hd, ...tl] =>
      switch (Hashtbl.find(root.children, hd)) {
      | None => None
      | Some(node) => get_leaf(node, tl)
      }
    };
  };

  let clear_closed = t =>
    t.subscribers =
      List.filter(t.subscribers, ~f=pipe => !Pipe.is_closed(pipe));

  let close_subscriber_pipes = t => {
    List.iter(t.subscribers, ~f=Pipe.close);
    clear_closed(t);
  };

  /* https://github.com/janestreet/async_rpc_kernel/blob/master/src/rpc.ml#L566*/
  // TODO  Use Pipe close deferred to clean up on connection closes. Look into create reader an dcreate writer
  let subscribe = (root, topic) => {
    let (r, w) = Pipe.create();
    let rec aux = (topic, node) => {
      switch (topic) {
      | [] =>
        node.subscribers = [w, ...node.subscribers];
        switch (node.last_message) {
        | None => ()
        | Some(msg) => don't_wait_for(Pipe.write(w, msg))
        };
      | [hd, ...tl] =>
        let next_node =
          Hashtbl.find_or_add(
            node.children,
            hd,
            ~default=() => {
              let children =
                Hashtbl.create(
                  (module Topic.Directory),
                  ~growth_allowed=true,
                  ~size=500,
                );
              {
                node_type: Leaf(node, hd),
                children,
                subscribers: [],
                last_message: None,
              };
            },
          );
        aux(tl, next_node);
      };
    };
    aux(topic, root);
    Ok(r);
  };

  let publish = (root, topic, msg) => {
    let rec aux = (t, nodes) => {
      switch (t) {
      | [] =>
        List.iter(
          nodes,
          ~f=node => {
            clear_closed(node);
            node.last_message = Some(msg);
            List.iter(node.subscribers, ~f=pipe =>
              don't_wait_for(Pipe.write(pipe, msg))
            );
          },
        )

      | [hd, ...tl] =>
        let nodes =
          List.fold_left(
            nodes,
            ~init=[],
            ~f=(nodes, node) => {
              let nodes = {
                switch (hd) {
                | Topic.Directory.Hashtag =>
                  switch (Hashtbl.find(node.children, Hashtag)) {
                  | None => nodes
                  | Some(node) => [node, ...nodes]
                  }
                | Star =>
                  let nodes =
                    switch (Hashtbl.find(node.children, Star)) {
                    | None => nodes
                    | Some(node) => [node, ...nodes]
                    };
                  switch (Hashtbl.find(node.children, Hashtag)) {
                  | None => nodes
                  | Some(node) => [node, ...nodes]
                  };
                | Word(_) as hd =>
                  let next_node =
                    Hashtbl.find_or_add(
                      node.children,
                      hd,
                      ~default=() => {
                        let children =
                          Hashtbl.create(
                            (module Topic.Directory),
                            ~growth_allowed=true,
                            ~size=500,
                          );
                        {
                          node_type: Leaf(node, hd),
                          children,
                          subscribers: [],
                          last_message: None,
                        };
                      },
                    );
                  let nodes = [next_node];
                  let nodes =
                    switch (Hashtbl.find(node.children, Star)) {
                    | None => nodes
                    | Some(node) => [node, ...nodes]
                    };
                  switch (Hashtbl.find(node.children, Hashtag)) {
                  | None => nodes
                  | Some(node) => [node, ...nodes]
                  };
                };
              };

              switch (node.node_type) {
              | Root => nodes
              | Leaf(_, dir) =>
                switch (dir) {
                | Hashtag => [node, ...nodes]
                | Star
                | Word(_) => nodes
                }
              };
            },
          );
        aux(tl, nodes);
      };
    };
    aux(topic, [root]);
  };

  let clear_topic = (root, topic) => {
    switch (get_leaf(root, topic)) {
    | None => ()
    | Some(node) =>
      /* unsubscribe all subscriber*/
      close_subscriber_pipes(node);
      /* clean up un-needed nodes */
      clean_unused_nodes(node);
    };
  };

  let get_topic_meta = (root, topic) => {
    switch (get_leaf(root, topic)) {
    | None => ()
    | Some(node) =>
      /* unsubscribe all subscriber*/
      close_subscriber_pipes(node)
    /* clean up un-needed nodes */
    };
  };
};