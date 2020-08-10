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
  module Group = Rpc.Pipe_rpc.Direct_stream_writer.Group;
  type t = {
    node_type,
    children: Core.Hashtbl.t(Topic.Directory.t, t),
    group: Group.t(Message.t),
    mutable last_message: option(Message.t),
  }
  and node_type =
    | Root
    | Leaf(t, Topic.Directory.t);


  let version = 1;

  let create = () => {
    node_type: Root,
    children:
      Hashtbl.create(
        (module Topic.Directory),
        ~growth_allowed=true,
        ~size=500,
      ),
    group: Group.create(),
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
      Hashtbl.is_empty(node.children) && Group.length(node.group) == 0
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

  let close_subscriber_streams = t => 
    List.iter(t.group |> Group.to_list, ~f=Rpc.Pipe_rpc.Direct_stream_writer.close);


  /* https://github.com/janestreet/async_rpc_kernel/blob/master/src/rpc.ml#L566*/
  let subscribe = (root, topic, writer) => {
    open Rpc.Pipe_rpc.Direct_stream_writer;
    let rec aux = (topic, node) => {
      switch (topic) {
      | [] =>
        Group.add_exn(node.group, writer);
        switch (node.last_message) {
        | None => ()
        | Some(msg) => 
          let _value = Rpc.Pipe_rpc.Direct_stream_writer.write_without_pushback(writer, msg)
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
                group: Group.create(),
                last_message: None,
              };
            },
          );
        aux(tl, next_node);
      };
    };
    aux(topic, root);
    Ok();
  };

  let publish = (root, topic, msg) => {
    let rec aux = (t, nodes) => {
      switch (t) {
      | [] =>
        List.iter(
          nodes,
          ~f=node => {
            node.last_message = Some(msg);
            Group.write_without_pushback(node.group, msg)
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
                          group: Group.create(),
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
      close_subscriber_streams(node);
      /* clean up un-needed nodes */
      clean_unused_nodes(node);
    };
  };
};