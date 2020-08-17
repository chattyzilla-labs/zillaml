open Core;
open Async;
open Exchange;

/**
 * TODO: 
 *  phantom typing for directory type for more efficient encoding of hashtags and stars
 */
module Make = (Message: Message) => {
  module Topic: {
    [@deriving (sexp, hash, bin_io, compare)]
    type t;
    let to_string: t => string;
    module Directory:{ 
      [@deriving (sexp, hash, bin_io, compare)]
      type t;
    };
    let hashtag: Directory.t;
    let star: Directory.t;
    let dir: string => Directory.t;
    let (>>): (Directory.t, Directory.t) => t; 
  } = {
    module Directory:{ 
      [@deriving (sexp, hash, bin_io, compare)]
      type t;
      let hashtag: t;
      let star: t;
      let string_of_directory: t => string;
      let dir: string => t;
    } = {
      [@deriving (sexp, hash, bin_io, compare)]
      type t = string;
      let hashtag = "#";
      let star = "*";
      let string_of_directory = s => s;
      // need to throw error if string contains dot
      let dir = str => sprintf("<%s>", str);
    };

    let hashtag = Directory.hashtag;
    let star = Directory.star;
    let string_of_directory = Directory.string_of_directory;
    let dir = Directory.dir;

    [@deriving (sexp, bin_io, hash, compare)]
    type t = string;
  
    let (>>) = (a, b) => sprintf("%s.%s", string_of_directory(a), string_of_directory(b));

    let to_string = a => a;
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

  module Topic_cache = Hash_queue.Make(Topic);

  let max_cache_size = 500;

  let topic_cache = lazy(Topic_cache.create())

  let remove_topic_from_cache = topic => 
    ignore(
      Topic_cache.remove(
        Lazy.force(topic_cache), 
        topic
      ) : [ `No_such_key | `Ok ]
    )
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

  let rec get_leaf' = (root, topic) => {
    switch (topic) {
    | [] => Some(root)
    | [hd, ...tl] =>
      switch (Hashtbl.find(root.children, hd)) {
      | None => None
      | Some(node) => get_leaf'(node, tl)
      }
    };
  };

  /**
   * Memorization logic taken from the lru function in core_kernal memo
   * https://github.com/janestreet/core_kernel/blob/master/src/memo.ml#L46
   */
  let get_leaf = (root, topic) => {
    let cache = Lazy.force(topic_cache)
    switch (Topic_cache.lookup_and_move_to_back(cache, topic)) {
    | Some(result) => Some(result)
    | None => 
      let result = get_leaf'(root, topic);
      switch (result) {
      | None => ()
      | Some(node) => 
        Topic_cache.enqueue_back_exn(cache, topic, node);
        /* eject least recently used cache entry */
        if (Topic_cache.length(cache) > max_cache_size) {
          ignore(Topic_cache.dequeue_front_exn(cache): t);
        };
      };
      result;
    }
  };

  let close_subscriber_streams = t => 
    List.iter(t.group |> Group.to_list, ~f=Rpc.Pipe_rpc.Direct_stream_writer.close);
  
  let close_topic = (~root, ~topic, ~closing_msg) => {
    switch (get_leaf(root, topic)) {
    | None => return()
    | Some(node) =>
      let%bind () = switch (closing_msg) {
      | None => return()
      | Some(msg) => 
        Group.write(node.group, msg)
      };
      /* clean up topic cache to avoid  */
      remove_topic_from_cache(topic);
      /* unsubscribe all subscriber*/
      close_subscriber_streams(node);
      /* clean up un-needed nodes */
      clean_unused_nodes(node);
      return()
    };
  };

  let find_or_add_topic = (root, topic) => {
    let rec aux = (topic, node) => {
      switch (topic) {
      | [] => node
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
                  ~size=50,
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
    switch (get_leaf(root, topic)) {
    | None => aux(topic, root)
    | Some(node) => node
    };
  };

  let add_topic = (root, topic) => ignore(find_or_add_topic(root, topic) : t);

  /* https://github.com/janestreet/async_rpc_kernel/blob/master/src/rpc.ml#L566*/
  let subscribe = (root, topic, writer) => {
    open Rpc.Pipe_rpc.Direct_stream_writer;
    let node = find_or_add_topic(root, topic);
    Group.add_exn(node.group, writer);
    closed(writer) >>> () => if(Group.length(node.group) == 0) {
      remove_topic_from_cache(topic)
      clean_unused_nodes(node)
    }
    switch (node.last_message) {
    | None => Ok()
    | Some(msg) => Ok(ignore(write_without_pushback(writer, msg) : [ `Closed | `Ok ]))
    };
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
                  let nodes = switch (Hashtbl.find(node.children, hd)) {
                  | None => []
                  | Some(node) => [node]
                  };
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
};