open Core;
open Async;

module type Topic = {
  [@deriving (sexp, bin_io, compare)]
  type t;
  let to_string: t => string;
};

module type Message = {
  [@deriving (sexp, bin_io, compare)]
  type t;
};

module type Exchange = {
  
  module Topic: Topic;

  module Message: Message;
  
  type t;

  let version: int;

  let create: unit => t;

  let publish: (t, Topic.t, Message.t) => unit;

  let subscribe: (t, Topic.t) => Result.t(Pipe.Reader.t(Message.t), string);

  let clear_topic: (t, Topic.t) => unit;
};

// add all the protocol code here using the methods from Exhange and client and server modules with rpc implementation
module MakeExchange = (Exchange: Exchange) => {

  [@deriving (sexp, bin_io, compare)]
  type payload = {
    topic: Exchange.Topic.t,
    message: Exchange.Message.t
  };

  let publish_rpc =
    Rpc.Rpc.create(
      ~name="publish",
      ~version=Exchange.version,
      ~bin_query=bin_payload,
      ~bin_response=Unit.bin_t,
    );

  let subscribe_rpc =
    Rpc.Pipe_rpc.create(
      (),
      ~name="subscribe",
      ~version=Exchange.version,
      ~bin_query=Exchange.Topic.bin_t,
      ~bin_response=Exchange.Message.bin_t,
      ~bin_error=String.bin_t,
    );

  let clear_rpc =
    Rpc.Rpc.create(
      ~name="clear",
      ~version=Exchange.version,
      ~bin_query=Exchange.Topic.bin_t,
      ~bin_response=Unit.bin_t,
    );

  module Client = {

     let get_connection = (~host, ~port) => {
       let host_and_port = Host_and_port.create(~host, ~port);
       let event:
         Persistent_connection.Rpc.Event.t => Deferred.t(unit) =
         fun
         | Obtained_address(_) => {
             printf("(Obtained_address <elided>)\n");
             return();
           }
         | event => {
             print_s([%sexp (event: Persistent_connection.Rpc.Event.t)]);
             return();
           };
       let unversioned_conn =
         Persistent_connection.Rpc.create'(
           ~on_event=event, ~server_name="Exchange rpc", () =>
           return(Ok(host_and_port))
         );
       Persistent_connection.Rpc.connected(unversioned_conn);
     };

     let publish = (~connection, ~topic, ~message) => {
       Rpc.Rpc.dispatch_exn(publish_rpc, connection, {topic, message });
     };

    // https://github.com/janestreet/async_rpc_kernel/blob/master/src/rpc.mli#L566
     let subscribe = (~connection, ~topic) => {
       switch%bind (Rpc.Pipe_rpc.dispatch(subscribe_rpc, connection, topic)) {
       | Error(err) => Error.raise(err)
       | Ok(Error(s)) =>
         eprintf("subscribe failed: %s\n", s);
         return(Error(s));
       | Ok(Ok((pipe, _id))) => return(Ok(pipe))
       };
     };

     let clear = (~connection, ~topic) => {
       Rpc.Rpc.dispatch_exn(clear_rpc, connection, topic);
     };
     
  };
  
  module Server = {
  
    let publish_impl = (exchange, payload) => {
      Log.Global.sexp(~level=`Debug, [%sexp (payload.message: Exchange.Message.t)]);
      return(Exchange.publish(exchange, payload.topic, payload.message));
    };

    let subscribe_impl = (exchange, topic) =>
      return(Exchange.subscribe(exchange, topic));


    let clear_impl = (exchange, topic) => {
      Log.Global.info("Clearing topic %s", Exchange.Topic.to_string(topic));
      Exchange.clear_topic(exchange, topic);
      return();
    };

    /* We then create a list of all the implementations we're going to support in
       the server. */

    let implementations = [
      Rpc.Rpc.implement(publish_rpc, publish_impl),
      Rpc.Pipe_rpc.implement(subscribe_rpc, subscribe_impl),
      Rpc.Rpc.implement(clear_rpc, clear_impl),
    ];

    /** TODO add ability for custom implementations*/

    let server = (~port) => {
      let exchange = Exchange.create()
      let implementations =
        Rpc.Implementations.create_exn(
          ~implementations,
          ~on_unknown_rpc=
            `Call(
              (_, ~rpc_tag, ~version) => {
                Log.Global.info(
                  "Unexpected RPC, tag %s, version %d",
                  rpc_tag,
                  version,
                );
                `Continue;
              },
            ),
        );
      let%bind _ =
        Rpc.Connection.serve(
          ~implementations,
          ~initial_connection_state=(_addr, _conn) => exchange,
          ~where_to_listen=Tcp.Where_to_listen.of_port(port),
          (),
        );
      Deferred.return();
    };
  };
};


module TopicRouterExchange = (Message: Message) => {

  module Topic = {
    module Directory = {
      [@deriving (sexp, hash, bin_io, compare)]
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
    node_type: node_type,
    children: Core.Hashtbl.t(Topic.Directory.t, t),
    mutable subscribers: list(Pipe.Writer.t(Message.t)),
    mutable last_message: option(Message.t),
  } and node_type = Root | Leaf(t, Topic.Directory.t);

  type topic_meta = {
    subscriber_count: int,
    last_message: option(Message.t),
  };

  let version = 1;

  let create = () => {
    node_type: Root,
    children:
      Hashtbl.create((module Topic.Directory), ~growth_allowed=true, ~size=500),
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
      switch(topic){
      | [] => {
        node.subscribers = [w, ...node.subscribers];
        switch (node.last_message) {
        | None => ()
        | Some(msg) => don't_wait_for(Pipe.write(w, msg))
        };
      }
      | [hd, ...tl] => 
        let next_node = Hashtbl.find_or_add(node.children, hd, ~default=() => {
          let children =
            Hashtbl.create((module Topic.Directory), ~growth_allowed=true, ~size=500);
          { node_type: Leaf(node, hd), children, subscribers: [], last_message: None}
        });
        aux(tl, next_node)
      };
    };
    aux(topic, root);
    Ok(r);
  };

  let publish = (root, topic, msg) => {
    let rec aux = (t, nodes) => {
      switch (t) {
      | [] =>
        List.iter(nodes, ~f=node => {
           clear_closed(node);
           node.last_message = Some(msg);
           List.iter(node.subscribers, ~f=pipe => don't_wait_for(Pipe.write(pipe, msg)));
        });

      | [hd, ...tl] => {
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
                  | Star => {
                    let nodes =
                      switch (Hashtbl.find(node.children, Star)) {
                      | None => nodes
                      | Some(node) => [node, ...nodes]
                      };
                    switch (Hashtbl.find(node.children, Hashtag)) {
                    | None => nodes
                    | Some(node) => [node, ...nodes]
                    };
                  }
                  | Word(_) as hd => {
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
                  }
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
    };
    aux(topic, [root]);
  };

  let clear_topic = (root, topic) => {
    switch (get_leaf(root, topic)) {
    | None => ()
    | Some(node) => {
        /* unsubscribe all subscriber*/
        close_subscriber_pipes(node);
        /* clean up un-needed nodes */
        clean_unused_nodes(node);
      };
    };
  };

  let get_topic_meta = (root, topic) => {
    switch (get_leaf(root, topic)) {
    | None => ()
    | Some(node) =>
      /* unsubscribe all subscriber*/
      close_subscriber_pipes(node);
      /* clean up un-needed nodes */
      
    };
  };
};






