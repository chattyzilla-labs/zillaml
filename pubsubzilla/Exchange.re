open Core;
open Async;

module type Topic = {
  [@deriving (sexp, bin_io, hash, compare)]
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

  let subscribe: (t, Topic.t, Rpc.Pipe_rpc.Direct_stream_writer.t(Message.t)) => Result.t(unit, string);

  let close_topic: (~root:t, ~topic:Topic.t, ~closing_msg:option(Message.t)) => Deferred.t(unit);

  let add_topic: (t, Topic.t) => unit;
};

// TODO: implement a version of this functor that uses Rpc.Expert implementation for pub;ish messages in Server module to avoid allocating the message payload to an ocaml type via bin_prot if the message is just going to be serialized and  sent to a subscriber
// TODO: LOOK into using rpc parrallel for parallel processing of topic meta and what not
//  https://github.com/janestreet/rpc_parallel/blob/master/example/stream_workers.ml
//https://ocaml.janestreet.com/ocaml-core/latest/doc/bin_prot/Bin_prot/Blob/index.html to avoid uneeded serialization
// https://github.com/janestreet/core_kernel/blob/master/bus/src/bus.mli for client side optimization to avoid multiple subscriptions to the same topic
module MakeExchange = (Exchange: Exchange) => {
  
  include Exchange;

  [@deriving (sexp, bin_io, compare)]
  type publish_payload = {
    topic: Exchange.Topic.t,
    message: Exchange.Message.t
  };

  [@deriving (sexp, bin_io, compare)]
  type close_topic_payload = {
    topic: Exchange.Topic.t,
    closing_msg: option(Exchange.Message.t)
  };

  let publish_rpc =
    Rpc.Rpc.create(
      ~name="publish",
      ~version=Exchange.version,
      ~bin_query=bin_publish_payload,
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

  let close_rpc =
    Rpc.Rpc.create(
      ~name="close",
      ~version=Exchange.version,
      ~bin_query=bin_close_topic_payload,
      ~bin_response=Unit.bin_t,
    );
  let add_rpc =
    Rpc.Rpc.create(
      ~name="add",
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
       Rpc.Rpc.dispatch_exn(publish_rpc, connection, { topic, message });
     };

    // https://github.com/janestreet/async_rpc_kernel/blob/master/src/rpc.mli#L566

    let subscribe = (~connection, ~topic, ~f) => {
      switch%bind (Rpc.Pipe_rpc.dispatch_iter(subscribe_rpc, connection, topic, ~f)) {
      | Error(err) => Error.raise(err)
      | Ok(Error(s)) =>
        eprintf("subscribe failed: %s\n", s);
        return(Error(s));
      | Ok(Ok(id)) => return(Ok(id))
      };
    };

     let close = (~connection, ~topic, ~closing_msg) => {
       Rpc.Rpc.dispatch_exn(close_rpc, connection, { topic, closing_msg });
     };

     let add_topic = (~connection, ~topic) => {
       Rpc.Rpc.dispatch_exn(add_rpc, connection, topic);
     };
     
  };
    
  module Server = {
  
    let publish_impl = (exchange, payload) => {
      Log.Global.sexp(~level=`Debug, [%sexp (payload.message: Exchange.Message.t)]);
      return(Exchange.publish(exchange, payload.topic, payload.message));
    };


    let subscribe_impl = (exchange, topic, writer) =>
      return(Exchange.subscribe(exchange, topic, writer));

    let close_impl = (exchange, payload) => {
      Log.Global.info("Clearing topic %s", Exchange.Topic.to_string(payload.topic));
      Exchange.close_topic(~root=exchange, ~topic=payload.topic, ~closing_msg=payload.closing_msg);
    };

    let add_impl = (exchange, topic) => {
      Log.Global.info("Clearing topic %s", Exchange.Topic.to_string(topic));
      Exchange.add_topic(exchange, topic) |> return;
    };

    /* We then create a list of all the implementations we're going to support in
       the server. */

    let implementations = [
      Rpc.Rpc.implement(publish_rpc, publish_impl),
      Rpc.Pipe_rpc.implement_direct(subscribe_rpc, subscribe_impl),
      Rpc.Rpc.implement(close_rpc, close_impl),
      Rpc.Rpc.implement(add_rpc, add_impl),
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