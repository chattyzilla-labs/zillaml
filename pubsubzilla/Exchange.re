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
  
  include Exchange;

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
     let subscribe_old = (~connection, ~topic) => {
       switch%bind (Rpc.Pipe_rpc.dispatch(subscribe_rpc, connection, topic)) {
       | Error(err) => Error.raise(err)
       | Ok(Error(s)) =>
         eprintf("subscribe failed: %s\n", s);
         return(Error(s));
       | Ok(Ok((pipe, _id))) => return(Ok(pipe))
       };
     };

    let subscribe = (~connection, ~topic, ~f) => {
      switch%bind (Rpc.Pipe_rpc.dispatch_iter(subscribe_rpc, connection, topic, ~f)) {
      | Error(err) => Error.raise(err)
      | Ok(Error(s)) =>
        eprintf("subscribe failed: %s\n", s);
        return(Error(s));
      | Ok(Ok(id)) => return(Ok(id))
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