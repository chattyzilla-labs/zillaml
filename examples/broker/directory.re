open Core;
open Async;
open Broker_protocol;

/* A publisher for a single topic */
module Topic_pub: {
  type t;
  let create: Message.t => t;

  let publish: (t, Message.t) => unit;
  let subscribe: t => Pipe.Reader.t(Message.t);
  let num_subscribers: t => int;
  let last_message: t => Message.t;
  let close_subscriber_pipes: t => unit;
} = {
  type t = {
    mutable last_message: Message.t,
    mutable subscribers: list(Pipe.Writer.t(Message.t)),
  };

  let last_message = t => t.last_message;

  let create = last_message => {last_message, subscribers: []};

  let clear_closed = t =>
    t.subscribers =
      List.filter(t.subscribers, ~f=pipe => !Pipe.is_closed(pipe));

  let close_subscriber_pipes = t => {
    List.iter(t.subscribers, ~f=Pipe.close);
    clear_closed(t);
  };

  let publish = (t, msg) => {
    clear_closed(t);
    t.last_message = msg;
    List.iter(t.subscribers, ~f=pipe =>
      don't_wait_for(Pipe.write(pipe, msg))
    );
  };

  let subscribe = t => {
    let (r, w) = Pipe.create();
    don't_wait_for(Pipe.write(w, t.last_message));
    t.subscribers = [w, ...t.subscribers];
    r;
  };

  let num_subscribers = t => List.length(t.subscribers);
};

type t = Hashtbl.t(Topic.t, Topic_pub.t);

let create = () => Topic.Table.create();

let clear_topic = (t, topic) =>
  switch (Hashtbl.find(t, topic)) {
  | None => ()
  | Some(s) =>
    Topic_pub.close_subscriber_pipes(s);
    Hashtbl.remove(t, topic);
  };

let publish = (t, message) => {
  let s =
    Hashtbl.find_or_add(t, message.Message.topic, ~default=() =>
      Topic_pub.create(message)
    );

  Topic_pub.publish(s, message);
};

let subscribe = (t, topic) =>
  Option.map(Hashtbl.find(t, topic), ~f=Topic_pub.subscribe);

let dump = t =>
  Hashtbl.data(t)
  |> List.map(~f=tpub => {
       let num_subscribers = Topic_pub.num_subscribers(tpub);
       let message = Topic_pub.last_message(tpub);
       {Dump.num_subscribers, message};
     });
