/** A directory for storing last-values for a basic pub/sub system */;

open! Core;
open Async;
open Broker_protocol;

type t;

/** Creates an empty directory */

let create: unit => t;

/** Publish a new message */

let publish: (t, Message.t) => unit;

/** If the topic is unknown, then None is returned */

let subscribe: (t, Topic.t) => option(Pipe.Reader.t(Message.t));

/** Creates a dump of the current state of directory */

let dump: t => Dump.t;

let clear_topic: (t, Topic.t) => unit;
