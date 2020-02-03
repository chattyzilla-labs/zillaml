open Core;
open Async;

module Username: Identifiable;
module Topic: Identifiable;

module Message: {
  [@deriving (sexp, bin_io, compare)]
  type t = {
    text: string,
    topic: Topic.t,
    from: Username.t,
    time: Time.t,
  };
};

module Dump: {
  [@deriving (sexp, bin_io, compare)]
  type single = {
    message: Message.t,
    num_subscribers: int,
  };
  [@deriving (sexp, bin_io, compare)]
  type t = list(single);
};

let publish_rpc: Rpc.Rpc.t(Message.t, unit);
let subscribe_rpc: Rpc.Pipe_rpc.t(Topic.t, Message.t, String.t);
let dump_rpc: Rpc.Rpc.t(unit, Dump.t);
let shutdown_rpc: Rpc.Rpc.t(unit, unit);
let clear_rpc: Rpc.Rpc.t(Topic.t, unit);
