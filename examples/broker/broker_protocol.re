open Core;
open Async;

module Username: Identifiable = String;
module Topic: Identifiable = String;

module Message = {
  [@deriving (sexp, bin_io, compare)]
  type t = {
    text: string,
    topic: Topic.t,
    from: Username.t,
    time: Time.t,
  };
};

module Dump = {
  [@deriving (sexp, bin_io, compare)]
  type single = {
    message: Message.t,
    num_subscribers: int,
  };
  [@deriving (sexp, bin_io, compare)]
  type t = list(single);
};

let publish_rpc =
  Rpc.Rpc.create(
    ~name="publish",
    ~version=0,
    ~bin_query=Message.bin_t,
    ~bin_response=Unit.bin_t,
  );

let subscribe_rpc =
  Rpc.Pipe_rpc.create(
    (),
    ~name="subscribe",
    ~version=0,
    ~bin_query=Topic.bin_t,
    ~bin_response=Message.bin_t,
    ~bin_error=String.bin_t,
  );

let dump_rpc =
  Rpc.Rpc.create(
    ~name="dump",
    ~version=0,
    ~bin_query=Unit.bin_t,
    ~bin_response=Dump.bin_t,
  );

let shutdown_rpc =
  Rpc.Rpc.create(
    ~name="shutdown",
    ~version=0,
    ~bin_query=Unit.bin_t,
    ~bin_response=Unit.bin_t,
  );

let clear_rpc =
  Rpc.Rpc.create(
    ~name="clear",
    ~version=0,
    ~bin_query=Topic.bin_t,
    ~bin_response=Unit.bin_t,
  );
