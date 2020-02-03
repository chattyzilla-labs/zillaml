/**
 *  url to ampq standards https://www.rabbitmq.com/tutorials/amqp-concepts.html
 *  implent protocoll step by step start with simple in  memory cacche and  simple message queue for messages that are not subscribed tio
 */
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