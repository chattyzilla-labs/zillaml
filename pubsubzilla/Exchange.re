open Core;
open Async;

module Direct_Exchange = {
    module Message = {

        module Topic: Identifiable = String;
        module Publisher = Unique_id.Int63();
        module Id = Unique_id.Int63();
        [@deriving (sexp, bin_io, compare)]
        type mandatory = bool;
        [@deriving (sexp, bin_io, compare)]
        type t('a) = {
            id: Id.t,
            payload: [ | `Message(string) | `Custom('a) ],
            topic: Topic.t,
            time: Time.t,
            mode: (mandatory, [ | `Persistent(Time.t) | `Not_Persistent]),
            publisher: Publisher.t,
            priority: option(int)
        };
    };
}