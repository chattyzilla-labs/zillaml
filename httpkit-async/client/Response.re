open Core;
open Async;
open Zillaml;

module M: Zillaml_httpkit.Client.Response.S with type io('a) = Deferred.t('a) =
  Zillaml_httpkit.Client.Response.Make({
    type io('a) = Deferred.t('a);
    let body = ((_response, response_body)) => {
     let finished = Ivar.create();
      let body_str = ref("");
      let on_eof = _ => Ivar.fill(finished, Ok(body_str^))
     let rec on_read = (bs, ~off, ~len) => {
       body_str := body_str^ ++ Bigstringaf.substring(~off, ~len, bs);
       Body.schedule_read(response_body, ~on_read, ~on_eof);
     };
     Body.schedule_read(response_body, ~on_read, ~on_eof);
     Ivar.read(finished);
    };
  });
include M;
