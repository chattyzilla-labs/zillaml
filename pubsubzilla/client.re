open Core;
open Async;
open Pubsub;

let connect = (~port=8000, host, exchange_name) => {
  let websocket_handler = (wsd) => {
    let _exchange = create_exchange(exchange_name);
    let accum = ref([]);
    let _send = str => {
      let payload = Bytes.of_string(str);
      Websocketzilla.Wsd.send_bytes(
        wsd,
        payload,
        ~kind=`Text,
        ~off=0,
        ~len=Bytes.length(payload),
      );
    };

    let finalise_content = accum_content =>
      String.concat(List.rev(accum_content));
    let on_message = msg => ();
    let on_close = () => ();
    let frame = (~opcode, ~is_fin, bs, ~off, ~len) =>
      switch (opcode) {
      | `Continuation =>
        if (List.is_empty(accum^)) {
          Log.Global.error("Bad frame in the middle of a fragmented message");
          Websocketzilla.Wsd.close(wsd);
          on_close();
        } else {
          accum := [Bigstringaf.substring(bs, ~off, ~len), ...accum^];
          if (is_fin) {
            let payload = finalise_content(accum^);
            accum := [];
            on_message(payload);
          };
        }
      | `Text
      | `Binary =>
        if (List.is_empty(accum^)) {
          if (is_fin) {
            Bigstringaf.substring(bs, ~off, ~len) |> on_message;
          } else {
            accum := [Bigstringaf.substring(bs, ~off, ~len)];
          };
        } else {
          Log.Global.error("Bad frame in the middle of a fragmented message");
          Websocketzilla.Wsd.close(wsd);
          accum := [];
          on_close();
        }
      | `Connection_close =>
        Websocketzilla.Wsd.close(wsd);
        accum := [];
        on_close();
      | `Ping => Websocketzilla.Wsd.send_ping(wsd)
      | `Pong
      | `Other(_) => print_endline("other")
      };
    let eof = () => {
      Log.Global.error("EOF\n%!");
      Websocketzilla.Wsd.close(wsd);
      on_close();
    };
    {Websocketzilla.Client_connection.frame, eof};
  };

  let error_handler = fun
  | `Handshake_failure((rsp, _body)) =>
    Format.eprintf("Handshake failure: %a\n%!", Zillaml.Response.pp_hum, rsp)
  | _ => assert(false);

  let where_to_connect = Tcp.Where_to_connect.of_host_and_port({ host, port });
  let%bind socket = Tcp.connect_sock(where_to_connect);
  let nonce = "0123456789ABCDEF";
  let resource = "/exchange/" ++ exchange_name;
  Websocketzilla_async.Client.connect(socket, ~nonce, ~host, ~port, ~resource, ~error_handler, ~websocket_handler);
};