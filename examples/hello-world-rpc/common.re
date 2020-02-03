open Core;
open Async;

let port = {
  open Command.Let_syntax;
  let%map_open port =
    flag("-port", optional_with_default(8124, int), ~doc=" Broker's port");

  port;
};

let host_port_pair = {
  open Command.Let_syntax;
  let%map_open port = port
  and host =
    flag(
      "-hostname",
      optional_with_default("127.0.0.1", string),
      ~doc=" Broker's hostname",
    );

  (host, port);
};

let with_rpc_conn = (f, ~host, ~port) =>
  Tcp.with_connection(
    Tcp.to_host_and_port(host, port), ~timeout=sec(1.), (_, r, w) =>
    switch%bind (Rpc.Connection.create(r, w, ~connection_state=_ => ())) {
    | Error(exn) => raise(exn)
    | Ok(conn) => f(conn)
    }
  );

let start_server = (~env, ~stop=Deferred.never(), ~implementations, ~port, ()) => {
  Log.Global.info("Starting server on %d", port);
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

  let%bind server =
    Tcp.Server.create(
      ~on_handler_error=
        `Call((_, exn) => Log.Global.sexp([%sexp (exn: Exn.t)])),
      Tcp.on_port(port),
      (_addr, r, w) =>
      Rpc.Connection.server_with_close(
        r,
        w,
        ~connection_state=_ => env,
        ~on_handshake_error=
          `Call(
            exn => {
              Log.Global.sexp([%sexp (exn: Exn.t)]);
              return();
            },
          ),
        ~implementations,
      )
    );

  Log.Global.info("Server started, waiting for close");
  Deferred.any([
    stop >>= (() => Tcp.Server.close(server)),
    Tcp.Server.close_finished(server),
  ]);
};
