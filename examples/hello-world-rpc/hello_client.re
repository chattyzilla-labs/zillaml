open! Core;
open! Async;

/* A command that sends the hello request  */
let say_hello = (~host, ~port) =>
  Common.with_rpc_conn(
    conn => {
      let%map response =
        Rpc.Rpc.dispatch_exn(Hello_protocol.hello_rpc, conn, "Hello");
      printf("%s\n%!", response);
    },
    ~host,
    ~port,
  );

let command =
  Command.async'(
    ~summary="Hello World client",
    {
      open Command.Let_syntax;
      let%map_open (host, port) = Common.host_port_pair;
      () => say_hello(~port, ~host);
    },
  );

let () = Command.run(command);
