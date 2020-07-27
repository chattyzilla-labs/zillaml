open Core;
open Async;

let sha1 = s =>
  s |> Digestif.SHA1.digest_string |> Digestif.SHA1.to_raw_string;

module Server = {
  let create_connection_handler =
      (
        ~config=Zillaml.Config.default,
        ~websocket_handler,
        ~error_handler,
        client_addr,
        socket,
      ) => {
    let connection =
      Websocketzilla.Server_connection.create(
        ~sha1,
        ~error_handler=error_handler(client_addr),
        websocket_handler(client_addr),
      );

    Gluten_async.Server.create_connection_handler(
      ~read_buffer_size=config.read_buffer_size,
      ~protocol=(module Websocketzilla.Server_connection),
      connection,
      client_addr,
      socket,
    );
  };
};

module Client = {
  let connect =
      (
        ~nonce,
        ~host,
        ~port,
        ~resource,
        ~error_handler,
        ~websocket_handler,
        socket,
      ) => {
    let headers =
      Zillaml.Headers.of_list([
        ("host", String.concat(~sep=":", [host, string_of_int(port)])),
      ]);

    let connection =
      Websocketzilla.Client_connection.connect(
        ~nonce,
        ~headers,
        ~sha1,
        ~error_handler,
        ~websocket_handler,
        resource,
      );

    Deferred.ignore_m(
      Gluten_async.Client.create(
        ~read_buffer_size=0x1000,
        ~protocol=(module Websocketzilla.Client_connection),
        connection,
        socket,
      ),
    );
  };
};
