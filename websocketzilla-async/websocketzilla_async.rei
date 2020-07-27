open Async;

module Server: {
  let create_connection_handler:
    (
      ~config: Zillaml.Config.t=?,
      ~websocket_handler: ('a, Websocketzilla.Wsd.t) =>
                          Websocketzilla.Server_connection.input_handlers,
      ~error_handler: 'a => Websocketzilla.Server_connection.error_handler,
      Socket.Address.Inet.t as 'a,
      Socket.t([ | `Active], 'a)
    ) =>
    Deferred.t(unit);
};

module Client: {
  /* Perform HTTP/1.1 handshake and upgrade to WS. */
  let connect:
    (
      ~nonce: string,
      ~host: string,
      ~port: int,
      ~resource: string,
      ~error_handler: Websocketzilla.Client_connection.error => unit,
      ~websocket_handler: Websocketzilla.Wsd.t =>
                          Websocketzilla.Client_connection.input_handlers,
      Socket.t([ | `Active], Socket.Address.Inet.t)
    ) =>
    Deferred.t(unit);
};
