open Async;

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
      Socket.t([ | `Active], [< Socket.Address.t])
    ) =>
    Deferred.t(unit);

  /* Starts speaking websockets, doesn't perform the handshake. */
  let create:
    (
      ~websocket_handler: Websocketzilla.Wsd.t =>
                          Websocketzilla.Client_connection.input_handlers,
      Socket.t([ | `Active], [< Socket.Address.t])
    ) =>
    Deferred.t(unit);
};

module Server: {
  let create_connection_handler:
    (
      ~config: Zillaml.Config.t=?,
      ~websocket_handler: ('a, Websocketzilla.Wsd.t) =>
                          Websocketzilla.Server_connection.input_handlers,
      ~error_handler: 'a => Zillaml.Server_connection.error_handler,
      [< Socket.Address.t] as 'a,
      Socket.t([ | `Active], 'a)
    ) =>
    Deferred.t(unit);

  let create_upgraded_connection_handler:
    (
      ~config: Zillaml.Config.t=?,
      ~websocket_handler: ([< Socket.Address.t] as 'a, Websocketzilla.Wsd.t) =>
                          Websocketzilla.Server_connection.input_handlers,
      ~error_handler: Websocketzilla.Server_connection.error_handler,
      'a,
      Socket.t([ | `Active], 'a)
    ) =>
    Deferred.t(unit);

  let respond_with_upgrade:
    (
      ~headers: Zillaml.Headers.t=?,
      Zillaml.Reqd.t(
        Socket.t([ | `Active], [< Socket.Address.t] as 'a),
        Deferred.t(unit),
      ),
      Socket.t([ | `Active], 'a) => Deferred.t(unit)
    ) =>
    Deferred.Result.t(unit, string);
};
