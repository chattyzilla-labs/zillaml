open! Core;

open! Async;

open! Import;

module type S = {
  include Persistent_connection_kernel.S;
  let create:
    (
      ~server_name: string,
      ~log: Log.t=?,
      /*** If [~log] is supplied then all events that would be passed to [on_event] will be
           written there as well, with a "persistent-connection-to" tag value of
           [server_name], which should be the name of the server we are connecting to. */
      ~on_event: Event.t => Deferred.t(unit)=?,
      ~retry_delay: unit => Time.Span.t=?,
      ~random_state: Random.State.t=?,
      ~time_source: Time_source.t=?,
      ~connect: address => Deferred.t(Or_error.t(conn)),
      unit => Deferred.t(Or_error.t(address))
    ) =>
    t;
};

module type T = Persistent_connection_kernel.T;

module type S_rpc = {
  include S with type address = Host_and_port.t;
  /** Like [create] but for Rpc-like connections (i.e. Async.Rpc and Async.Versioned_rpc)
      where there is an obvious default for [connect] -- with a handful of extra optional
      parameters to pass to the [Rpc.Connection] functions. */
  let create':
    (
      ~server_name: string,
      ~log: Log.t=?,
      ~on_event: Event.t => Deferred.t(unit)=?,
      ~retry_delay: unit => Time.Span.t=?,
      ~bind_to_address: Unix.Inet_addr.t=?,
      ~implementations: Rpc.Connection.Client_implementations.t(_)=?,
      ~max_message_size: int=?,
      ~make_transport: Rpc.Connection.transport_maker=?,
      ~handshake_timeout: Time.Span.t=?,
      ~heartbeat_config: Rpc.Connection.Heartbeat_config.t=?,
      unit => Deferred.t(Or_error.t(Host_and_port.t))
    ) =>
    t;
};

module type Persistent_connection = {
  /** A persistent connection is one that is automatically reestablished whenever lost. */;
  module type S = S;
  module type T = T;
  module Make: (Conn: T) => S with type conn = Conn.t and type address = Conn.Address.t;
  module Rpc: S_rpc with type conn = Rpc.Connection.t;
  module Versioned_rpc: S_rpc with type conn = Versioned_rpc.Connection_with_menu.t;
};