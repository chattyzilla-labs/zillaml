open! Core;

open! Async;

open! Import;

open! Require_explicit_time_source;

include Persistent_connection_intf;

module Make = (Conn: T) => {
  include Persistent_connection_kernel.Make(Conn);
  let create =
      (
        ~server_name,
        ~log=?,
        ~on_event=(_) => Deferred.unit,
        ~retry_delay=?,
        ~random_state=?,
        ~time_source=?,
        ~connect,
        get_address
      ) => {
    let retry_delay =
      Option.map(retry_delay, ~f=(f, ()) => f() |> Time_ns.Span.of_span_float_round_nearest);
    let on_event = (event) => {
      Option.iter(
        log,
        ~f=
          (log) =>
            if (Log.would_log(log, Some(Event.log_level(event)))) {
              Log.sexp(
                log,
                ~tags=[("persistent-connection-to", server_name)],
                ~level=Event.log_level(event),
                Event.sexp_of_t(event)
              );
            }
      );
      on_event(event);
    };
    create(
      ~server_name,
      ~on_event,
      ~retry_delay?,
      ~random_state?,
      ~time_source?,
      ~connect,
      get_address
    );
  };
};

let create_convenience_wrapper =
    (
      ~create,
      ~connection_of_rpc_connection,
      ~server_name,
      ~log=?,
      ~on_event=?,
      ~retry_delay=?,
      ~bind_to_address=?,
      ~implementations=?,
      ~max_message_size=?,
      ~make_transport=?,
      ~handshake_timeout=?,
      ~heartbeat_config=?,
      get_address
    ) => {
  let connect = (host_and_port) => [%bind.Deferred.Or_error
    {
      let conn =
        Rpc.Connection.client(
          Tcp.Where_to_connect.of_host_and_port(~bind_to_address?, host_and_port),
          ~implementations?,
          ~max_message_size?,
          ~make_transport?,
          ~handshake_timeout?,
          ~heartbeat_config?,
          ~description=Info.of_string("persistent connection to " ++ server_name)
        )
        >>| Or_error.of_exn_result;
      connection_of_rpc_connection(conn);
    }
  ];
  create(
    ~server_name,
    ~log?,
    ~on_event?,
    ~retry_delay?,
    ~random_state=?None,
    ~time_source=?None,
    ~connect,
    get_address
  );
};

module Versioned_rpc = {
  include
    Make(
      {
        module Address = Host_and_port;
        type t = Versioned_rpc.Connection_with_menu.t;
        let rpc_connection = Versioned_rpc.Connection_with_menu.connection;
        let close = (t) => Rpc.Connection.close(rpc_connection(t));
        let is_closed = (t) => Rpc.Connection.is_closed(rpc_connection(t));
        let close_finished = (t) => Rpc.Connection.close_finished(rpc_connection(t));
      }
    );
  let create' = (~server_name) =>
    create_convenience_wrapper(
      ~server_name,
      ~create,
      ~connection_of_rpc_connection=Versioned_rpc.Connection_with_menu.create
    );
};

module Rpc = {
  include
    Make(
      {
        module Address = Host_and_port;
        type t = Rpc.Connection.t;
        let close = (t) => Rpc.Connection.close(t);
        let is_closed = (t) => Rpc.Connection.is_closed(t);
        let close_finished = (t) => Rpc.Connection.close_finished(t);
      }
    );
  let create' = (~server_name) =>
    create_convenience_wrapper(
      ~server_name,
      ~create,
      ~connection_of_rpc_connection=Deferred.Or_error.return
    );
};