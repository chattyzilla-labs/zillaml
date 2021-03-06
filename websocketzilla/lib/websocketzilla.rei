module IOVec = Zillaml.IOVec;

module Wsd: {
  type mode = [ | `Client(unit => int32) | `Server];

  type t;

  let create: mode => t;

  let schedule:
    (
      t,
      ~kind: [ | `Text | `Binary | `Continuation],
      Bigstringaf.t,
      ~off: int,
      ~len: int
    ) =>
    unit;

  let send_bytes:
    (
      t,
      ~kind: [ | `Text | `Binary | `Continuation],
      Bytes.t,
      ~off: int,
      ~len: int
    ) =>
    unit;

  let send_ping: t => unit;
  let send_pong: t => unit;

  let flushed: (t, unit => unit) => unit;
  let close: t => unit;

  let is_closed: t => bool;
};

module Handshake: {
  let create_request:
    (~nonce: string, ~headers: Zillaml.Headers.t, string) => Zillaml.Request.t;

  let upgrade_headers:
    (
      ~sha1: string => string,
      ~request_method: Zillaml.Method.t,
      Zillaml.Headers.t
    ) =>
    result(list((string, string)), string);

  let respond_with_upgrade:
    (
      ~headers: Zillaml.Headers.t=?,
      ~sha1: string => string,
      Zillaml.Reqd.t,
      unit => unit
    ) =>
    result(unit, string);
};

module Client_connection: {
  type t;

  type error = [
    Zillaml.Client_connection.error
    | `Handshake_failure(Zillaml.Response.t, Zillaml.Body.t([ | `read]))
  ];

  type input_handlers = {
    frame:
      (
        ~opcode: Websocket.Opcode.t,
        ~is_fin: bool,
        Bigstringaf.t,
        ~off: int,
        ~len: int
      ) =>
      unit,
    eof: unit => unit,
  };

  let connect:
    (
      ~nonce: string,
      ~headers: Zillaml.Headers.t=?,
      ~sha1: string => string,
      ~error_handler: error => unit,
      ~websocket_handler: Wsd.t => input_handlers,
      string
    ) =>
    t;

  let create:
    (
      ~error_handler: (Wsd.t, [ | `Exn(exn)]) => unit=?,
      Wsd.t => input_handlers
    ) =>
    t;

  let next_read_operation: t => [ | `Read | `Yield | `Close];
  let next_write_operation:
    t => [ | `Write(list(IOVec.t(Bigstringaf.t))) | `Yield | `Close(int)];

  let read: (t, Bigstringaf.t, ~off: int, ~len: int) => int;
  let read_eof: (t, Bigstringaf.t, ~off: int, ~len: int) => int;

  let yield_reader: (t, unit => unit) => unit;

  let report_write_result: (t, [ | `Ok(int) | `Closed]) => unit;

  let yield_writer: (t, unit => unit) => unit;

  let report_exn: (t, exn) => unit;

  let is_closed: t => bool;

  let shutdown: t => unit;
};

module Server_connection: {
  type t;

  type input_handlers = {
    frame:
      (
        ~opcode: Websocket.Opcode.t,
        ~is_fin: bool,
        Bigstringaf.t,
        ~off: int,
        ~len: int
      ) =>
      unit,
    eof: unit => unit,
  };

  type error = [ | `Exn(exn)];

  type error_handler = (Wsd.t, error) => unit;

  /* TODO: should take handshake error handler. */
  let create:
    (
      ~sha1: string => string,
      ~error_handler: error_handler=?,
      Wsd.t => input_handlers
    ) =>
    t;

  let create_websocket:
    (~error_handler: error_handler=?, Wsd.t => input_handlers) => t;

  let next_read_operation: t => [ | `Read | `Yield | `Close];
  let next_write_operation:
    t => [ | `Write(list(IOVec.t(Bigstringaf.t))) | `Yield | `Close(int)];

  let read: (t, Bigstringaf.t, ~off: int, ~len: int) => int;
  let read_eof: (t, Bigstringaf.t, ~off: int, ~len: int) => int;
  let report_write_result: (t, [ | `Ok(int) | `Closed]) => unit;

  let report_exn: (t, exn) => unit;

  let yield_reader: (t, unit => unit) => unit;
  let yield_writer: (t, unit => unit) => unit;

  let is_closed: t => bool;

  let shutdown: t => unit;
};

module Websocket: {
  module Opcode: {
    type standard_non_control = [ | `Continuation | `Text | `Binary];

    type standard_control = [ | `Connection_close | `Ping | `Pong];

    type standard = [ standard_non_control | standard_control];

    type t = [ standard | `Other(int)];

    let code: t => int;

    let of_code: int => option(t);
    let of_code_exn: int => t;

    let to_int: t => int;

    let of_int: int => option(t);
    let of_int_exn: int => t;

    let pp_hum: (Format.formatter, t) => unit;
  };

  module Close_code: {
    type standard = [
      | `Normal_closure
      | `Going_away
      | `Protocol_error
      | `Unsupported_data
      | `No_status_rcvd
      | `Abnormal_closure
      | `Invalid_frame_payload_data
      | `Policy_violation
      | `Message_too_big
      | `Mandatory_ext
      | `Internal_server_error
      | `TLS_handshake
    ];

    type t = [ standard | `Other(int)];

    let code: t => int;

    let of_code: int => option(t);
    let of_code_exn: int => t;

    let to_int: t => int;

    let of_int: int => option(t);
    let of_int_exn: int => t;
  };

  module Frame: {
    type t;

    let is_fin: t => bool;
    let rsv: t => int;

    let opcode: t => Opcode.t;

    let has_mask: t => bool;
    let mask: t => option(int32);
    let mask_exn: t => int32;

    let mask_inplace: t => unit;
    let unmask_inplace: t => unit;

    let length: t => int;

    let payload_length: t => int;
    let with_payload:
      (t, ~f: (Bigstringaf.t, ~off: int, ~len: int) => 'a) => 'a;

    let copy_payload: t => Bigstringaf.t;
    let copy_payload_bytes: t => Bytes.t;

    let parse: Angstrom.t(t);

    let serialize_control:
      (~mask: int32=?, Faraday.t, ~opcode: Opcode.standard_control) => unit;

    let schedule_serialize:
      (
        ~mask: int32=?,
        Faraday.t,
        ~is_fin: bool,
        ~opcode: Opcode.t,
        ~payload: Bigstringaf.t,
        ~off: int,
        ~len: int
      ) =>
      unit;

    let serialize_bytes:
      (
        ~mask: int32=?,
        Faraday.t,
        ~is_fin: bool,
        ~opcode: Opcode.t,
        ~payload: Bytes.t,
        ~off: int,
        ~len: int
      ) =>
      unit;
  };
};
