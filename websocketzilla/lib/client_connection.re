type state =
  | Handshake(Client_handshake.t)
  | Websocket(Client_websocket.t);

type t = {mutable state};

type error = [
  Zillaml.Client_connection.error
  | `Handshake_failure(Zillaml.Response.t, Zillaml.Body.t([ | `read]))
];

type input_handlers =
  Client_websocket.input_handlers = {
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

let passes_scrutiny = (~accept, headers) => {
  let upgrade = Zillaml.Headers.get(headers, "upgrade");
  let connection = Zillaml.Headers.get(headers, "connection");
  let sec_websocket_accept =
    Zillaml.Headers.get(headers, "sec-websocket-accept");
  sec_websocket_accept == Some(accept)
  && (
    switch (upgrade) {
    | None => false
    | Some(upgrade) => String.lowercase_ascii(upgrade) == "websocket"
    }
  )
  && (
    switch (connection) {
    | None => false
    | Some(connection) => String.lowercase_ascii(connection) == "upgrade"
    }
  );
};

let handshake_exn = t =>
  switch (t.state) {
  | Handshake(handshake) => handshake
  | Websocket(_) => assert(false)
  };

let connect =
    (
      ~nonce,
      ~headers=Zillaml.Headers.empty,
      ~sha1,
      ~error_handler,
      ~websocket_handler,
      target,
    ) => {
  let nonce = Base64.encode_exn(nonce);
  let rec response_handler = (response, response_body) => {
    let t = Lazy.force(t);
    let accept = sha1(nonce ++ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11");
    switch (response.Zillaml.Response.status) {
    | `Switching_protocols when passes_scrutiny(~accept, response.headers) =>
      Zillaml.Body.close_reader(response_body);
      let handshake = handshake_exn(t);
      t.state = Websocket(Client_websocket.create(~websocket_handler));
      Client_handshake.close(handshake);
    | _ => error_handler(`Handshake_failure((response, response_body)))
    };
  }
  and t =
    lazy({
      state:
        Handshake(
          Client_handshake.create(
            ~nonce,
            ~headers,
            ~(error_handler :> Zillaml.Client_connection.error_handler),
            ~response_handler,
            target,
          ),
        ),
    });

  Lazy.force(t);
};

/* TODO: Doesn't the Websocket handler need an error handler too?! */
let create = (~websocket_handler) => {
  state: Websocket(Client_websocket.create(~websocket_handler)),
};

let next_read_operation = t =>
  switch (t.state) {
  | Handshake(handshake) => Client_handshake.next_read_operation(handshake)
  | Websocket(websocket) =>
    switch (Client_websocket.next_read_operation(websocket)) {
    | `Error(`Parse(_, _message)) =>
      /* TODO(anmonteiro): handle this */
      assert(false)
    /* set_error_and_handle t (`Exn (Failure message)); `Close */
    | (`Read | `Close) as operation => operation
    }
  };

let read = (t, bs, ~off, ~len) =>
  switch (t.state) {
  | Handshake(handshake) => Client_handshake.read(handshake, bs, ~off, ~len)
  | Websocket(websocket) => Client_websocket.read(websocket, bs, ~off, ~len)
  };

let read_eof = (t, bs, ~off, ~len) =>
  switch (t.state) {
  | Handshake(handshake) => Client_handshake.read(handshake, bs, ~off, ~len)
  | Websocket(websocket) =>
    Client_websocket.read_eof(websocket, bs, ~off, ~len)
  };

let next_write_operation = t =>
  switch (t.state) {
  | Handshake(handshake) => Client_handshake.next_write_operation(handshake)
  | Websocket(websocket) => Client_websocket.next_write_operation(websocket)
  };

let report_write_result = (t, result) =>
  switch (t.state) {
  | Handshake(handshake) =>
    Client_handshake.report_write_result(handshake, result)
  | Websocket(websocket) =>
    Client_websocket.report_write_result(websocket, result)
  };

let yield_reader = (t, f) =>
  switch (t.state) {
  | Handshake(handshake) => Client_handshake.yield_reader(handshake, f)
  | Websocket(_websocket) => assert(false)
  };

let yield_writer = (t, f) =>
  switch (t.state) {
  | Handshake(handshake) => Client_handshake.yield_writer(handshake, f)
  | Websocket(websocket) => Client_websocket.yield_writer(websocket, f)
  };

let close = t =>
  switch (t.state) {
  | Handshake(handshake) => Client_handshake.close(handshake)
  | Websocket(websocket) => Client_websocket.close(websocket)
  };
