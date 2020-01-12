module IOVec = Zillaml.IOVec;
module Server_handshake = Zillaml.Server_connection;

type state('fd, 'io) =
  | Handshake(Server_handshake.t('fd, 'io))
  | Websocket(Server_websocket.t);

type input_handlers =
  Server_websocket.input_handlers = {
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

type t('fd, 'io) = {
  mutable state: state('fd, 'io),
  websocket_handler: Wsd.t => input_handlers,
  error_handler,
  wakeup_reader: ref(list(unit => unit)),
};

let is_closed = t =>
  switch (t.state) {
  | Handshake(handshake) => Server_handshake.is_closed(handshake)
  | Websocket(websocket) => Server_websocket.is_closed(websocket)
  };

let on_wakeup_reader = (t, k) =>
  if (is_closed(t)) {
    failwith("called on_wakeup_reader on closed conn");
  } else {
    t.wakeup_reader := [k, ...t.wakeup_reader^];
  };

let wakeup_reader = t => {
  let fs = t.wakeup_reader^;
  t.wakeup_reader := [];
  List.iter(f => f(), fs);
};

let passes_scrutiny = _headers => true; /* XXX(andreas): missing! */

let default_error_handler = (wsd, `Exn(exn)) => {
  let message = Printexc.to_string(exn);
  let payload = Bytes.of_string(message);
  Wsd.send_bytes(
    wsd,
    ~kind=`Text,
    payload,
    ~off=0,
    ~len=Bytes.length(payload),
  );
  Wsd.close(wsd);
};

let respond_with_upgrade =
    (~headers=Zillaml.Headers.empty, ~sha1, reqd, upgrade_handler) => {
  let request = Zillaml.Reqd.request(reqd);
  if (passes_scrutiny(request.headers)) {
    let sec_websocket_key =
      Zillaml.Headers.get_exn(request.headers, "sec-websocket-key");

    let upgrade_headers =
      Handshake.create_response_headers(~sha1, ~sec_websocket_key, ~headers);

    Ok(
      Zillaml.Reqd.respond_with_upgrade(
        reqd,
        upgrade_headers,
        upgrade_handler,
      ),
    );
  } else {
    Error("Didn't pass scrutiny");
  };
};

/* TODO(anmonteiro): future is a terrible name for this */
let create =
    (~sha1, ~future, ~error_handler=default_error_handler, websocket_handler) => {
  let rec upgrade_handler = _fd => {
    let t = Lazy.force(t);
    t.state = Websocket(Server_websocket.create(~websocket_handler));
    wakeup_reader(t);
    future;
  }
  and request_handler = reqd =>
    switch (
      respond_with_upgrade(~headers=?None, ~sha1, reqd, upgrade_handler)
    ) {
    | Ok () => ()
    | Error(msg) =>
      let response =
        Zillaml.(
          Response.create(
            ~headers=Headers.of_list([("Connection", "close")]),
            `Bad_request,
          )
        );

      Zillaml.Reqd.respond_with_string(reqd, response, msg);
    }
  and t =
    lazy({
      state: Handshake(Server_handshake.create(request_handler)),
      websocket_handler,
      error_handler,
      wakeup_reader: ref([]),
    });

  Lazy.force(t);
};

let create_upgraded =
    (~error_handler=default_error_handler, ~websocket_handler) => {
  state: Websocket(Server_websocket.create(~websocket_handler)),
  websocket_handler,
  error_handler,
  wakeup_reader: ref([]),
};

let close = t =>
  switch (t.state) {
  | Handshake(handshake) => Server_handshake.shutdown(handshake)
  | Websocket(websocket) => Server_websocket.close(websocket)
  };

let set_error_and_handle = (t, error) =>
  switch (t.state) {
  | Handshake(_) =>
    /* TODO: we need to handle this properly. There was an error in the upgrade */
    assert(false)
  | Websocket({wsd, _}) =>
    if (!Wsd.is_closed(wsd)) {
      t.error_handler(wsd, error);
      close(t);
    }
  };

let report_exn = (t, exn) => set_error_and_handle(t, `Exn(exn));

let next_read_operation = t =>
  switch (t.state) {
  | Handshake(handshake) => Server_handshake.next_read_operation(handshake)
  | Websocket(websocket) =>
    switch (Server_websocket.next_read_operation(websocket)) {
    | `Error(`Parse(_, message)) =>
      set_error_and_handle(t, `Exn(Failure(message)));
      `Close;
    | (`Read | `Close) as operation => operation
    }
  };

let read = (t, bs, ~off, ~len) =>
  switch (t.state) {
  | Handshake(handshake) => Server_handshake.read(handshake, bs, ~off, ~len)
  | Websocket(websocket) => Server_websocket.read(websocket, bs, ~off, ~len)
  };

let read_eof = (t, bs, ~off, ~len) =>
  switch (t.state) {
  | Handshake(handshake) =>
    Server_handshake.read_eof(handshake, bs, ~off, ~len)
  | Websocket(websocket) =>
    Server_websocket.read_eof(websocket, bs, ~off, ~len)
  };

let yield_reader = (t, f) => on_wakeup_reader(t, f);

let next_write_operation = t =>
  switch (t.state) {
  | Handshake(handshake) => Server_handshake.next_write_operation(handshake)
  | Websocket(websocket) => Server_websocket.next_write_operation(websocket)
  };

let report_write_result = (t, result) =>
  switch (t.state) {
  | Handshake(handshake) =>
    Server_handshake.report_write_result(handshake, result)
  | Websocket(websocket) =>
    Server_websocket.report_write_result(websocket, result)
  };

let yield_writer = (t, f) =>
  switch (t.state) {
  | Handshake(handshake) => Server_handshake.yield_writer(handshake, f)
  | Websocket(websocket) => Server_websocket.yield_writer(websocket, f)
  };
