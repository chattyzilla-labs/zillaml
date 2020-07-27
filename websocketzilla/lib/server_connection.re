module IOVec = Zillaml.IOVec;
module Server_handshake = Gluten.Server;

type state =
  | Handshake(Server_handshake.t)
  | Websocket(Websocket_connection.t);

type input_handlers =
  Websocket_connection.input_handlers = {
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

type error = Websocket_connection.error;
type error_handler = Websocket_connection.error_handler;

type t = {
  mutable state,
  websocket_handler: Wsd.t => input_handlers,
};

let is_closed = t =>
  switch (t.state) {
  | Handshake(handshake) => Server_handshake.is_closed(handshake)
  | Websocket(websocket) => Websocket_connection.is_closed(websocket)
  };

let create = (~sha1, ~error_handler=?, websocket_handler) => {
  let rec upgrade_handler = (upgrade, ()) => {
    let t = Lazy.force(t);
    let ws_connection =
      Websocket_connection.create(
        ~mode=`Server,
        ~error_handler?,
        websocket_handler,
      );

    t.state = Websocket(ws_connection);
    upgrade(Gluten.make((module Websocket_connection), ws_connection));
  }
  and request_handler = ({Gluten.reqd, upgrade}) => {
    let error = msg => {
      let response =
        Zillaml.(
          Response.create(
            ~headers=Headers.of_list([("Connection", "close")]),
            `Bad_request,
          )
        );

      Zillaml.Reqd.respond_with_string(reqd, response, msg);
    };

    let ret =
      Zillaml.Reqd.try_with(reqd, () =>
        switch (
          Handshake.respond_with_upgrade(
            ~sha1,
            reqd,
            upgrade_handler(upgrade),
          )
        ) {
        | Ok () => ()
        | Error(msg) => error(msg)
        }
      );

    switch (ret) {
    | Ok () => ()
    | Error(exn) => error(Printexc.to_string(exn))
    };
  }
  and t =
    lazy({
      state:
        Handshake(
          Server_handshake.create_upgradable(
            ~protocol=(module Zillaml.Server_connection),
            ~create=
              Zillaml.Server_connection.create(
                ~config=?None,
                ~error_handler=?None,
              ),
            request_handler,
          ),
        ),
      websocket_handler,
    });

  Lazy.force(t);
};

let create_websocket = (~error_handler=?, websocket_handler) => {
  state:
    Websocket(
      Websocket_connection.create(
        ~mode=`Server,
        ~error_handler?,
        websocket_handler,
      ),
    ),
  websocket_handler,
};

let shutdown = t =>
  switch (t.state) {
  | Handshake(handshake) => Server_handshake.shutdown(handshake)
  | Websocket(websocket) => Websocket_connection.shutdown(websocket)
  };

let report_exn = (t, exn) =>
  switch (t.state) {
  | Handshake(_) =>
    /* TODO: we need to handle this properly. There was an error in the upgrade */
    assert(false)
  | Websocket(websocket) => Websocket_connection.report_exn(websocket, exn)
  };

let next_read_operation = t =>
  switch (t.state) {
  | Handshake(handshake) => Server_handshake.next_read_operation(handshake)
  | Websocket(websocket) =>
    Websocket_connection.next_read_operation(websocket)
  };

let read = (t, bs, ~off, ~len) =>
  switch (t.state) {
  | Handshake(handshake) => Server_handshake.read(handshake, bs, ~off, ~len)
  | Websocket(websocket) =>
    Websocket_connection.read(websocket, bs, ~off, ~len)
  };

let read_eof = (t, bs, ~off, ~len) =>
  switch (t.state) {
  | Handshake(handshake) =>
    Server_handshake.read_eof(handshake, bs, ~off, ~len)
  | Websocket(websocket) =>
    Websocket_connection.read_eof(websocket, bs, ~off, ~len)
  };

let yield_reader = (t, f) =>
  switch (t.state) {
  | Handshake(handshake) => Server_handshake.yield_reader(handshake, f)
  | Websocket(_) => assert(false)
  };

let next_write_operation = t =>
  switch (t.state) {
  | Handshake(handshake) => Server_handshake.next_write_operation(handshake)
  | Websocket(websocket) =>
    Websocket_connection.next_write_operation(websocket)
  };

let report_write_result = (t, result) =>
  switch (t.state) {
  | Handshake(handshake) =>
    Server_handshake.report_write_result(handshake, result)
  | Websocket(websocket) =>
    Websocket_connection.report_write_result(websocket, result)
  };

let yield_writer = (t, f) =>
  switch (t.state) {
  | Handshake(handshake) => Server_handshake.yield_writer(handshake, f)
  | Websocket(websocket) => Websocket_connection.yield_writer(websocket, f)
  };
