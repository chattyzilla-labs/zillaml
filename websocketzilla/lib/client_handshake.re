module IOVec = Zillaml.IOVec;

type t = {
  connection: Zillaml.Client_connection.t,
  body: Zillaml.Body.t([ | `write]),
};

let create = (~nonce, ~headers, ~error_handler, ~response_handler, target) => {
  let connection = Zillaml.Client_connection.create(~config=?None, ());
  let body =
    Zillaml.Client_connection.request(
      connection,
      Handshake.create_request(~nonce, ~headers, target),
      ~error_handler,
      ~response_handler,
    );

  {connection, body};
};

let next_read_operation = t =>
  Zillaml.Client_connection.next_read_operation(t.connection);

let next_write_operation = t =>
  Zillaml.Client_connection.next_write_operation(t.connection);

let read = t => Zillaml.Client_connection.read(t.connection);

let yield_reader = t => Zillaml.Client_connection.yield_reader(t.connection);

let report_write_result = t =>
  Zillaml.Client_connection.report_write_result(t.connection);

let yield_writer = t => Zillaml.Client_connection.yield_writer(t.connection);

let close = t => Zillaml.Body.close_writer(t.body);
