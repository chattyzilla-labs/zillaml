module Writer = Serialize.Writer;

type error = [
  | `Malformed_response(string)
  | `Invalid_response_body_length(Response.t)
  | `Exn(exn)
];

type state =
  | Uninitialized
  | Awaiting_response
  | Received_response(Response.t, Body.t([ | `read]))
  | Upgraded(Response.t)
  | Closed;

type t = {
  request: Request.t,
  request_body: Body.t([ | `write]),
  response_handler: (Response.t, Body.t([ | `read])) => unit,
  error_handler: error => unit,
  mutable error_code: [ | `Ok | error],
  writer: Writer.t,
  mutable state,
  mutable persistent: bool,
};

let create = (error_handler, request, request_body, writer, response_handler) => {
  let rec handler = (response, body) => {
    let t = Lazy.force(t);
    if (t.persistent) {
      t.persistent = Response.persistent_connection(response);
    };
    let next_state =
      switch (response.status) {
      | `Switching_protocols => Upgraded(response)
      | _ => [@implicit_arity] Received_response(response, body)
      };

    t.state = next_state;
    response_handler(response, body);
  }
  and t =
    lazy({
      request,
      request_body,
      response_handler: handler,
      error_handler,
      error_code: `Ok,
      writer,
      state: Uninitialized,
      persistent: Request.persistent_connection(request),
    });

  Lazy.force(t);
};

let request = ({request, _}) => request;

let request_body = ({request_body, _}) => request_body;

let write_request = t => {
  Writer.write_request(t.writer, t.request);
  t.state = Awaiting_response;
};

let on_more_output_available = ({request_body, _}, f) =>
  Body.when_ready_to_write(request_body, f);

/* TODO: wondering if any of the `Received_response` changes here
 * apply to us: https://github.com/inhabitedtype/httpaf/pull/148 */
let report_error = (t, error) =>
  /* t.persistent <- false; */
  /* TODO: drain queue? */
  switch (t.state, t.error_code) {
  | (
      Uninitialized | Awaiting_response | Received_response(_) | Upgraded(_),
      `Ok,
    ) =>
    t.state = Closed;
    t.error_code = (error :> [ | `Ok | error]);
    t.error_handler(error);
  | (Uninitialized, `Exn(_)) =>
    /* TODO(anmonteiro): Not entirely sure this is possible in the client. */
    failwith("httpaf.Reqd.report_exn: NYI")
  | (
      Uninitialized | Awaiting_response | Received_response(_) | Closed |
      Upgraded(_),
      _,
    ) =>
    /* XXX(seliopou): Once additional logging support is added, log the error
     * in case it is not spurious. */
    ()
  };

let persistent_connection = t => t.persistent;

let close_response_body = t =>
  switch (t.state) {
  | Uninitialized
  | Awaiting_response
  | Closed => ()
  | Received_response(_, response_body) =>
    Body.close_reader(response_body)
  | Upgraded(_) => t.state = Closed
  };

let requires_input = t =>
  switch (t.state) {
  | Uninitialized => true
  | Awaiting_response => true
  | Upgraded(_) => false
  | Received_response(_, response_body) =>
    !Body.is_closed(response_body)
  | Closed => false
  };

let requires_output = ({request_body, state, _}) =>
  switch (state) {
  | Upgraded(_) =>
    /* XXX(anmonteiro): Connections that have been upgraded "require output"
     * forever, but outside the HTTP layer, meaning they're permanently
     * "yielding". For now they need to be explicitly shutdown in order to
     * transition the response descriptor to the `Closed` state. */
    true
  | state =>
    state == Uninitialized
    || !Body.is_closed(request_body)
    || Body.has_pending_output(request_body)
  };

let is_complete = t => !(requires_input(t) || requires_output(t));

let flush_request_body = ({request, request_body, writer, _}) =>
  if (Body.has_pending_output(request_body)) {
    let encoding =
      switch (Request.body_length(request)) {
      | (`Fixed(_) | `Chunked) as encoding => encoding
      | `Error(_) => assert(false)
      }; /* XXX(seliopou): This needs to be handled properly */

    Body.transfer_to_writer_with_encoding(request_body, ~encoding, writer);
  };

let flush_response_body = t =>
  switch (t.state) {
  | Uninitialized
  | Awaiting_response
  | Closed
  | Upgraded(_) => ()
  | Received_response(_, response_body) =>
    try(Body.execute_read(response_body)) {
    /* TODO: report_exn */
    | _exn => Format.eprintf("EXN@.")
    }
  };
/* report_exn t exn */
