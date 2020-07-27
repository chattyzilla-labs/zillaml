type t =
  | Waiting
  | Fixed(Response.t)
  | Streaming(Response.t, Body.t([ | `write]))
  | Upgrade(Response.t, unit => unit);

let output_state = (t): Output_state.t =>
  switch (t) {
  | Fixed(_) => Complete
  | Waiting => Waiting
  | Streaming(_, response_body) =>
    if (Body.requires_output(response_body)) {
      Ready;
    } else {
      Complete;
    }
  | Upgrade(_) => Ready
  };

let flush_response_body = (t, ~request_method, writer) =>
  switch (t) {
  | Streaming(response, response_body) =>
    let encoding =
      switch (Response.body_length(~request_method, response)) {
      | (`Fixed(_) | `Close_delimited | `Chunked) as encoding => encoding
      | `Error(_) => assert(false)
      }; /* XXX(seliopou): This needs to be handled properly */

    Body.transfer_to_writer_with_encoding(response_body, ~encoding, writer);
  | _ => ()
  };
