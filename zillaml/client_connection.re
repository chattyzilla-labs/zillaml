/*----------------------------------------------------------------------------
    Copyright (c) 2017-2019 Inhabited Type LLC.
    Copyright (c) 2019 Antonio Nuno Monteiro.
    Copyright (c) 2019 Dakota Murphy.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*/

module Reader = Parse.Reader;
module Writer = Serialize.Writer;

type error = [
  | `Malformed_response(string)
  | `Invalid_response_body_length(Response.t)
  | `Exn(exn)
];

type response_handler = (Response.t, Body.t([ | `read])) => unit;
type error_handler = error => unit;

type t = {
  config: Config.t,
  reader: Reader.response,
  writer: Writer.t,
  request_queue: Queue.t(Respd.t),
  /* invariant: If [request_queue] is not empty, then the head of the queue
     has already written the request headers to the wire. */
};

let is_closed = t =>
  Reader.is_closed(t.reader) && Writer.is_closed(t.writer);

let is_waiting = t => !is_closed(t) && Queue.is_empty(t.request_queue);

let is_active = t => !Queue.is_empty(t.request_queue);

let current_respd_exn = t => Queue.peek(t.request_queue);

let yield_reader = (t, k) => Reader.on_wakeup(t.reader, k);

let wakeup_reader = t => Reader.wakeup(t.reader);

let yield_writer = (t, k) => Writer.on_wakeup(t.writer, k);

let wakeup_writer = t => Writer.wakeup(t.writer);

[@ocaml.warning "-16"]
let create = (~config=Config.default) => {
  let request_queue = Queue.create();
  {
    config,
    reader: Reader.response(request_queue),
    writer: Writer.create(),
    request_queue,
  };
};

let request = (t, request, ~error_handler, ~response_handler) => {
  let request_body =
    Body.create(
      Bigstringaf.create(t.config.request_body_buffer_size),
      Optional_thunk.some(() => wakeup_writer(t)),
    );

  if (!(Request.body_length(request) == `Chunked)) {
    Body.set_non_chunked(request_body);
  };
  let respd =
    Respd.create(
      error_handler,
      request,
      request_body,
      t.writer,
      response_handler,
    );
  let handle_now = Queue.is_empty(t.request_queue);
  Queue.push(respd, t.request_queue);
  if (handle_now) {
    Respd.write_request(respd);
  };
  /* Not handling the request now means it may be pipelined.
   * `advance_request_queue_if_necessary` will take care of it, but we still
   * wanna wake up the writer so that the function gets called. */
  wakeup_writer(t);
  request_body;
};

let shutdown_reader = t => {
  Reader.force_close(t.reader);
  if (is_active(t)) {
    Respd.close_response_body(current_respd_exn(t));
  } else {
    wakeup_reader(t);
  };
};

let shutdown_writer = t => {
  Writer.close(t.writer);
  if (is_active(t)) {
    let respd = current_respd_exn(t);
    Body.close_writer(respd.request_body);
  };
};

let shutdown = t => {
  Queue.iter(Respd.close_response_body, t.request_queue);
  Queue.clear(t.request_queue);
  shutdown_reader(t);
  shutdown_writer(t);
  wakeup_reader(t);
  wakeup_writer(t);
};

let set_error_and_handle = (t, error) => {
  Queue.iter(
    respd =>
      switch (Respd.input_state(respd)) {
      | Wait
      | Ready => Respd.report_error(respd, error)
      | Complete =>
        switch (Reader.next(t.reader)) {
        | `Error(_)
        | `Read => Respd.report_error(respd, error)
        | _ =>
          /* Don't bother reporting errors to responses that have already
           * completed. */
          ()
        }
      },
    t.request_queue,
  );
  /* From RFC7230§6.5:
   *   A client sending a message body SHOULD monitor the network connection
   *   for an error response while it is transmitting the request.  If the
   *   client sees a response that indicates the server does not wish to
   *   receive the message body and is closing the connection, the client
   *   SHOULD immediately cease transmitting the body and close its side of the
   *   connection. */
  shutdown(t);
};

let unexpected_eof = t => {
  set_error_and_handle(t, `Malformed_response("unexpected eof"));
  shutdown(t);
};

let report_exn = (t, exn) => set_error_and_handle(t, `Exn(exn));

exception Local;

let maybe_pipeline_queued_requests = t =>
  /* Don't bother trying to pipeline if there aren't multiple requests in the
   * queue. */
  if (Queue.length(t.request_queue) > 1) {
    try({
      let _ =
        Queue.fold(
          (prev, respd) => {
            switch (prev) {
            | None => ()
            | Some(prev) =>
              switch (respd.Respd.state, Respd.output_state(prev)) {
              | (Uninitialized, Complete) => Respd.write_request(respd)
              | _ =>
                /* bail early. If we can't pipeline this request, we can't write
                 * next ones either. */
                raise(Local)
              }
            };
            Some(respd);
          },
          None,
          t.request_queue,
        );
      ();
    }) {
    | _ => ()
    };
  };

let advance_request_queue = t => {
  ignore(Queue.take(t.request_queue));
  if (!Queue.is_empty(t.request_queue)) {
    /* write request to the wire */
    let respd = current_respd_exn(t);
    switch (respd.state) {
    | Uninitialized =>
      /* Only write request if it hasn't been written to the wire yet (e.g. via
       * pipelining). */
      Respd.write_request(respd);
      wakeup_writer(t);
    | _ => ()
    };
  };
};

let rec _next_read_operation = t =>
  if (!is_active(t)) {
    if (Reader.is_closed(t.reader)) {
      shutdown(t);
    };
    Reader.next(t.reader);
  } else {
    let respd = current_respd_exn(t);
    switch (Respd.input_state(respd)) {
    | Wait => `Yield
    | Ready => Reader.next(t.reader)
    | Complete => _final_read_operation_for(t, respd)
    };
  }

and _final_read_operation_for = (t, respd) => {
  let next =
    if (!Respd.persistent_connection(respd)) {
      shutdown_reader(t);
      Reader.next(t.reader);
    } else {
      switch (Respd.output_state(respd)) {
      | Waiting
      | Ready => `Yield
      | Complete =>
        switch (Reader.next(t.reader)) {
        | (`Error(_) | `Read) as operation =>
          /* Keep reading when in a "partial" state (`Read).
           * Don't advance the request queue if in an error state. */
          operation
        | _ =>
          advance_request_queue(t);
          _next_read_operation(t);
        }
      };
    };

  wakeup_writer(t);
  next;
};

let next_read_operation = t =>
  switch (_next_read_operation(t)) {
  | `Error(`Parse(marks, message)) =>
    let message =
      String.concat("", [String.concat(">", marks), ": ", message]);
    set_error_and_handle(t, `Malformed_response(message));
    `Close;
  | `Error(`Invalid_response_body_length(_) as error) =>
    set_error_and_handle(t, error);
    `Close;
  | `Start => `Read
  | (`Read | `Yield | `Close) as operation => operation
  };

let read_with_more = (t, bs, ~off, ~len, more) => {
  let consumed = Reader.read_with_more(t.reader, bs, ~off, ~len, more);
  if (is_active(t)) {
    Respd.flush_response_body(current_respd_exn(t));
  };
  consumed;
};

let read = (t, bs, ~off, ~len) =>
  read_with_more(t, bs, ~off, ~len, Incomplete);

let read_eof = (t, bs, ~off, ~len) => {
  let bytes_read = read_with_more(t, bs, ~off, ~len, Complete);
  if (is_active(t)) {
    unexpected_eof(t);
  };
  bytes_read;
};

let rec _next_write_operation = t =>
  if (!is_active(t)) {
    if (Reader.is_closed(t.reader)) {
      shutdown(t);
    };
    Writer.next(t.writer);
  } else {
    let respd = current_respd_exn(t);
    switch (Respd.output_state(respd)) {
    | Waiting => `Yield
    | Ready =>
      Respd.flush_request_body(respd);
      Writer.next(t.writer);
    | Complete => _final_write_operation_for(t, respd)
    };
  }

and _final_write_operation_for = (t, respd) => {
  let next =
    if (!Respd.persistent_connection(respd)) {
      shutdown_writer(t);
      Writer.next(t.writer);
    } else {
      /* From RFC7230§6.3.2:
       *   A client that supports persistent connections MAY "pipeline" its
       *   requests (i.e., send multiple requests without waiting for each
       *   response). */
      maybe_pipeline_queued_requests(t);
      switch (Respd.input_state(respd)) {
      | Wait
      | Ready => Writer.next(t.writer)
      | Complete =>
        switch (Reader.next(t.reader)) {
        | `Error(_)
        | `Read => Writer.next(t.writer)
        | _ =>
          advance_request_queue(t);
          _next_write_operation(t);
        }
      };
    };

  wakeup_reader(t);
  next;
};

let next_write_operation = t => _next_write_operation(t);

let report_write_result = (t, result) =>
  Writer.report_result(t.writer, result);
