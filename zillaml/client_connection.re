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
  request_queue:
    Queue.t(Respd.t),
    /* invariant: If [request_queue] is not empty, then the head of the queue
       has already written the request headers to the wire. */
  wakeup_writer: ref(list(unit => unit)),
  wakeup_reader: ref(list(unit => unit)),
};

let is_closed = t =>
  Reader.is_closed(t.reader) && Writer.is_closed(t.writer);

let is_waiting = t => !is_closed(t) && Queue.is_empty(t.request_queue);

let is_active = t => !Queue.is_empty(t.request_queue);

let current_respd_exn = t => Queue.peek(t.request_queue);

let on_wakeup_reader = (t, k) =>
  if (is_closed(t)) {
    failwith("on_wakeup_reader on closed conn");
  } else {
    t.wakeup_reader := [k, ...t.wakeup_reader^];
  };

let on_wakeup_writer = (t, k) =>
  if (is_closed(t)) {
    failwith("on_wakeup_writer on closed conn");
  } else {
    t.wakeup_writer := [k, ...t.wakeup_writer^];
  };

let wakeup_writer = t => {
  let fs = t.wakeup_writer^;
  t.wakeup_writer := [];
  List.iter(f => f(), fs);
};

let wakeup_reader = t => {
  let fs = t.wakeup_reader^;
  t.wakeup_reader := [];
  List.iter(f => f(), fs);
};

let create = (~config=Config.default, ()) => {
  let request_queue = Queue.create();
  {
    config,
    reader: Reader.response(request_queue),
    writer: Writer.create(),
    request_queue,
    wakeup_writer: ref([]),
    wakeup_reader: ref([]),
  };
};

let request = (t, request, ~error_handler, ~response_handler) => {
  let request_body =
    Body.create(Bigstringaf.create(t.config.request_body_buffer_size));

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

let flush_request_body = t =>
  if (is_active(t)) {
    let respd = current_respd_exn(t);
    Respd.flush_request_body(respd);
  };

let set_error_and_handle_without_shutdown = (t, error) =>
  if (is_active(t)) {
    let respd = current_respd_exn(t);
    Respd.report_error(respd, error);
  };
/* TODO: not active?! can be because of a closed FD for example. */

let unexpected_eof = t =>
  set_error_and_handle_without_shutdown(
    t,
    `Malformed_response("unexpected eof"),
  );

let shutdown_reader = t => {
  Reader.force_close(t.reader);
  if (is_active(t)) {
    Respd.close_response_body(current_respd_exn(t));
  } else {
    wakeup_reader(t);
  };
};

let shutdown_writer = t => {
  flush_request_body(t);
  Writer.close(t.writer);
  if (is_active(t)) {
    let respd = current_respd_exn(t);
    Body.close_writer(respd.request_body);
  };
};

let shutdown = t => {
  shutdown_reader(t);
  shutdown_writer(t);
  wakeup_reader(t);
  wakeup_writer(t);
};

/* TODO: Need to check in the RFC if reporting an error, e.g. in a malformed
 * response causes the whole connection to shutdown. */
let set_error_and_handle = (t, error) => {
  shutdown(t);
  set_error_and_handle_without_shutdown(t, error);
};

let report_exn = (t, exn) => set_error_and_handle(t, `Exn(exn));

exception Local;

let maybe_pipeline_queued_requests = t =>
  /* Don't bother trying to pipeline if there aren't multiple requests in the
   * queue. */
  if (Queue.length(t.request_queue) > 1) {
    switch (
      Queue.fold(
        (prev, respd) => {
          switch (prev) {
          | None => ()
          | Some(prev) =>
            if (respd.Respd.state == Uninitialized
                && !Respd.requires_output(prev)) {
              Respd.write_request(respd);
            } else {
              /* bail early. If we can't pipeline this request, we can't write
               * next ones either. */
              raise(Local);
            }
          };
          Some(respd);
        },
        None,
        t.request_queue,
      )
    ) {
    | _ => ()
    | exception Local => ()
    };
  };

let advance_request_queue_if_necessary = t =>
  if (is_active(t)) {
    let respd = current_respd_exn(t);
    if (Respd.persistent_connection(respd)) {
      if (Respd.is_complete(respd)) {
        ignore(Queue.take(t.request_queue));
        if (!Queue.is_empty(t.request_queue)) {
          /* write request to the wire */
          let respd = current_respd_exn(t);
          Respd.write_request(respd);
        };
        wakeup_writer(t);
      } else if (!Respd.requires_output(respd)) {
        /* From RFC7230§6.3.2:
         *   A client that supports persistent connections MAY "pipeline" its
         *   requests (i.e., send multiple requests without waiting for each
         *   response). */
        maybe_pipeline_queued_requests(t);
      };
    } else {
      ignore(Queue.take(t.request_queue));
      Queue.iter(Respd.close_response_body, t.request_queue);
      Queue.clear(t.request_queue);
      Queue.push(respd, t.request_queue);
      wakeup_writer(t);
      if (Respd.is_complete(respd)) {
        shutdown(t);
      } else if (!Respd.requires_output(respd)) {
        shutdown_writer(t);
      };
    };
  } else if (Reader.is_closed(t.reader)) {
    shutdown(t);
  };

let _next_read_operation = t => {
  advance_request_queue_if_necessary(t);
  if (is_active(t)) {
    let respd = current_respd_exn(t);
    if (Respd.requires_input(respd)) {
      Reader.next(t.reader);
    } else if (Respd.persistent_connection(respd)) {
      `Yield;
    } else {
      shutdown_reader(t);
      Reader.next(t.reader);
    };
  } else {
    Reader.next(t.reader);
  };
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
    let respd = current_respd_exn(t);
    /* TODO: could just check for `Respd.requires_input`? */
    switch (respd.state) {
    | Uninitialized => assert(false)
    | Received_response(_)
    | Closed
    | Upgraded(_) => ()
    | Awaiting_response =>
      /* TODO: review this. It makes sense to tear down the connection if an
       * unexpected EOF is received. */
      shutdown(t);
      unexpected_eof(t);
    };
  };
  bytes_read;
};

let next_write_operation = t => {
  advance_request_queue_if_necessary(t);
  flush_request_body(t);
  Writer.next(t.writer);
};

let yield_reader = (t, k) => on_wakeup_reader(t, k);

let yield_writer = (t, k) =>
  if (is_active(t)) {
    let respd = current_respd_exn(t);
    if (Respd.requires_output(respd)) {
      Respd.on_more_output_available(respd, k);
    } else if (Respd.persistent_connection(respd)) {
      on_wakeup_writer(t, k);
    } else {
      /*  TODO: call shutdown? */
      Writer.close(t.writer);
      k();
    };
  } else {
    on_wakeup_writer(t, k);
  };

let report_write_result = (t, result) =>
  Writer.report_result(t.writer, result);
