/*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.

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

type request_handler('fd, 'io) = Reqd.t('fd, 'io) => unit;

type error = [
  | `Bad_gateway
  | `Bad_request
  | `Internal_server_error
  | `Exn(exn)
];

type error_handler =
  (~request: Request.t=?, error, Headers.t => Body.t([ | `write])) => unit;

type t('fd, 'io) = {
  reader: Reader.request,
  writer: Writer.t,
  response_body_buffer: Bigstringaf.t,
  request_handler: request_handler('fd, 'io),
  error_handler,
  request_queue:
    Queue.t(Reqd.t('fd, 'io)),
    /* invariant: If [request_queue] is not empty, then the head of the queue
       has already had [request_handler] called on it. */
  wakeup_writer: ref(list(unit => unit)),
  wakeup_reader:
    ref(list(unit => unit)),
    /* Represents an unrecoverable error that will cause the connection to
     * shutdown. Holds on to the response body created by the error handler
     * that might be streaming to the client. */
  mutable error_code: [ | `Ok | `Error(Body.t([ | `write]))],
};

let is_closed = t =>
  Reader.is_closed(t.reader) && Writer.is_closed(t.writer);

let is_waiting = t => !is_closed(t) && Queue.is_empty(t.request_queue);

let is_active = t => !Queue.is_empty(t.request_queue);

let current_reqd_exn = t => Queue.peek(t.request_queue);

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

let rec _transfer_writer_callbacks = (fs, reqd) =>
  switch (fs) {
  | [] => ()
  | [f, ...fs] =>
    Reqd.on_more_output_available(reqd, f);
    _transfer_writer_callbacks(fs, reqd);
  };

let transfer_writer_callbacks = (t, reqd) => {
  let fs = t.wakeup_writer^;
  t.wakeup_writer := [];
  _transfer_writer_callbacks(fs, reqd);
};

let wakeup_reader = t => {
  let fs = t.wakeup_reader^;
  t.wakeup_reader := [];
  List.iter(f => f(), fs);
};

let default_error_handler = (~request as _=?, error, handle) => {
  let message =
    switch (error) {
    | `Exn(exn) => Printexc.to_string(exn)
    | (#Status.client_error | #Status.server_error) as error =>
      Status.to_string(error)
    };

  let body = handle(Headers.empty);
  Body.write_string(body, message);
  Body.close_writer(body);
};

let create =
    (
      ~config=Config.default,
      ~error_handler=default_error_handler,
      request_handler,
    ) => {
  let {Config.response_buffer_size, response_body_buffer_size, _} = config;

  let writer = Writer.create(~buffer_size=response_buffer_size, ());
  let request_queue = Queue.create();
  let response_body_buffer = Bigstringaf.create(response_body_buffer_size);
  let handler = (request, request_body) => {
    let reqd =
      Reqd.create(
        error_handler,
        request,
        request_body,
        writer,
        response_body_buffer,
      );

    Queue.push(reqd, request_queue);
  };

  {
    reader: Reader.request(handler),
    writer,
    response_body_buffer,
    request_handler,
    error_handler,
    request_queue,
    wakeup_writer: ref([]),
    wakeup_reader: ref([]),
    error_code: `Ok,
  };
};

let shutdown_reader = t => {
  Reader.force_close(t.reader);
  if (is_active(t)) {
    Reqd.close_request_body(current_reqd_exn(t));
  } else {
    wakeup_reader(t);
  };
};

let shutdown_writer = t => {
  Writer.close(t.writer);
  if (is_active(t)) {
    Reqd.close_request_body(current_reqd_exn(t));
  } else {
    wakeup_writer(t);
  };
};

let error_code = t =>
  if (is_active(t)) {
    Reqd.error_code(current_reqd_exn(t));
  } else {
    None;
  };

let shutdown = t => {
  shutdown_reader(t);
  shutdown_writer(t);
  wakeup_reader(t);
  wakeup_writer(t);
};

let set_error_and_handle = (~request=?, t, error) =>
  if (is_active(t)) {
    assert(request == None);
    let reqd = current_reqd_exn(t);
    Reqd.report_error(reqd, error);
  } else {
    let status =
      switch ((error :> [ error | Status.standard])) {
      | `Exn(_) => `Internal_server_error
      | #Status.standard as status => status
      };

    shutdown_reader(t);
    let writer = t.writer;
    switch (t.error_code) {
    | `Ok =>
      let body = Body.of_faraday(Writer.faraday(writer));
      t.error_code = `Error(body);
      t.error_handler(
        ~request?,
        error,
        headers => {
          Writer.write_response(writer, Response.create(~headers, status));
          wakeup_writer(t);
          body;
        },
      );
    | `Error(_) =>
      /* This should not happen. Even if we try to read more, the parser does
       * not ingest it, and even if someone attempts to feed more bytes to the
       * server when we already told them to [`Close], it's not really our
       * problem. */
      assert(false)
    };
  };

let report_exn = (t, exn) => set_error_and_handle(t, `Exn(exn));

let advance_request_queue_if_necessary = t =>
  if (is_active(t)) {
    let reqd = current_reqd_exn(t);
    if (Reqd.persistent_connection(reqd)) {
      if (Reqd.is_complete(reqd)) {
        ignore(Queue.take(t.request_queue));
        if (!Queue.is_empty(t.request_queue)) {
          t.request_handler(current_reqd_exn(t));
        };
        wakeup_reader(t);
      };
    } else {
      ignore(Queue.take(t.request_queue));
      Queue.iter(Reqd.close_request_body, t.request_queue);
      Queue.clear(t.request_queue);
      Queue.push(reqd, t.request_queue);
      wakeup_writer(t);
      if (Reqd.is_complete(reqd)) {
        shutdown(t);
      } else if (!Reqd.requires_input(reqd)) {
        shutdown_reader(t);
      };
    };
  } else if (Reader.is_failed(t.reader)) {
    /* Don't tear down the whole connection if we saw an unrecoverable parsing
     * error, as we might be in the process of streaming back the error
     * response body to the client. */
    shutdown_reader(t);
  } else if (Reader.is_closed(t.reader)) {
    shutdown(t);
  };

let _next_read_operation = t => {
  advance_request_queue_if_necessary(t);
  if (is_active(t)) {
    let reqd = current_reqd_exn(t);
    if (Reqd.requires_input(reqd)) {
      Reader.next(t.reader);
    } else if (Reqd.persistent_connection(reqd)) {
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
  | `Error(`Parse(_)) =>
    set_error_and_handle(t, `Bad_request);
    `Close;
  | `Error(`Bad_request(request)) =>
    set_error_and_handle(~request, t, `Bad_request);
    `Close;
  | (`Read | `Yield | `Close) as operation =>
    if (is_active(t)) {
      let reqd = current_reqd_exn(t);
      switch (Reqd.upgrade_handler(reqd)) {
      | Some(_) => `Upgrade
      | None => operation
      };
    } else {
      operation;
    }
  };

let read_with_more = (t, bs, ~off, ~len, more) => {
  let call_handler = Queue.is_empty(t.request_queue);
  let consumed = Reader.read_with_more(t.reader, bs, ~off, ~len, more);
  if (is_active(t)) {
    let reqd = current_reqd_exn(t);
    if (call_handler) {
      transfer_writer_callbacks(t, reqd);
      t.request_handler(reqd);
    };
    Reqd.flush_request_body(reqd);
  };
  consumed;
};

let read = (t, bs, ~off, ~len) =>
  read_with_more(t, bs, ~off, ~len, Incomplete);

let read_eof = (t, bs, ~off, ~len) =>
  read_with_more(t, bs, ~off, ~len, Complete);

let yield_reader = (t, k) => on_wakeup_reader(t, k);

let next_write_operation = t => {
  advance_request_queue_if_necessary(t);
  if (is_active(t)) {
    let reqd = current_reqd_exn(t);
    Reqd.flush_response_body(reqd);
    switch (Writer.next(t.writer)) {
    | `Write(iovecs) as write_op =>
      switch (Reqd.upgrade_handler(reqd)) {
      | Some(upgrade) => `Upgrade((iovecs, upgrade))
      | None => write_op
      }
    | operation => operation
    };
  } else {
    Writer.next(t.writer);
  };
};

let report_write_result = (t, result) =>
  Writer.report_result(t.writer, result);

let yield_writer = (t, k) =>
  if (is_active(t)) {
    let reqd = current_reqd_exn(t);
    if (Reqd.requires_output(reqd)) {
      Reqd.on_more_output_available(reqd, k);
    } else if (Reqd.persistent_connection(reqd)) {
      on_wakeup_writer(t, k);
    } else {
      shutdown(t);
      k();
    };
  } else if (Writer.is_closed(t.writer)) {
    k();
  } else {
    switch (t.error_code) {
    | `Ok => on_wakeup_writer(t, k)
    | `Error(body) => Body.when_ready_to_write(body, k)
    };
  };
