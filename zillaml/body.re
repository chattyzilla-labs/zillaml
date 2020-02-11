/*----------------------------------------------------------------------------
    Copyright (c) 2018 Inhabited Type LLC.
    Copyright (c) 2020 Dakota Murphy.
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

type t(_) = {
  faraday: Faraday.t,
  mutable read_scheduled: bool,
  mutable write_final_if_chunked: bool,
  mutable on_eof: unit => unit,
  mutable on_read: (Bigstringaf.t, ~off: int, ~len: int) => unit,
  mutable when_ready_to_write: unit => unit,
  buffered_bytes: ref(int),
};

let default_on_eof = Sys.opaque_identity(() => ());
let default_on_read = Sys.opaque_identity((_, ~off as _, ~len as _) => ());
let default_ready_to_write = Sys.opaque_identity(() => ());

let of_faraday = faraday => {
  faraday,
  read_scheduled: false,
  write_final_if_chunked: true,
  on_eof: default_on_eof,
  on_read: default_on_read,
  when_ready_to_write: default_ready_to_write,
  buffered_bytes: ref(0),
};

let create = buffer => of_faraday(Faraday.of_bigstring(buffer));

let create_empty = () => {
  let t = create(Bigstringaf.empty);
  t.write_final_if_chunked = false;
  Faraday.close(t.faraday);
  t;
};

let empty = create_empty();

let set_non_chunked = t => t.write_final_if_chunked = false;

let write_char = (t, c) => Faraday.write_char(t.faraday, c);

let write_string = (t, ~off=?, ~len=?, s) =>
  Faraday.write_string(~off?, ~len?, t.faraday, s);

let write_bigstring = (t, ~off=?, ~len=?, b) =>
  Faraday.write_bigstring(~off?, ~len?, t.faraday, b);

let schedule_bigstring = (t, ~off=?, ~len=?, b: Bigstringaf.t) =>
  Faraday.schedule_bigstring(~off?, ~len?, t.faraday, b);

let ready_to_write = t => {
  let callback = t.when_ready_to_write;
  t.when_ready_to_write = default_ready_to_write;
  callback();
};

let flush = (t, kontinue) => {
  Faraday.flush(t.faraday, kontinue);
  ready_to_write(t);
};

let is_closed = t => Faraday.is_closed(t.faraday);

let close_writer = t => {
  Faraday.close(t.faraday);
  ready_to_write(t);
};

let unsafe_faraday = t => t.faraday;

let rec do_execute_read = (t, on_eof, on_read) =>
  switch (Faraday.operation(t.faraday)) {
  | `Yield => ()
  | `Close =>
    t.read_scheduled = false;
    t.on_eof = default_on_eof;
    t.on_read = default_on_read;
    on_eof();
  | `Writev([]) => assert(false)
  | `Writev([iovec, ..._]) =>
    t.read_scheduled = false;
    t.on_eof = default_on_eof;
    t.on_read = default_on_read;
    let {IOVec.buffer, off, len} = iovec;
    Faraday.shift(t.faraday, len);
    on_read(buffer, ~off, ~len);
    execute_read(t);
  }
and execute_read = t =>
  if (t.read_scheduled) {
    do_execute_read(t, t.on_eof, t.on_read);
  };

let schedule_read = (t, ~on_eof, ~on_read) => {
  if (t.read_scheduled) {
    failwith("Body.schedule_read: reader already scheduled");
  };
  if (is_closed(t)) {
    do_execute_read(t, on_eof, on_read);
  } else {
    t.read_scheduled = true;
    t.on_eof = on_eof;
    t.on_read = on_read;
  };
};

let has_pending_output = t =>
  /* Force another write poll to make sure that the final chunk is emitted for
     chunk-encoded bodies.

     Note that the body data type does not keep track of encodings, so it is
     necessary for [transfer_to_writer_with_encoding] to check the encoding and
     clear the [write_final_if_chunked] field when outputting a fixed or
     close-delimited body. */
  Faraday.has_pending_output(t.faraday)
  || Faraday.is_closed(t.faraday)
  && t.write_final_if_chunked;

let close_reader = t => {
  Faraday.close(t.faraday);
  execute_read(t);
};

let when_ready_to_write = (t, callback) =>
  if (!(t.when_ready_to_write === default_ready_to_write)) {
    failwith(
      "Body.when_ready_to_write: only one callback can be registered at a time",
    );
  } else if (is_closed(t)) {
    callback();
  } else {
    t.when_ready_to_write = callback;
  };

let transfer_to_writer_with_encoding = (t, ~encoding, writer) => {
  let faraday = t.faraday;
  switch (t.write_final_if_chunked, encoding) {
  | (true, `Fixed(_) | `Close_delimited) =>
    /* Play nicely with [has_pending_output] in the case of a fixed or
       close-delimited encoding.

       Immediately set `t.write_final_if_chunked` to `false` because we may
       not have another opportunity to do so before advancing the request
       queue. */
    t.write_final_if_chunked = false
  | (false, `Fixed(_) | `Close_delimited)
  | (_, `Chunked) =>
    /* Handled explicitly later when closing the writer. */
    ()
  };
  switch (Faraday.operation(faraday)) {
  | `Yield => ()
  | `Close =>
    let must_write_the_final_chunk = t.write_final_if_chunked;
    t.write_final_if_chunked = false;
    if (must_write_the_final_chunk) {
      switch (encoding) {
      | `Chunked => Serialize.Writer.schedule_chunk(writer, [])
      | `Fixed(_)
      | `Close_delimited => ()
      };
    };
    Serialize.Writer.unyield(writer);
  | `Writev(iovecs) =>
    let buffered = t.buffered_bytes;
    switch (IOVec.shiftv(iovecs, buffered^)) {
    | [] => ()
    | iovecs =>
      let lengthv = IOVec.lengthv(iovecs);
      buffered := buffered^ + lengthv;
      switch (encoding) {
      | `Fixed(_)
      | `Close_delimited => Serialize.Writer.schedule_fixed(writer, iovecs)
      | `Chunked => Serialize.Writer.schedule_chunk(writer, iovecs)
      };
      Serialize.Writer.flush(
        writer,
        () => {
          Faraday.shift(faraday, lengthv);
          buffered := buffered^ - lengthv;
        },
      );
    };
  };
};