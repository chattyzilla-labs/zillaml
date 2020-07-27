/*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.
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

include Angstrom;

module P = {
  let is_space =
    fun
    | ' '
    | '\t' => true
    | _ => false;

  let is_cr =
    fun
    | '\r' => true
    | _ => false;

  let is_space_or_colon =
    fun
    | ' '
    | '\t'
    | ':' => true
    | _ => false;

  let is_hex =
    fun
    | '0'..'9'
    | 'a'..'f'
    | 'A'..'F' => true
    | _ => false;

  let is_digit =
    fun
    | '0'..'9' => true
    | _ => false;

  let is_separator =
    fun
    | ')'
    | '('
    | '<'
    | '>'
    | '@'
    | ','
    | ';'
    | ':'
    | '\\'
    | '"'
    | '/'
    | '['
    | ']'
    | '?'
    | '='
    | '{'
    | '}'
    | ' '
    | '\t' => true
    | _ => false;

  let is_token =
    /* The commented-out ' ' and '\t' are not necessary because of the range at
     * the top of the match. */
    fun
    | '\000'..'\031'
    | '\127'
    | ')'
    | '('
    | '<'
    | '>'
    | '@'
    | ','
    | ';'
    | ':'
    | '\\'
    | '"'
    | '/'
    | '['
    | ']'
    | '?'
    | '='
    | '{'
    | '}' /* | ' ' | '\t' */ => false
    | _ => true;
};

let unit = return();
let token = take_while1(P.is_token);
let spaces = skip_while(P.is_space);

let digit =
  satisfy(P.is_digit)
  >>| (
    fun
    | '0' => 0
    | '1' => 1
    | '2' => 2
    | '3' => 3
    | '4' => 4
    | '5' => 5
    | '6' => 6
    | '7' => 7
    | '8' => 8
    | '9' => 9
    | _ => assert(false)
  );

let eol = string("\r\n") <?> "eol";
let hex = str =>
  try(return(Int64.of_string("0x" ++ str))) {
  | _ => fail("hex")
  };
let skip_line = take_till(P.is_cr) *> eol;

let version =
  string("HTTP/")
  *> lift2(
       (major, minor) => {Version.major, minor},
       digit <* char('.'),
       digit,
     );

let header =
  /* From RFC7230§3.2.4:

        "No whitespace is allowed between the header field-name and colon.  In
        the past, differences in the handling of such whitespace have led to
        security vulnerabilities in request routing and response handling.  A
        server MUST reject any received request message that contains whitespace
        between a header field-name and colon with a response code of 400 (Bad
        Request).  A proxy MUST remove any such whitespace from a response
        message before forwarding the message downstream."

     This can be detected by checking the message and marks in a parse failure,
     which should look like this when serialized "... > header > :". */
  lift2(
    (key, value) => (key, value),
    take_till(P.is_space_or_colon) <* char(':') <* spaces,
    take_till(P.is_cr) <* eol >>| String.trim,
  )
  <* commit
  <?> "header";

let headers = {
  let cons = (x, xs) => [x, ...xs];
  fix(headers => {
    let _emp = return([]);
    let _rec = lift2(cons, header, headers);
    peek_char_fail
    >>= (
      fun
      | '\r' => _emp
      | _ => _rec
    );
  })
  >>| Headers.of_list;
};

let request = {
  let meth = take_till(P.is_space) >>| Method.of_string;
  lift4(
    (meth, target, version, headers) =>
      Request.create(~version, ~headers, meth, target),
    meth <* char(' '),
    take_till(P.is_space) <* char(' '),
    version <* eol <* commit,
    headers <* eol,
  );
};

let response = {
  let status =
    take_while(P.is_digit)
    >>= (
      str =>
        if (String.length(str) == 0) {
          fail("status-code empty");
        } else if (String.length(str) > 3) {
          fail(Printf.sprintf("status-code too long: %S", str));
        } else {
          return(Status.of_string(str));
        }
    );

  lift4(
    (version, status, reason, headers) =>
      Response.create(~reason, ~version, ~headers, status),
    version <* char(' '),
    status <* char(' '),
    take_till(P.is_cr) <* eol <* commit,
    headers <* eol,
  );
};

let finish = writer => {
  Body.close_reader(writer);
  commit;
};

let schedule_size = (writer, n) => {
  let faraday = Body.unsafe_faraday(writer);
  /* XXX(seliopou): performance regression due to switching to a single output
   * format in Farady. Once a specialized operation is exposed to avoid the
   * intemediate copy, this should be back to the original performance. */
  (
    if (Faraday.is_closed(faraday)) {
      advance(n);
    } else {
      take(n) >>| (s => Faraday.write_string(faraday, s));
    }
  )
  *> commit;
};

let body = (~encoding, writer) => {
  let rec fixed = (n, ~unexpected) =>
    if (n == 0L) {
      unit;
    } else {
      at_end_of_input
      >>= (
        fun
        | true => commit *> fail(unexpected)
        | false =>
          available
          >>= (
            m => {
              let m' = Int64.(min(of_int(m), n));
              let n' = Int64.sub(n, m');
              schedule_size(writer, Int64.to_int(m'))
              >>= (() => fixed(n', ~unexpected));
            }
          )
      );
    };

  switch (encoding) {
  | `Fixed(n) =>
    fixed(n, ~unexpected="expected more from fixed body")
    >>= (() => finish(writer))
  | `Chunked =>
    /* XXX(seliopou): The [eol] in this parser should really parse a collection
     * of "chunk extensions", as defined in RFC7230§4.1. These do not show up
     * in the wild very frequently, and the httpaf API has no way of exposing
     * them to the suer, so for now the parser does not attempt to recognize
     * them. This means that any chunked messages that contain chunk extensions
     * will fail to parse. */
    fix(p =>{
      let _hex =
        take_while1(P.is_hex)
        >>= (size => hex(size))
        /* swallows chunk-ext, if present, and CRLF */
        <* eol
        *> commit;

      _hex
      >>= (
        size =>
          if (size == 0L) {
            eol *> finish(writer);
          } else {
            fixed(size, ~unexpected="expected more from body chunk")
            *> eol
            *> p;
          }
      );
    })
  | `Close_delimited =>
    fix(p => {
      let _rec = (available >>= (n => schedule_size(writer, n))) *> p;
      at_end_of_input
      >>= (
        fun
        | true => finish(writer)
        | false => _rec
      );
    })
  };
};

module Reader = {
  module AU = Angstrom.Unbuffered;

  type request_error = [
    | `Bad_request(Request.t)
    | `Parse(list(string), string)
  ];

  type response_error = [
    | `Invalid_response_body_length(Response.t)
    | `Parse(list(string), string)
  ];

  type parse_state('error) =
    | Done
    | Fail('error)
    | Partial(
        (Bigstringaf.t, ~off: int, ~len: int, AU.more) =>
        AU.state(result(unit, 'error)),
      );

  type t('error) = {
    parser: Angstrom.t(result(unit, 'error)),
    mutable parse_state:
      parse_state('error),
      /* The state of the parse for the current request */
    mutable closed:
      bool,
      /* Whether the input source has left the building, indicating that no
       * further input will be received. */
    mutable wakeup: Optional_thunk.t,
  };

  type request = t(request_error);
  type response = t(response_error);

  let create = parser => {
    parser,
    parse_state: Done,
    closed: false,
    wakeup: Optional_thunk.none,
  };

  let ok = return(Ok());

  let is_closed = t => t.closed;

  let on_wakeup = (t, k) =>
    if (is_closed(t)) {
      failwith("on_wakeup on closed reader");
    } else if (Optional_thunk.is_some(t.wakeup)) {
      failwith("on_wakeup: only one callback can be registered at a time");
    } else {
      t.wakeup = Optional_thunk.some(k);
    };

  let wakeup = t => {
    let f = t.wakeup;
    t.wakeup = Optional_thunk.none;
    Optional_thunk.call_if_some(f);
  };

  let request = handler => {
    let parser = (t, handler) =>
      request
      <* commit
      >>= (
        request =>
          switch (Request.body_length(request)) {
          | `Error(`Bad_request) => return(Error(`Bad_request(request)))
          | `Fixed(0L) =>
            handler(request, Body.empty);
            ok;
          | (`Fixed(_) | `Chunked | `Close_delimited) as encoding =>
            let request_body =
              Body.create(
                Bigstringaf.empty,
                Optional_thunk.some(() => wakeup(Lazy.force(t))),
              );

            handler(request, request_body);
            body(~encoding, request_body) *> ok;
          }
      );

    let rec t = lazy(create(parser(t, handler)));
    Lazy.force(t);
  };

  let response = request_queue => {
    let parser = (t, request_queue) =>
      response
      <* commit
      >>= (
        response => {
          assert(!Queue.is_empty(request_queue));
          exception Local(Respd.t);
          let respd =
            switch (
              Queue.iter(
                respd =>
                  if (respd.Respd.state == Awaiting_response) {
                    raise(Local(respd));
                  },
                request_queue,
              )
            ) {
            | exception (Local(respd)) => respd
            | _ => assert(false)
            };

          let request = Respd.request(respd);
          let proxy = false;
          switch (
            Response.body_length(~request_method=request.meth, response)
          ) {
          | `Error(`Bad_gateway) =>
            assert(!proxy);
            assert(false);
          | `Error(`Internal_server_error) =>
            return(Error(`Invalid_response_body_length(response)))
          | `Fixed(0L) =>
            respd.response_handler(response, Body.empty);
            ok;
          | (`Fixed(_) | `Chunked | `Close_delimited) as encoding =>
            let response_body =
              Body.create(
                Bigstringaf.empty,
                Optional_thunk.some(() => wakeup(Lazy.force(t))),
              );

            respd.response_handler(response, response_body);
            body(~encoding, response_body) *> ok;
          };
        }
      );

    let rec t = lazy(create(parser(t, request_queue)));
    Lazy.force(t);
  };

  let transition = (t, state) =>
    switch (state) {
    | AU.Done(consumed, Ok ()) =>
      t.parse_state = Done;
      consumed;
    | AU.Done(consumed, Error(error)) =>
      t.parse_state = Fail(error);
      consumed;
    | AU.Fail(consumed, marks, msg) =>
      t.parse_state = Fail(`Parse((marks, msg)));
      consumed;
    | AU.Partial({committed, continue}) =>
      t.parse_state = Partial(continue);
      committed;
    }
  and start = (t, state) =>
    switch (state) {
    | AU.Done(_) => failwith("httpaf.Parse.unable to start parser")
    | AU.Fail(0, marks, msg) =>
      t.parse_state = Fail(`Parse((marks, msg)))
    | AU.Partial({committed: 0, continue}) =>
      t.parse_state = Partial(continue)
    | _ => assert(false)
    };

  let rec read_with_more = (t, bs, ~off, ~len, more) => {
    let initial =
      switch (t.parse_state) {
      | Done => true
      | _ => false
      };
    let consumed =
      switch (t.parse_state) {
      | Fail(_) => 0
      | Done =>
        start(t, AU.parse(t.parser));
        read_with_more(t, bs, ~off, ~len, more);
      | Partial(continue) => transition(t, continue(bs, more, ~off, ~len))
      };

    /* Special case where the parser just started and was fed a zero-length
     * bigstring. Avoid putting them parser in an error state in this scenario.
     * If we were already in a `Partial` state, return the error. */
    if (initial && len == 0) {
      t.parse_state = Done;
    };
    switch (more) {
    | Complete => t.closed = true
    | Incomplete => ()
    };
    consumed;
  };

  let force_close = t => t.closed = true;

  let next = t =>
    switch (t.parse_state) {
    | Fail(failure) => `Error(failure)
    | _ when t.closed => `Close
    | Done => `Start
    | Partial(_) => `Read
    };
};