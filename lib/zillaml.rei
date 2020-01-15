/*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.
    Copyright (c) 2019 Antonio Nuno Monteiro.
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

/** Http/af is a high-performance, memory-efficient, and scalable web server
    for OCaml. It implements the HTTP 1.1 specification with respect to
    parsing, serialization, and connection pipelining. For compatibility,
    http/af respects the imperatives of the [Server_connection] header when handling
    HTTP 1.0 connections.

    To use this library effectively, the user must be familiar with the HTTP
    1.1 specification, and the basic principles of memory management and
    vectorized IO. */;

open Result;

/** {2 Basic HTTP Types} */;

/** Protocol Version

    HTTP uses a "<major>.<minor>" numbering scheme to indicate versions of the
    protocol. The protocol version as a whole indicates the sender's conformance
    with the set of requirements laid out in that version's corresponding
    specification of HTTP.

    See {{:https://tools.ietf.org/html/rfc7230#section-2.6} RFC7230§2.6} for
    more details. */

module Version: {
  type t = {
    /** The major protocol number. */
    major: int,
    /** The minor protocol number. */
    minor: int,
  };

  let compare: (t, t) => int;

  let to_string: t => string;
  let of_string: string => t;

  [@ocaml.toplevel_printer]
  let pp_hum: (Format.formatter, t) => unit;
};

/** Request Method

    The request method token is the primary source of request semantics;
    it indicates the purpose for which the client has made this request
    and what is expected by the client as a successful result.

    See {{:https://tools.ietf.org/html/rfc7231#section-4} RFC7231§4} for more
    details. */

module Method: {
  type standard = [
    | /** {{:https://tools.ietf.org/html/rfc7231#section-4.3.1} RFC7231§4.3.1}. Safe, Cacheable. */
      `GET
    | /** {{:https://tools.ietf.org/html/rfc7231#section-4.3.2} RFC7231§4.3.2}. Safe, Cacheable. */
      `HEAD
    | /** {{:https://tools.ietf.org/html/rfc7231#section-4.3.3} RFC7231§4.3.3}. Cacheable. */
      `POST
    | /** {{:https://tools.ietf.org/html/rfc7231#section-4.3.4} RFC7231§4.3.4}. Idempotent. */
      `PUT
    | /** {{:https://tools.ietf.org/html/rfc7231#section-4.3.5} RFC7231§4.3.5}. Idempotent. */
      `DELETE
    | /** {{:https://tools.ietf.org/html/rfc7231#section-4.3.6} RFC7231§4.3.6}. */
      `CONNECT
    | /** {{:https://tools.ietf.org/html/rfc7231#section-4.3.7} RFC7231§4.3.7}. Safe.*/
      `OPTIONS
    | /** {{:https://tools.ietf.org/html/rfc7231#section-4.3.8} RFC7231§4.3.8}. Safe.*/
      `TRACE
  ];

  type t = [
    standard
    | /** Methods defined outside of RFC7231, or custom methods. */
      `Other(
        string,
      )
  ];

  /** Request methods are considered "safe" if their defined semantics are
      essentially read-only; i.e., the client does not request, and does not
      expect, any state change on the origin server as a result of applying a
      safe method to a target resource.  Likewise, reasonable use of a safe
      method is not expected to cause any harm, loss of property, or unusual
      burden on the origin server.

      See {{:https://tools.ietf.org/html/rfc7231#section-4.2.1} RFC7231§4.2.1}
      for more details. */

  let is_safe: standard => bool;

  /**  Request methods can be defined as "cacheable" to indicate that responses
       to them are allowed to be stored for future reuse.

       See {{:https://tools.ietf.org/html/rfc7234} RFC7234} for more details. */

  let is_cacheable: standard => bool;

  /** A request method is considered "idempotent" if the intended effect on
      the server of multiple identical requests with that method is the same as
      the effect for a single such request.  Of the request methods defined by
      this specification, PUT, DELETE, and safe request methods are idempotent.

      See {{:https://tools.ietf.org/html/rfc7231#section-4.2.2} RFC7231§4.2.2}
      for more details. */

  let is_idempotent: standard => bool;

  let to_string: t => string;
  let of_string: string => t;

  [@ocaml.toplevel_printer]
  let pp_hum: (Format.formatter, t) => unit;
};

/** Response Status Codes

   The status-code element is a three-digit integer code giving the result of
   the attempt to understand and satisfy the request.

   See {{:https://tools.ietf.org/html/rfc7231#section-6} RFC7231§6} for more
   details. */

module Status: {
  /** The 1xx (Informational) class of status code indicates an interim
      response for communicating connection status or request progress
      prior to completing the requested action and sending a final
      response.

      See {{:https://tools.ietf.org/html/rfc7231#section-6.2} RFC7231§6.2}
      for more details. */

  type informational = [ | `Continue | `Switching_protocols];

  /** The 2xx (Successful) class of status code indicates that the client's
      request was successfully received, understood, and accepted.

      See {{:https://tools.ietf.org/html/rfc7231#section-6.3} RFC7231§6.3}
      for more details. */

  type successful = [
    | `OK
    | `Created
    | `Accepted
    | `Non_authoritative_information
    | `No_content
    | `Reset_content
    | `Partial_content
  ];

  /** The 3xx (Redirection) class of status code indicates that further
      action needs to be taken by the user agent in order to fulfill the
      request.

      See {{:https://tools.ietf.org/html/rfc7231#section-6.4} RFC7231§6.4} for
      more details. */

  type redirection = [
    | `Multiple_choices
    | `Moved_permanently
    | `Found
    | `See_other
    | `Not_modified
    | `Use_proxy
    | `Temporary_redirect
  ];

  /** The 4xx (Client Error) class of status code indicates that the client
      seems to have erred.

      See {{:https://tools.ietf.org/html/rfc7231#section-6.5} RFC7231§6.5} for
      more details. */

  type client_error = [
    | `Bad_request
    | `Unauthorized
    | `Payment_required
    | `Forbidden
    | `Not_found
    | `Method_not_allowed
    | `Not_acceptable
    | `Proxy_authentication_required
    | `Request_timeout
    | `Conflict
    | `Gone
    | `Length_required
    | `Precondition_failed
    | `Payload_too_large
    | `Uri_too_long
    | `Unsupported_media_type
    | `Range_not_satisfiable
    | `Expectation_failed
    | `Upgrade_required
    | `I_m_a_teapot
    | `Enhance_your_calm
  ];

  /** The 5xx (Server Error) class of status code indicates that the server is
      aware that it has erred or is incapable of performing the requested
      method.

      See {{:https://tools.ietf.org/html/rfc7231#section-6.6} RFC7231§6.6} for
      more details. */

  type server_error = [
    | `Internal_server_error
    | `Not_implemented
    | `Bad_gateway
    | `Service_unavailable
    | `Gateway_timeout
    | `Http_version_not_supported
  ];

  /** The status codes defined in the HTTP 1.1 RFCs */

  type standard = [
    informational
    | successful
    | redirection
    | client_error
    | server_error
  ];

  /** The standard codes along with support for custom codes. */

  type t = [ standard | `Code(int)];

  /** [default_reason_phrase standard] is the example reason phrase provided
      by RFC7231 for the [standard] status code. The RFC allows servers to use
      reason phrases besides these in responses. */

  let default_reason_phrase: standard => string;

  /** [to_code t] is the integer representation of [t]. */

  let to_code: t => int;

  /** [of_code i] is the [t] representation of [i]. [of_code] raises [Failure]
      if [i] is not a positive three-digit number. */

  let of_code: int => t;

  /** [unsafe_of_code i] is equivalent to [of_code i], except it accepts any
      positive code, regardless of the number of digits it has. On negative
      codes, it will still raise [Failure]. */

  let unsafe_of_code: int => t;

  /** [is_informational t] is true iff [t] belongs to the Informational class
      of status codes. */

  let is_informational: t => bool;

  /** [is_successful t] is true iff [t] belongs to the Successful class of
      status codes. */

  let is_successful: t => bool;

  /** [is_redirection t] is true iff [t] belongs to the Redirection class of
      status codes. */

  let is_redirection: t => bool;

  /** [is_client_error t] is true iff [t] belongs to the Client Error class of
      status codes. */

  let is_client_error: t => bool;

  /** [is_server_error t] is true iff [t] belongs to the Server Error class of
      status codes. */

  let is_server_error: t => bool;

  /** [is_error t] is true iff [t] belongs to the Client Error or Server Error
      class of status codes. */

  let is_error: t => bool;

  let to_string: t => string;
  let of_string: string => t;

  [@ocaml.toplevel_printer]
  let pp_hum: (Format.formatter, t) => unit;
};

/** Header Fields

    Each header field consists of a case-insensitive {b field name} and a {b
    field value}. The order in which header fields {i with differing field
    names} are received is not significant. However, it is good practice to
    send header fields that contain control data first so that implementations
    can decide when not to handle a message as early as possible.

    A sender MUST NOT generate multiple header fields with the same field name
    in a message unless either the entire field value for that header field is
    defined as a comma-separated list or the header field is a well-known
    exception, e.g., [Set-Cookie].

    A recipient MAY combine multiple header fields with the same field name
    into one "field-name: field-value" pair, without changing the semantics of
    the message, by appending each subsequent field value to the combined field
    value in order, separated by a comma. {i The order in which header fields
    with the same field name are received is therefore significant to the
    interpretation of the combined field value}; a proxy MUST NOT change the
    order of these field values when forwarding a message.

    {i Note.} Unless otherwise specified, all operations preserve header field
    order and all reference to equality on names is assumed to be
    case-insensitive.

    See {{:https://tools.ietf.org/html/rfc7230#section-3.2} RFC7230§3.2} for
    more details. */

module Headers: {
  type t;

  /** The type of a case-insensitive header name. */

  type name = string;

  /** The type of a header value. */

  type value = string;

  /** {3 Constructor} */;

  /** [empty] is the empty collection of header fields. */

  let empty: t;

  /** [of_list assoc] is a collection of header fields defined by the
      association list [assoc]. [of_list] assumes the order of header fields in
      [assoc] is the intended transmission order. The following equations
      should hold:

        {ul
        {- [to_list (of_list lst) = lst] }
        {- [get (of_list [("k", "v1"); ("k", "v2")]) "k" = Some "v2"]. }} */

  let of_list: list((name, value)) => t;

  /** [of_list assoc] is a collection of header fields defined by the
      association list [assoc]. [of_list] assumes the order of header fields in
      [assoc] is the {i reverse} of the intended trasmission order. The
      following equations should hold:

        {ul
        {- [to_list (of_rev_list lst) = List.rev lst] }
        {- [get (of_rev_list [("k", "v1"); ("k", "v2")]) "k" = Some "v1"]. }} */

  let of_rev_list: list((name, value)) => t;

  /** [to_list t] is the association list of header fields contained in [t] in
      transmission order. */

  let to_list: t => list((name, value));

  /** [to_rev_list t] is the association list of header fields contained in [t]
      in {i reverse} transmission order. */

  let to_rev_list: t => list((name, value));

  /** [add t name value] is a collection of header fields that is the same as
      [t] except with [(name, value)] added at the end of the trasmission order.
      The following equations should hold:

        {ul
        {- [get (add t name value) name = Some value] }} */

  let add: (t, name, value) => t;

  /** [add_unless_exists t name value] is a collection of header fields that is
      the same as [t] if [t] already inclues [name], and otherwise is
      equivalent to [add t name value]. */

  let add_unless_exists: (t, name, value) => t;

  /** [add_list t assoc] is a collection of header fields that is the same as
      [t] except with all the header fields in [assoc] added to the end of the
      transmission order, in reverse order. */

  let add_list: (t, list((name, value))) => t;

  /** [add_multi t assoc] is the same as

      {[
        add_list t (List.concat_map assoc ~f:(fun (name, values) ->
          List.map values ~f:(fun value -> (name, value))))
      ]}

      but is implemented more efficiently. For example,

      {[
        add_multi t ["name1", ["x", "y"]; "name2", ["p", "q"]]
          = add_list ["name1", "x"; "name1", "y"; "name2", "p"; "name2", "q"]
      ]} */

  let add_multi: (t, list((name, list(value)))) => t;

  /** [remove t name] is a collection of header fields that contains all the
      header fields of [t] except those that have a header-field name that are
      equal to [name]. If [t] contains multiple header fields whose name is
      [name], they will all be removed. */

  let remove: (t, name) => t;

  /** [replace t name value] is a collection of header fields that is the same
      as [t] except with all header fields with a name equal to [name] removed
      and replaced with a single header field whose name is [name] and whose
      value is [value]. This new header field will appear in the transmission
      order where the first occurrence of a header field with a name matching
      [name] was found.

      If no header field with a name equal to [name] is present in [t], then
      the result is simply [t], unchanged. */

  let replace: (t, name, value) => t;

  /** {3 Destructors} */;

  /** [mem t name] is true iff [t] includes a header field with a name that is
      equal to [name]. */

  let mem: (t, name) => bool;

  /** [get t name] returns the last header from [t] with name [name], or [None]
      if no such header is present. */

  let get: (t, name) => option(value);

  /** [get t name] returns the last header from [t] with name [name], or raises
      if no such header is present. */

  let get_exn: (t, name) => value;

  /** [get_multi t name] is the list of header values in [t] whose names are
      equal to [name]. The returned list is in transmission order. */

  let get_multi: (t, name) => list(value);

  /** {3 Iteration} */;

  let iter: (~f: (name, value) => unit, t) => unit;
  let fold: (~f: (name, value, 'a) => 'a, ~init: 'a, t) => 'a;

  /** {3 Utilities} */;

  let to_string: t => string;

  [@ocaml.toplevel_printer]
  let pp_hum: (Format.formatter, t) => unit;
};

/** {2 Message Body} */;

module Body: {
  type t('rw);

  /** [schedule_read t ~on_eof ~on_read] will setup [on_read] and [on_eof] as
      callbacks for when bytes are available in [t] for the application to
      consume, or when the input channel has been closed and no further bytes
      will be received by the application.

      Once either of these callbacks have been called, they become inactive.
      The application is responsible for scheduling subsequent reads, either
      within the [on_read] callback or by some other mechanism. */

  let schedule_read:
    (
      t([ | `read]),
      ~on_eof: unit => unit,
      ~on_read: (Bigstringaf.t, ~off: int, ~len: int) => unit
    ) =>
    unit;

  /** [write_char w char] copies [char] into an internal buffer. If possible,
      this write will be combined with previous and/or subsequent writes before
      transmission. */

  let write_char: (t([ | `write]), char) => unit;

  /** [write_string w ?off ?len str] copies [str] into an internal buffer. If
      possible, this write will be combined with previous and/or subsequent
      writes before transmission. */

  let write_string:
    (t([ | `write]), ~off: int=?, ~len: int=?, string) => unit;

  /** [write_bigstring w ?off ?len bs] copies [bs] into an internal buffer. If
      possible, this write will be combined with previous and/or subsequent
      writes before transmission. */

  let write_bigstring:
    (t([ | `write]), ~off: int=?, ~len: int=?, Bigstringaf.t) => unit;

  /** [schedule_bigstring w ?off ?len bs] schedules [bs] to be transmitted at
      the next opportunity without performing a copy. [bs] should not be
      modified until a subsequent call to {!flush} has successfully
      completed. */

  let schedule_bigstring:
    (t([ | `write]), ~off: int=?, ~len: int=?, Bigstringaf.t) => unit;

  /** [flush t f] makes all bytes in [t] available for writing to the awaiting
      output channel. Once those bytes have reached that output channel, [f]
      will be called.

      The type of the output channel is runtime-dependent, as are guarantees about
      whether those packets have been queued for delivery or have actually been
      received by the intended recipient. */

  let flush: (t([ | `write]), unit => unit) => unit;

  /** [close_reader t] closes [t], indicating that any subsequent input
      received should be discarded. */

  let close_reader: t([ | `read]) => unit;

  /** [close_writer t] closes [t], causing subsequent write calls to raise. If
      [t] is writable, this will cause any pending output to become available
      to the output channel. */

  let close_writer: t([ | `write]) => unit;

  /** [is_closed t] is [true] if {!close} has been called on [t] and [false]
      otherwise. A closed [t] may still have pending output. */

  let is_closed: t(_) => bool;
};

/** {2 Message Types} */;

/** Request

    A client-initiated HTTP message. */

module Request: {
  type t = {
    meth: Method.t,
    target: string,
    version: Version.t,
    headers: Headers.t,
  };

  let create:
    (
      ~version: /** default is HTTP 1.1 */ Version.t=?,
      ~headers: /** default is {!Headers.empty} */ Headers.t=?,
      Method.t,
      string
    ) =>
    t;

  /** [body_length t] is the length of the message body accompanying [t]. It is
      an error to generate a request with a close-delimited message body.

      See {{:https://tools.ietf.org/html/rfc7230#section-3.3.3} RFC7230§3.3.3}
      for more details. */

  let body_length:
    t => [ | `Fixed(Int64.t) | `Chunked | `Error([ | `Bad_request])];

  /** [persistent_connection ?proxy t] indicates whether the connection for [t]
      can be reused for multiple requests and responses. If the calling code
      is acting as a proxy, it should pass [~proxy:true].

      See {{:https://tools.ietf.org/html/rfc7230#section-6.3} RFC7230§6.3 for
      more details. */

  let persistent_connection: (~proxy: bool=?, t) => bool;

  [@ocaml.toplevel_printer]
  let pp_hum: (Format.formatter, t) => unit;
};

/** Response

    A server-generated message to a {Request}. */

module Response: {
  type t = {
    version: Version.t,
    status: Status.t,
    reason: string,
    headers: Headers.t,
  };

  /** [create ?reason ?version ?headers status] creates an HTTP response with
      the given parameters. For typical use cases, it's sufficient to provide
      values for [headers] and [status]. */

  let create:
    (
      ~reason: /** default is determined by {!Status.default_reason_phrase} */ string
                 =?,
      ~version: /** default is HTTP 1.1 */ Version.t=?,
      ~headers: /** default is {!Headers.empty} */ Headers.t=?,
      Status.t
    ) =>
    t;

  /** [body_length ?proxy ~request_method t] is the length of the message body
      accompanying [t] assuming it is a response to a request whose method was
      [request_method]. If the calling code is acting as a proxy, it should
      pass [~proxy:true]. This optional parameter only affects error reporting.

      See {{:https://tools.ietf.org/html/rfc7230#section-3.3.3} RFC7230§3.3.3}
      for more details. */

  let body_length:
    (~proxy: bool=?, ~request_method: Method.standard, t) =>
    [
      | `Fixed(Int64.t)
      | `Chunked
      | `Close_delimited
      | `Error([ | `Bad_gateway | `Internal_server_error])
    ];

  /** [persistent_connection ?proxy t] indicates whether the connection for [t]
      can be reused for multiple requests and responses. If the calling code
      is acting as a proxy, it should pass [~proxy:true].

      See {{:https://tools.ietf.org/html/rfc7230#section-6.3} RFC7230§6.3 for
      more details. */

  let persistent_connection: (~proxy: bool=?, t) => bool;

  [@ocaml.toplevel_printer]
  let pp_hum: (Format.formatter, t) => unit;
};

/** IOVec */

module IOVec: {
  type t('a) =
    Faraday.iovec('a) = {
      buffer: 'a,
      off: int,
      len: int,
    };

  let length: t(_) => int;
  let lengthv: list(t(_)) => int;

  let shift: (t('a), int) => t('a);
  let shiftv: (list(t('a)), int) => list(t('a));

  [@ocaml.toplevel_printer]
  let pp_hum: (Format.formatter, t(_)) => unit;
};

/** {2 Request Descriptor} */

module Reqd: {
  type t('handle, 'io);

  let request: t(_) => Request.t;
  let request_body: t(_) => Body.t([ | `read]);

  let response: t(_) => option(Response.t);
  let response_exn: t(_) => Response.t;

  /** Responding

      The following functions will initiate a response for the corresponding
      request in [t]. Depending on the state of the current connection, and the
      header values of the response, this may cause the connection to close or
      to persist for reuse by the client.

      See {{:https://tools.ietf.org/html/rfc7230#section-6.3} RFC7230§6.3} for
      more details. */;

  let respond_with_string: (t(_), Response.t, string) => unit;
  let respond_with_bigstring: (t(_), Response.t, Bigstringaf.t) => unit;
  let respond_with_streaming:
    (~flush_headers_immediately: bool=?, t(_), Response.t) =>
    Body.t([ | `write]);
  let respond_with_upgrade: (t('fd, 'io), Headers.t, 'fd => 'io) => unit;

  /** {3 Exception Handling} */;

  let report_exn: (t(_), exn) => unit;
  let try_with: (t(_), unit => unit) => result(unit, exn);
};

/** {2 Buffer Size Configuration} */

module Config: {
  type t = {
    /** Default is [4096] */
    read_buffer_size: int,
    /** Default is [4096] */
    request_body_buffer_size: int,
    /** Default is [1024] */
    response_buffer_size: int,
    /** Default is [4096] */
    response_body_buffer_size: int,
  };

  /** [default] is a configuration record with all parameters set to their
      default values. */

  let default: t;
};

/** {2 Server Connection} */;

module Server_connection: {
  type t('fd, 'io);

  type error = [
    | `Bad_request
    | `Bad_gateway
    | `Internal_server_error
    | `Exn(exn)
  ];

  type request_handler('fd, 'io) = Reqd.t('fd, 'io) => unit;

  type error_handler =
    (~request: Request.t=?, error, Headers.t => Body.t([ | `write])) => unit;

  /** [create ?config ?error_handler ~request_handler] creates a connection
      handler that will service individual requests with [request_handler]. */

  let create:
    (
      ~config: Config.t=?,
      ~error_handler: error_handler=?,
      request_handler('fd, 'io)
    ) =>
    t('fd, 'io);

  /** [next_read_operation t] returns a value describing the next operation
      that the caller should conduct on behalf of the connection. */

  let next_read_operation: t(_) => [ | `Read | `Yield | `Close | `Upgrade];

  /** [read t bigstring ~off ~len] reads bytes of input from the provided range
      of [bigstring] and returns the number of bytes consumed by the
      connection.  {!read} should be called after {!next_read_operation}
      returns a [`Read] value and additional input is available for the
      connection to consume. */

  let read: (t(_), Bigstringaf.t, ~off: int, ~len: int) => int;

  /** [read_eof t bigstring ~off ~len] reads bytes of input from the provided
      range of [bigstring] and returns the number of bytes consumed by the
      connection.  {!read_eof} should be called after {!next_read_operation}
      returns a [`Read] and an EOF has been received from the communication
      channel. The connection will attempt to consume any buffered input and
      then shutdown the HTTP parser for the connection. */

  let read_eof: (t(_), Bigstringaf.t, ~off: int, ~len: int) => int;

  /** [yield_reader t continue] registers with the connection to call
      [continue] when reading should resume. {!yield_reader} should be called
      after {next_read_operation} returns a [`Yield] value. */

  let yield_reader: (t(_), unit => unit) => unit;

  /** [next_write_operation t] returns a value describing the next operation
      that the caller should conduct on behalf of the connection.
      In the case of [`Upgrade], it is the responsibility of the caller to
      guarantee that the upgrade callback is called only once; the function
      will keep returning [`Upgrade] if called again. */

  let next_write_operation:
    t('fd, 'io) =>
    [
      | `Write(list(IOVec.t(Bigstringaf.t)))
      | `Upgrade(list(IOVec.t(Bigstringaf.t)), 'fd => 'io)
      | `Yield
      | `Close(int)
    ];

  /** [report_write_result t result] reports the result of the latest write
      attempt to the connection. {report_write_result} should be called after a
      call to {next_write_operation} that returns a [`Write buffer] value.

        {ul
        {- [`Ok n] indicates that the caller successfully wrote [n] bytes of
        output from the buffer that the caller was provided by
        {next_write_operation}. }
        {- [`Closed] indicates that the output destination will no longer
        accept bytes from the write processor. }} */

  let report_write_result: (t(_), [ | `Ok(int) | `Closed]) => unit;

  /** [yield_writer t continue] registers with the connection to call
      [continue] when writing should resume. {!yield_writer} should be called
      after {next_write_operation} returns a [`Yield] value. */

  let yield_writer: (t(_), unit => unit) => unit;

  /** [report_exn t exn] reports that an error [exn] has been caught and
      that it has been attributed to [t]. Calling this function will switch [t]
      into an error state. Depending on the state [t] is transitioning from, it
      may call its error handler before terminating the connection. */

  let report_exn: (t(_), exn) => unit;

  /** [is_closed t] is [true] if both the read and write processors have been
      shutdown. When this is the case {!next_read_operation} will return
      [`Close _] and {!next_write_operation} will return [`Write _] until all
      buffered output has been flushed. */

  let is_closed: t(_) => bool;

  /** [error_code t] returns the [error_code] that caused the connection to
      close, if one exists. */

  let error_code: t(_) => option(error);

  /**/
  let shutdown: t(_) => unit;
  /**/
};

/** {2 Client Connection} */;

module Client_connection: {
  type t;

  type error = [
    | `Malformed_response(string)
    | `Invalid_response_body_length(Response.t)
    | `Exn(exn)
  ];

  type response_handler = (Response.t, Body.t([ | `read])) => unit;

  type error_handler = error => unit;

  let create: (~config: Config.t=?, unit) => t;

  let request:
    (
      t,
      Request.t,
      ~error_handler: error_handler,
      ~response_handler: response_handler
    ) =>
    Body.t([ | `write]);

  /** [next_read_operation t] returns a value describing the next operation
      that the caller should conduct on behalf of the connection. */

  let next_read_operation: t => [ | `Read | `Yield | `Close];

  /** [read t bigstring ~off ~len] reads bytes of input from the provided range
      of [bigstring] and returns the number of bytes consumed by the
      connection.  {!read} should be called after {!next_read_operation}
      returns a [`Read] value and additional input is available for the
      connection to consume. */

  let read: (t, Bigstringaf.t, ~off: int, ~len: int) => int;

  /** [read_eof t bigstring ~off ~len] reads bytes of input from the provided
      range of [bigstring] and returns the number of bytes consumed by the
      connection.  {!read_eof} should be called after {!next_read_operation}
      returns a [`Read] and an EOF has been received from the communication
      channel. The connection will attempt to consume any buffered input and
      then shutdown the HTTP parser for the connection. */

  let read_eof: (t, Bigstringaf.t, ~off: int, ~len: int) => int;

  /** [next_write_operation t] returns a value describing the next operation
      that the caller should conduct on behalf of the connection. */

  let next_write_operation:
    t => [ | `Write(list(IOVec.t(Bigstringaf.t))) | `Yield | `Close(int)];

  /** [report_write_result t result] reports the result of the latest write
      attempt to the connection. {report_write_result} should be called after a
      call to {next_write_operation} that returns a [`Write buffer] value.

        {ul
        {- [`Ok n] indicates that the caller successfully wrote [n] bytes of
        output from the buffer that the caller was provided by
        {next_write_operation}. }
        {- [`Closed] indicates that the output destination will no longer
        accept bytes from the write processor. }} */

  let report_write_result: (t, [ | `Ok(int) | `Closed]) => unit;

  let yield_reader: (t, unit => unit) => unit;

  /** [yield_writer t continue] registers with the connection to call
      [continue] when writing should resume. {!yield_writer} should be called
      after {next_write_operation} returns a [`Yield] value. */

  let yield_writer: (t, unit => unit) => unit;

  /** [report_exn t exn] reports that an error [exn] has been caught and
      that it has been attributed to [t]. Calling this function will switch [t]
      into an error state. Depending on the state [t] is transitioning from, it
      may call its error handler before terminating the connection. */

  let report_exn: (t, exn) => unit;

  /** [is_closed t] is [true] if both the read and write processors have been
      shutdown. When this is the case {!next_read_operation} will return
      [`Close _] and {!next_write_operation} will return [`Write _] until all
      buffered output has been flushed, at which point it will also return
      `Close. */

  let is_closed: t => bool;

  /** [shutdown connection] closes the underlying input and output channels of
      the connection, rendering it unusable for any further communication. */

  let shutdown: t => unit;
};


module Zillaml_private: {
  module Serialize: {
    let write_request: (Faraday.t, Request.t) => unit;
    let write_response: (Faraday.t, Response.t) => unit;
  };
};
