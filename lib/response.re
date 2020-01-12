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

type t = {
  version: Version.t,
  status: Status.t,
  reason: string,
  headers: Headers.t,
};

let create =
    (~reason=?, ~version=Version.v1_1, ~headers=Headers.empty, status) => {
  let reason =
    switch (reason) {
    | Some(reason) => reason
    | None =>
      switch (status) {
      | #Status.standard as status => Status.default_reason_phrase(status)
      | `Code(_) => "Non-standard status code"
      }
    };

  {version, status, reason, headers};
};

let persistent_connection = (~proxy=?, {version, headers, _}) =>
  Message.persistent_connection(~proxy?, version, headers);

let proxy_error = `Error(`Bad_gateway);
let server_error = `Error(`Internal_server_error);
let body_length = (~proxy=false, ~request_method, {status, headers, _}) =>
  switch (status, request_method) {
  | (`No_content | `Not_modified, _) => `Fixed(0L)
  | (s, _) when Status.is_informational(s) => `Fixed(0L)
  | (s, `CONNECT) when Status.is_successful(s) => `Close_delimited
  | (_, _) =>
    switch (Headers.get_multi(headers, "transfer-encoding")) {
    | ["chunked", ..._] => `Chunked
    | [_, ...es] when List.mem("chunked", es) => `Close_delimited
    | []
    | _ =>
      switch (Message.unique_content_length_values(headers)) {
      | [] => `Close_delimited
      | [len] =>
        let len = Message.content_length_of_string(len);
        if (len >= 0L) {
          `Fixed(len);
        } else if (proxy) {
          proxy_error;
        } else {
          server_error;
        };
      | _ => if (proxy) {proxy_error} else {server_error}
      }
    }
  };

let pp_hum = (fmt, {version, status, reason, headers}) =>
  Format.fprintf(
    fmt,
    "((version \"%a\") (status %a) (reason %S) (headers %a))",
    Version.pp_hum,
    version,
    Status.pp_hum,
    status,
    reason,
    Headers.pp_hum,
    headers,
  );
