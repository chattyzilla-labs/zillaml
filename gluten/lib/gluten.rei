/*----------------------------------------------------------------------------
 *  Copyright (c) 2019-2020 António Nuno Monteiro
 *  Copyright (c) 2019-2020 Dakota Murphy
 * 
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its
 *  contributors may be used to endorse or promote products derived from this
 *  software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*/

module type RUNTIME = {
  type t;

  let next_read_operation: t => [ | `Read | `Yield | `Close];

  let read: (t, Bigstringaf.t, ~off: int, ~len: int) => int;

  let read_eof: (t, Bigstringaf.t, ~off: int, ~len: int) => int;

  let yield_reader: (t, unit => unit) => unit;

  let next_write_operation:
    t =>
    [ | `Write(list(Faraday.iovec(Bigstringaf.t))) | `Yield | `Close(int)];

  let report_write_result: (t, [ | `Ok(int) | `Closed]) => unit;

  let yield_writer: (t, unit => unit) => unit;

  let report_exn: (t, exn) => unit;

  let is_closed: t => bool;

  let shutdown: t => unit;
};

type runtime('t) = (module RUNTIME with type t = 't);

type impl;

let make: (runtime('t), 't) => impl;

module Reqd: {
  type t('reqd) =
    pri {
      reqd: 'reqd,
      upgrade: impl => unit,
    };
};

module Server: {
  include RUNTIME;

  let create: (~protocol: runtime('t), 't) => t;

  let upgrade_protocol: (t, impl) => unit;

  type request_handler('reqd) = Reqd.t('reqd) => unit;

  let create_upgradable:
    (
      ~protocol: runtime('t),
      ~create: ('reqd => unit) => 't,
      request_handler('reqd)
    ) =>
    t;
};

module Client: {
  include RUNTIME;

  let create: (~protocol: runtime('t), 't) => t;

  let upgrade_protocol: (t, impl) => unit;
};

/* Export upgradable Reqd for convenience */
type reqd('reqd) =
  Reqd.t('reqd) =
    pri {
      reqd: 'reqd,
      upgrade: impl => unit,
    };
