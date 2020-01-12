/*----------------------------------------------------------------------------
 *  Copyright (c) 2019 António Nuno Monteiro
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

open Async;
open Zillaml;

module type IO = {
  type socket;
  type addr;

  /** The region [[off, off + len)] is where read bytes can be written to */

  let read:
    (socket, Bigstringaf.t, ~off: int, ~len: int) =>
    Deferred.t([ | `Eof | `Ok(int)]);

  let writev:
    (socket, list(Faraday.iovec(Faraday.bigstring))) =>
    Deferred.t([ | `Closed | `Ok(int)]);

  let shutdown_send: socket => unit;

  let shutdown_receive: socket => unit;

  let close: socket => Deferred.t(unit);

  let state: socket => [ | `Open | `Error | `Closed];
};

module type Server = {
  type socket;

  type addr;

  let create_connection_handler:
    (
      ~config: Config.t=?,
      ~request_handler: addr =>
                        Server_connection.request_handler(
                          socket,
                          Deferred.t(unit),
                        ),
      ~error_handler: addr => Server_connection.error_handler,
      addr,
      socket
    ) =>
    Deferred.t(unit);
};

module type Client = {
  type t;

  type socket;

  let create_connection: (~config: Config.t=?, socket) => Deferred.t(t);

  let request:
    (
      t,
      Request.t,
      ~error_handler: Client_connection.error_handler,
      ~response_handler: Client_connection.response_handler
    ) =>
    Body.t([ | `write]);

  let shutdown: t => unit;

  let is_closed: t => bool;
};