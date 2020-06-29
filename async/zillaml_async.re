/*----------------------------------------------------------------------------
    Copyright (c) 2018 Inhabited Type LLC.
    Copyright (c) 2019 António Nuno Monteiro
    Copyright (c) 2020 Dakota Ryan Murphy

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

open Core;
open Async;

/** XXX(seliopou): Replace Angstrom.Buffered with a module like this, while
    also supporting growing the buffer. Clients can use this to buffer and the
    use the unbuffered interface for actually running the parser. */
module Buffer: {
  type t;

  let create: int => t;

  let get: (t, ~f: (Bigstring.t, ~off: int, ~len: int) => int) => int;
  let put:
    (
      t,
      ~f: (Bigstring.t, ~off: int, ~len: int) =>
          Deferred.t([ | `Eof | `Ok(int)])
    ) =>
    Deferred.t([ | `Eof | `Ok(int)]);
} = {
  type t = {
    buffer: Bigstring.t,
    mutable off: int,
    mutable len: int,
  };

  let create = size => {
    let buffer = Bigstring.create(size);
    {buffer, off: 0, len: 0};
  };

  let compress = t =>
    if (t.len == 0) {
      t.off = 0;
      t.len = 0;
    } else if (t.off > 0) {
      Bigstring.blit(
        ~src=t.buffer,
        ~src_pos=t.off,
        ~dst=t.buffer,
        ~dst_pos=0,
        ~len=t.len,
      );
      t.off = 0;
    };

  let get = (t, ~f) => {
    let n = f(t.buffer, ~off=t.off, ~len=t.len);
    t.off = t.off + n;
    t.len = t.len - n;
    if (t.len == 0) {
      t.off = 0;
    };
    n;
  };

  let put = (t, ~f) => {
    compress(t);
    f(t.buffer, ~off=t.off + t.len, ~len=Bigstring.length(t.buffer) - t.len)
    >>| (
      fun
      | `Eof => `Eof
      | `Ok(n) as ret => {
          t.len = t.len + n;
          ret;
        }
    );
  };
};

include Zillaml_async_intf;

open Zillaml;

module Make_server = (Io: IO) => {
  type socket = Io.socket;

  let report_exn = (connection, socket, exn) =>
    /* This needs to handle two cases. The case where the socket is
     * still open and we can gracefully respond with an error, and the
     * case where the client has already left. The second case is more
     * common when communicating over HTTPS, given that the remote peer
     * can close the connection without requiring an acknowledgement:
     *
     * From RFC5246§7.2.1:
     *   Unless some other fatal alert has been transmitted, each party
     *   is required to send a close_notify alert before closing the
     *   write side of the connection.  The other party MUST respond
     *   with a close_notify alert of its own and close down the
     *   connection immediately, discarding any pending writes. It is
     *   not required for the initiator of the close to wait for the
     *   responding close_notify alert before closing the read side of
     *   the connection. */
    switch (Io.state(socket)) {
    | `Error
    | `Closed => Zillaml.Server_connection.shutdown(connection)
    | `Open => Zillaml.Server_connection.report_exn(connection, exn)
    };

  let create_connection_handler =
      (
        ~config=Config.default,
        ~request_handler,
        ~error_handler,
        client_addr,
        socket,
      ) => {
    let connection =
      Server_connection.create(
        ~config,
        ~error_handler=error_handler(client_addr),
        request_handler(client_addr),
      );

    let read_buffer = Buffer.create(config.read_buffer_size);
    let read_complete = Ivar.create();
    let rec reader_thread = () =>
      switch (Server_connection.next_read_operation(connection)) {
      | `Read =>
        /* Log.Global.printf "read(%d)%!" (Fd.to_int_exn fd); */
        Buffer.put(~f=Io.read(socket), read_buffer)
        >>> (
          fun
          | `Eof => {
              Buffer.get(read_buffer, ~f=(bigstring, ~off, ~len) =>
                Server_connection.read_eof(connection, bigstring, ~off, ~len)
              )
              |> ignore;
              reader_thread();
            }
          | `Ok(_) => {
              Buffer.get(read_buffer, ~f=(bigstring, ~off, ~len) =>
                Server_connection.read(connection, bigstring, ~off, ~len)
              )
              |> ignore;
              reader_thread();
            }
        )
      | `Yield =>
        /* Log.Global.printf "read_yield(%d)%!" (Fd.to_int_exn fd); */
        Server_connection.yield_reader(connection, reader_thread)
      | `Upgrade => Ivar.fill(read_complete, ())
      | `Close =>
        /* Log.Global.printf "read_close(%d)%!" (Fd.to_int_exn fd); */
        Ivar.fill(read_complete, ());
        Io.shutdown_receive(socket);
      };

    let writev = Io.writev(socket);
    let write_complete = Ivar.create();
    let rec writer_thread = () =>
      switch (Server_connection.next_write_operation(connection)) {
      | `Write(iovecs) =>
        /* Log.Global.printf "write(%d)%!" (Fd.to_int_exn fd); */
        writev(iovecs)
        >>> (
          result => {
            Server_connection.report_write_result(connection, result);
            writer_thread();
          }
        )
      | `Upgrade(iovecs, upgrade_handler) =>
        writev(iovecs)
        >>> (
          result => {
            Server_connection.report_write_result(connection, result);
            upgrade_handler(socket) >>> Ivar.fill(write_complete);
          }
        )
      | `Yield =>
        /* Log.Global.printf "write_yield(%d)%!" (Fd.to_int_exn fd); */
        Server_connection.yield_writer(connection, writer_thread)
      | `Close(_) =>
        /* Log.Global.printf "write_close(%d)%!" (Fd.to_int_exn fd); */
        Ivar.fill(write_complete, ());
        Io.shutdown_send(socket);
      };

    let conn_monitor = Monitor.create();
    Scheduler.within(~monitor=conn_monitor, reader_thread);
    Scheduler.within(~monitor=conn_monitor, writer_thread);
    Monitor.detach_and_iter_errors(conn_monitor, ~f=exn =>
      report_exn(connection, socket, exn)
    );
    /* The Tcp module will close the file descriptor once this becomes determined. */
    Deferred.all_unit([
      Ivar.read(read_complete),
      Ivar.read(write_complete),
    ]);
  };
};

module Unix_io:
  Zillaml_async_intf.IO with
    type socket = Socket.t([ | `Active], Socket.Address.Inet.t) and
    type addr = Socket.Address.Inet.t = {
  type socket = Socket.t([ | `Active], Socket.Address.Inet.t);
  type addr = Socket.Address.Inet.t;

  let read = (socket, bigstring, ~off, ~len) => {
    let fd = Socket.fd(socket);
    let badfd = fd =>
      failwithf("read got back fd: %s", Fd.to_string(fd), ());
    let rec finish = (fd, buffer, result) =>
      Unix.Error.(
        switch (result) {
        | `Already_closed
        | `Ok(0) => return(`Eof)
        | `Ok(n) => return(`Ok(n))
        | `Error(
            Unix.Unix_error(EWOULDBLOCK | EAGAIN, _, _),
          ) =>
          Fd.ready_to(fd, `Read)
          >>= (
            fun
            | `Bad_fd => badfd(fd)
            | `Closed => return(`Eof)
            | `Ready => go(fd, buffer)
          )
        | `Error(Unix.Unix_error(EBADF, _, _)) =>
          badfd(fd)
        | `Error(exn) =>
          Deferred.don't_wait_for(Fd.close(fd));
          raise(exn);
        }
      )
    and go = (fd, buffer) =>
      if (Fd.supports_nonblock(fd)) {
        finish(
          fd,
          buffer,
          Fd.syscall(fd, ~nonblocking=true, file_descr =>
            Unix.Syscall_result.Int.ok_or_unix_error_exn(
              ~syscall_name="read",
              Bigstring_unix.read_assume_fd_is_nonblocking(
                file_descr,
                bigstring,
                ~pos=off,
                ~len,
              ),
            )
          ),
        );
      } else {
        Fd.syscall_in_thread(fd, ~name="read", file_descr =>
          Bigstring_unix.read(file_descr, bigstring, ~pos=off, ~len)
        )
        >>= (result => finish(fd, buffer, result));
      };

    go(fd, bigstring);
  };

  let writev = socket => Faraday_async.writev_of_fd(Socket.fd(socket));

  let shutdown_send = socket => {
    let fd = Socket.fd(socket);
    if (!Fd.is_closed(fd)) {
      Socket.shutdown(socket, `Send);
    };
  };

  let shutdown_receive = socket => {
    let fd = Socket.fd(socket);
    if (!Fd.is_closed(fd)) {
      Socket.shutdown(socket, `Receive);
    };
  };

  let close = socket => {
    let fd = Socket.fd(socket);
    if (!Fd.is_closed(fd)) {
      Fd.close(fd);
    } else {
      Deferred.unit;
    };
  };

  let state = socket => {
    let fd = Socket.fd(socket);
    switch (Fd.with_file_descr(fd, _ => ())) {
    | `Ok () => `Open
    | `Already_closed => `Closed
    | `Error(_) => `Error
    };
  };
};

module Server = {
  include Make_server(Unix_io);

  module SSL = {
    include Make_server(Ssl_io.Io);

    let create_connection_handler_with_default =
        (~certfile, ~keyfile, ~config=?, ~request_handler, ~error_handler) => {
      let make_ssl_server = Ssl_io.make_server(~certfile, ~keyfile);
      (client_addr, socket) =>
        make_ssl_server(socket)
        >>= (
          ssl_server =>
            create_connection_handler(
              ~config?,
              ~request_handler,
              ~error_handler,
              client_addr,
              ssl_server,
            )
        );
    };
  };
};

module Make_client = (Io: IO) => {
  type socket = Io.socket;
  type t = Client_connection.t;

  let create_connection = (~config=Config.default, socket) => {
    let connection = Client_connection.create(~config, ());

    let read_complete = Ivar.create();
    let read_buffer = Buffer.create(config.Config.read_buffer_size);
    let rec reader_thread = () =>
      switch (Client_connection.next_read_operation(connection)) {
      | `Read =>
        /* Log.Global.printf "read(%d)%!" (Fd.to_int_exn fd); */
        Buffer.put(~f=Io.read(socket), read_buffer)
        >>> (
          fun
          | `Eof => {
              Buffer.get(read_buffer, ~f=(bigstring, ~off, ~len) =>
                Client_connection.read_eof(connection, bigstring, ~off, ~len)
              )
              |> ignore;
              reader_thread();
            }
          | `Ok(_) => {
              Buffer.get(read_buffer, ~f=(bigstring, ~off, ~len) =>
                Client_connection.read(connection, bigstring, ~off, ~len)
              )
              |> ignore;
              reader_thread();
            }
        )
      | `Yield => Client_connection.yield_writer(connection, reader_thread)
      | `Close =>
        /* Log.Global.printf "read_close(%d)%!" (Fd.to_int_exn fd); */
        Ivar.fill(read_complete, ());
        Io.shutdown_receive(socket);
      };

    let writev = Io.writev(socket);
    let write_complete = Ivar.create();
    let rec writer_thread = () =>
      switch (Client_connection.next_write_operation(connection)) {
      | `Write(iovecs) =>
        /* Log.Global.printf "write(%d)%!" (Fd.to_int_exn fd); */
        writev(iovecs)
        >>> (
          result => {
            Client_connection.report_write_result(connection, result);
            writer_thread();
          }
        )
      | `Yield =>
        /* Log.Global.printf "write_yield(%d)%!" (Fd.to_int_exn fd); */
        Client_connection.yield_writer(connection, writer_thread)
      | `Close(_) =>
        /* Log.Global.printf "write_close(%d)%!" (Fd.to_int_exn fd); */
        Ivar.fill(write_complete, ())
      };

    let conn_monitor = Monitor.create();
    Scheduler.within(~monitor=conn_monitor, reader_thread);
    Scheduler.within(~monitor=conn_monitor, writer_thread);
    Monitor.detach_and_iter_errors(conn_monitor, ~f=exn =>
      Client_connection.report_exn(connection, exn)
    );
    don't_wait_for(
      Deferred.all_unit([
        Ivar.read(read_complete),
        Ivar.read(write_complete),
      ])
      >>= (() => Io.close(socket)),
    );
    Deferred.return(connection);
  };

  let request = Client_connection.request;

  let shutdown = Client_connection.shutdown;

  let is_closed = Client_connection.is_closed;
};

module Client = {
  include Make_client(Unix_io);

  module SSL = {
    include Make_client(Ssl_io.Io);

    let create_connection_with_default = (~config=?, socket) =>
      Ssl_io.make_default_client(socket)
      >>= (ssl_client => create_connection(~config?, ssl_client));
  };
};
