open Core;
open Async;

let sha1 = s =>
  s
  |> Digestif.SHA1.digest_string
  |> Digestif.SHA1.to_raw_string
  |> Base64.encode_exn(~pad=true);

/** XXX(seliopou): Replace Angstrom.Buffered with a module like this, while
    also supporting growing the buffer. Clients can use this to buffer and the
    use the unbuffered interface for actually running the parser. */
module Buffer = {
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
    let n =
      f(
        t.buffer,
        ~off=t.off + t.len,
        ~len=Bigstring.length(t.buffer) - t.len,
      );
    t.len = t.len + n;
    n;
  };
};

let read = (fd, buffer) => {
  let badfd = fd => failwithf("read got back fd: %s", Fd.to_string(fd), ());
  let rec finish = (fd, buffer, result) =>
    Unix.Error.(
      switch (result) {
      | `Already_closed
      | `Ok(0) => return(`Eof)
      | `Ok(n) => return(`Ok(n))
      | `Error([@implicit_arity] Unix.Unix_error(EWOULDBLOCK | EAGAIN, _, _)) =>
        Fd.ready_to(fd, `Read)
        >>= (
          fun
          | `Bad_fd => badfd(fd)
          | `Closed => return(`Eof)
          | `Ready => go(fd, buffer)
        )
      | `Error([@implicit_arity] Unix.Unix_error(EBADF, _, _)) => badfd(fd)
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
          Buffer.put(buffer, ~f=(bigstring, ~off, ~len) =>
            Unix.Syscall_result.Int.ok_or_unix_error_exn(
              ~syscall_name="read",
              Bigstring.read_assume_fd_is_nonblocking(
                file_descr,
                bigstring,
                ~pos=off,
                ~len,
              ),
            )
          )
        ),
      );
    } else {
      Fd.syscall_in_thread(fd, ~name="read", file_descr =>
        Buffer.put(buffer, ~f=(bigstring, ~off, ~len) =>
          Bigstring.read(file_descr, bigstring, ~pos=off, ~len)
        )
      )
      >>= (result => finish(fd, buffer, result));
    };

  go(fd, buffer);
};

module Server = {
  module Server_connection = Websocketzilla.Server_connection;

  let start_read_write_loops = (~socket, connection) => {
    let fd = Socket.fd(socket);
    let read_complete = Ivar.create();
    let buffer = Buffer.create(0x1000);
    let rec reader_thread = () =>
      switch (Server_connection.next_read_operation(connection)) {
      | `Read =>
        read(fd, buffer)
        >>> (
          fun
          | `Eof => {
              Buffer.get(buffer, ~f=(bigstring, ~off, ~len) =>
                Server_connection.read_eof(connection, bigstring, ~off, ~len)
              )
              |> ignore;
              reader_thread();
            }
          | `Ok(_) => {
              Buffer.get(buffer, ~f=(bigstring, ~off, ~len) =>
                Server_connection.read(connection, bigstring, ~off, ~len)
              )
              |> ignore;
              reader_thread();
            }
        )

      | `Yield
      | `Upgrade => Server_connection.yield_reader(connection, reader_thread)

      | `Close =>
        Ivar.fill(read_complete, ());
        if (!Fd.is_closed(fd)) {
          Socket.shutdown(socket, `Receive);
        };
      };

    let writev = Faraday_async.writev_of_fd(fd);
    let write_complete = Ivar.create();
    let rec writer_thread = () =>
      switch (Server_connection.next_write_operation(connection)) {
      | `Write(iovecs) =>
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
            upgrade_handler(socket) >>> writer_thread;
          }
        )

      | `Yield => Server_connection.yield_writer(connection, writer_thread)

      | `Close(_) =>
        Ivar.fill(write_complete, ());
        if (!Fd.is_closed(fd)) {
          Socket.shutdown(socket, `Send);
        };
      };

    let conn_monitor = Monitor.create();
    Scheduler.within(~monitor=conn_monitor, reader_thread);
    Scheduler.within(~monitor=conn_monitor, writer_thread);
    Monitor.detach_and_iter_errors(
      conn_monitor,
      ~f=exn => {
        Server_connection.close(connection);
        Log.Global.error("%s\n%!", Exn.to_string(exn));
        if (!Fd.is_closed(fd)) {
          don't_wait_for(Fd.close(fd));
        };
      },
    );
    /* The Tcp module will close the file descriptor once this becomes determined. */
    Deferred.all_unit([
      Ivar.read(read_complete),
      Ivar.read(write_complete),
    ]);
  };

  let create_connection_handler =
      (
        ~config as _: option(Zillaml.Config.t)=?,
        ~websocket_handler,
        ~error_handler as _,
        client_addr: [< Socket.Address.t],
        socket,
      ) => {
    let websocket_handler = websocket_handler(client_addr);
    let conn =
      Server_connection.create(
        ~future=Deferred.unit,
        ~sha1,
        websocket_handler,
      );
    start_read_write_loops(~socket, conn);
  };

  let create_upgraded_connection_handler =
      (
        ~config as _=?,
        ~websocket_handler,
        ~error_handler,
        client_addr,
        socket,
      ) => {
    let websocket_handler = websocket_handler(client_addr);
    let connection =
      Server_connection.create_upgraded(~error_handler, ~websocket_handler);

    start_read_write_loops(~socket, connection);
  };

  let respond_with_upgrade = (~headers=?, reqd, upgrade_handler) =>
    Deferred.return(
      Server_connection.respond_with_upgrade(
        ~headers?,
        ~sha1,
        reqd,
        upgrade_handler,
      ),
    );
};

module Client = {
  module Client_connection = Websocketzilla.Client_connection;

  let start_read_write_loops = (socket, conn) => {
    let fd = Socket.fd(socket);
    let writev = Faraday_async.writev_of_fd(fd);
    let read_complete = Ivar.create();
    let buffer = Buffer.create(0x1000);
    let rec reader_thread = () =>
      switch (Client_connection.next_read_operation(conn)) {
      | `Read =>
        read(fd, buffer)
        >>> (
          fun
          | `Eof => {
              Buffer.get(buffer, ~f=(bigstring, ~off, ~len) =>
                Client_connection.read_eof(conn, bigstring, ~off, ~len)
              )
              |> ignore;
              reader_thread();
            }
          | `Ok(_) => {
              Buffer.get(buffer, ~f=(bigstring, ~off, ~len) =>
                Client_connection.read(conn, bigstring, ~off, ~len)
              )
              |> ignore;
              reader_thread();
            }
        )
      | `Yield => Client_connection.yield_reader(conn, reader_thread)
      | `Close =>
        Ivar.fill(read_complete, ());
        if (!Fd.is_closed(fd)) {
          Socket.shutdown(socket, `Receive);
        };
      };

    let write_complete = Ivar.create();
    let rec writer_thread = () =>
      switch (Client_connection.next_write_operation(conn)) {
      | `Write(iovecs) =>
        writev(iovecs)
        >>> (
          result => {
            Client_connection.report_write_result(conn, result);
            writer_thread();
          }
        )
      | `Yield => Client_connection.yield_writer(conn, writer_thread)
      | `Close(_) => Ivar.fill(write_complete, ())
      };

    let conn_monitor = Monitor.create();
    Scheduler.within(~monitor=conn_monitor, reader_thread);
    Scheduler.within(~monitor=conn_monitor, writer_thread);
    Monitor.detach_and_iter_errors(
      conn_monitor,
      ~f=exn => {
        Client_connection.close(conn);
        Log.Global.error("%s", Exn.to_string(exn));
        if (!Fd.is_closed(fd)) {
          don't_wait_for(Fd.close(fd));
        };
      },
    );
    Deferred.all_unit([Ivar.read(read_complete), Ivar.read(write_complete)])
    >>| (
      () =>
        if (!Fd.is_closed(fd)) {
          don't_wait_for(Fd.close(fd));
        }
    );
  };

  let connect =
      (
        ~nonce,
        ~host,
        ~port,
        ~resource,
        ~error_handler,
        ~websocket_handler,
        socket,
      ) => {
    let headers =
      Zillaml.Headers.of_list([
        ("host", String.concat(~sep=":", [host, string_of_int(port)])),
      ]);

    let connection =
      Client_connection.connect(
        ~nonce,
        ~headers,
        ~sha1,
        ~error_handler,
        ~websocket_handler,
        resource,
      );

    start_read_write_loops(socket, connection);
  };

  let create = (~websocket_handler, socket) => {
    let connection = Client_connection.create(~websocket_handler);
    start_read_write_loops(socket, connection);
  };
};
