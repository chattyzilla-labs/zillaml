open Core;
open Async;
open Zillaml_async;
open Router;

type error_handler =
  (
    Unix.sockaddr,
    ~request: Zillaml.Request.t=?,
    Zillaml.Server_connection.error,
    Zillaml.Headers.t => Zillaml.Body.t([ | `write])
  ) =>
  unit;

let sha1 = s =>
  s |> Digestif.SHA1.digest_string |> Digestif.SHA1.to_raw_string;

let read_body = reqd => {
  let req = reqd |> Zillaml.Reqd.request;
  switch (req.meth) {
  | `POST
  | `PUT =>
    let finished = Ivar.create();
    let request_body = Zillaml.Reqd.request_body(reqd);
    let body = ref("");
    let on_eof = () => Some(body^) |> Ivar.fill(finished);
    let rec on_read = (buffer, ~off as _, ~len as _) => {
      body := body^ ++ Bigstringaf.to_string(buffer);
      Zillaml.Body.schedule_read(request_body, ~on_eof, ~on_read);
    };
    Zillaml.Body.schedule_read(request_body, ~on_eof, ~on_read);
    Ivar.read(finished);

  | _ => Deferred.return(None)
  };
};

let create_respond = (reqd, ~status, ~headers, content) => {
  let headers =
    (
      switch (headers) {
      | None => []
      | Some(hs) => hs
      }
    )
    @ [("Content-Length", content |> String.length |> string_of_int)]
    |> Zillaml.Headers.of_list;
  let res = Zillaml.Response.create(status, ~headers);
  Zillaml.Reqd.respond_with_string(reqd, res, content);
};

let start =
    (
      ~port,
      ~on_start,
      ~request_handler,
      ~max_accepts_per_batch,
      ~error_handler,
    ) => {
  let where_to_listen = Tcp.Where_to_listen.of_port(port);
  Tcp.(
    Server.create_sock(
      ~on_handler_error=`Raise,
      ~backlog=10000,
      ~max_connections=10000,
      ~max_accepts_per_batch,
      where_to_listen,
    )
  )(
    Server.create_connection_handler(~request_handler, ~error_handler),
  )
  >>= (
    server => {
      on_start(server, port);
      Deferred.never();
    }
  );
};

let create_server =
    (
      ~port,
      ~on_start,
      ~max_accepts_per_batch,
      ~router,
      ~error_handler,
      ~reqlogger,
    ) => {
  let request_handler = (_conn, reqd: Gluten.Reqd.t(Zillaml.Reqd.t)) => {
    let {Gluten.Reqd.reqd, _} = reqd;
    let req = Zillaml.Reqd.request(reqd);
    let res = create_respond(reqd);
    switch (reqlogger) {
    | Some(fn) => fn(req)
    | None => ()
    };
    let run_router = body => {
      switch%bind (router(req, body)) {
      | Some({status, headers, body}) =>
        Deferred.return(res(~status, ~headers, body))
      | None =>
        let body =
          switch (body) {
          | Some(str) => str
          | None => ""
          };
        let body_ =
          Printf.sprintf(
            "Path: %s\nMethod: %s\nBody: %s",
            req.target,
            req.meth |> Zillaml.Method.to_string,
            body,
          );
        Deferred.return(res(~status=`Not_found, ~headers=None, body_));
      };
    };
    read_body(reqd) >>= run_router |> Deferred.don't_wait_for;
  };

  start(
    ~port,
    ~on_start,
    ~max_accepts_per_batch,
    ~request_handler,
    ~error_handler,
  );
};

module type Websocket_interface = {
  type server_to_client;
  type client_to_server;
  let server_to_client_of_string: string => server_to_client;
  let string_of_server_to_client: server_to_client => string;
  let client_to_server_of_string: string => client_to_server;
  let string_of_client_to_server: client_to_server => string;
};

module Make_server_with_ws = (Ws: Websocket_interface) => {
  module Body = Zillaml.Body;
  module Headers = Zillaml.Headers;
  module Reqd = Zillaml.Reqd;
  module Response = Zillaml.Response;
  module Status = Zillaml.Status;
  let send = (~wsd, server_to_client) =>
    if (!Websocketzilla.Wsd.is_closed(wsd)) {
      let str = Ws.string_of_server_to_client(server_to_client);
      let bs = Bigstringaf.of_string(~off=0, ~len=String.length(str), str);
      Websocketzilla.Wsd.schedule(
        wsd,
        bs,
        ~kind=`Text,
        ~off=0,
        ~len=String.length(str),
      );
    };
  let websocket_handler =
      (~state, ~on_ws_connect, req: Zillaml.Request.t, _client_address, wsd) => {
    let accum = ref([]);

    let {Websocket_async.on_message, on_close} =
      on_ws_connect({
        Websocket_async.wsd,
        send: send(~wsd),
        path: get_path(req),
        query: get_query(req),
        server_state: state
      });
    let finalise_content = accum_content =>
      String.concat(List.rev(accum_content));
    let frame = (~opcode, ~is_fin, bs, ~off, ~len) =>
      switch (opcode) {
      | `Continuation =>
        if (List.is_empty(accum^)) {
          Log.Global.error("Bad frame in the middle of a fragmented message");
          Websocketzilla.Wsd.close(wsd);
          on_close();
        } else {
          accum := [Bigstringaf.substring(bs, ~off, ~len), ...accum^];
          if (is_fin) {
            let payload =
              finalise_content(accum^) |> Ws.client_to_server_of_string;
            accum := [];
            on_message(payload);
          };
        }
      | `Text
      | `Binary =>
        if (List.is_empty(accum^)) {
          if (is_fin) {
            Bigstringaf.substring(bs, ~off, ~len)
            |> Ws.client_to_server_of_string
            |> on_message;
          } else {
            accum := [Bigstringaf.substring(bs, ~off, ~len)];
          };
        } else {
          Log.Global.error("Bad frame in the middle of a fragmented message");
          Websocketzilla.Wsd.close(wsd);
          accum := [];
          on_close();
        }
      | `Connection_close =>
        Websocketzilla.Wsd.close(wsd);
        accum := [];
        on_close();
      | `Ping => Websocketzilla.Wsd.send_ping(wsd)
      | `Pong
      | `Other(_) => print_endline("other")
      };
    let eof = () => {
      Log.Global.error("EOF\n%!");
      Websocketzilla.Wsd.close(wsd);
      on_close();
    };
    {Websocketzilla.Server_connection.frame, eof};
  };

  let ws_error_handler = (wsd, `Exn(exn)) => {
    let message = Exn.to_string(exn);
    let payload = Bytes.of_string(message);
    Websocketzilla.Wsd.send_bytes(
      wsd,
      ~kind=`Text,
      payload,
      ~off=0,
      ~len=Bytes.length(payload),
    );
    Websocketzilla.Wsd.close(wsd);
  };
  let http_error_handler = (_client_address, ~request as _=?, error, handle) => {
    let message =
      switch (error) {
      | `Exn(exn) => Exn.to_string(exn)
      | (#Status.client_error | #Status.server_error) as error =>
        Status.to_string(error)
      };
    let body = handle(Headers.empty);
    Body.write_string(body, message);
    Body.close_writer(body);
  };

  let upgrade_handler = (~state, ~on_ws_connect, req, addr, upgrade, ()) => {
    let ws_conn =
      Websocketzilla.Server_connection.create_websocket(
        ~error_handler=ws_error_handler,
        websocket_handler(~state, ~on_ws_connect, req, addr),
      );

    upgrade(Gluten.make((module Websocketzilla.Server_connection), ws_conn));
  };
  let http_request_handler = (~req_logger, ~http_router, _conn, reqd) => {
    let req = Zillaml.Reqd.request(reqd);
    let res = create_respond(reqd);
    switch (req_logger) {
    | Some(fn) => fn(req)
    | None => ()
    };
    let run_router = body => {
      switch%bind (http_router(req, body)) {
      | Some({status, headers, body}) =>
        Deferred.return(res(~status, ~headers, body))
      | None =>
        let body =
          switch (body) {
          | Some(str) => str
          | None => ""
          };
        let body_ =
          Printf.sprintf(
            "Path: %s\nMethod: %s\nBody: %s",
            req.target,
            req.meth |> Zillaml.Method.to_string,
            body,
          );
        Deferred.return(res(~status=`Not_found, ~headers=None, body_));
      };
    };
    read_body(reqd) >>= run_router |> Deferred.don't_wait_for;
  };

  let request_handler =
      (
        ~req_logger,
        ~http_router,
        ~on_ws_connect,
        ~check_for_websocket_request,
        addr,
        reqd: Gluten.Reqd.t(Zillaml.Reqd.t),
        ~state
      ) => {
    let {Gluten.Reqd.reqd, upgrade} = reqd;
    let req = Zillaml.Reqd.request(reqd);
    let io_handler = {
      switch%bind (check_for_websocket_request(req)) {
      | false =>
        Deferred.return(
          http_request_handler(~req_logger, ~http_router, addr, reqd),
        )
      | true =>
        switch (
          Websocketzilla.Handshake.respond_with_upgrade(
            ~sha1,
            reqd,
            upgrade_handler(~state, ~on_ws_connect, req, addr, upgrade),
          )
        ) {
        | Ok () => Deferred.return()
        | Error(err_str) =>
          let response =
            Response.create(
              ~headers=Zillaml.Headers.of_list([("Connection", "close")]),
              `Bad_request,
            );
          Reqd.respond_with_string(reqd, response, err_str);
          Deferred.return();
        }
      };
    };

    Deferred.don't_wait_for(io_handler);
  };

  let start =
      (
        ~port,
        ~on_start,
        ~max_accepts_per_batch,
        ~http_router,
        ~on_ws_connect,
        ~check_for_websocket_request,
        ~req_logger,
        ~state
      ) =>
    start(
      ~port,
      ~on_start,
      ~max_accepts_per_batch,
      ~request_handler=
        request_handler(
          ~req_logger,
          ~http_router,
          ~on_ws_connect,
          ~check_for_websocket_request,
          ~state
        ),
      ~error_handler=http_error_handler,
    );
};
