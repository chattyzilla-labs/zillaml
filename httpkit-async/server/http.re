

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

  let read_body =
    reqd => {
        let req = reqd |> Zillaml.Reqd.request;
        switch (req.meth) {
        | `POST
        | `PUT => {
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
        }

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
  (~port, ~on_start, ~request_handler, ~max_accepts_per_batch, ~error_handler) => {
    let where_to_listen = Tcp.Where_to_listen.of_port(port);
    Tcp.(
      Server.create_sock(
        ~on_handler_error=`Raise,
        ~backlog=10000,
        ~max_connections=10000,
        ~max_accepts_per_batch,
        where_to_listen
      )
    )(
      Server.create_connection_handler(~request_handler, ~error_handler)
    )
    >>= (
      (_server) => {
        on_start(port);
        Deferred.never();
      }
    );
  };


  let create_socket_server = (~port, ~on_start, ~max_accepts_per_batch, ~check_request) => {

    let module Body = Zillaml.Body;
    let module Headers = Zillaml.Headers;
    let module Reqd = Zillaml.Reqd;
    let module Response = Zillaml.Response;
    let module Status = Zillaml.Status;

    let websocket_handler = (_client_address, wsd) => {
      let accum = ref([]);
      let send = (str) => {
        let bs = Bigstringaf.of_string(~off=0, ~len=String.length(str), str)
        Websocketzilla.Wsd.schedule(wsd, bs, ~kind=`Text, ~off=0, ~len= String.length(str))
      }

      let finalise_content = (accum_content) => String.concat(List.rev(accum_content) |> List.map(~f=Bigstringaf.to_string));
      let frame = (~opcode, ~is_fin, bs, ~off, ~len) =>
        switch opcode {
        | `Continuation =>
          if(List.is_empty(accum^)) {
            Log.Global.error("Bad frame in the middle of a fragmented message");
            Websocketzilla.Wsd.close(wsd);
          } else {
            accum := [bs, ...accum^];
            if(is_fin){
              let payload = "ClientMessage: " ++ finalise_content(accum^)
              accum := []
              send(payload)
            }
          };
        | `Text
        | `Binary =>
          if(List.is_empty(accum^)) {
            ("ClientMessage: " ++ Bigstringaf.substring(bs, ~off, ~len)) |> send;
          } else {
            Log.Global.error("Bad frame in the middle of a fragmented message");
            Websocketzilla.Wsd.close(wsd);
            accum := []
          };
        | `Connection_close =>
            Websocketzilla.Wsd.close(wsd);
            accum := [];
        | `Ping => Websocketzilla.Wsd.send_ping(wsd)
        | `Pong
        | `Other(_) => print_endline("other")
        };
      let eof = () => {
        Log.Global.error("EOF\n%!");
        Websocketzilla.Wsd.close(wsd);
      };
      {Websocketzilla.Server_connection.frame, eof};
    };
    let error_handler = (wsd, `Exn(exn)) => {
      let message = Exn.to_string(exn);
      let payload = Bytes.of_string(message);
      Websocketzilla.Wsd.send_bytes(wsd, ~kind=`Text, payload, ~off=0, ~len=Bytes.length(payload));
      Websocketzilla.Wsd.close(wsd);
    };
    let http_error_handler = (_client_address, ~request as _=?, error, handle) => {
      let message =
        switch error {
        | `Exn(exn) => Exn.to_string(exn)
        | (#Status.client_error | #Status.server_error) as error => Status.to_string(error)
        };
      let body = handle(Headers.empty);
      Body.write_string(body, message);
      Body.close_writer(body);
    };
    let upgrade_handler = (addr, socket) =>
      Websocketzilla_async.Server.create_upgraded_connection_handler(
        ~error_handler,
        ~websocket_handler,
        addr,
        socket
      );
    let request_handler = (addr, reqd) => {
      let req = Zillaml.Reqd.request(reqd);
      let io_handler = {
          switch%bind(check_request(req)) {
          | true => Websocketzilla_async.Server.respond_with_upgrade(reqd, upgrade_handler(addr))
              >>| (
                fun
                | Ok () => ()
                | Error(err_str) => {
                    let response =
                      Response.create(
                        ~headers=Zillaml.Headers.of_list([("Connection", "close")]),
                        `Bad_request
                      );
                    Reqd.respond_with_string(reqd, response, err_str);
                  }
              )
          | false =>
                  Deferred.return(create_respond(reqd, ~status=`Forbidden, ~headers=None, "403 Forbidden"))
          }
      };

      Deferred.don't_wait_for(io_handler);
    };

    start(
     ~port,
     ~on_start,
     ~max_accepts_per_batch,
     ~request_handler,
     ~error_handler=http_error_handler,
    );
  };

  let create_server = (~port, ~on_start, ~max_accepts_per_batch, ~router, ~error_handler, ~reqlogger) => {
    let request_handler =
      (_conn, reqd) => {
        let req = Zillaml.Reqd.request(reqd);
        let res = create_respond(reqd);
        switch reqlogger {
        | Some(fn) => fn(req)
        | None => ()
        };
        let run_router = body => {
          switch%bind (router(req, body)) {
          | Some({status, headers, body}) => Deferred.return(res(~status, ~headers, body));
          | None =>
            let body =
            switch (body) {
            | Some(str) => str
            | None => ""
            };
            let body_ = Printf.sprintf(
              "Path: %s\nMethod: %s\nBody: %s",
              req.target,
              req.meth |> Zillaml.Method.to_string,
              body,
            );
            Deferred.return(res(~status=`Not_found, ~headers=None, body_));
          };

        };
        // change to deffered.ignore
        let _: Deferred.t(unit) = {
          let%bind x = read_body(reqd);
          run_router(x);
        };
        ();
      };

      start(
        ~port,
        ~on_start,
        ~max_accepts_per_batch,
        ~request_handler,
        ~error_handler,
      );
  };



