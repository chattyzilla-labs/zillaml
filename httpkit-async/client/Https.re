open Core;
open Async;

module M: (Zillaml_httpkit.Client.Request.S with type io('a) = Deferred.t('a) and type config = unit) =
  Zillaml_httpkit.Client.Request.Make({
    type io('a) = Deferred.t('a);

    type config = unit;
    open Zillaml_httpkit.Client;
    let send = (~config as _=?, req) => {


      let body = Request.body(req);
      let uri = Request.uri(req);

      let host = Uri.host_with_default(uri);
      let port = 443;
      let where_to_connect = Tcp.Where_to_connect.of_host_and_port( {host, port});
      Tcp.connect_sock(where_to_connect) >>= (socket => {
        let%bind conn = Zillaml_async.Client.SSL.create_connection_with_default(socket);
        let request = Request.as_zillaml(req);
        let finished = Ivar.create();
        let response_handler = (response, response_body) => {
          Ivar.fill(finished, Ok((response, response_body)));
        };
        let error_handler = error => Result.Error(`Connection_error(error)) |> Ivar.fill(finished);
        let request_body =
          Zillaml_async.Client.SSL.request(
            ~error_handler,
            ~response_handler,
            conn,
            request,
          );
          switch (body) {
          | Some(body) => Zillaml.Body.write_string(request_body, body)
          | None => ()
          };
          Zillaml.Body.flush(request_body, () => Zillaml.Body.close_writer(request_body));
          Ivar.read(finished);
        })

      };

  });

include M;
