open Core;
open Async;

module Hello = {
  module Model = {
    let name = "hello";
    type query = string;
    type response = { name: string };
  };
  include Model;
  include Versioned_rpc.Caller_converts.Rpc.Make(Model);
  module V1 =
    Register({
      let version = 1;
      [@deriving bin_io]
      type query = string;
      [@deriving bin_io]
      type response = string;
      let query_of_model = (str: string) => str;
      let model_of_response = name => { Model.name: name };
    });
};

let implementations =
  Rpc.Implementations.create_exn(
    ~on_unknown_rpc=`Close_connection,
    ~implementations=
      Versioned_rpc.Menu.add([
        Rpc.Rpc.implement(
          Hello.V1.rpc,
          ((), name) => {
            printf("server says hi\n");
            return(name ++ " is cool");
          },
        ),
      ]),
  );

let testServer = () => {
  let%bind _ =
    Rpc.Connection.serve(
      ~implementations,
      ~initial_connection_state=(_addr, _conn) => (),
      ~where_to_listen=Tcp.Where_to_listen.of_port(5000),
      (),
    );
  Deferred.return()
}

let testClient = () => {
  let host_and_port = Host_and_port.create(~host="localhost", ~port=5000);
  let on_unversioned_event:
    Persistent_connection.Rpc.Event.t => Deferred.t(unit) =
    fun
    | Obtained_address(_) => {
        printf("(Obtained_address <elided>)\n");
        return();
      }
    | event => {
        print_s([%sexp (event: Persistent_connection.Rpc.Event.t)]);
        return();
      };
  let unversioned_conn =
    Persistent_connection.Rpc.create'(
      ~on_event=on_unversioned_event, ~server_name="unversioned rpc", () =>
      return(Ok(host_and_port))
    );
  let%bind this_conn = Persistent_connection.Rpc.connected(unversioned_conn);
  let%bind name = Rpc.Rpc.dispatch_exn(Hello.V1.rpc, this_conn, "dakota");
  printf("name: %s", name);
  // let%bind () = Persistent_connection.Rpc.close(unversioned_conn);
  let on_versioned_event:
    Persistent_connection.Versioned_rpc.Event.t => Deferred.t(unit) =
    fun
    | Obtained_address(_) => {
        printf("(Obtained_address <elided>)\n");
        return();
      }
    | event => {
        print_s([%sexp (event: Persistent_connection.Versioned_rpc.Event.t)]);
        return();
      };
  let versioned_conn =
    Persistent_connection.Versioned_rpc.create'(
      ~on_event=on_versioned_event, ~server_name="versioned rpc", () =>
      return(Ok(host_and_port))
    );
  let%bind this_conn = Persistent_connection.Versioned_rpc.connected(versioned_conn);
  let%bind { name } = Hello.dispatch_multi(this_conn, "Dakota") |> Deferred.Or_error.ok_exn;
  printf("name: %s", name);
  Deferred.never()
  // Persistent_connection.Versioned_rpc.close(versioned_conn);
};

// https://github.com/janestreet/async/tree/master/persistent_connection

  let () = Command.async(
    ~summary="ping POST requests",
    Command.Param.(
      map(
        both(
          both(
            flag("-p", optional_with_default(8000, int), ~doc="int Source port to listen on"),
            flag("-a", optional_with_default(1, int), ~doc="int Maximum accepts per batch")
          ),
          flag("-server", optional_with_default(true, bool), ~doc="is server or client")
        ),
        ~f= ((server_flags, is_server), ()) => {
          switch is_server {
          | true =>
            // let%bind () = testServer()
            // Mlib.Server.server(server_flags, ());
            // let%bind result = Lib.Db.add_org("apple", "apple.com", "apple@apple.com")
            // switch result {
            // | Ok((id, _, _, _)) => print_endline("it worked baby: " ++ id)
            // | Error(err) => switch err {
            //   | Database_error(str) => print_endline("db error still good: " ++ str)
            // };
            // };
            Mlib.Server.socket_server(server_flags, ());
          | false => testClient()
          };

        }
      )
    )
  ) |> Command.run;
