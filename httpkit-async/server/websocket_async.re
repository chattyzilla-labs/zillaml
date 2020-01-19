open Core;

type ws_connection_handler = {
  on_message: string => unit,
  on_close: unit => unit
};

type ws_connection = {
  path: list(string),
  wsd: Websocketzilla.Wsd.t,
  send: string => unit,
  id: int32
};

let regexComma = Str.regexp_string(",");
let upgrade_present = headers =>
  Zillaml.Headers.get_multi(headers, "connection")
  |> List.map(~f=hs => Str.split(regexComma, hs))
  |> List.join
  |> List.exists(~f=h =>
       String.lowercase(h) |> String.strip |> String.equal("upgrade")
     );

let default_ws_path = Router.check_req_path(~path="/ws");