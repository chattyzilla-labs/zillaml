type t = {
  req: Zillaml.Request.t,
  uri: Uri.t,
  body: option(string),
  headers: list((string, string)),
};

let create:
  (
    ~headers: list((string, string))=?,
    ~body: string=?,
    Zillaml.Method.t,
    Uri.t
  ) =>
  t =
  (~headers=[], ~body="", meth, uri) => {
    let host = Uri.host_with_default(uri);
    let content_length = body |> String.length |> string_of_int;
    let headers =
      [("Host", host), ("Content-Length", content_length)] @ headers;

    {
      req:
        Zillaml.Request.create(
          ~headers=Zillaml.Headers.of_list(headers),
          meth,
          uri |> Uri.to_string,
        ),
      uri,
      headers,
      body:
        switch (body) {
        | "" => None
        | _ => Some(body)
        },
    };
  };

let as_zillaml = req => req.req;
let body = req => req.body;
let headers = req => req.headers;
let uri = req => req.uri;

module type S = {
  type io('a);

  type config;

  let send:
    (~config: config=?, t) =>
    io(
      result(
        (Zillaml.Response.t, Zillaml.Body.t([ | `read])),
        [> | `Connection_error(Zillaml.Client_connection.error)],
      ),
    );
};

module Make =
       (M: S)
       : (S with type io('a) = M.io('a) and type config = M.config) => {
  type io('a) = M.io('a);

  type config = M.config;

  let send = M.send;
};

module Headers = {
  let json = ("Content-Type", "application/json");
  let form_url_encoded = (
    "Content-Type",
    "application/x-www-form-urlencoded",
  );
  let text = ("Content-Type", "text/plain");
  let html = ("Content-Type", "text/html");
  let xml = ("Content-Type", "application/xml");

  let add_header = (header, req: t) => {
    ...req,
    headers: [header, ...req.headers],
  };
  let user_agent_header = agent => ("User-Agent", agent);
  let basic_auth_header = token => ("Authorization", "Basic " ++ token);
};

let set_body = (body, req: t) => {...req, body: Some(body)};

let set_content_type = (content, req: t) =>
  switch (content) {
  | `JSON => req |> Headers.add_header(Headers.json)
  | `FORM_URL_ENCODE => req |> Headers.add_header(Headers.form_url_encoded)
  | `TEXT => req |> Headers.add_header(Headers.text)
  | `HTML => req |> Headers.add_header(Headers.html)
  | `XML => req |> Headers.add_header(Headers.xml)
  };
let set_user_agent = (agent, req) =>
  req |> Headers.add_header(Headers.user_agent_header(agent));
let set_basic_auth = (token, req) =>
  req |> Headers.add_header(Headers.basic_auth_header(token));