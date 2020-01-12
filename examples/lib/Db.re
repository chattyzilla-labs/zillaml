open Async;


let connection_url = "postgresql://postgres@localhost:32768";
let pool =
  switch (Caqti_async.connect_pool(~max_size=10, Uri.of_string(connection_url))) {
  | Ok(pool) => pool
  | Error(err) => failwith(Caqti_error.show(err))
  };

type error =
  | Database_error(string);

module type db = Caqti_async.CONNECTION;
let or_error = m =>
  switch%bind(m) {
  | Ok(a) => Ok(a) |> Async.return
  | Error(e) => Error(Database_error(Caqti_error.show(e))) |> Async.return
  };

let run_query = value => Caqti_async.Pool.use(value, pool) |> or_error;
let add_org_query_string = "INSERT INTO organization_vault.organization (name, domain, email) VALUES ($1, $2, $3) RETURNING id, name, domain, email";
let add_org_query =
  Caqti_request.find(
    Caqti_type.(tup3(string, string, string)),
    Caqti_type.(tup4(string, string, string, string)),
    add_org_query_string,
  );

let add_org = (name, domain, email) => {
  let add_org' = (module C: db) =>
    C.find(add_org_query, (name, domain, email))
  run_query(add_org')
};

