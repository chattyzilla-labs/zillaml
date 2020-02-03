open Async;

let port: Command.Param.t(int);

let host_port_pair: Command.Param.t((string, int));

let with_rpc_conn:
  (Rpc.Connection.t => Deferred.t('a), ~host: string, ~port: int) =>
  Deferred.t('a);

let start_server:
  (
    ~env: 'a,
    ~stop: Deferred.t(unit)=?,
    ~implementations: list(Rpc.Implementation.t('a)),
    ~port: int,
    unit
  ) =>
  Deferred.t(unit);
