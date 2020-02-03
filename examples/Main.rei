module Hello: {
  module Model: {
    let name: string;
    type query = string;
    type response = {name: query};
  };
  type query = string;
  type response = Model.response = {name: query};
  module Register:
    (
      Version_i: {
        let version: int;
        type query;
        let bin_shape_query: Bin_prot.Shape.t;
        let bin_size_query: Bin_prot.Size.sizer(query);
        let bin_write_query: Bin_prot.Write.writer(query);
        let bin_writer_query: Bin_prot.Type_class.writer0(query);
        let bin_read_query: Bin_prot.Read.reader(query);
        let __bin_read_query__: Bin_prot.Read.reader(int => query);
        let bin_reader_query: Bin_prot.Type_class.reader0(query);
        let bin_query: Bin_prot.Type_class.t0(query);
        type response;
        let bin_shape_response: Bin_prot.Shape.t;
        let bin_size_response: Bin_prot.Size.sizer(response);
        let bin_write_response: Bin_prot.Write.writer(response);
        let bin_writer_response: Bin_prot.Type_class.writer0(response);
        let bin_read_response: Bin_prot.Read.reader(response);
        let __bin_read_response__: Bin_prot.Read.reader(int => response);
        let bin_reader_response: Bin_prot.Type_class.reader0(response);
        let bin_response: Bin_prot.Type_class.t0(response);
        let query_of_model: string => query;
        let model_of_response: response => Model.response;
      },
    ) =>
     {
      let rpc:
        Async_rpc_kernel.Rpc.Rpc.t(Version_i.query, Version_i.response);
    };
  module Register':
    (
      Version_i: {
        let version: int;
        type query;
        let bin_shape_query: Bin_prot.Shape.t;
        let bin_size_query: Bin_prot.Size.sizer(query);
        let bin_write_query: Bin_prot.Write.writer(query);
        let bin_writer_query: Bin_prot.Type_class.writer0(query);
        let bin_read_query: Bin_prot.Read.reader(query);
        let __bin_read_query__: Bin_prot.Read.reader(int => query);
        let bin_reader_query: Bin_prot.Type_class.reader0(query);
        let bin_query: Bin_prot.Type_class.t0(query);
        type response;
        let bin_shape_response: Bin_prot.Shape.t;
        let bin_size_response: Bin_prot.Size.sizer(response);
        let bin_write_response: Bin_prot.Write.writer(response);
        let bin_writer_response: Bin_prot.Type_class.writer0(response);
        let bin_read_response: Bin_prot.Read.reader(response);
        let __bin_read_response__: Bin_prot.Read.reader(int => response);
        let bin_reader_response: Bin_prot.Type_class.reader0(response);
        let bin_response: Bin_prot.Type_class.t0(response);
        let query_of_model: string => query;
        let model_of_response: (string, response) => Model.response;
      },
    ) =>
     {
      let rpc:
        Async_rpc_kernel.Rpc.Rpc.t(Version_i.query, Version_i.response);
    };
  let dispatch_multi:
    (Persistent_connection.Versioned_rpc.conn, query) =>
    Async_kernel.Deferred.t(Core_kernel.Or_error.t(response));
  let rpcs: unit => list(Async_rpc_kernel.Rpc.Any.t);
  let versions: unit => Core_kernel.Int.Set.t;
  let name: query;
  module V1: {let rpc: Async_rpc_kernel.Rpc.Rpc.t(query, query);};
};
let implementations: Async_rpc_kernel.Rpc.Implementations.t(unit);
let testServer: unit => Async_kernel.Deferred.t(unit);
let testClient: unit => Async_kernel.Deferred.t('a);