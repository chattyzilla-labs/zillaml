module Username: Core.Identifiable;
module Topic: Core.Identifiable;
module Message: {
  type t = {
    text: string,
    topic: Topic.t,
    from: Username.t,
    time: Core.Time.t,
  };
  let t_of_sexp: Ppx_sexp_conv_lib.Sexp.t => t;
  let sexp_of_t: t => Ppx_sexp_conv_lib.Sexp.t;
  let bin_shape_t: Core.Bin_prot.Shape.t;
  let bin_size_t: Core.Bin_prot.Size.sizer(t);
  let bin_write_t: Core.Bin_prot.Write.writer(t);
  let bin_writer_t: Core.Bin_prot.Type_class.writer(t);
  let __bin_read_t__: Core.Bin_prot.Read.reader(int => t);
  let bin_read_t: Core.Bin_prot.Read.reader(t);
  let bin_reader_t: Core.Bin_prot.Type_class.reader(t);
  let bin_t: Core.Bin_prot.Type_class.t(t);
  let compare: (t, t) => int;
};