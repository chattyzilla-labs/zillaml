module IOVec = Zillaml.IOVec;

type t = {
  reader: Reader.t([ | `Parse(list(string), string)]),
  wsd: Wsd.t,
  eof: unit => unit,
};

type input_handlers = {
  frame:
    (
      ~opcode: Websocket.Opcode.t,
      ~is_fin: bool,
      Bigstringaf.t,
      ~off: int,
      ~len: int
    ) =>
    unit,
  eof: unit => unit,
};

let create = (~websocket_handler) => {
  let mode = `Server;
  let wsd = Wsd.create(mode);
  let {frame, eof} = websocket_handler(wsd);
  {reader: Reader.create(frame), wsd, eof};
};

let next_read_operation = t => Reader.next(t.reader);

let next_write_operation = t => Wsd.next(t.wsd);

let read = (t, bs, ~off, ~len) =>
  Reader.read_with_more(t.reader, bs, ~off, ~len, Incomplete);

let read_eof = (t, bs, ~off, ~len) => {
  let r = Reader.read_with_more(t.reader, bs, ~off, ~len, Complete);
  t.eof();
  r;
};

let report_write_result = (t, result) => Wsd.report_result(t.wsd, result);

let yield_writer = (t, k) =>
  if (Wsd.is_closed(t.wsd)) {
    Wsd.close(t.wsd);
    k();
  } else {
    Wsd.when_ready_to_write(t.wsd, k);
  };

let close = ({wsd, _}) => Wsd.close(wsd);

let is_closed = ({wsd, _}) => Wsd.is_closed(wsd);
