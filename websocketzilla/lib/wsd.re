module IOVec = Zillaml.IOVec;

type mode = [ | `Client(unit => int32) | `Server];

type t = {
  faraday: Faraday.t,
  mode,
  mutable wakeup: Optional_thunk.t,
};

let default_ready_to_write = Sys.opaque_identity(() => ());

let create = mode => {
  faraday: Faraday.create(0x1000),
  mode,
  wakeup: Optional_thunk.none,
};

let mask = t =>
  switch (t.mode) {
  | `Client(m) => Some(m())
  | `Server => None
  };

let is_closed = t => Faraday.is_closed(t.faraday);

let on_wakeup = (t, k) =>
  if (Faraday.is_closed(t.faraday)) {
    failwith("on_wakeup on closed writer");
  } else if (Optional_thunk.is_some(t.wakeup)) {
    failwith("on_wakeup: only one callback can be registered at a time");
  } else {
    t.wakeup = Optional_thunk.some(k);
  };

let wakeup = t => {
  let f = t.wakeup;
  t.wakeup = Optional_thunk.none;
  Optional_thunk.call_if_some(f);
};

let schedule = (t, ~kind, payload, ~off, ~len) => {
  let mask = mask(t);
  Websocket.Frame.schedule_serialize(
    t.faraday,
    ~mask?,
    ~is_fin=true,
    ~opcode=(kind :> Websocket.Opcode.t),
    ~payload,
    ~off,
    ~len,
  );
  wakeup(t);
};

let send_bytes = (t, ~kind, payload, ~off, ~len) => {
  let mask = mask(t);
  Websocket.Frame.serialize_bytes(
    t.faraday,
    ~mask?,
    ~is_fin=true,
    ~opcode=(kind :> Websocket.Opcode.t),
    ~payload,
    ~off,
    ~len,
  );
  wakeup(t);
};

let send_ping = t => {
  Websocket.Frame.serialize_control(t.faraday, ~opcode=`Ping);
  wakeup(t);
};

let send_pong = t => {
  Websocket.Frame.serialize_control(t.faraday, ~opcode=`Pong);
  wakeup(t);
};

let flushed = (t, f) => Faraday.flush(t.faraday, f);

let close = t => {
  Websocket.Frame.serialize_control(t.faraday, ~opcode=`Connection_close);
  Faraday.close(t.faraday);
  wakeup(t);
};

let next = t =>
  switch (Faraday.operation(t.faraday)) {
  | `Close => `Close(0) /* XXX(andreas): should track unwritten bytes */
  | `Yield => `Yield
  | `Writev(iovecs) => `Write(iovecs)
  };

let report_result = (t, result) =>
  switch (result) {
  | `Closed => close(t)
  | `Ok(len) => Faraday.shift(t.faraday, len)
  };
