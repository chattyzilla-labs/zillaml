module IOVec = Zillaml.IOVec;

type mode = [ | `Client(unit => int32) | `Server];

type t = {
  faraday: Faraday.t,
  mode,
  mutable when_ready_to_write: unit => unit,
};

let default_ready_to_write = Sys.opaque_identity(() => ());

let create = mode => {
  faraday: Faraday.create(0x1000),
  mode,
  when_ready_to_write: default_ready_to_write,
};

let mask = t =>
  switch (t.mode) {
  | `Client(m) => Some(m())
  | `Server => None
  };

let ready_to_write = t => {
  let callback = t.when_ready_to_write;
  t.when_ready_to_write = default_ready_to_write;
  callback();
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
  ready_to_write(t);
};

let send_bytes = (t, ~kind, payload, ~off, ~len) => {
  let mask = mask(t);
  Websocket.Frame.schedule_serialize_bytes(
    t.faraday,
    ~mask?,
    ~is_fin=true,
    ~opcode=(kind :> Websocket.Opcode.t),
    ~payload,
    ~off,
    ~len,
  );
  ready_to_write(t);
};

let send_ping = t => {
  Websocket.Frame.serialize_control(t.faraday, ~opcode=`Ping);
  ready_to_write(t);
};

let send_pong = t => {
  Websocket.Frame.serialize_control(t.faraday, ~opcode=`Pong);
  ready_to_write(t);
};

let flushed = (t, f) => Faraday.flush(t.faraday, f);

let close = t => {
  Websocket.Frame.serialize_control(t.faraday, ~opcode=`Connection_close);
  Faraday.close(t.faraday);
  ready_to_write(t);
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

let is_closed = t => Faraday.is_closed(t.faraday);

let when_ready_to_write = (t, callback) =>
  if (!(t.when_ready_to_write === default_ready_to_write)) {
    failwith(
      "Wsd.when_ready_to_write: only one callback can be registered at a time",
    );
  } else if (is_closed(t)) {
    callback();
  } else {
    t.when_ready_to_write = callback;
  };
