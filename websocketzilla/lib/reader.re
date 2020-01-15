module AU = Angstrom.Unbuffered;

type parse_state('error) =
  | Done
  | Fail('error)
  | Partial(
      (Bigstringaf.t, ~off: int, ~len: int, AU.more) => AU.state(unit),
    );

type t('error) = {
  parser: Angstrom.t(unit),
  mutable parse_state: parse_state('error),
  mutable closed: bool,
};

let create = frame_handler => {
  let parser =
    Angstrom.(
      Websocket.Frame.parse
      >>| (
        frame => {
          let is_fin = Websocket.Frame.is_fin(frame);
          let opcode = Websocket.Frame.opcode(frame);
          Websocket.Frame.unmask_inplace(frame);
          Websocket.Frame.with_payload(
            frame,
            ~f=frame_handler(~opcode, ~is_fin),
          );
        }
      )
    );

  {parser, parse_state: Done, closed: false};
};

let transition = (t, state) =>
  switch (state) {
  | AU.Done(consumed, ())
  | AU.Fail(0 as consumed, _, _) =>
    t.parse_state = Done;
    consumed;
  | AU.Fail(consumed, marks, msg) =>
    t.parse_state = Fail(`Parse((marks, msg)));
    consumed;
  | AU.Partial({committed, continue}) =>
    t.parse_state = Partial(continue);
    committed;
  }
and start = (t, state) =>
  switch (state) {
  | AU.Done(_) => failwith("websocketaf.Reader.unable to start parser")
  | AU.Fail(0, marks, msg) =>
    t.parse_state = Fail(`Parse((marks, msg)))
  | AU.Partial({committed: 0, continue}) => t.parse_state = Partial(continue)
  | _ => assert(false)
  };

let next = t =>
  switch (t.parse_state) {
  | Done =>
    if (t.closed) {
      `Close;
    } else {
      `Read;
    }
  | Fail(failure) => `Error(failure)
  | Partial(_) => `Read
  };

let rec read_with_more = (t, bs, ~off, ~len, more) => {
  let consumed =
    switch (t.parse_state) {
    | Fail(_) => 0
    | Done =>
      start(t, AU.parse(t.parser));
      read_with_more(t, bs, ~off, ~len, more);
    | Partial(continue) => transition(t, continue(bs, more, ~off, ~len))
    };

  switch (more) {
  | Complete => t.closed = true
  | Incomplete => ()
  };
  consumed;
};
