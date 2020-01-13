module Opcode = {
  type standard_non_control = [ | `Continuation | `Text | `Binary];

  type standard_control = [ | `Connection_close | `Ping | `Pong];

  type standard = [ standard_non_control | standard_control];

  type t = [ standard | `Other(int)];

  let code =
    fun
    | `Continuation => 0x0
    | `Text => 0x1
    | `Binary => 0x2
    | `Connection_close => 0x8
    | `Ping => 0x9
    | `Pong => 0xa
    | `Other(code) => code;

  let code_table: array(t) = (
    [|
      `Continuation,
      `Text,
      `Binary,
      `Other(0x3),
      `Other(0x4),
      `Other(0x5),
      `Other(0x6),
      `Other(0x7),
      `Connection_close,
      `Ping,
      `Other(0xb),
      `Other(0xc),
      `Other(0xd),
      `Other(0xe),
      `Other(0xf),
    |]:
      array(t)
  );

  let unsafe_of_code = code => Array.unsafe_get(code_table, code);

  let of_code = code =>
    if (code > 0xf) {
      None;
    } else {
      Some(Array.unsafe_get(code_table, code));
    };

  let of_code_exn = code => {
    if (code > 0xf) {
      failwith("Opcode.of_code_exn: value can't fit in four bits");
    };
    Array.unsafe_get(code_table, code);
  };

  let to_int = code;
  let of_int = of_code;
  let of_int_exn = of_code_exn;

  let pp_hum = (fmt, t) => Format.fprintf(fmt, "%d", to_int(t));
};

module Close_code = {
  type standard = [
    | `Normal_closure
    | `Going_away
    | `Protocol_error
    | `Unsupported_data
    | `No_status_rcvd
    | `Abnormal_closure
    | `Invalid_frame_payload_data
    | `Policy_violation
    | `Message_too_big
    | `Mandatory_ext
    | `Internal_server_error
    | `TLS_handshake
  ];

  type t = [ standard | `Other(int)];

  let code =
    fun
    | `Normal_closure => 1000
    | `Going_away => 1001
    | `Protocol_error => 1002
    | `Unsupported_data => 1003
    | `No_status_rcvd => 1005
    | `Abnormal_closure => 1006
    | `Invalid_frame_payload_data => 1007
    | `Policy_violation => 1008
    | `Message_too_big => 1009
    | `Mandatory_ext => 1010
    | `Internal_server_error => 1011
    | `TLS_handshake => 1015
    | `Other(code) => code;

  let code_table: array(t) = (
    [|
      `Normal_closure,
      `Going_away,
      `Protocol_error,
      `Unsupported_data,
      `Other(1004),
      `No_status_rcvd,
      `Abnormal_closure,
      `Invalid_frame_payload_data,
      `Policy_violation,
      `Message_too_big,
      `Mandatory_ext,
      `Internal_server_error,
      `Other(1012),
      `Other(1013),
      `Other(1014),
      `TLS_handshake,
    |]:
      array(t)
  );

  let unsafe_of_code = code => Array.unsafe_get(code_table, code);

  let of_code = code =>
    if (code > 0xffff || code < 1000) {
      None;
    } else if (code < 1016) {
      Some(unsafe_of_code(code land 0b1111));
    } else {
      Some(`Other(code));
    };

  let of_code_exn = code => {
    if (code > 0xffff) {
      failwith("Close_code.of_code_exn: value can't fit in two bytes");
    };
    if (code < 1000) {
      failwith("Close_code.of_code_exn: value in invalid range 0-999");
    };
    if (code < 1016) {
      unsafe_of_code(code land 0b1111);
    } else {
      `Other(code);
    };
  };

  let to_int = code;
  let of_int = of_code;
  let of_int_exn = of_code_exn;
};

module Frame = {
  type t = Bigstringaf.t;

  let bit_is_set = (idx, v) => (v lsr idx) land 1 == 1;
  let is_fin = t => {
    let bits = Bigstringaf.unsafe_get(t, 0) |> Char.code;
    //bits land 1 lsl 8 == 1 lsl 8;
    bit_is_set(7, bits);
  };

  let rsv = t => {
    let bits = Bigstringaf.unsafe_get(t, 0) |> Char.code;
    bits lsr 4 land 0b0111;
  };

  let opcode = t => {
    let bits = Bigstringaf.unsafe_get(t, 0) |> Char.code;
    bits land 0b1111 |> Opcode.unsafe_of_code;
  };

  let payload_length_of_offset = (t, off) => {
    let bits = Bigstringaf.unsafe_get(t, off + 1) |> Char.code;
    let length = bits land 0b01111111;
    if (length == 126) {
      Bigstringaf.unsafe_get_int16_be(t, off + 2);
    } else if
      /* This is technically unsafe, but if somebody's asking us to read 2^63
       * bytes, then we're already screwed. */
      (length == 127) {
      Bigstringaf.unsafe_get_int64_be(t, off + 2) |> Int64.to_int;
    } else {
      length;
    };
  };

  let payload_length = t => payload_length_of_offset(t, 0);

  let has_mask = t => {
    let bits = Bigstringaf.unsafe_get(t, 1) |> Char.code;
    bits land 1 lsl 7 == 1 lsl 7;
  };

  let mask = t =>
    if (!has_mask(t)) {
      None;
    } else {
      Some(
        {
          let bits = Bigstringaf.unsafe_get(t, 1) |> Char.code;
          if (bits == 254) {
            Bigstringaf.unsafe_get_int32_be(t, 4);
          } else if (bits == 255) {
            Bigstringaf.unsafe_get_int32_be(t, 10);
          } else {
            Bigstringaf.unsafe_get_int32_be(t, 2);
          };
        },
      );
    };

  let mask_exn = t => {
    let bits = Bigstringaf.unsafe_get(t, 1) |> Char.code;
    if (bits == 254) {
      Bigstringaf.unsafe_get_int32_be(t, 4);
    } else if (bits == 255) {
      Bigstringaf.unsafe_get_int32_be(t, 10);
    } else if (bits >= 127) {
      Bigstringaf.unsafe_get_int32_be(t, 2);
    } else {
      failwith("Frame.mask_exn: no mask present");
    };
  };

  let payload_offset_of_bits = bits => {
    let initial_offset = 2;
    let mask_offset = (bits land 1 lsl 7) lsr (7 - 2);
    let length_offset = {
      let length = bits land 0b01111111;
      if (length < 126) {
        0;
      } else {
        2 lsl (length land 0b1) lsl 2;
      };
    };

    initial_offset + mask_offset + length_offset;
  };

  let payload_offset = t => {
    let bits = Bigstringaf.unsafe_get(t, 1) |> Char.code;
    payload_offset_of_bits(bits);
  };

  let with_payload = (t, ~f) => {
    let len = payload_length(t);
    let off = payload_offset(t);
    f(t, ~off, ~len);
  };

  let copy_payload = t => with_payload(t, ~f=Bigstringaf.copy);

  let copy_payload_bytes = t =>
    with_payload(
      t,
      ~f=(bs, ~off as src_off, ~len) => {
        let bytes = Bytes.create(len);
        Bigstringaf.blit_to_bytes(bs, ~src_off, bytes, ~dst_off=0, ~len);
        bytes;
      },
    );

  let length_of_offset = (t, off) => {
    let bits = Bigstringaf.unsafe_get(t, off + 1) |> Char.code;
    let payload_offset = payload_offset_of_bits(bits);
    let payload_length = payload_length_of_offset(t, off);
    payload_offset + payload_length;
  };

  let length = t => length_of_offset(t, 0);

  let apply_mask = (mask, bs, ~off, ~len) =>
    for (i in off to off + len - 1) {
      let j = (i - off) mod 4;
      let c = Bigstringaf.unsafe_get(bs, i) |> Char.code;
      let c =
        c
        lxor Int32.(logand(shift_right(mask, 8 * (3 - j)), 0xffl) |> to_int);
      Bigstringaf.unsafe_set(bs, i, Char.unsafe_chr(c));
    };

  let apply_mask_bytes = (mask, bs, ~off, ~len) =>
    for (i in off to off + len - 1) {
      let j = (i - off) mod 4;
      let c = Bytes.unsafe_get(bs, i) |> Char.code;
      let c =
        c
        lxor Int32.(logand(shift_right(mask, 8 * (3 - j)), 0xffl) |> to_int);
      Bytes.unsafe_set(bs, i, Char.unsafe_chr(c));
    };

  let unmask_inplace = t =>
    if (has_mask(t)) {
      let mask = mask_exn(t);
      let len = payload_length(t);
      let off = payload_offset(t);
      apply_mask(mask, t, ~off, ~len);
    };

  let mask_inplace = unmask_inplace;

  let parse =
    Angstrom.(
      Unsafe.peek(2, (bs, ~off, ~len as _) => length_of_offset(bs, off))
      >>= (len => Unsafe.take(len, Bigstringaf.sub))
    );

  let serialize_headers =
      (faraday, ~mask=?, ~is_fin, ~opcode, ~payload_length) => {
    let opcode = Opcode.to_int(opcode);
    let is_fin =
      if (is_fin) {
        1 lsl 7;
      } else {
        0;
      };
    let is_mask =
      switch (mask) {
      | None => 0
      | Some(_) => 1 lsl 7
      };

    Faraday.write_uint8(faraday, is_fin lor opcode);
    if (payload_length <= 125) {
      Faraday.write_uint8(faraday, is_mask lor payload_length);
    } else if (payload_length <= 0xffff) {
      Faraday.write_uint8(faraday, is_mask lor 126);
      Faraday.BE.write_uint16(faraday, payload_length);
    } else {
      Faraday.write_uint8(faraday, is_mask lor 127);
      Faraday.BE.write_uint64(faraday, Int64.of_int(payload_length));
    };
    switch (mask) {
    | None => ()
    | Some(mask) => Faraday.BE.write_uint32(faraday, mask)
    };
  };

  let serialize_control = (~mask=?, faraday, ~opcode) => {
    let opcode = (opcode :> Opcode.t);
    serialize_headers(
      faraday,
      ~mask?,
      ~is_fin=true,
      ~opcode,
      ~payload_length=0,
    );
  };

  let schedule_serialize =
      (~mask=?, faraday, ~is_fin, ~opcode, ~payload, ~off, ~len) => {
    serialize_headers(faraday, ~mask?, ~is_fin, ~opcode, ~payload_length=len);
    switch (mask) {
    | None => ()
    | Some(mask) => apply_mask(mask, payload, ~off, ~len)
    };
    Faraday.schedule_bigstring(faraday, payload, ~off, ~len);
  };

  let serialize_bytes =
      (~mask=?, faraday, ~is_fin, ~opcode, ~payload, ~off, ~len) => {
    serialize_headers(faraday, ~mask?, ~is_fin, ~opcode, ~payload_length=len);
    switch (mask) {
    | None => ()
    | Some(mask) => apply_mask_bytes(mask, payload, ~off, ~len)
    };
    Faraday.write_bytes(faraday, payload, ~off, ~len);
  };

  let schedule_serialize_bytes =
      (~mask=?, faraday, ~is_fin, ~opcode, ~payload, ~off, ~len) => {
    serialize_headers(faraday, ~mask?, ~is_fin, ~opcode, ~payload_length=len);
    switch (mask) {
    | None => ()
    | Some(mask) => apply_mask_bytes(mask, payload, ~off, ~len)
    };
    Faraday.write_bytes(faraday, payload, ~off, ~len);
  };
};
