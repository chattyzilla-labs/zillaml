exception OUTPUT_ERROR(string);

let rec last =
  fun
  | [] => None
  | [a] => Some(a)
  | [_, ...b] => last(b);

let (): unit = {
  switch (last(["a", "b", "c", "d"])) {
  | Some(a) =>
    print_endline(
      Printf.sprintf(
        "last([\"a\", \"b\", \"c\", \"d\"]) = Some(\"%s\")\n",
        a,
      ),
    )
  | None => print_endline("None")
  };
};

let rec last_two =
  fun
  | []
  | [_] => None
  | [a, b] => Some((a, b))
  | [_, ...rest] => last_two(rest);

let () = {
  switch (last_two(["a", "b", "c", "d"])) {
  | Some((a, b)) when a == "c" && b == "d" =>
    print_endline(
      Printf.sprintf(
        "last_two([\"a\", \"b\", \"c\", \"d\"]) = Some((\"%s\", \"%s\"))\n",
        a,
        b,
      ),
    )
  | _ => raise(OUTPUT_ERROR("Unexpected value returned for last_two test"))
  };
};

type node('a) =
  | One('a)
  | Many(list(node('a)));

// Flatten a nested list structure.
let rec flatten =
  fun
  | [] => []
  | [a, ...b] =>
    switch (a) {
    | One(v) => [v, ...flatten(b)]
    | Many(nodes) => flatten(nodes) @ flatten(b)
    };

let flatten_with_aux = list => {
  let rec aux = acc =>
    fun
    | [] => acc
    | [One(a), ...b] => aux([a, ...acc], b)
    | [Many(nodes), ...b] => aux(aux(acc, nodes), b);

  List.rev(aux([], list));
};

let () = {
  switch (
    flatten([
      One("a"),
      Many([One("b"), Many([One("c"), One("d")]), One("e")]),
    ])
  ) {
  | ["a", "b", "c", "d", "e"] =>
    print_endline(
      {|flatten([One("a"), Many([One("b"), Many([One("c"), One("d")]), One("e")])]) = ["a", "b", "c", "d", "e"]|},
    );
    print_newline();
  | _ => raise(OUTPUT_ERROR("Unexpected value returned for flatten test"))
  };
};

let () = {
  switch (
    flatten_with_aux([
      One("a"),
      Many([One("b"), Many([One("c"), One("d")]), One("e")]),
    ])
  ) {
  | ["a", "b", "c", "d", "e"] =>
    print_endline(
      {|flatten_with_aux([One("a"), Many([One("b"), Many([One("c"), One("d")]), One("e")])]) = ["a", "b", "c", "d", "e"]|},
    );
    print_newline();
  | _ =>
    raise(
      OUTPUT_ERROR("Unexpected value returned for flatten_with_aux test"),
    )
  };
};

// Eliminate consecutive duplicates of list elements
let rec compress =
  fun
  | [a, ...[b, ..._] as t] => a == b ? compress(t) : [a, ...compress(t)]
  | smaller => smaller;

let compress_with_aux = list => {
  let rec aux = (acc, last) =>
    fun
    | [] => acc
    | [a, ...b] => a != last ? aux([a, ...acc], a, b) : aux(acc, last, b);

  switch (list) {
  | [] => list
  | [head, ...tail] => [head, ...List.rev(aux([], head, tail))]
  };
};

let () = {
  switch (
    compress([
      "a",
      "a",
      "a",
      "b",
      "c",
      "c",
      "c",
      "a",
      "a",
      "d",
      "e",
      "e",
      "e",
    ])
  ) {
  | ["a", "b", "c", "a", "d", "e"] =>
    print_endline(
      {|compress(["a", "a", "a", "b", "c", "c", "c", "a", "a", "d", "e", "e", "e"]) = ["a", "b", "c", "a", "d", "e"]|},
    );
    print_newline();
  | _ => raise(OUTPUT_ERROR("Unexpected value returned for compress test"))
  };
};

let () = {
  switch (
    compress_with_aux([
      "a",
      "a",
      "a",
      "b",
      "c",
      "c",
      "c",
      "a",
      "a",
      "d",
      "e",
      "e",
      "e",
    ])
  ) {
  | ["a", "b", "c", "a", "d", "e"] =>
    print_endline(
      {|compress_with_aux(["a", "a", "a", "b", "c", "c", "c", "a", "a", "d", "e", "e", "e"]) = ["a", "b", "c", "a", "d", "e"]|},
    );
    print_newline();
  | _ =>
    raise(
      OUTPUT_ERROR("Unexpected value returned for compress_with_aux test"),
    )
  };
};

//  Pack consecutive duplicates of list elements into sublists
let pack = list => {
  let rec aux = (sublist, acc) =>
    fun
    | [] => []
    | [a] => [[a, ...sublist], ...acc]
    | [a, ...[b, ..._] as t] =>
      a == b
        ? aux([a, ...sublist], acc, t)
        : aux([], [[a, ...sublist], ...acc], t);

  List.rev(aux([], [], list));
};

let () = {
  switch (
    pack(["a", "a", "a", "b", "c", "c", "c", "a", "a", "d", "e", "e", "e"])
  ) {
  | [
      ["a", "a", "a"],
      ["b"],
      ["c", "c", "c"],
      ["a", "a"],
      ["d"],
      ["e", "e", "e"],
    ] =>
    print_endline(
      {|pack(["a", "a", "a", "b", "c", "c", "c", "a", "a", "d", "e", "e", "e"]) = [["a", "a", "a"], ["b"], ["c", "c", "c"], ["a", "a"], ["d"], ["e", "e", "e"]]|},
    );
    print_newline();
  | _ => raise(OUTPUT_ERROR("Unexpected value returned for pack test"))
  };
};

// Rotate a list N places to the left
// todo implement in a more efficent way using a slice and modulo approach
let rec rotate = list =>
  fun
  | 0 => list
  | n when n < 0 => List.rev(rotate(List.rev(list), n * (-1)))
  | n =>
    switch (list) {
    | [] => list
    | [head, ...tail] => rotate(List.rev([head, ...List.rev(tail)]), n - 1)
    };

let () = {
  switch (rotate(["a", "b", "c", "d", "e", "f", "g", "h"], -2)) {
  | ["g", "h", "a", "b", "c", "d", "e", "f"] =>
    print_endline(
      {|rotate(["a", "b", "c", "d", "e", "f", "g", "h"], -2) = ["g", "h", "a", "b", "c", "d", "e", "f"]|},
    );
    print_newline();
  | _ =>
    raise(
      OUTPUT_ERROR(
        "Unexpected value returned for rotate test given negative int",
      ),
    )
  };
};

let () = {
  switch (rotate(["a", "b", "c", "d", "e", "f", "g", "h"], 3)) {
  | ["d", "e", "f", "g", "h", "a", "b", "c"] =>
    print_endline(
      {|rotate(["a", "b", "c", "d", "e", "f", "g", "h"], 3) = ["d", "e", "f", "g", "h", "a", "b", "c"]|},
    );
    print_newline();
  | _ =>
    raise(
      OUTPUT_ERROR(
        "Unexpected value returned for rotate test given positive int",
      ),
    )
  };
};

let rec extract_head = (set, n) =>
  fun
  | k when k == n => [set]
  | k when n - k < 0 => []
  | 1 =>
    switch (set) {
    | [] => []
    | [head, ..._] => [[head]]
    }
  | 2 =>
    switch (set) {
    | [_]
    | [] => []
    | [head, ...tail] => List.map(variation => [head, variation], tail)
    }
  | k => sum_extract_heads([], k, n, List.hd(set), List.tl(set))
and sum_extract_heads = (acc, k, n, s0, set) =>
  if (0 > n - k) {
    acc;
  } else {
    let acc =
      List.rev_append(
        extract_head(set, n - 1, k - 1) |> List.map(lst => [s0, ...lst]),
        acc,
      );
    let tail =
      switch (set) {
      | [] => []
      | [_, ...tl] => tl
      };
    sum_extract_heads(acc, k, n - 1, s0, tail);
  };
let extract = (k, set) => {
  let setLength = List.length(set);
  let rec aux = (acc, set) =>
    fun
    | i when i > setLength - k => acc
    | i => {
        let acc = List.rev_append(extract_head(set, setLength - i, k), acc);
        let tail =
          switch (set) {
          | [] => []
          | [_, ...t] => t
          };
        aux(acc, tail, i + 1);
      };
  List.rev(aux([], set, 0));
};
let rec extract_non_stack_safe = (k, list) =>
  if (k <= 0) {
    [[]];
  } else {
    switch (list) {
    | [] => []
    | [h, ...tl] =>
      let with_h =
        List.map(l => [h, ...l], extract_non_stack_safe(k - 1, tl));
      let without_h = extract_non_stack_safe(k, tl);
      List.rev_append(with_h, without_h);
    };
  };