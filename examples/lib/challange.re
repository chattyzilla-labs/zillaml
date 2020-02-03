let (><) = (lst, value) => List.map(p => [value, ...p], lst);
let smallest_sum = leaves => {
  let size = List.length(leaves);
  // print_endline(size |> string_of_int)
  let rec aux  = (root, max, l, r, acc) => {
    let left_parents: list(list(int)) =
      l < 0
        ? [[]]
        : {
          let value = List.nth(leaves, l);
          switch(value < root){
          | false => [[]]
          | true =>
            let rnext = value * max;

            let max = max >= value ? max : value;
            // print_endline(rnext |> string_of_int);
            r  >= size && l - 1 < 0 ?  acc >< rnext : aux(rnext, max, l - 1, r, acc >< rnext);

          }
        };
    let right_parents =
      switch (r < size) {
      | false => [[]]
      | true =>
        let value = List.nth(leaves, r);
        switch (value >= root) {
        | false => [[]]
        | true =>
          let rnext = value * max;
          // print_endline(rnext |> string_of_int);
          let max = max >= value ? max : value;
          //  print_endline(max |> string_of_int);
          r + 1 >= size && l < 0 ?  acc >< rnext : aux(rnext, max, l, r + 1, acc >< rnext);

        };
      };

    left_parents @ right_parents;
  };
  leaves
  |> List.mapi(
    (i, leaf) => {
      // print_endline(leaf |> string_of_int);
      aux(leaf, leaf, i - 1, i + 1, [[]])
       |> List.map(lst => {
            // print_endline(i |> string_of_int);
            print_endline(List.length(lst) |> string_of_int);
            List.fold_left((a, b) => a + b, 0, lst);
          });
    }
  )
  |> List.concat
  |> List.fold_left((a, b) => a > b ? b : a, Core.Int.max_value)
};