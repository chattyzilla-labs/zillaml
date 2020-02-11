open Lmdb;

let test = env => (
  "Problem reports",
  [
    (
      "#15",
      `Slow,
      () => {
        Env.set_map_size(env, 104857600);
        let t =
          Map.(create(Nodup, ~key=Conv.string, ~value=Conv.string))(
            env,
            ~name="pr#15",
          );
        /* Put some entries */
        let rec put_count = t =>
          fun
          | 0 => ()
          | count => {
              let value_bytes = Bytes.make(10 * 8 * 1024, '1');
              Map.add(
                t,
                string_of_int(count),
                Bytes.to_string(value_bytes),
              );
              put_count(t, count - 1);
            };

        let count = 250;
        put_count(t, count);
        assert(Map.stat(t).entries == count);
        /* Iterate using cursor and print keys */
        ignore @@
        Lmdb.Cursor.go(
          Ro,
          t,
          cur => {
            /* Triggering GC here also SEGFAULTs */
            Gc.full_major();
            let rec print_keys =
              fun
              | 0 => ()
              | count => {
                  let (key, _) = Cursor.next(cur);
                  print_endline(key);
                  print_keys(count - 1);
                };

            print_keys(count);
          },
        );
      },
    ),
  ],
);
