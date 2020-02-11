open Lmdb;

let env =
  Env.create(
    Rw,
    ~flags=Env.Flags.(no_subdir + no_sync + write_map + no_lock + no_mem_init),
    ~map_size=104857600,
    ~max_maps=10,
    "/tmp/lmdb_test.db",
  );

let benchmark = repeat => {
  let errors = ref(0);

  let bench = (name, conv_key, conv_val, key, value, n) => {
    let map =
      Map.(create(Nodup, ~key=conv_key, ~value=conv_val))(env, ~name);
    let bench = (map, cycles) => {
      open Map;
      for (i in 0 to cycles - 1) {
        Map.(set(map, key(i), value(i),));
      };
      for (i in 0 to cycles - 1) {
        let v = get(map, key(i));
        if (v != value(i)) {
          incr(errors);
        };
      };
      drop(~delete=false, map);
    };

    (name, bench(map), n);
  };

  open Benchmark;
  let samples = {
    let n = 500;
    throughputN(
      ~repeat,
      1,
      [
        bench(
          "string",
          Conv.string,
          Conv.string,
          string_of_int,
          string_of_int,
          n,
        ),
        bench(
          "int32_be",
          Conv.int32_be,
          Conv.string,
          Int32.of_int,
          string_of_int,
          n,
        ),
        bench(
          "int32_le",
          Conv.int32_le,
          Conv.string,
          Int32.of_int,
          string_of_int,
          n,
        ),
        bench(
          "int64_be",
          Conv.int64_be,
          Conv.string,
          Int64.of_int,
          string_of_int,
          n,
        ),
        bench(
          "int64_le",
          Conv.int64_le,
          Conv.string,
          Int64.of_int,
          string_of_int,
          n,
        ),
      ],
    );
  };

  tabulate(samples);
  errors^;
};

let () = {
  let n =
    if (Array.length(Sys.argv) == 2) {
      int_of_string @@ Sys.argv[1];
    } else {
      1;
    };

  assert(benchmark(n) == 0);
};
