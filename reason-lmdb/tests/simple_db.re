open Lmdb;

let () = {
  print_endline("create an env");
  let env = Env.(create(Rw, ~flags=Flags.no_subdir, "/tmp/foo.db"));

  print_endline("create the map");
  let map = Map.(create(Nodup, ~key=Conv.string, ~value=Conv.string))(env);

  print_endline("put the key-value binding in the map");
  Map.set(map, "foo", read_line());

  print_endline("get the value from the map");
  let s = Map.get(map, "foo");
  print_endline(s);
};
