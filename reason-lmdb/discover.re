module C = Configurator.V1;

let split_env = var =>
  try(Str.(split(regexp_string(":")))(Sys.getenv(var))) {
  | Not_found => []
  };
let include_candidates =
  split_env("CPATH") @ ["/usr/include", "/usr/local/include", "/opt/include"]
and lib_candidates =
  split_env("LIBRARY_PATH")
  @ ["/lib", "/usr/lib", "/usr/local/lib", "/opt/lib"];

let () =
  C.main(~name="foo", c => {
    let lmdb_pc =
      switch (C.Pkg_config.get(c)) {
      | None => None
      | Some(pc) => C.Pkg_config.query(pc, ~package="lmdb")
      };

    let lmdb =
      switch (lmdb_pc) {
      | Some(lmdb) => lmdb
      | None =>
        let include_path =
          try(
            List.find(
              path => Sys.file_exists(path ++ "/lmdb.h"),
              include_candidates,
            )
          ) {
          | Not_found => failwith("lmdb.h not found")
          }
        and lib_path =
          try(
            List.find(
              path => Sys.file_exists(path ++ "/liblmdb.a"),
              lib_candidates,
            )
          ) {
          | Not_found => failwith("liblmdb.a not found")
          };

        C.Pkg_config.{
          cflags: ["-I" ++ include_path],
          libs: ["-L" ++ lib_path, "-llmdb"],
        };
      };

    C.Flags.write_sexp("cflags.sexp", lmdb.cflags);
    C.Flags.write_sexp("clibs.sexp", lmdb.libs);
  }
    
  );
