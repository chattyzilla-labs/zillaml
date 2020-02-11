# Reason-lmdb

The [LMDB][] database is a fast in-file database that supports ACID transactions.

These bindings expose a typesafe yet low-overhead API. Both transactions and cursors are available. 
Database implementations are specialized both by keys and values.
Two module are predefined: `Lmdb.Db` (string keys and string values) and `Lmdb.IntDb` (int keys and string values). 
New implementation (which can use special LMDB features such as multi-values) can be added via a functorial interface.

A [simple example](tests/simple_db.re).

```reason
open Lmdb
let env = Env.create("mydb")
let db = Db.create(~create:true env "Camelidae")
Db.put(db, "Bactrian camel", "Elegant and beautiful animal with two humps.")
```

[lmdb]: http://symas.com/mdb/#overview
