type word =
  | Hashtag
  | Star
  | Word(string);

type topic = list(word);

type action =
  | Publish((topic, string))
  | Subscribe(topic)
  | Unsubscribe(topic);

type topic_node = {
  id: int32,
  edge_count: int,
  binding_count: int,
};

type topic_edge = {
  id: int32,
  parent: int32,
  child: option(int32),
  word,
};

type connection = {
  id: int32,
  exchange_name: string,
  send: [ | `Message(topic, string)] => unit,
};

type connection_handler = {
  on_action: action => unit,
  on_close: unit => unit,
};

type exchange = {
  name: string,
  root_node: int32,
  topic_node: Core.Hashtbl.t(int32, topic_node),
  topic_edge: Core.Hashtbl.t(int32, Core.Hashtbl.t(string, topic_edge)),
  bindings: Core.Hashtbl.t(int32, Core.Hashtbl.t(int32, list(int32))),
  connections: Core.Hashtbl.t(int32, connection),
};

type broker = {
  exchange_table: Core.Hashtbl.t(string, exchange),
  on_connect: connection => connection_handler,
};

let create_node:
  (~edge_count: int=?, ~binding_count: int=?, int32) => topic_node;

let create_edge: (~child: option(int32)=?, int32, word) => topic_edge;

let create_conn:
  (int32, [ | `Message(topic, string)] => unit, string) => connection;

let string_of_word: word => string;

let string_of_topic: list(word) => string;

module Topic_Parser: {
  let str_to_word: string => word;
  let is_dot: char => bool;
  let dot: Angstrom.t(unit);
  let word: Angstrom.t(word);
  let extract_topic: string => list(word);
};

let parse_msg: string => action;

let get_exchange: list(string) => string;

let valid_path: list(string) => bool;

let random_int32: unit => int32;

let get_node_and_edges:
  (exchange, Core.Hashtbl.key(Core.Hashtbl.key(int32))) =>
  option((topic_node, Core.Hashtbl.t(string, topic_edge)));

let get_leaf: (exchange, list(word)) => (topic_node, topic_edge);

let get_edge_by_word: (Core.Hashtbl.t(string, 'a), string) => option('a);

let get_bindings_tbl_exn:
  (exchange, Core.Hashtbl.key(int32)) => Core.Hashtbl.t(int32, list(int32));

let get_bindings_tbl:
  (exchange, Core.Hashtbl.key(int32)) =>
  option(Core.Hashtbl.t(int32, list(int32)));

let get_bindings_by_edge_id:
  (Core.Hashtbl.t(int32, 'a), int32) => option('a);

let get_bindings_by_edge_id_exn: (Core.Hashtbl.t(int32, 'a), int32) => 'a;

let create_exchange: string => exchange;

let add_exchange:
  (Core.Hashtbl.t(string, exchange), Core.Hashtbl.key(string)) => exchange;

let subscribe: (exchange, connection, list(word)) => unit;

let unsubscribe: (exchange, connection, list(word)) => unit;

let publish: (exchange, Core.Int32.t, [< | `Message(topic, string)]) => unit;

let create_broker: unit => broker;