type symbol = string * int
[@@deriving show]

let next_symbol = ref 0

(** key是符号的字符串，value是符号字符串对应的整数，
    保证了每个相同的符号都对应相同的整数 *)
let hashtable : (string, int) Hashtbl.t = Hashtbl.create 128

(** 将一个字符串转成一个symbol *)
let symbol name =
  try
    let i = Hashtbl.find hashtable name in (name, i)
  with Not_found ->
    let i = !next_symbol in
    let _ = next_symbol := i + 1 in
    let _ = Hashtbl.add hashtable name i in
    (name, i)

let name (name, _) = name

(** 构建符号表，以下定义表示map的key是symbol，
    value在实例化时确定 *)
module Table = Map.Make(
  struct
    type t = symbol
    let compare (_,i1) (_,i2) = compare i1 i2
  end
)

let empty = Table.empty
let enter (t, k, v) = Table.add k v t
let look (table, key) =
  try Some(Table.find key table)
  with Not_found -> None