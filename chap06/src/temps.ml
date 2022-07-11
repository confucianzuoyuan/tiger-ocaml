type temp = int
[@@deriving show]

let temps = ref 100

let newtemp () = let t = !temps in let _ = temps := t + 1 in t

let make_string t = "t" ^ (string_of_int t)

type label = Symbol.symbol
[@@deriving show]

let labs = ref 0

let postinc x = let i = !x in let _ = x := i + 1 in i

let namedlabel = Symbol.symbol

let new_label () = Symbol.symbol ("L" ^ (string_of_int (postinc labs)))