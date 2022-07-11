module S = Symbol
module T = Types

type access = unit
[@@deriving show]

type enventry =
  | VarEntry of { ty : T.ty }
  | FunEntry of { formals : T.ty list; result : T.ty }
[@@deriving show]

let from_list xs =
  List.fold_left
  (fun acc (id, v) -> S.enter (S.symbol id) v acc)
  S.empty
  xs

let base_tenv = from_list [("int", T.INT); ("string", T.STRING)]

let base_venv = from_list [
  ("print", FunEntry{formals=[T.STRING]; result=T.UNIT});
  ("printi", FunEntry{formals=[T.INT]; result=T.UNIT});
  ("flush", FunEntry{formals=[]; result=T.UNIT});
  ("getchar", FunEntry{formals=[]; result=T.STRING});
  ("ord", FunEntry{formals=[T.STRING]; result=T.INT});
  ("chr", FunEntry{formals=[T.INT]; result=T.STRING});
  ("size", FunEntry{formals=[T.STRING]; result=T.INT});
  ("substring", FunEntry{formals=[T.STRING; T.INT; T.INT]; result=T.INT});
  ("concat", FunEntry{formals=[T.STRING; T.STRING]; result=T.STRING});
  ("not", FunEntry{formals=[T.INT]; result=T.INT});
  ("exit", FunEntry{formals=[T.INT]; result=T.UNIT});
]

let print_tenv map = Symbol.Table.iter (fun k v -> print_endline ((Symbol.symbol_name k) ^ " => " ^ (Types.show_ty v))) map

let print_venv map = Symbol.Table.iter (fun k v -> print_endline ((Symbol.symbol_name k) ^ " => " ^ (show_enventry v))) map