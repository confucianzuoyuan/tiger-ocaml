module S = Symbol
module T = Types

type access = unit
[@@deriving show]

type enventry =
  | VarEntry of {
    access : Translate.access;
    ty : T.ty;
    read_only : bool;
  }
  | FunEntry of {
    level : Translate.level;
    label : Temp.label;
    formals : T.ty list;
    result : T.ty;
  }
[@@deriving show]

let from_list xs =
  List.fold_left
  (fun acc (id, v) -> S.enter(acc, S.symbol id, v))
  S.empty
  xs

let base_tenv = from_list [("int", T.INT); ("string", T.STRING)]

let base_venv = from_list [
  ("print",     FunEntry{level=Translate.outermost; label=Temp.newlabel(); formals=[T.STRING]; result=T.UNIT});
  ("printi",    FunEntry{level=Translate.outermost; label=Temp.newlabel(); formals=[T.INT]; result=T.UNIT});
  ("flush",     FunEntry{level=Translate.outermost; label=Temp.newlabel(); formals=[]; result=T.UNIT});
  ("getchar",   FunEntry{level=Translate.outermost; label=Temp.newlabel(); formals=[]; result=T.STRING});
  ("ord",       FunEntry{level=Translate.outermost; label=Temp.newlabel(); formals=[T.STRING]; result=T.INT});
  ("chr",       FunEntry{level=Translate.outermost; label=Temp.newlabel(); formals=[T.INT]; result=T.STRING});
  ("size",      FunEntry{level=Translate.outermost; label=Temp.newlabel(); formals=[T.STRING]; result=T.INT});
  ("substring", FunEntry{level=Translate.outermost; label=Temp.newlabel(); formals=[T.STRING; T.INT; T.INT]; result=T.INT});
  ("concat",    FunEntry{level=Translate.outermost; label=Temp.newlabel(); formals=[T.STRING; T.STRING]; result=T.STRING});
  ("not",       FunEntry{level=Translate.outermost; label=Temp.newlabel(); formals=[T.INT]; result=T.INT});
  ("exit",      FunEntry{level=Translate.outermost; label=Temp.newlabel(); formals=[T.INT]; result=T.UNIT});
]