open Symbol

type var =
  | SimpleVar of symbol
  | FieldVar of var * symbol
  | SubscriptVar of var * exp

and exp =
  | VarExp of var
  | NilExp
  | IntExp of int
  | StringExp of string
  | CallExp of {
    func: symbol;
    args: exp list;
  }
  | OpExp of {
    left: exp;
    oper: oper;
    right: exp;
  }
  | RecordExp of {
    fields: (symbol * exp) list;
    typ: symbol;
  }
  | SeqExp of exp list
  | AssignExp of {
    var: var;
    exp: exp;
  }
  | IfExp of {
    test: exp;
    then': exp;
    else': exp option;
  }
  | WhileExp of {
    test: exp;
    body: exp;
  }
  | ForExp of {
    var: symbol;
    escape: bool ref;
    lo: exp;
    hi: exp;
    body: exp;
  }
  | BreakExp
  | LetExp of {
    decs: dec list;
    body: exp;
  }
  | ArrayExp of {
    typ: symbol;
    size: exp;
    init: exp;
  }

and dec =
  | FunctionDec of fundec list
  | VarDec of vardec
  | TypeDec of tydec list

and fundec = {
  fun_name: symbol;
  fun_params: field list;
  fun_result: symbol option;
  fun_body: exp;
}

and vardec = {
  var_name: symbol;
  var_escape: bool ref;
  var_ty: symbol option;
  var_init: exp;
}

and tydec = {
  ty_name: symbol;
  ty_ty: ty;
}

and ty =
  | NameTy of symbol
  | RecordTy of field list
  | ArrayTy of symbol

and oper =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp

and field = {
  field_name: symbol;
  field_escape: bool ref;
  field_ty: symbol;
}
[@@deriving show]