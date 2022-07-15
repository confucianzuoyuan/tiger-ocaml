module A = Ast

let bool_to_string = function
  | true -> "true"
  | false -> "false"
;;

let say s = print_string s
;;

let sayln s = (say s; say "\n")
;;

let rec indent = function
  | 0 -> ()
  | i -> (say " "; indent(i-1))
;;

let opname = function
  | A.PlusOp -> "PlusOp"
  | A.MinusOp -> "MinusOp"
  | A.TimesOp -> "TimesOp"
  | A.DivideOp -> "DivideOp"
  | A.EqOp -> "EqOp"
  | A.NeqOp -> "NeqOp"
  | A.LtOp -> "LtOp"
  | A.LeOp -> "LeOp"
  | A.GtOp -> "GtOp"
  | A.GeOp -> "GeOp"
;;

let rec dolist d f l = match l with
  | [a] -> (sayln ""; f(a,d+1))
  | (a::r) -> (sayln ""; f(a,d+1); say ","; dolist d f r)
  | [] -> ()
;;

let rec var = function
  | (A.SimpleVar(s),d) -> (indent d; say "SimpleVar("; say(Symbol.name s); say ")")
  | (A.FieldVar(v,s),d) -> (indent d; sayln "FieldVar("; var(v,d+1); sayln ","; indent(d+1); say(Symbol.name s); say ")")
  | (A.SubscriptVar(v,e),d) -> (indent d; sayln "SubscriptVar("; var(v,d+1); sayln ","; exp(e,d+1); say ")")

and exp = function
  | (A.VarExp(v), d) -> (indent d; sayln "VarExp("; var(v,d+1); say ")")
  | (A.NilExp, d) -> (indent d; say "NilExp")
  | (A.IntExp(i), d) -> (indent d; say "IntExp("; say(string_of_int i); say ")")
  | (A.StringExp(s), d) -> (indent d; say "StringExp(\""; say s; say "\"")
  | (A.CallExp{func;args}, d) -> (indent d; say "CallExp("; say(Symbol.name func); say ",["; dolist d exp args; say "])")
  | (A.OpExp{left;oper;right}, d) -> (indent d; say "OpExp("; say(opname oper); sayln ","; exp(left,d+1); sayln ","; exp(right,d+1); say ")")
  | (A.RecordExp{fields;typ}, d) ->
    let f ((name,e),d) = (indent d; say "("; say(Symbol.name name); sayln ","; exp(e,d+1); say ")") in
    (indent d; say "RecordExp("; say(Symbol.name typ); sayln ",["; dolist d f fields; say "])")
  | (A.SeqExp(l),d) -> (indent d; say "SeqExp["; dolist d exp l; say "]")
  | (A.IfExp{test;then';else'},d) ->
    (indent d; sayln "IfExp("; exp(test, d+1); sayln ","; exp(then',d+1);
     match else' with
     | None -> ()
     | Some(e) -> (sayln ","; exp(e,d+1));
     say ")")
  | (A.WhileExp{test;body}, d) -> (indent d; sayln "WhileExp("; exp(test,d+1); sayln ","; exp(body,d+1); say ")")
  | (A.ForExp{var=v;escape=b;lo;hi;body},d) ->
    (indent d; sayln "ForExp("; say(Symbol.name v); say(bool_to_string (!b)); sayln ",";
     exp(lo,d+1); sayln ","; exp(hi,d+1); sayln ",";
     exp(body,d+1); say ")")
  | (A.BreakExp,d) -> (indent d; say "BreakExp")
  | (A.AssignExp{var=v;exp=e},d) -> (indent d; sayln "AssignExp("; var(v,d+1); sayln ","; exp(e,d+1); say ")")
  | (A.LetExp{decs;body},d) -> (indent d; say "LetExp(["; dolist d dec decs; sayln "],"; exp(body,d+1); say ")")
  | (A.ArrayExp{typ;size;init},d) -> (indent d; say "ArrayExp("; say (Symbol.name typ); sayln ",";
                                      exp(size,d+1); sayln ","; exp(init,d+1); say ")")
and dec = function
  | (A.FunctionDec(l),d) ->
    let field ((fd : A.field),d) = (indent d; say "("; say(Symbol.name fd.field_name); say ","; say(bool_to_string (!(fd.field_escape)));
                                say ","; say(Symbol.name fd.field_name); say ")")
    in
    let f ((fundec : A.fundec),d) =
      begin
      indent d;
      say "(";
      say(Symbol.name fundec.fun_name);
      say ",[";
      dolist d field fundec.fun_params;
      sayln "],";
      match fundec.fun_result with
      | None -> say "None"
      | Some(s) -> (say "Some("; say(Symbol.name s); say ")");
      sayln ",";
      exp(fundec.fun_body,d+1);
      say ")"
      end
    in (indent d; say "FunctionDec["; dolist d f l; say "]")
  | (A.VarDec{var_name;var_escape;var_ty;var_init},d) ->
    begin
    indent d; say "VarDec("; say(Symbol.name var_name); say ",";
    say(bool_to_string (!var_escape)); say ",";
    match var_ty with
    | None -> say "None"
    | Some(s) -> (say "Some("; say(Symbol.name s); say ")");
    sayln ","; exp(var_init,d+1); say ")"
    end
  | (A.TypeDec(l),d) ->
    let tdec ((td : A.tydec), d) = (indent d; say "("; say(Symbol.name td.ty_name); sayln ","; ty(td.ty_ty,d+1); say ")") in
    (indent d; say "TypeDec["; dolist d tdec l; say "]")

and ty = function
  | (A.NameTy(s),d) -> (indent d; say "NameTy("; say(Symbol.name s); say ")")
  | (A.RecordTy(l),d) ->
    let f ((fd : A.field),d) = (indent d; say "("; say(Symbol.name fd.field_name); say ",";
                                say(Symbol.name fd.field_ty); say ")")
    in (indent d; say "RecordTy["; dolist d f l; say "]")
  | (A.ArrayTy(s),d) -> (indent d; say "ArrayTy("; say(Symbol.name s); say ")")
;;

let print e = (exp(e,0); sayln "")