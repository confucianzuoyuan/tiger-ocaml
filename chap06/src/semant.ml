module A = Ast
module E = Env
module S = Symbol
module T = Types
module R = Translate

type venv = E.enventry Symbol.Table.t
;;

type tenv = T.ty S.Table.t
;;

type expty = {
  exp: R.exp;  
  ty: T.ty;
}
[@@deriving show]
;;

let check_int = function
  | {exp=_; ty=T.INT} -> ()
  | {exp=_; ty=_} -> print_endline "error: integer required"
;;

let check_equality_op = function
  | ({exp=_; ty=T.INT}, {exp=_; ty=T.INT})
  | ({exp=_; ty=T.STRING}, {exp=_; ty=T.STRING})
  | ({exp=_; ty=T.NIL}, {exp=_; ty=T.RECORD(_, _)})
  | ({exp=_; ty=T.RECORD(_, _)}, {exp=_; ty=T.NIL}) -> ()
  | ({exp=_; ty=T.RECORD(_, ref1)}, {exp=_; ty=T.RECORD(_, ref2)}) ->
    if ref1 = ref2
    then ()
    else print_endline "can't compare different record types"
  | ({exp=_; ty=T.ARRAY(_, ref1)}, {exp=_; ty=T.ARRAY(_, ref2)}) ->
    if ref1 = ref2
    then ()
    else print_endline "can't compare different array types"
  | ({exp=_; ty=_}, {exp=_; ty=_}) -> print_endline "error : comparison expected both int, string, record, or array"
;;

let check_comparison_op = function
  | ({exp=_; ty=T.INT}, {exp=_; ty=T.INT})
  | ({exp=_; ty=T.STRING}, {exp=_; ty=T.STRING}) -> ()
  | ({exp=_; ty=_ }, {exp=_; ty=_ }) -> print_endline "error: comparison of incompatible types"
;;

let check_types_equal (ty_a, ty_b, errmsg) = if T.eq(ty_a, ty_b) then () else print_endline errmsg 
;;

let check_types_assignable (var, value, errmsg) =
  if T.comp(var, value) = T.EQ || T.comp(var, value) = T.GT
  then ()
  else print_endline errmsg
;;

let loop_depth : int ref = ref 0
let increment_loop_depth () = loop_depth := !loop_depth + 1
let decrement_loop_depth () = loop_depth := !loop_depth - 1
let get_loop_depth () = !loop_depth
let set_loop_depth n = loop_depth := n
let check_in_loop errmsg =
  if !loop_depth = 0
  then print_endline errmsg
  else ()
;;

let rec trans_exp (venv, tenv, exp, (level : R.level), break) : expty =
  let rec trexp = function
    | A.VarExp var -> trvar var
    | A.NilExp -> {exp=R.nil; ty=T.NIL}
    | A.IntExp(intvalue) -> {exp=R.Ex(Ir.CONST intvalue); ty=T.INT}
    | A.StringExp(stringvalue) -> {exp=R.stringIR(stringvalue); ty=T.STRING}
    | A.CallExp{func;args} ->
      begin
      let rec check_args = function
        | (for_ty::formal_list, arg_exp::arg_list) ->
          if T.eq(for_ty, ((trexp arg_exp).ty))
          then check_args(formal_list, arg_list)
          else print_endline "error: formals and actuals have different types"
        | ([], _::_) -> print_endline "error: formals are fewer than actuals"
        | (_::_, []) -> print_endline "error: formals are more than actuals"
        | ([], []) -> ()
      in
      let make_arglist acc a = ((trexp a).exp)::acc in
      let arg_explist = List.fold_left make_arglist [] args
      in
        match S.look(venv, func) with
        | Some(Env.FunEntry{level=declevel;label;formals;result}) -> (check_args(formals, args); {exp=R.callexpIR(declevel, level, label, arg_explist); ty=result})
        | Some(_) -> (print_endline ("symbol not function " ^ S.name func); {exp=R.Ex(Ir.CONST 0); ty=T.BOTTOM})
        | None -> (print_endline ("no such function " ^ S.name func); {exp=R.Ex(Ir.CONST 0); ty=T.BOTTOM})
      end
    | A.OpExp{left;oper;right} ->
      begin
      match oper with
      | A.PlusOp -> (check_int(trexp left); check_int(trexp right); {exp=R.binopIR(Ir.PLUS, (trexp left).exp, (trexp right).exp); ty=T.INT})
      | A.MinusOp -> (check_int(trexp left); check_int(trexp right); {exp=R.binopIR(Ir.MINUS, (trexp left).exp, (trexp right).exp); ty=T.INT})
      | A.TimesOp -> (check_int(trexp left); check_int(trexp right); {exp=R.binopIR(Ir.MUL, (trexp left).exp, (trexp right).exp); ty=T.INT})
      | A.DivideOp -> (check_int(trexp left); check_int(trexp right); {exp=R.binopIR(Ir.DIV, (trexp left).exp, (trexp right).exp); ty=T.INT})
      | A.EqOp -> (check_equality_op(trexp left, trexp right);
                   let {exp;ty} = trexp left in
                   {exp=R.relopIR(Ir.EQ, exp, (trexp right).exp, ty); ty=T.INT})
      | A.NeqOp -> (check_equality_op(trexp left, trexp right);
                   let {exp;ty} = trexp left in
                   {exp=R.relopIR(Ir.NE, exp, (trexp right).exp, ty); ty=T.INT})
      | A.LtOp -> (check_equality_op(trexp left, trexp right);
                   let {exp;ty} = trexp left in
                   {exp=R.relopIR(Ir.LT, exp, (trexp right).exp, ty); ty=T.INT})
      | A.LeOp -> (check_equality_op(trexp left, trexp right);
                   let {exp;ty} = trexp left in
                   {exp=R.relopIR(Ir.LE, exp, (trexp right).exp, ty); ty=T.INT})
      | A.GtOp -> (check_equality_op(trexp left, trexp right);
                   let {exp;ty} = trexp left in
                   {exp=R.relopIR(Ir.GT, exp, (trexp right).exp, ty); ty=T.INT})
      | A.GeOp -> (check_equality_op(trexp left, trexp right);
                   let {exp;ty} = trexp left in
                   {exp=R.relopIR(Ir.GE, exp, (trexp right).exp, ty); ty=T.INT})
      end
    | A.SeqExp(explist) ->
      let helper acc seqExp =
        let {exp;ty} = trexp seqExp in ((fst acc) @ [(R.unNx exp)], ty)
      in
        let checkSequence sequence = List.fold_left helper ([], T.UNIT) sequence
      in
        {exp=R.Nx(Ir.SEQ(fst (checkSequence explist))); ty=snd (checkSequence explist)}
    | A.AssignExp{var; exp} ->
      begin
      let rec get_var_symbol = function
        | A.SimpleVar(sym) -> S.look(venv, sym)
        | A.FieldVar(var',_) -> get_var_symbol(var')
        | A.SubscriptVar(var',_) -> get_var_symbol(var')
      in
      let can_assign var' = 
        match get_var_symbol(var') with
        | Some(Env.VarEntry{access=_; ty=_; read_only}) ->
          if read_only
          then (print_endline "error : index variable erroneously assigned to")
          else ()
        | _ -> print_endline "cannot assign to a function"
      in
      let _ = can_assign(var) in
      let _ = check_types_assignable((trvar var).ty, (trexp exp).ty, "error: mismatched types in assignment") in
      {exp=R.assignIR((trvar var).exp, (trexp exp).exp); ty=T.UNIT}
      end
    | A.IfExp{test; then'; else'} ->
      begin
      let _ = check_types_equal((trexp test).ty, T.INT, "test in if exp does not evaluate to an int") in
      match else' with
      | Some(else_exp) ->
        begin
        match ((trexp then').ty, (trexp else_exp).ty) with
        | (T.RECORD(_), T.NIL) -> {exp=R.ifIR((trexp test).exp, (trexp then').exp, (trexp else_exp).exp); ty=((trexp then').ty)}
        | (T.NIL, T.RECORD(_)) -> {exp=R.ifIR((trexp test).exp, (trexp then').exp, (trexp else_exp).exp); ty=((trexp else_exp).ty)}
        | (ty_a, ty_b) -> (check_types_equal(ty_a, ty_b, "error: types of then - else differ");
                           {exp=R.ifIR((trexp test).exp, (trexp then').exp, (trexp else_exp).exp); ty=((trexp then').ty)})
        end
      | None -> (check_types_equal((trexp then').ty, T.UNIT, "error: if-then returns non unit");
                 {exp=R.Ex(Ir.CONST 0); ty=((trexp then').ty)})
      end
    | A.BreakExp -> (check_in_loop("incorrect break"); {exp=R.breakIR(break); ty=T.UNIT})
    | A.WhileExp{test;body} ->
      begin
      check_types_equal((trexp test).ty, T.INT, "test does not evaluate to an int");
      increment_loop_depth();
      check_types_equal((trexp body).ty, T.UNIT, "error: body of while not unit");
      let breakpoint = Temp.newlabel() in
      let answer = {
        exp=R.whileIR(
          (trans_exp(venv, tenv, test, level, break)).exp,
          (trans_exp(venv, tenv, body, level, breakpoint)).exp,
          breakpoint);
        ty=T.UNIT;
      } in
      (decrement_loop_depth(); answer)
      end
    | A.LetExp{decs; body} ->
      let cur_depth = !loop_depth in
      let _ = set_loop_depth(0) in
      let (venv', tenv', explist) = trans_dec(venv, tenv, decs, level, break) in
      let _ = set_loop_depth(cur_depth) in
      let body_expty = trans_exp(venv', tenv', body, level, break) in
      let newbody = R.concatExpList(explist, body_expty.exp) in
      {exp=newbody; ty=body_expty.ty}
    | A.ForExp{var; escape; lo; hi; body} ->
      let
        venv' = S.enter(venv, var, Env.VarEntry{access=R.alloclocal(level, true); ty=T.INT; read_only=true})
      in
      let
        breakpoint = Temp.newlabel()
      in
      let _ = check_types_equal((trexp lo).ty, T.INT, "error: lo exp is not int") in
      let _ = check_types_equal((trexp hi).ty, T.INT, "error: hi exp is not int") in
      let {exp=bodyexp; ty=bodytype} = (increment_loop_depth(); trans_exp(venv', tenv, body, level, breakpoint))
      in
      let _ = decrement_loop_depth() in
      let _ = check_types_equal(bodytype, T.UNIT, "for body must be no value") in
        begin
        match S.look(venv', var) with
        | Some(x) ->
          begin
          match x with
          | Env.VarEntry{access; ty=_; read_only=_} -> {
            exp=R.forIR(R.simpleVarIR(access, level), escape, (trexp lo).exp, (trexp hi).exp, bodyexp, breakpoint);
            ty=T.UNIT
          }
          | _ -> (print_endline "compiler bug: forexp var isn't VarEntry"; {exp=R.Ex(Ir.CONST 0); ty=T.UNIT})
          end
        | _ -> (print_endline "cannot find forexp var"; {exp=R.Ex(Ir.CONST 0); ty=T.UNIT})
        end
    | A.RecordExp{fields; typ} ->
      begin
      match S.look(tenv, typ) with
      | Some(x) ->
        begin
        match x with
        | T.RECORD(f, _) ->
          let rec_formal = f() in
          let rec get_field_type = function
            | (_, []) -> T.BOTTOM
            | (name, (sym, exp)::l) -> if name = S.name sym then (trexp exp).ty else get_field_type(name, l)
          in
          let check_formal (sym, ty) =
            if not (T.leq(get_field_type(S.name sym, fields), ty))
            then print_endline ("actual type doesn't match formal type: " ^ S.name sym)
            else ()
          in
          let iterator () (fieldname, typeid) =
            match S.look(tenv, typeid) with
            | Some(x) -> (check_formal(fieldname, x); ())
            | None -> (print_endline ("unknown type in record: " ^ S.name typ); ())
          in
            if List.length(rec_formal) <> List.length(fields)
            then (print_endline ("record list is wrong length: " ^ S.name typ); {exp=R.Ex(Ir.CONST 0); ty=x})
            else (List.fold_left iterator () rec_formal; {exp=R.Ex(Ir.CONST 0); ty=x}) (** todo *)
        | _ -> (print_endline ("error : expected record type, not: " ^ S.name typ); {exp=R.Ex(Ir.CONST 0); ty=T.NIL})
        end
      | None -> (print_endline ("error : invalid record type: " ^ S.name typ); {exp=R.Ex(Ir.CONST 0); ty=T.NIL})
      end
    | A.ArrayExp{typ; size; init} ->
      begin
        let get_type = function
          | Some(ty) -> ty
          | None -> T.BOTTOM
        in
        let rec actual_ty = function
          | T.NAME(name, _) -> actual_ty(get_type(S.look(tenv, name)))
          | ty -> ty
        in
        match S.look(tenv, typ) with
        | Some(x) ->
          begin
          match actual_ty(x) with
          | T.ARRAY(ty, unique) -> (check_int(trexp size);
                                    check_types_equal((trexp init).ty, actual_ty(ty), "error: initializing exp and array type differ");
                                    {exp=R.arrayIR((trexp size).exp, (trexp init).exp); ty=T.ARRAY(ty, unique)})
          | _ -> (print_endline "no such array type in array creation"; {exp=R.Ex(Ir.CONST 0); ty=T.BOTTOM})
          end
        | None -> (print_endline "no such type"; {exp=R.Ex(Ir.CONST 0); ty=T.BOTTOM})
      end

  and trvar = function
    | A.SimpleVar(id) ->
      begin
      match S.look(venv, id) with
      | Some(Env.VarEntry{access; ty; read_only=_}) -> {exp=R.simpleVarIR(access, level); ty=ty}
      | _ -> (print_endline ("error: undeclared variable " ^ S.name id); {exp=R.Ex(Ir.CONST 0); ty=T.BOTTOM})
      end
    | A.FieldVar(v, id) ->
      begin
      match trvar(v) with
      | {exp=_; ty=T.RECORD(rec_gen, _)} ->
        let fields = rec_gen() in
        let rec get_field_type = function
        | ((f_symbol, f_ty)::l, id) ->
          if (S.name f_symbol) = (S.name id)
          then
            match S.look(tenv, f_ty) with
            | Some(ty) -> ty
            | None -> (print_endline "type error in record"; T.BOTTOM)
          else get_field_type(l, id)
        | ([], _) -> (print_endline "no such field"; T.BOTTOM)
        in
        let rec get_index = function
          | ([], _, cur) -> cur
          | (h::t, id, cur) -> if h = id then cur else get_index(t, id, cur+1)
        in
          {exp=R.fieldIR((trvar v).exp, get_index(List.map fst fields, id, 0)); ty=get_field_type(fields, id)}
      | {exp=_; ty=_} -> (print_endline "error: variable not record"; {exp=R.Ex(Ir.CONST 0); ty=T.BOTTOM})
      end
    | A.SubscriptVar(v, sub_exp) ->
      let get_type = function
        | Some(ty) -> ty
        | None -> T.BOTTOM
      in
      let rec actual_ty = function
        | T.NAME(name, _) -> actual_ty(get_type(S.look(tenv, name)))
        | ty -> ty
      in
      match trvar(v) with
      | {exp=_; ty=T.ARRAY(arr_ty, _)} -> (check_int(trexp sub_exp); {exp=R.subscriptIR((trvar v).exp, (trexp sub_exp).exp); ty=actual_ty(arr_ty)})
      | {exp=_; ty=_} -> (print_endline "requires array"; {exp=R.Ex(Ir.CONST 0); ty=T.BOTTOM})
  in
    trexp exp

and trans_dec (venv, tenv, decs, level, break) : venv * tenv * R.exp list =
  let rec trdec (venv, tenv, dec, explist) = match dec with
  | A.VarDec{var_name; var_escape; var_ty; var_init} ->
    let get_type = function
      | Some(ty) -> ty
      | None -> T.BOTTOM
    in
    let rec actual_ty = function
      | T.NAME(name, _) -> actual_ty(get_type(S.look(tenv, name)))
      | ty -> ty
    in
    let access = R.alloclocal(level, !var_escape) in
    let create_assignexp () =
      let left = R.simpleVarIR(access, level) in
      let right = (trans_exp(venv, tenv, var_init, level, break)).exp in
      R.assignIR(left, right)
    in
    begin
    match var_ty with
    | Some(symbol) ->
      begin
      match S.look(tenv, symbol) with
      | Some(ty) -> (check_types_assignable(actual_ty(ty), (trans_exp(venv, tenv, var_init, level, break)).ty, "error: mismatched types in vardec");
                     (S.enter(venv, var_name, (Env.VarEntry{access=access; ty=actual_ty(ty); read_only=false})), tenv, create_assignexp()::explist))
      | None -> (print_endline "type not recognized"; (venv, tenv, create_assignexp()::explist))
      end
    | None ->
      let {exp=_; ty} = trans_exp(venv, tenv, var_init, level, break) in
      if T.eq(ty, T.NIL)
      then (print_endline "error: initializing nil expressions not constrained by record type")
      else ();
      (S.enter(venv, var_name, (Env.VarEntry{access=access; ty=ty; read_only=false})), tenv, create_assignexp()::explist)
    end
  | A.TypeDec(tydeclist) ->
    let maketemptydec acc (tydec : A.tydec) = S.enter(acc, tydec.ty_name, T.BOTTOM) in
    let temp_tenv = List.fold_left maketemptydec tenv tydeclist in
    let foldtydec _ (tydec: A.tydec) = (venv, S.enter(tenv, tydec.ty_name, trans_ty(temp_tenv, tydec.ty_ty)), explist) in
    let new_env = List.fold_left foldtydec (venv, tenv, explist) tydeclist in
    let check_illegal_cycle () (tydec : A.tydec) =
      let rec check_helper (seen_list, name) =
        let second (_,x,_) = x in
        match S.look(second new_env, name) with
        | Some(T.NAME(symb, _)) ->
          (match List.find_opt (fun y -> S.name y = S.name symb) seen_list with
          | Some(_) -> (print_endline "error: mutually recursive types thet do not pass through record or array - cycle")
          | _ -> check_helper(name::seen_list, symb))
        | _ -> ()
      in
        check_helper([], tydec.ty_name)
    in
    let check_duplicates seen_list (tydec : A.tydec) =
      (match List.find_opt (fun y -> y = S.name tydec.ty_name) seen_list with
      | Some(_) -> (print_endline "error : two types of same name in mutually recursive tydec"; seen_list)
      | _ -> (S.name tydec.ty_name)::seen_list)
    in
      let _ = List.fold_left check_duplicates [] tydeclist in
      let _ = List.fold_left check_illegal_cycle () tydeclist in
      new_env
  | A.FunctionDec(fundeclist) ->
    let transrt rt =
      match S.look(tenv, rt) with
      | Some(rt') -> rt'
      | None -> (print_endline ("return type unrecognized: " ^ S.name rt); T.BOTTOM)
    in
    let transparam (field : A.field) =
      match S.look(tenv, field.field_ty) with
      | Some(t) -> (field.field_name, field.field_escape, t)
      | None -> (print_endline ("parameter type unrecognized: " ^ S.name field.field_ty); (field.field_name, field.field_escape, T.BOTTOM))
    in
    let enter_funcs venv (fundec : A.fundec) =
      let newlabel = Temp.newlabel() in
      let get_escape (field : A.field) = !(field.field_escape) in
      let get_escape_list params = List.map get_escape params in
      match fundec with
      | {fun_name; fun_params; fun_body=_; fun_result=Some(rt)} ->
        S.enter(venv,
                fun_name,
                E.FunEntry{
                  level=R.newlevel(level, newlabel, get_escape_list fun_params);
                  label=newlabel;
                  formals=(let thd (_,_,x) = x in (List.map thd (List.map transparam fun_params)));
                  result=transrt(rt);
                })
      | {fun_name; fun_params; fun_body=_; fun_result=None} ->
        S.enter(venv,
                fun_name,
                E.FunEntry{
                  level=R.newlevel(level, newlabel, get_escape_list fun_params);
                  label=newlabel;
                  formals=(let thd (_,_,x) = x in (List.map thd (List.map transparam fun_params)));
                  result=T.UNIT;
                })
    in
    let venv' = List.fold_left enter_funcs venv fundeclist
    in
    let check_fun_dec (fundec : A.fundec) =
      let newlevel = match S.look(venv', fundec.fun_name) with
        | Some(Env.FunEntry{level=level'; label=_; formals=_; result=_}) -> level'
        | _ -> R.newlevel(R.outermost, Temp.newlabel(), [])
      in
      let result_ty = match fundec.fun_result with
        | Some(rt) -> transrt(rt)
        | None -> T.UNIT
      in
      let params' = List.map transparam fundec.fun_params in
      let allocated_formals = R.formals(newlevel) in
      let enter_param (venv, cur_index) (name, _, ty) =
        (S.enter(
          venv,
          name,
          E.VarEntry{
            access=List.nth allocated_formals cur_index;
            ty=ty;
            read_only=false;
          }
        ), cur_index + 1)
      in
      let venv'' = fst (List.fold_left enter_param (venv',1) params') in
      let body' = trans_exp(venv'', tenv, fundec.fun_body, newlevel, break) in
        R.proc_entry_exit(newlevel, body'.exp);
        if not (T.eq(body'.ty, result_ty))
        then (print_endline ("Function body type doesn't match return type in function " ^ S.name fundec.fun_name))
        else ()
    in
    let fold_fun_dec () fundec = check_fun_dec fundec in
    let check_duplicates seen_list (fundec : A.fundec) =
      match List.find_opt (fun y -> S.name fundec.fun_name = y) seen_list with
      | Some(_) -> (print_endline "error : two types of same name in mutually recursive fundec"; seen_list)
      | _ -> (S.name fundec.fun_name)::seen_list
    in
      let _ = List.fold_left check_duplicates [] fundeclist in
      let _ = List.fold_left fold_fun_dec () fundeclist in
      (venv', tenv, explist)
  and folddec (venv, tenv, explist) dec = trdec(venv, tenv, dec, explist)
  in
    List.fold_left folddec (venv, tenv, []) decs

and trans_ty (tenv, ty) : T.ty =
  let trty (tenv, ty') = match ty' with
    | A.NameTy(name) ->
      begin
      match S.look(tenv, name) with
      | Some(_) -> T.NAME(name, ref None)
      | None -> (print_endline ("unrecognized name type: " ^ S.name name); T.NAME(name, ref None))
      end
    | A.RecordTy(fields) ->
      let field_process (field : A.field) =
        match S.look(tenv, field.field_ty) with
        | Some(_) -> (field.field_name, field.field_ty)
        | None -> (print_endline ("undefined type in rec: " ^ S.name field.field_ty); (field.field_name, field.field_ty))
      in
      let list_concat acc a = field_process(a)::acc
      in
      let rec_gen () = List.fold_left list_concat [] fields
      in
        let _ = rec_gen() in
        T.RECORD(rec_gen, ref ())
    | A.ArrayTy(sym) -> T.ARRAY(trans_ty(tenv, A.NameTy(sym)), ref ())
  in
    trty(tenv, ty)
;;

let trans_prog exp =
  let mainlabel = Temp.newlabel() in
  let mainlevel = Translate.newlevel(Translate.outermost, mainlabel, []) in
  let _ = Findescape.find_escape exp in
  let mainexp = (trans_exp(Env.base_venv, Env.base_tenv, exp, mainlevel, mainlabel)).exp in
  let _ = R.proc_entry_exit(mainlevel, mainexp) in
  R.getResult()
;;