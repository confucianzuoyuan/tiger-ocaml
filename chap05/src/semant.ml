module A = Ast
module E = Env
module S = Symbol
module T = Types

type venv = Env.enventry Symbol.Table.t
;;

type tenv = T.ty Symbol.Table.t
;;

type expty = {
  exp: Translate.exp;  
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

let nest_depth = ref 0
let increment_nest_depth () = nest_depth := !nest_depth + 1
let decrement_nest_depth () = nest_depth := !nest_depth - 1
let get_nest_depth () = !nest_depth
let set_nest_depth n = nest_depth := n
let check_in_loop errmsg =
  if !nest_depth = 0
  then print_endline errmsg
  else ()
;;

let rec trans_exp (venv, tenv, exp) : expty =
  let rec trexp = function
    | A.VarExp var -> trvar var
    | A.NilExp -> {exp=(); ty=T.NIL}
    | A.IntExp(_) -> {exp=(); ty=T.INT}
    | A.StringExp(_) -> {exp=(); ty=T.STRING}
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
        match S.look(venv, func) with
        | Some(Env.FunEntry{formals;result}) -> (check_args(formals, args); {exp=(); ty=result})
        | Some(_) -> (print_endline ("symbol not function " ^ S.name func); {exp=(); ty=T.BOTTOM})
        | None -> (print_endline ("no such function " ^ S.name func); {exp=(); ty=T.BOTTOM})
      end
    | A.OpExp{left;oper;right} ->
      begin
      match oper with
      | A.PlusOp 
      | A.MinusOp
      | A.TimesOp
      | A.DivideOp -> (check_int(trexp left); check_int(trexp right); {exp=(); ty=T.INT})
      | A.EqOp
      | A.NeqOp -> (check_equality_op(trexp left, trexp right); {exp=(); ty=T.INT})
      | A.LtOp
      | A.LeOp
      | A.GtOp
      | A.GeOp -> (check_comparison_op(trexp left, trexp right); {exp=(); ty=T.INT})
      end
    | A.SeqExp(explist) ->
      let helper _ seq_exp = trexp seq_exp in
      let check_sequence sequence = List.fold_left helper {exp=(); ty=T.UNIT} sequence in
      check_sequence explist
    | A.AssignExp{var; exp} ->
      begin
      let rec get_var_symbol = function
        | A.SimpleVar(sym) -> S.look(venv, sym)
        | A.FieldVar(var',_) -> get_var_symbol(var')
        | A.SubscriptVar(var',_) -> get_var_symbol(var')
      in
      let can_assign var' = 
        match get_var_symbol(var') with
        | Some(Env.VarEntry{ty=_; read_only}) -> if read_only then (print_endline "error : index variable erroneously assigned to") else ()
        | _ -> print_endline "cannot assign to a function"
      in
      let _ = can_assign(var) in
      let _ = check_types_assignable((trvar var).ty, (trexp exp).ty, "error: mismatched types in assignment") in
      {exp=(); ty=T.UNIT}
      end
    | A.IfExp{test; then'; else'} ->
      begin
      let _ = check_types_equal((trexp test).ty, T.INT, "test in if exp does not evaluate to an int") in
      let _ = match else' with
      | Some(else_exp) ->
        begin
        match ((trexp then').ty, (trexp else_exp).ty) with
        | (T.RECORD(_), T.NIL) -> ()
        | (T.NIL, T.RECORD(_)) -> ()
        | (ty_a, ty_b) -> check_types_equal(ty_a, ty_b, "error: types of then - else differ")
        end
      | None -> check_types_equal((trexp then').ty, T.UNIT, "error: if-then returns non unit")
      in {exp=(); ty=((trexp then').ty)}
      end
    | A.BreakExp -> (check_in_loop("incorrect break"); {exp=(); ty=T.UNIT})
    | A.WhileExp{test;body} ->
      begin
      check_types_equal((trexp test).ty, T.INT, "test does not evaluate to an int");
      increment_nest_depth();
      check_types_equal((trexp body).ty, T.UNIT, "error: body of while not unit");
      decrement_nest_depth();
      {exp=(); ty=T.UNIT}
      end
    | A.LetExp{decs; body} ->
      let cur_depth = !nest_depth in
      let _ = set_nest_depth(0) in
      let (venv', tenv') = trans_dec(venv, tenv, decs) in
      let _ = set_nest_depth(cur_depth) in
      trans_exp(venv', tenv', body)
    | A.ForExp{var; escape=_; lo; hi; body} ->
      let
        venv' = S.enter(venv, var, Env.VarEntry{ty=T.INT; read_only=true})
      in
        check_types_equal((trexp lo).ty, T.INT, "error: lo expr is not int");
        check_types_equal((trexp hi).ty, T.INT, "error: hi expr is not int");
        increment_nest_depth();
        check_types_equal((trans_exp(venv', tenv, body)).ty, T.UNIT, "for body must be no value");
        decrement_nest_depth();
        {exp=(); ty=T.UNIT}
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
            then (print_endline ("record list is wrong length: " ^ S.name typ); {exp=(); ty=x})
            else (List.fold_left iterator () rec_formal; {exp=(); ty=x})
        | _ -> (print_endline ("error : expected record type, not: " ^ S.name typ); {exp=(); ty=T.NIL})
        end
      | None -> (print_endline ("error : invalid record type: " ^ S.name typ); {exp=(); ty=T.NIL})
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
          | T.ARRAY(ty, unique) -> (check_int(trexp size); check_types_equal((trexp init).ty, actual_ty(ty), "error: initializing exp and array type differ"); {exp=(); ty=T.ARRAY(ty, unique)})
          | _ -> (print_endline "no such array type in array creation"; {exp=(); ty=T.BOTTOM})
          end
        | None -> (print_endline "no such type"; {exp=(); ty=T.BOTTOM})
      end

  and trvar = function
    | A.SimpleVar(id) ->
      begin
      match S.look(venv, id) with
      | Some(Env.VarEntry{ty; read_only=_}) -> {exp=(); ty=ty}
      | Some(Env.FunEntry{formals=_; result}) -> {exp=(); ty=result}
      | None -> (print_endline ("error: undeclared variable " ^ S.name id); {exp=(); ty=T.BOTTOM})
      end
    | A.FieldVar(v, id) ->
      begin
      match trvar(v) with
      | {exp=(); ty=T.RECORD(rec_gen, _)} ->
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
          {exp=(); ty=get_field_type(fields, id)}
      | {exp=_; ty=_} -> (print_endline "error: variable not record"; {exp=(); ty=T.BOTTOM})
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
      | {exp=(); ty=T.ARRAY(arr_ty, _)} -> (check_int(trexp sub_exp); {exp=(); ty=actual_ty(arr_ty)})
      | {exp=_; ty=_} -> (print_endline "requires array"; {exp=(); ty=T.BOTTOM})
  in
    trexp exp

and trans_dec (venv, tenv, decs) : venv * tenv =
  let rec trdec (venv, tenv, dec) = match dec with
  | A.VarDec{var_name; var_escape=_; var_ty; var_init} ->
    begin
    match var_ty with
    | Some(symbol) ->
      begin
      let get_type = function
        | Some(ty) -> ty
        | None -> T.BOTTOM
      in
      let rec actual_ty = function
        | T.NAME(name, _) -> actual_ty(get_type(S.look(tenv, name)))
        | ty -> ty
      in
      match S.look(tenv, symbol) with
      | Some(ty) ->
        (check_types_assignable(actual_ty(ty), (trans_exp(venv, tenv, var_init)).ty, "error: mismatched types in vardec");
         (S.enter(venv, var_name, (Env.VarEntry{ty=actual_ty(ty); read_only=false})), tenv))
      | None -> (print_endline "type not recognized"; (venv, tenv))
      end
    | None ->
      let {exp=_; ty} = trans_exp(venv, tenv, var_init) in
      if T.eq(ty, T.NIL)
      then (print_endline "error: initializing nil expressions not constrained by record type")
      else ();
      (S.enter(venv, var_name, (Env.VarEntry{ty=ty; read_only=false})), tenv)
    end
  | A.TypeDec(tydeclist) ->
    let maketemptydec acc (tydec : A.tydec) = S.enter(acc, tydec.ty_name, T.BOTTOM) in
    let temp_tenv = List.fold_left maketemptydec tenv tydeclist in
    let foldtydec _ (tydec: A.tydec) = (venv, S.enter(tenv, tydec.ty_name, trans_ty(temp_tenv, tydec.ty_ty))) in
    let new_env = List.fold_left foldtydec (venv, tenv) tydeclist in
    let check_illegal_cycle () (tydec : A.tydec) =
      let rec check_helper (seen_list, name) =
        match S.look(snd new_env, name) with
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
      | Some(t) -> (field.field_name, t)
      | None -> (print_endline ("parameter type unrecognized: " ^ S.name field.field_ty); (field.field_name, T.BOTTOM))
    in
    let enter_funcs venv (fundec : A.fundec) = match fundec with
      | {fun_name; fun_params; fun_body=_; fun_result=Some(rt)} -> S.enter(venv, fun_name, Env.FunEntry{formals=List.map snd (List.map transparam fun_params); result=transrt(rt)})
      | {fun_name; fun_params; fun_body=_; fun_result=None} -> S.enter(venv, fun_name, Env.FunEntry{formals=List.map snd (List.map transparam fun_params); result=T.UNIT})
    in
    let venv' = List.fold_left enter_funcs venv fundeclist
    in
    let check_fun_dec (fundec : A.fundec) =
      let result_ty = match fundec.fun_result with
        | Some(rt) -> transrt(rt)
        | None -> T.UNIT
      in
      let params' = List.map transparam fundec.fun_params in
      let enter_param venv (name, ty) = S.enter(venv, name, Env.VarEntry{ty=ty; read_only=false}) in
      let venv'' = List.fold_left enter_param venv' params' in
      let body' = trans_exp(venv'', tenv, fundec.fun_body) in
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
      (venv', tenv)
  and folddec (venv, tenv) dec = trdec(venv, tenv, dec)

  in
    List.fold_left folddec (venv, tenv) decs

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

let trans_prog exp = trans_exp(Env.base_venv, Env.base_tenv, exp)

