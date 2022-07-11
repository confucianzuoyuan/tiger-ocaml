[@@@warning "-8"]

module A = Ast
module E = Env
module S = Symbol
module T = Types

type expty = ExpTy of { ty : T.ty }
[@@deriving show]

type optype = Arith | Comp | Eq
[@@deriving show]

let rec actual_ty ty =
  match ty with
  | T.NAME(_, tyref) ->
    begin
      match !tyref with
      | None -> T.NIL
      | Some t -> actual_ty t
    end
  | T.ARRAY(t, u) -> T.ARRAY(actual_ty t, u)
  | _ -> ty

let rec type2str ty =
  match ty with
  | T.NIL -> "nil"
  | T.UNIT -> "unit"
  | T.INT -> "int"
  | T.STRING -> "string"
  | T.ARRAY(t,_) -> "array of " ^ (type2str t)
  | T.NAME(sym,_) -> "name of " ^ (S.symbol_name sym)
  | T.RECORD(_,_) -> "record"

let type_mismatch e a = failwith ("type mismatch: expected " ^ e ^ ", actual " ^ a)

let check_type t1 t2 =
  let t1' = actual_ty t1 in
  if t1' <> t2
  then
    match (t1', t2) with
    | (T.RECORD(_,_), T.NIL) -> true
    | (T.NIL, T.RECORD(_,_)) -> true
    | _ -> type_mismatch (type2str t1') (type2str t2)
  else true

let rec checkdup = function
  | [] -> true
  | (name::ns) ->
    if List.for_all (fun name' -> name' <> name) ns
    then checkdup ns
    else failwith ("duplicated definition: " ^ name)

let check_int ty = match ty with
  | T.INT -> true
  | _ -> failwith ("integer required. but got type: " ^ (type2str ty))

let classify oper =
  match oper with
  | A.PlusOp -> Arith
  | A.MinusOp -> Arith
  | A.TimesOp -> Arith
  | A.DivideOp -> Arith
  | A.LtOp -> Comp
  | A.GtOp -> Comp
  | A.LeOp -> Comp
  | A.GeOp -> Comp
  | A.EqOp -> Eq
  | A.NeqOp -> Eq

let check_eq lty rty =
  match lty with
  | T.INT
  | T.STRING
  | T.ARRAY(_,_)
  | T.RECORD(_,_) -> check_type lty rty
  | _ -> failwith ("type error for equality operator: " ^ (type2str lty) ^ " ===> " ^ (type2str rty))

let check_arith lty rty = (check_int lty) && (check_int rty)

let check_comp lty rty =
  match lty with
  | T.INT
  | T.STRING -> check_type lty rty
  | _ -> failwith ("type error for comparison: " ^ (type2str lty) ^ " ===> " ^ (type2str rty))

let rec transExp venv tenv =
  let rec trexp e = match e with
    | A.NilExp -> ExpTy{ty=T.NIL}
    | A.IntExp _ -> ExpTy{ty=T.INT}
    | A.StringExp _ -> ExpTy{ty=T.STRING}
    | A.OpExp{left;oper;right} ->
      begin
      let ExpTy{ty=lty} = trexp left in
      let ExpTy{ty=rty} = trexp right in
      let check_result () =
        match classify oper with
        | Arith -> check_arith lty rty
        | Comp -> check_comp lty rty
        | Eq -> check_eq lty rty
      in
      if check_result ()
      then ExpTy{ty=T.INT}
      else failwith "must not reach"
      end
    | A.VarExp var -> trvar var
    | A.RecordExp{fields;typ} ->
      begin
      match S.look typ tenv with
      | Some ty ->
        begin
        match actual_ty ty with
        | T.RECORD(ftys_ty,u) ->
          let ftys_exp = List.map (fun (_,e) -> trexp e) fields in
          let checkrecord ftys_ty ftys_exp =
            let checker ((_,t1),ExpTy{ty=t2}) = check_type t1 t2 in
            let fs = List.combine ftys_ty ftys_exp in
            (List.length ftys_ty = List.length ftys_exp) && (List.for_all checker fs)
          in
            if checkrecord ftys_ty ftys_exp
            then ExpTy{ty=T.RECORD(ftys_ty,u)}
            else failwith "must not reach" 
        end
      | None -> failwith ("record type not found: " ^ (S.symbol_name typ))
      end
    | A.SeqExp exps ->
      let es = List.map trexp exps in
      let ty =
        if exps = []
        then T.UNIT
        else
          let rec last = function
          | [] -> failwith "no element"
          | [x] -> x
          | _::tl -> last tl
          in
          match last es with
          | ExpTy{ty=ty'} -> ty'
      in ExpTy{ty}
    | A.AssignExp{var;exp} ->
      let ExpTy{ty=vty} = trvar var in
      let ExpTy{ty=ety} = trexp exp in
      if check_type vty ety
      then ExpTy{ty=T.UNIT}
      else failwith "赋值表达式左边和右边的类型不相同。"
    | A.IfExp{test;then';else'} ->
      let ExpTy{ty=testty} = trexp test in
      let ExpTy{ty=thenty} = trexp then' in
      if check_type T.INT testty
      then
        match else' with
        | Some else'' -> let ExpTy{ty=elsety} = trexp else'' in if check_type thenty elsety then ExpTy{ty=thenty} else failwith "undefined2"
        | None -> if check_type T.UNIT thenty then ExpTy{ty=thenty} else failwith "then表达式的求值结果必须是unit。因为没有else表达式存在。"
      else failwith "if表达式的条件表达式求值结果不为int。"
    | A.WhileExp{test;body} ->
      let ExpTy{ty=testty} = trexp test in
      let ExpTy{ty=bodyty} = trexp body in
      if check_type T.INT testty && check_type T.UNIT bodyty
      then ExpTy{ty=T.UNIT}
      else failwith "while循环的测试表达式求值结果必须是int。"
    | A.BreakExp -> ExpTy{ty=T.UNIT}
    | A.LetExp{decs;body} ->
      let transdecs (venv,tenv) dec = transDec venv tenv dec in
      let (venv',tenv') = List.fold_left transdecs (venv,tenv) decs in
      let ExpTy{ty=bodyty} = transExp venv' tenv' body in
      ExpTy{ty=bodyty}
    | A.ArrayExp{typ;size;init} ->
      begin
      match S.look typ tenv with
      | None -> failwith ("type not found: " ^ (S.symbol_name typ))
      | Some t -> let ty = actual_ty t in
        begin
        match ty with
        | T.ARRAY(ty',_) ->
          let ExpTy{ty=sizety} = trexp size in
          let ExpTy{ty=initty} = trexp init in
          if (check_type T.INT sizety) && (check_type ty' initty)
          then ExpTy{ty=ty}
          else failwith "数组大小不是int，或者初始化表达式的类型和数组元素类型不一样。"
        end
      end
    | A.ForExp{var;lo;hi;body;_} ->
      (** translate to let/while expression *)
      let ivar = A.SimpleVar var in
      let limitvar = A.SimpleVar (S.symbol "limit") in
      let decs = [
        A.VarDec{var_name=var;var_escape=ref false;var_ty=None;var_init=lo};
        A.VarDec{var_name=(S.symbol "limit");var_escape=ref false;var_ty=None;var_init=hi};
      ] in
      let loop = A.WhileExp{
        test=A.OpExp{oper=A.LeOp;left=A.VarExp ivar;right=A.VarExp limitvar};
        body=A.SeqExp[body;A.AssignExp{var=ivar;exp=A.OpExp{oper=A.PlusOp;left=A.VarExp ivar;right=A.IntExp 1}}]
      } in
      trexp (A.LetExp{decs=decs;body=loop})
    | A.CallExp{func;args} ->
        match S.look func venv with
        | None -> failwith ("function not defined: " ^ (S.symbol_name func))
        | Some(E.VarEntry _) -> failwith ("not a function: " ^ (S.symbol_name func))
        | Some(E.FunEntry{formals;result}) ->
          let argtys = List.map trexp args in
          let checkformals formals argtys =
            let checker (t1,ExpTy{ty=t2}) = check_type t1 t2 in
            let ts = List.combine formals argtys in
            let szcheck = if (List.length formals) = (List.length argtys) then true else failwith "wrong number of arguments." in
            szcheck && (List.for_all checker ts)
            in
            if checkformals formals argtys then ExpTy{ty=actual_ty result} else failwith "函数调用时形参和实参类型不一样。"
  and trvar = function
    | A.SimpleVar sym ->
      begin
      match S.look sym venv with
      | Some E.VarEntry{ty=ty} -> ExpTy {ty=actual_ty ty}
      | Some _ -> failwith ("not a variable: " ^ (S.symbol_name sym))
      | _ -> failwith ("undefined variable: " ^ (S.symbol_name sym))
      end
    | A.FieldVar(var,id) ->
      begin
      let ExpTy{ty} = trvar var in
      match ty with
      | T.RECORD(fs,_) ->
        begin
        match List.assoc_opt id fs with
        | None -> failwith ("field not found: " ^ (S.symbol_name id))
        | Some ty' -> ExpTy{ty=actual_ty ty'}
        end
      | _ -> failwith ("not a record: " ^ (type2str ty))
      end
    | A.SubscriptVar(var,exp) ->
      let ExpTy{ty} = trvar var in
      begin
      match actual_ty ty with
      | T.ARRAY(ty',_) ->
        let ExpTy{ty=ty''} = trexp exp in
        begin
        match ty'' with
        | T.INT -> ExpTy{ty=ty'}
        | _ -> failwith ("array subscript type: " ^ (type2str ty''))
        end
      | _ -> failwith "not an array"
      end
  in trexp

and transTy tenv =
  let transty = function
  | A.NameTy sym ->
    begin
    match S.look sym tenv with
    | Some t -> t
    end
  | A.RecordTy fs ->
    let f (field : A.field) =
      match S.look field.field_ty tenv with
      | Some ty -> (field.field_name,ty)
      | _ -> failwith ("type not defined (field): " ^ (S.symbol_name field.field_ty))
    in
      if checkdup (List.map (fun (fd : A.field) -> (S.symbol_name fd.field_name)) fs)
      then T.RECORD(List.map f fs, ref ())
      else failwith "undefined8"
  | A.ArrayTy sym ->
    match S.look sym tenv with
    | Some ty -> T.ARRAY(ty,ref ())
    | None -> failwith ("type not defined (array): " ^ (S.symbol_name sym))
  in transty

and transDec venv tenv =
  let trdec = function
  | A.VarDec vardec ->
    let ExpTy{ty} = transExp venv tenv vardec.var_init in
    let ret name ty = (S.enter name (Env.VarEntry{ty}) venv, tenv) in
    begin
    match vardec.var_ty with
    | None ->
      if ty = T.NIL
      then failwith "nil can be used only in the long form."
      else ret vardec.var_name ty
    | Some sym ->
      begin
      match S.look sym tenv with
      | None -> failwith ("type not found: " ^ (S.symbol_name sym))
      | Some ty' ->
        if check_type ty' ty
        then ret vardec.var_name ty
        else failwith "undefined9"
      end
    end
  | A.TypeDec tydecs ->
    begin
    let
      tenv' =
      List.fold_left
      (fun acc (t : A.tydec) -> S.enter t.ty_name (T.NAME(t.ty_name,ref None)) acc)
      tenv
      tydecs
    in
      let tenv'' =
      List.fold_left
      (fun acc (t : A.tydec) ->
         match S.look t.ty_name acc with
         | Some (T.NAME(n,_)) -> S.enter n (T.NAME(n,ref (Some(transTy acc t.ty_ty)))) acc)
      tenv'
      tydecs
    in
      let names = List.map (fun (t : A.tydec) -> S.symbol_name t.ty_name) tydecs
    in
      let rec check_cyclic_dep (l : A.tydec list) = match l with
      | [] -> true
      | t::ts ->
        begin
        let rec chkcyc seen ty =
          match ty with
          | None -> failwith ("type not found: ")
          | Some ty' ->
            begin
            match ty' with
            | T.NAME(sym,ty'') ->
              if List.for_all (fun i -> i <> sym) seen
              then chkcyc (sym::seen) !ty''
              else false
            | _ -> true
            end
        in
        match S.look t.ty_name tenv'' with
        | Some (T.NAME(_,ty)) ->
            if chkcyc [t.ty_name] !ty
            then check_cyclic_dep ts
            else failwith ("cyclic dependency: ")
        end
    in
      if check_cyclic_dep tydecs && checkdup names
      then (venv, tenv'')
      else failwith "undefined10"
    end
  | A.FunctionDec fundecs ->
    let
      transfun venv (fundec : A.fundec) =
        let rty =
          match fundec.fun_result with
          | None -> T.UNIT
          | Some typ ->
            match S.look typ tenv with
            | Some t -> t
            | None -> failwith ("result type not found: " ^ (S.symbol_name typ))
        in
        let ftys =
          List.map
          (fun (fd : A.field) ->
             match S.look fd.field_ty tenv with
             | Some t -> t
             | None -> failwith ("type not found: " ^ (S.symbol_name fd.field_ty)))
          fundec.fun_params
        in
          if checkdup (List.map (fun (fd : A.field) -> S.symbol_name fd.field_name) fundec.fun_params)
          then S.enter fundec.fun_name (Env.FunEntry{formals=ftys;result=rty}) venv
          else failwith "undefined11"
    in
      let venv' = List.fold_left transfun venv fundecs
    in
      let transbody acc (fundec : A.fundec) =
        let Some(Env.FunEntry{result=rty;formals=formals}) = S.look fundec.fun_name venv' in
        let transparam acc ((field : A.field),ty) = S.enter field.field_name (Env.VarEntry{ty}) acc in
        let venv_loc = List.fold_left transparam venv' (List.combine fundec.fun_params formals) in
        let ExpTy{ty=bodyty} = transExp venv_loc tenv fundec.fun_body in
        check_type rty bodyty && acc
    in
      let check_bodies = List.fold_left transbody true fundecs
    in
      if check_bodies
      then (venv',tenv)
      else failwith "undefined12"
  in trdec