[@@@warning "-8"]

module S = Symbol
module A = Ast

type depth = int

let rec tranverseVar escape_env depth s =
  match s with
  | A.SimpleVar sym -> (match S.look sym escape_env with Some(d',esc) -> if depth > d' then esc := true else ())
  | A.FieldVar(var,_)
  | A.SubscriptVar(var,_) -> tranverseVar escape_env depth var

and tranverseExp escape_env depth s =
  match s with
  | A.VarExp var -> tranverseVar escape_env depth var
  | A.NilExp -> ()
  | A.IntExp _ -> ()
  | A.StringExp _ -> ()
  | A.CallExp{args;_} ->
    List.fold_left
    (fun _ arg -> tranverseExp escape_env depth arg)
    ()
    args
  | A.OpExp{left;right;_} ->
    let _ = tranverseExp escape_env depth left in
    let _ = tranverseExp escape_env depth right in
    ()
  | A.RecordExp{fields;_} ->
    List.fold_left
    (fun _ (_,exp) -> tranverseExp escape_env depth exp)
    ()
    fields
  | A.SeqExp exps ->
    List.fold_left
    (fun _ exp -> tranverseExp escape_env depth exp)
    ()
    exps
  | A.AssignExp{var;exp} ->
    let _ = tranverseVar escape_env depth var in
    let _ = tranverseExp escape_env depth exp in
    ()
  | A.IfExp{test;then';else'} ->
    let _ = tranverseExp escape_env depth test in
    let _ = tranverseExp escape_env depth then' in
    begin
    match else' with
    | Some else'' -> tranverseExp escape_env depth else''
    | None -> ()
    end
  | A.WhileExp{test;body} -> let _ = tranverseExp escape_env depth test in tranverseExp escape_env depth body
  | A.ForExp{var;escape;lo;hi;body} ->
    let new_escape_env = S.enter var (depth,escape) escape_env in
    let _ = escape := false in
    let _ = tranverseExp escape_env depth lo in
    let _ = tranverseExp escape_env depth hi in
    tranverseExp new_escape_env depth body
  | A.BreakExp -> ()
  | A.LetExp{decs;body} ->
    let new_escape_env = tranverseDecs escape_env depth decs in
    tranverseExp new_escape_env depth body
  | A.ArrayExp{size;init;_} ->
    let _ = tranverseExp escape_env depth size in
    tranverseExp escape_env depth init

and tranverseDecs escape_env depth s =
  let do_dec escape_env dec =
    match dec with
    | A.VarDec vardec ->
      let _ = vardec.var_escape := false in let _ = tranverseExp escape_env depth vardec.var_init in
      S.enter vardec.var_name (depth,vardec.var_escape) escape_env
    | A.TypeDec _ -> escape_env
    | A.FunctionDec fundecs ->
      let helper acc (fd : A.field) =
        let _ = fd.field_escape := false in S.enter fd.field_name (depth+1,fd.field_escape) acc
      in
      List.fold_left
      (fun acc (f : A.fundec) -> let new_escape_env = List.fold_left helper acc f.fun_params in let _ = tranverseExp new_escape_env (depth+1) f.fun_body in acc)
      escape_env
      fundecs
  in
  List.fold_left
  do_dec
  escape_env
  s

let find_escape prog = let tb = S.empty in tranverseExp tb 0 prog