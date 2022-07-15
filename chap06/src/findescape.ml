module S = Symbol
module A = Ast

type depth = int
[@@deriving show]

type escape_env = (depth * bool ref) S.Table.t

let rec traverse_var esc_env depth var =
  let rec trvar = function
    | A.SimpleVar(sym) ->
      begin match S.look(esc_env, sym) with
        | Some(depth', escape') -> if depth' < depth then escape' := true else ()
        | _ -> ()
      end
    | A.FieldVar(var', _) -> trvar var'
    | A.SubscriptVar(var', exp) -> (trvar var'; traverse_exp esc_env depth exp)
  in trvar var

and traverse_exp esc_env depth exp =
  let rec trexp = function
    | A.VarExp(var) -> traverse_var esc_env depth var
    | A.NilExp
    | A.IntExp(_)
    | A.StringExp(_) -> ()
    | A.CallExp{func=_; args} -> List.iter trexp args
    | A.OpExp{left;oper=_;right} -> (trexp left; trexp right)
    | A.RecordExp{fields; typ=_} -> let eval_field (_, exp) = trexp exp in List.iter eval_field fields
    | A.SeqExp(explist) -> let eval_exp exp = trexp exp in List.iter eval_exp explist
    | A.AssignExp{var; exp} -> (traverse_var esc_env depth var; trexp exp)
    | A.IfExp{test;then';else'} -> (trexp test; trexp then'; (match else' with Some(exp') -> trexp exp' | _ -> ()))
    | A.WhileExp{test;body} -> (trexp test; trexp body)
    | A.ForExp{var;escape;lo;hi;body} -> let env' = S.enter(esc_env, var, (depth, escape)) in (traverse_exp env' depth lo; traverse_exp env' depth hi; traverse_exp env' depth body)
    | A.BreakExp -> ()
    | A.LetExp{decs;body} -> let env' = traverse_decs esc_env depth decs in traverse_exp env' depth body
    | A.ArrayExp{typ=_;size;init} -> (trexp size; trexp init)
  in
    trexp exp

and traverse_decs esc_env depth dec =
  let rec trdec = function
    | A.FunctionDec(fundecs) ->
      let add_param_to_env acc (field : A.field) = S.enter(acc, field.field_name, (depth+1, field.field_escape)) in
      let eval_fundec (fundec : A.fundec) = let env' = List.fold_left add_param_to_env esc_env fundec.fun_params in (traverse_exp env' (depth+1) fundec.fun_body)
      in
        (List.iter eval_fundec fundecs; esc_env)
    | A.VarDec{var_name;var_escape;var_ty=_;var_init} -> let env' = S.enter(esc_env, var_name, (depth, var_escape)) in (traverse_exp env' depth var_init; env')
    | A.TypeDec(_) -> esc_env
  and fold_decs _ dec = trdec dec
  in
    List.fold_left fold_decs esc_env dec
;;

let find_escape prog = traverse_exp S.empty 0 prog
;;