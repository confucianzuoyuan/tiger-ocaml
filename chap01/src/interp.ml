open Program

(*
  key: string
  value: int
*)
module Table = Map.Make(String)
;;

let empty = Table.empty
;;

let enter = Table.add
;;

let look k t =
  try Some (Table.find k t)
  with Not_found -> None
;;

let interp_op = function
  | Plus -> ( + )
  | Minus -> ( - )
  | Times -> ( * )
  | Div -> ( / )
;;

let rec print_exps exps table =
  match exps with
  | [] -> let _ = print_endline "" in table
  | (e::es) -> let (v,table') = interp_exp e table in
               let _ = print_int v in
               let _ = print_string " " in
               print_exps es table'

and interp_stm stm table =
  match stm with
  | CompoundStm(stm1,stm2) -> let table' = interp_stm stm1 table in
                              let table'' = interp_stm stm2 table' in
                              table''
  | AssignStm(id,exp) -> let (v,table') = interp_exp exp table in
                         enter id v table'
  | PrintStm exps -> print_exps exps table

and interp_exp exp table =
  match exp with
  | NumExp v -> (v,table)
  | IdExp id ->
    begin
    match look id table with
    | Some v -> (v,table)
    | None -> failwith "id not exist."
    end
  | OpExp(op,exp1,exp2) -> let (v1,table') = interp_exp exp1 table in
                           let (v2,table'')= interp_exp exp2 table' in
                           (((interp_op op) v1 v2),table'')
  | EseqExp(stm,exp) -> let table' = interp_stm stm table in interp_exp exp table'
;;

let interp stm = interp_stm stm empty
;;

let main () = interp prog
;;