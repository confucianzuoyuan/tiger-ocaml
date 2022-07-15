let say s =  print_string s
;;
 
let sayln s = (say s; say "\n")
;; 

let rec indent = function
  | 0 -> ()
  | i -> (say " "; indent(i-1))
;;

let rec stm = function
  | (Ir.SEQ(stmlist),d) -> (indent d; sayln "SEQ("; List.fold_left (fun _ a -> (stm(a,d+1); sayln ","; ())) () stmlist; say ")")
  | (Ir.LABEL lab, d) -> (indent d; say "LABEL "; say (Symbol.name lab))
  | (Ir.JUMP (e,_), d) ->  (indent d; sayln "JUMP("; exp(e,d+1); say ")")
  | (Ir.CJUMP(r,a,b,t,f),d) -> (indent d; say "CJUMP("; relop r; sayln ","; exp(a,d+1); sayln ","; exp(b,d+1); sayln ","; indent(d+1); say(Symbol.name t); say ","; say (Symbol.name f); say ")")
  | (Ir.MOVE(a,b),d) -> (indent d; sayln "MOVE("; loc(a,d+1); sayln ","; exp(b,d+1); say ")")
  | (Ir.EXP e, d) -> (indent d; sayln "EXP("; exp(e,d+1); say ")")

and exp = function
  | (Ir.BINOP(p,a,b),d) -> (indent d; say "BINOP("; binop p; sayln ","; exp(a,d+1); sayln ","; exp(b,d+1); say ")")
  | (Ir.MEM(e),d) -> (indent d; sayln "MEM("; exp(e,d+1); say ")")
  | (Ir.TEMP t, d) -> (indent d; say "TEMP t"; say(string_of_int t))
  | (Ir.ESEQ(s,e),d) -> (indent d; sayln "ESEQ("; stm(s,d+1); sayln ","; exp(e,d+1); say ")")
  | (Ir.NAME lab, d) -> (indent d; say "NAME "; say (Symbol.name lab))
  | (Ir.CONST i, d) -> (indent d; say "CONST "; say(string_of_int i))
  | (Ir.CALL(e,el),d) -> (indent d; sayln "CALL("; exp(e,d+1); List.iter (fun a -> (sayln ","; exp(a,d+2))) el; say ")")
  | (Ir.TODO, d) -> (indent d; say "TODO")

and loc = function
  | (Ir.TEMPLOC t, d) -> (indent d; say "TEMP t"; say(string_of_int t))
  | (Ir.MEMLOC(e),d) -> (indent d; sayln "MEM("; exp(e,d+1); say ")")
  | (Ir.ESEQLOC(s,e),d) -> (indent d; sayln "ESEQ("; stm(s,d+1); sayln ","; exp(e,d+1); say ")")

and binop = function
  | Ir.PLUS -> say "PLUS"
  | Ir.MINUS -> say "MINUS"
  | Ir.MUL -> say "MUL"
  | Ir.DIV -> say "DIV"
  | Ir.AND -> say "AND"
  | Ir.OR -> say "OR"
  | Ir.LSHIFT -> say "LSHIFT"
  | Ir.RSHIFT -> say "RSHIFT"
  | Ir.ARSHIFT -> say "ARSHIFT"
  | Ir.XOR -> say "XOR"

and relop = function
  | Ir.EQ -> say "EQ"
  | Ir.NE -> say "NE"
  | Ir.LT -> say "LT"
  | Ir.GT -> say "GT"
  | Ir.LE -> say "LE"
  | Ir.GE -> say "GE"
  | Ir.ULT -> say "ULT"
  | Ir.ULE -> say "ULE"
  | Ir.UGT -> say "UGT"
  | Ir.UGE -> say "UGE"
;;

let print s = (stm(s,0); sayln "")
;;