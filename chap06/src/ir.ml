type label = Temp.label
[@@deriving show]

type stm =
  | SEQ of stm list
  | LABEL of label
  | JUMP of exp * label list
  | CJUMP of relop * exp * exp * label * label
  | MOVE of loc * exp
  | EXP of exp

and exp =
  | BINOP of binop * exp * exp
  | MEM of exp
  | TEMP of Temp.temp
  | ESEQ of stm * exp
  | NAME of label
  | CONST of int
  | CALL of exp * exp list
  | TODO

and loc =
  | TEMPLOC of Temp.temp
  | MEMLOC of exp
  | ESEQLOC of stm * exp

and binop =
  | PLUS
  | MINUS
  | MUL
  | DIV
  | AND
  | OR
  | LSHIFT
  | RSHIFT
  | ARSHIFT
  | XOR

and relop =
  | EQ
  | NE
  | LT
  | GT
  | LE
  | GE
  | ULT
  | ULE
  | UGT
  | UGE
[@@deriving show]

(** todo *)
let not_rel = fun _ -> EQ
;;

(** todo *)
let commute = fun _ -> EQ
;;