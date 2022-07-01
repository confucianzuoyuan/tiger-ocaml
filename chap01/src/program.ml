type id = string

type binop = Plus | Minus | Times | Div

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of binop * exp * exp
  | EseqExp of stm * exp

(* a := 5 + 3 *)
let stm1 = AssignStm(
  "a",
  OpExp(
    Plus,
    NumExp 5,
    NumExp 3
  )
)

(* print (a,a-1) *)
let stm2 = PrintStm [
  IdExp "a";
  OpExp(
    Minus,
    IdExp "a",
    NumExp 1
  )
]

(* 10 * a *)
let exp3 = OpExp(
  Times,
  NumExp 10,
  IdExp "a"
)

(* b := (print (a, a-1), 10 * a); *)
let stm4 = AssignStm(
  "b",
  EseqExp(
    stm2,
    exp3
  )
)

(* print (b) *)
let stm5 = PrintStm [
  IdExp "b"
]

(* a := 5 + 3; b := (print (a, a-1), 10 * a); print (b) *)
let prog = CompoundStm(
  stm1,
  CompoundStm(
    stm4,
    stm5
  )
)