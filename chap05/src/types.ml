type unique = unit ref
[@@deriving show]

type ty =
  | INT
  | STRING
  | RECORD of (unit -> (Symbol.symbol * Symbol.symbol) list) * unique (** (fieldname, typeid) *)
  | ARRAY of ty * unique
  | NIL
  | UNIT
  | BOTTOM
  | NAME of Symbol.symbol * ty option ref
[@@deriving show]

type comp =
  | LT
  | GT
  | EQ
  | INCOMP (** incomparable *)
[@@deriving show]

let leq = function
  | (BOTTOM,_)
  | (_,UNIT)
  | (NIL,RECORD(_))
  | (INT,INT)
  | (NIL,NIL)
  | (STRING,STRING) -> true
  | (RECORD(_, unique1), RECORD(_,unique2)) -> (unique1 = unique2)
  | (ARRAY(_, unique1), ARRAY(_,unique2)) -> (unique1 = unique2)
  | (NAME(sym1,_),NAME(sym2,_)) -> (Symbol.name sym1 = Symbol.name sym2)
  | (_,_) -> false

let comp (t1, t2) =
  if leq(t1, t2) && leq(t2, t1) 
  then EQ
  else if leq(t1, t2)
  then LT
  else if leq(t2, t1)
  then GT
  else INCOMP

let eq (t1, t2) = comp(t1, t2) = EQ

