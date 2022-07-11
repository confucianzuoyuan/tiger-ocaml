type register = string
[@@deriving show]

type access =
  | InFrame of int
  | InReg of Temps.temp
[@@deriving show]

type frame = {
  name: Temps.label;
  formals: access list;
  pointer: int ref;
}
[@@deriving show]

let pointer_size = 8

let alloc_local escape frame =
  if escape
  then let _ = (frame.pointer := !(frame.pointer) - pointer_size) in InFrame(!(frame.pointer))
  else InReg(Temps.newtemp())