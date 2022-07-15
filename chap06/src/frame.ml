type access =
  | InFrame of int
  | InReg of Temp.temp
[@@deriving show]

type frame = {
  name: Temp.label;
  formals: access list;
  num_locals: int ref;
  cur_offset: int ref;
}
[@@deriving show]

type frag =
  | PROC of {body: Ir.stm; frame: frame}
  | STRING of Temp.label * string
[@@deriving show]

let fp = Temp.newtemp()
;;

let rv = Temp.newtemp()
;;

let wordsize = 8
;;

let argregs = 6
;;

let name (frame : frame) = frame.name
;;

let formals (frame : frame) = frame.formals
;;

let newframe (name, formals) =
  let rec allocformals = function
    | (_, [], alloclist, _) -> alloclist
    | (offset, curformal::l, alloclist, numregs) ->
      begin
      match curformal with
      | true -> allocformals (offset+wordsize, l, (InFrame offset)::alloclist, numregs)
      | false ->
        if numregs < argregs
        then allocformals (offset, l, (InReg(Temp.newtemp()))::alloclist, numregs+1)
        else allocformals (offset+wordsize, l, (InFrame offset)::alloclist, numregs)
      end
  in
    {name=name; formals=allocformals(0, formals, [], 0); num_locals=ref 0; cur_offset=ref 0}
;;

let alloclocal frame escape =
  let increment_num_locals (frame' : frame) =
    (frame'.num_locals := !(frame'.num_locals) + 1)
  in
  let increment_offset (frame' : frame) =
    (frame'.cur_offset := !(frame'.cur_offset) - wordsize)
  in
  let get_offset_value (frame' : frame) =
    !(frame'.cur_offset)
  in
  let _ = increment_num_locals(frame)
  in
  match escape with
  | true -> (increment_offset frame; InFrame(get_offset_value frame))
  | false -> InReg(Temp.newtemp())
;;

let exp (frameaccess, frameaddr) =
  match frameaccess with
  | InFrame(offset) -> Ir.MEM(Ir.BINOP(Ir.PLUS, frameaddr, Ir.CONST(offset)))
  | InReg(temp) -> Ir.TEMP(temp)
;;

let external_call (s, args) = Ir.CALL(Ir.NAME(Temp.namedlabel s), args)
;;

let proc_entry_exit1 (_, stm) = stm
;;

let print_frame (frame : frame) =
  begin
  print_string ("FRAME with name = " ^ (Symbol.name frame.name) ^ "\n");
  print_string ("numlocals = " ^ (string_of_int (!(frame.num_locals))) ^ " cur_offset = " ^ (string_of_int (!(frame.cur_offset))) ^ "\n")
  end
;;

let print_access fraccess =
  match fraccess with
  | InFrame(offset) -> print_string ("inframe " ^ (string_of_int offset) ^ "\n")
  | _ -> print_string "temp\n"
;;

