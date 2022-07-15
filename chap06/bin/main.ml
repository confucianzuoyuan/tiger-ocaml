open Interp
open Main

let _ = List.map print_endline (List.map Frame.show_frag (semant ()))