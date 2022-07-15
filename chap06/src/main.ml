let parse () =
  let cin =
    if Array.length Sys.argv > 1
    then open_in Sys.argv.(1)
    else stdin
  in
    let lexbuf = Lexing.from_channel cin in
    let ast = Parser.program Lexer.read lexbuf in
    ast
;;

let print_frag = function
  | Frame.PROC{body;frame} -> (Frame.print_frame(frame); Printir.print(body); print_string "\n")
  | Frame.STRING(label,lit) -> print_string ("Label: " ^ (Symbol.name label) ^ " with string " ^ lit ^ "\n")
;;

let compile () =
  let ast = parse () in let _ = Printast.print ast in
  (Findescape.find_escape ast; Semant.trans_prog ast)