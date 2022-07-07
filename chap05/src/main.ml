let parse () =
  let cin =
    if Array.length Sys.argv > 1
    then open_in Sys.argv.(1)
    else stdin
  in
    let lexbuf = Lexing.from_channel cin in
    let ast = Parser.program Lexer.read lexbuf in
    ast

let semant () =
  let ast = parse () in
  Semant.transExp Env.base_venv Env.base_tenv ast