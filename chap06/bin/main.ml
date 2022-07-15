open Interp

let _ =
  begin
  Translate.resetFragList();
  let fraglist = Main.compile() in (List.iter Main.print_frag fraglist)
  end