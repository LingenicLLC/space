module Space.Compiler.GenUCD

(** Driver to generate the C UCD header file
    Outputs Space.Compiler.CGen.UCD.gen_ucd_header to stdout *)

open FStar.IO
open FStar.All
open Space.Compiler.CGen.UCD

let main () : ML unit =
  print_string gen_ucd_header

let _ = main ()
