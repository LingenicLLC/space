module Space.Compiler.GenRuntime

(** Driver to generate the C runtime header file
    Outputs Space.Compiler.CGen.Runtime.gen_runtime_header to stdout *)

open FStar.IO
open FStar.All
open Space.Compiler.CGen.Runtime

let main () : ML unit =
  print_string gen_runtime_header

let _ = main ()
