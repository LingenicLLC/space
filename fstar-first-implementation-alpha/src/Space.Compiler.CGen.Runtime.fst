module Space.Compiler.CGen.Runtime

(** Backward compatibility wrapper
    Re-exports C99 backend as the default runtime generator *)

open Space.Compiler.CGen.C99

(** Re-export the C99 runtime header generator *)
let gen_runtime_header = Space.Compiler.CGen.C99.gen_runtime_header
