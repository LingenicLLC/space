module Space.Compiler.CGen.Runtime

(** C runtime generation - DEPRECATED

    The C backend has been replaced by the SPARK/Ada implementation.
    See spark/ directory for the verified native implementation.

    This module is retained for backward compatibility but generates
    only a deprecation notice. *)

open Space.Compiler.CGen.C99

(** Re-export the stub *)
let gen_runtime_header = Space.Compiler.CGen.C99.gen_runtime_header
