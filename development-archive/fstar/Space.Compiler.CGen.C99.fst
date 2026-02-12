module Space.Compiler.CGen.C99

(** C99 backend stub

    The C backend has been deprecated. The SPARK/Ada implementation
    provides verified native code generation directly.

    See spark/ directory for the actively maintained implementation. *)

open FStar.String
open Space.Compiler.CGen.Common

(** Stub - returns deprecation notice *)
let gen_runtime_header : string =
"/* Space C99 Backend - DEPRECATED
 *
 * This backend has been replaced by the SPARK/Ada implementation.
 * See spark/ directory for the verified native implementation.
 *
 * The SPARK implementation provides:
 * - Direct native compilation via GNAT
 * - Formal verification via GNATprove contracts
 * - Universe isolation with discipline enforcement
 * - Complete borrow and warp primitives
 * - DO-178C certification pathway
 */

#error \"C99 backend deprecated. Use SPARK/Ada implementation instead.\"
"
