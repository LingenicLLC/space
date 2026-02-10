module Space.Instruction

(** Instruction set for Space interpreter *)

open FStar.UInt64
open Space.Types

(** Instruction pointer *)
type ip = UInt64.t

(** Primitive operations *)
type prim_op =
  (* Stack manipulation *)
  | PrimDup
  | PrimDrop
  | PrimSwap
  | PrimOver
  | PrimRot
  (* Arithmetic *)
  | PrimAdd
  | PrimSub
  | PrimMul
  | PrimDivU
  | PrimMod
  (* Bitwise *)
  | PrimAnd
  | PrimOr
  | PrimXor
  | PrimNot
  | PrimShl
  | PrimShr
  (* Comparison *)
  | PrimEq
  | PrimNeq
  | PrimLtU
  | PrimGtU
  (* Memory *)
  | PrimFetch
  | PrimStore
  | PrimAlloc
  (* System *)
  | PrimEmit
  | PrimKey
  | PrimHalt

(** Instructions *)
type instruction =
  | IPush of cell              (* Push literal value *)
  | ICall of ip                (* Call word at address *)
  | IReturn                    (* Return from word *)
  | IPrimitive of prim_op      (* Execute primitive *)
  | IBranch of ip              (* Unconditional jump *)
  | IBranchZero of ip          (* Jump if top = 0 *)
  | IBranchNonZero of ip       (* Jump if top != 0 *)
  | ICreateUniverse of universe_name * discipline  (* Create named universe *)
  | IEndUniverse of universe_name     (* End universe scope *)
  | IReleaseUniverse of universe_name (* Release affine universe *)
  | ITransferTo of universe_name      (* Transfer value to named universe *)

(** Decode primitive from opcode *)
let decode_prim (op: UInt64.t) : option prim_op =
  if op = 0uL then Some PrimDup
  else if op = 1uL then Some PrimDrop
  else if op = 2uL then Some PrimSwap
  else if op = 3uL then Some PrimOver
  else if op = 4uL then Some PrimRot
  else if op = 10uL then Some PrimAdd
  else if op = 11uL then Some PrimSub
  else if op = 12uL then Some PrimMul
  else if op = 13uL then Some PrimDivU
  else if op = 14uL then Some PrimMod
  else if op = 20uL then Some PrimAnd
  else if op = 21uL then Some PrimOr
  else if op = 22uL then Some PrimXor
  else if op = 23uL then Some PrimNot
  else if op = 24uL then Some PrimShl
  else if op = 25uL then Some PrimShr
  else if op = 30uL then Some PrimEq
  else if op = 31uL then Some PrimNeq
  else if op = 32uL then Some PrimLtU
  else if op = 33uL then Some PrimGtU
  else if op = 40uL then Some PrimFetch
  else if op = 41uL then Some PrimStore
  else if op = 42uL then Some PrimAlloc
  else if op = 50uL then Some PrimEmit
  else if op = 51uL then Some PrimKey
  else if op = 52uL then Some PrimHalt
  else None
