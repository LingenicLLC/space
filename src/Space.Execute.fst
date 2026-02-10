module Space.Execute

(** Execute primitives on universe stack *)

open FStar.UInt64
open Space.Types
open Space.Stack
open Space.Universe
open Space.Arithmetic
open Space.Bitwise
open Space.Comparison
open Space.Instruction

(** Execution result *)
noeq type exec_result =
  | ExecOk of universe
  | ExecError of string
  | ExecHalt

(** Execute stack manipulation primitive *)
let exec_stack_prim (op: prim_op) (u: universe) : exec_result =
  if not (is_live u) then ExecError "universe not live"
  else
    let s = u.stack in
    let result = match op with
      | PrimDup -> dup s
      | PrimDrop -> drop s
      | PrimSwap -> swap s
      | PrimOver -> over s
      | PrimRot -> rot s
      | _ -> None
    in
    match result with
    | Some s' -> ExecOk { u with stack = s' }
    | None -> ExecError "stack operation failed"

(** Execute arithmetic primitive *)
let exec_arith_prim (op: prim_op) (u: universe) : exec_result =
  if not (is_live u) then ExecError "universe not live"
  else
    let s = u.stack in
    let result = match op with
      | PrimAdd -> stack_add s
      | PrimSub -> stack_sub s
      | PrimMul -> stack_mul s
      | PrimDivU -> stack_div s
      | PrimMod ->
        (match s with
         | a :: b :: xs ->
           (match mod_cells b a with
            | Some r -> Some (r :: xs)
            | None -> None)
         | _ -> None)
      | _ -> None
    in
    match result with
    | Some s' -> ExecOk { u with stack = s' }
    | None -> ExecError "arithmetic operation failed"

(** Execute bitwise primitive *)
let exec_bitwise_prim (op: prim_op) (u: universe) : exec_result =
  if not (is_live u) then ExecError "universe not live"
  else
    let s = u.stack in
    let result = match op with
      | PrimAnd -> stack_and s
      | PrimOr -> stack_or s
      | PrimXor -> stack_xor s
      | PrimNot -> stack_not s
      | _ -> None
    in
    match result with
    | Some s' -> ExecOk { u with stack = s' }
    | None -> ExecError "bitwise operation failed"

(** Execute comparison primitive *)
let exec_compare_prim (op: prim_op) (u: universe) : exec_result =
  if not (is_live u) then ExecError "universe not live"
  else
    let s = u.stack in
    let result = match op with
      | PrimEq -> stack_eq s
      | PrimNeq -> stack_neq s
      | PrimLtU -> stack_lt s
      | PrimGtU -> stack_gt s
      | _ -> None
    in
    match result with
    | Some s' -> ExecOk { u with stack = s' }
    | None -> ExecError "comparison operation failed"

(** Execute any primitive *)
let exec_prim (op: prim_op) (u: universe) : exec_result =
  match op with
  (* Stack *)
  | PrimDup | PrimDrop | PrimSwap | PrimOver | PrimRot ->
    exec_stack_prim op u
  (* Arithmetic *)
  | PrimAdd | PrimSub | PrimMul | PrimDivU | PrimMod ->
    exec_arith_prim op u
  (* Bitwise *)
  | PrimAnd | PrimOr | PrimXor | PrimNot | PrimShl | PrimShr ->
    exec_bitwise_prim op u
  (* Comparison *)
  | PrimEq | PrimNeq | PrimLtU | PrimGtU ->
    exec_compare_prim op u
  (* System *)
  | PrimHalt -> ExecHalt
  (* Memory and I/O - placeholder *)
  | PrimFetch | PrimStore | PrimAlloc | PrimEmit | PrimKey ->
    ExecError "not implemented"
