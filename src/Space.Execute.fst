module Space.Execute

(** Execute primitives on universe stack *)

open FStar.UInt64
open FStar.Int.Cast
open Space.Types
open Space.Stack
open Space.Universe
open Space.Memory
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
      | PrimNip -> nip s
      | PrimTuck -> tuck s
      | PrimPick -> pick s
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
      | PrimDivS -> stack_div_signed s
      | PrimMod -> stack_mod s
      | PrimNeg -> stack_negate s
      | PrimMin -> stack_min s
      | PrimMax -> stack_max s
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
      | PrimShl -> stack_shl s
      | PrimShr -> stack_shr s
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
      | PrimLtS -> stack_lt_signed s
      | PrimGtS -> stack_gt_signed s
      | _ -> None
    in
    match result with
    | Some s' -> ExecOk { u with stack = s' }
    | None -> ExecError "comparison operation failed"

(** Execute memory fetch: ( addr -- value ) *)
let exec_mem_fetch (u: universe) : exec_result =
  if not (is_live u) then ExecError "universe not live"
  else match u.stack with
    | addr_cell :: rest ->
      let addr = v addr_cell in
      (match mem_fetch u.memory addr with
       | None -> ExecError "fetch: invalid address"
       | Some value -> ExecOk { u with stack = value :: rest })
    | _ -> ExecError "fetch: stack underflow"

(** Execute memory store: ( value addr -- ) *)
let exec_mem_store (u: universe) : exec_result =
  if not (is_live u) then ExecError "universe not live"
  else match u.stack with
    | addr_cell :: value :: rest ->
      let addr = v addr_cell in
      (match mem_store u.memory addr value with
       | None -> ExecError "store: invalid address"
       | Some mem' -> ExecOk { u with stack = rest; memory = mem' })
    | _ -> ExecError "store: stack underflow"

(** Execute memory alloc: ( n -- addr ) *)
let exec_mem_alloc (u: universe) : exec_result =
  if not (is_live u) then ExecError "universe not live"
  else match u.stack with
    | n_cell :: rest ->
      let n = v n_cell in
      let (mem', base_addr) = mem_alloc u.memory n in
      let addr_cell = if base_addr < pow2 64 then uint_to_t base_addr else 0uL in
      ExecOk { u with stack = addr_cell :: rest; memory = mem' }
    | _ -> ExecError "alloc: stack underflow"

(** Execute emit: ( char -- ) - placeholder, just drops *)
let exec_emit (u: universe) : exec_result =
  if not (is_live u) then ExecError "universe not live"
  else match u.stack with
    | _ :: rest -> ExecOk { u with stack = rest }
    | _ -> ExecError "emit: stack underflow"

(** Execute key: ( -- char ) - placeholder, pushes 0 *)
let exec_key (u: universe) : exec_result =
  if not (is_live u) then ExecError "universe not live"
  else ExecOk { u with stack = 0uL :: u.stack }

(** Execute bytes alloc: ( n -- addr ) - allocates n bytes *)
let exec_bytes_alloc (u: universe) : exec_result =
  if not (is_live u) then ExecError "universe not live"
  else match u.stack with
    | n_cell :: rest ->
      let n = v n_cell in
      (* Bytes need (n+7)/8 cells *)
      let cells_needed = (n + 7) / 8 in
      let (mem', base_addr) = mem_alloc u.memory cells_needed in
      let addr_cell = if base_addr < pow2 64 then uint_to_t base_addr else 0uL in
      ExecOk { u with stack = addr_cell :: rest; memory = mem' }
    | _ -> ExecError "bytes_alloc: stack underflow"

(** Execute bytes fetch: ( addr offset -- byte ) *)
let exec_bytes_fetch (u: universe) : exec_result =
  if not (is_live u) then ExecError "universe not live"
  else match u.stack with
    | offset_cell :: addr_cell :: rest ->
      let addr = v addr_cell in
      let offset = v offset_cell in
      let cell_idx = addr + (offset / 8) in
      let byte_idx = offset % 8 in
      (match mem_fetch u.memory cell_idx with
       | None -> ExecError "bytes_fetch: invalid address"
       | Some cell_val ->
         (* Extract byte from cell - byte_idx in [0,7], shift in [0,56] *)
         let shift_amount = byte_idx * 8 in
         let n32 : FStar.UInt32.t = uint64_to_uint32 (uint_to_t shift_amount) in
         if FStar.UInt32.v n32 < 64 then
           let shifted = shift_right cell_val n32 in
           let byte_val = v shifted % 256 in
           ExecOk { u with stack = uint_to_t byte_val :: rest }
         else
           ExecError "bytes_fetch: internal error")
    | _ -> ExecError "bytes_fetch: stack underflow"

(** Execute bytes store: ( byte addr offset -- ) *)
let exec_bytes_store (u: universe) : exec_result =
  if not (is_live u) then ExecError "universe not live"
  else match u.stack with
    | offset_cell :: addr_cell :: byte_cell :: rest ->
      let addr = v addr_cell in
      let offset = v offset_cell in
      let byte_val = v byte_cell % 256 in
      let cell_idx = addr + (offset / 8) in
      let byte_idx = offset % 8 in
      (match mem_fetch u.memory cell_idx with
       | None -> ExecError "bytes_store: invalid address"
       | Some old_cell ->
         (* Modify byte in cell *)
         let shift = byte_idx * 8 in
         let mask = sub_mod (shift_left 0xFFuL (FStar.UInt32.uint_to_t shift)) 0uL in
         let inv_mask = lognot mask in
         let cleared = old_cell &^ inv_mask in
         let new_byte = shift_left (uint_to_t byte_val) (FStar.UInt32.uint_to_t shift) in
         let new_cell = cleared |^ new_byte in
         (match mem_store u.memory cell_idx new_cell with
          | None -> ExecError "bytes_store: store failed"
          | Some mem' -> ExecOk { u with stack = rest; memory = mem' }))
    | _ -> ExecError "bytes_store: stack underflow"

(** Execute bytes len: ( addr -- len ) - not really possible without metadata *)
let exec_bytes_len (u: universe) : exec_result =
  ExecError "bytes_len: requires metadata (not implemented)"

(** Execute bytes copy: ( src dst len -- ) *)
let exec_bytes_copy (u: universe) : exec_result =
  ExecError "bytes_copy: requires byte-level copy loop (not implemented)"

(** Execute any primitive *)
let exec_prim (op: prim_op) (u: universe) : exec_result =
  match op with
  (* Stack *)
  | PrimDup | PrimDrop | PrimSwap | PrimOver | PrimRot | PrimNip | PrimTuck | PrimPick ->
    exec_stack_prim op u
  (* Arithmetic *)
  | PrimAdd | PrimSub | PrimMul | PrimDivU | PrimDivS | PrimMod | PrimNeg | PrimMin | PrimMax ->
    exec_arith_prim op u
  (* Bitwise *)
  | PrimAnd | PrimOr | PrimXor | PrimNot | PrimShl | PrimShr ->
    exec_bitwise_prim op u
  (* Comparison *)
  | PrimEq | PrimNeq | PrimLtU | PrimGtU | PrimLtS | PrimGtS ->
    exec_compare_prim op u
  (* Memory - cell level *)
  | PrimFetch -> exec_mem_fetch u
  | PrimStore -> exec_mem_store u
  | PrimAlloc -> exec_mem_alloc u
  (* Memory - byte level *)
  | PrimBytesAlloc -> exec_bytes_alloc u
  | PrimBytesFetch -> exec_bytes_fetch u
  | PrimBytesStore -> exec_bytes_store u
  | PrimBytesLen -> exec_bytes_len u
  | PrimBytesCopy -> exec_bytes_copy u
  (* I/O *)
  | PrimEmit -> exec_emit u
  | PrimKey -> exec_key u
  (* System *)
  | PrimHalt -> ExecHalt
