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

(* Note: exec_emit/exec_key removed - routed through step_io_prim with I/O buffers *)

(* Note: exec_bytes_alloc removed - routed through step_bytes_meta_prim for metadata tracking *)

(** Execute bytes fetch: ( addr offset -- byte ) *)
let exec_bytes_fetch (u: universe) : exec_result =
  if not (is_live u) then ExecError "universe not live"
  else match u.stack with
    | offset_cell :: addr_cell :: rest ->
      let addr = v addr_cell in
      let offset = v offset_cell in
      let cell_idx = addr + (offset / 8) in
      (* Compute byte position within cell using UInt64 ops *)
      let byte_pos = offset_cell %^ 8uL in   (* 0-7 *)
      let shift_u64 = mul_mod byte_pos 8uL in (* 0-56 *)
      let n32 : FStar.UInt32.t = uint64_to_uint32 shift_u64 in
      (match mem_fetch u.memory cell_idx with
       | None -> ExecError "bytes_fetch: invalid address"
       | Some cell_val ->
         if FStar.UInt32.v n32 < 64 then
           let shifted = shift_right cell_val n32 in
           let byte_val = shifted %^ 256uL in
           ExecOk { u with stack = byte_val :: rest }
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
      let cell_idx = addr + (offset / 8) in
      (* Compute byte position within cell using UInt64 ops *)
      let byte_pos = offset_cell %^ 8uL in   (* 0-7 *)
      let shift_u64 = mul_mod byte_pos 8uL in (* 0-56 *)
      let n32 : FStar.UInt32.t = uint64_to_uint32 shift_u64 in
      let byte_masked = byte_cell &^ 0xFFuL in  (* mask to single byte *)
      (match mem_fetch u.memory cell_idx with
       | None -> ExecError "bytes_store: invalid address"
       | Some old_cell ->
         if FStar.UInt32.v n32 < 64 then
           let mask = shift_left 0xFFuL n32 in
           let inv_mask = lognot mask in
           let cleared = old_cell &^ inv_mask in
           let new_byte = shift_left byte_masked n32 in
           let new_cell = cleared |^ new_byte in
           (match mem_store u.memory cell_idx new_cell with
            | None -> ExecError "bytes_store: store failed"
            | Some mem' -> ExecOk { u with stack = rest; memory = mem' })
         else
           ExecError "bytes_store: internal error")
    | _ -> ExecError "bytes_store: stack underflow"

(* Note: exec_bytes_len removed - routed through step_bytes_meta_prim with metadata *)

(** Helper: copy one byte from src to dst at given offset *)
let copy_one_byte (mem: memory) (src_addr dst_addr offset: nat) : option memory =
  let src_cell_idx = src_addr + (offset / 8) in
  let dst_cell_idx = dst_addr + (offset / 8) in
  let byte_pos = offset % 8 in
  if byte_pos >= 8 then None
  else
    let byte_pos_u64 = uint_to_t byte_pos in
    let shift_u64 = mul_mod byte_pos_u64 8uL in
    let n32 : FStar.UInt32.t = uint64_to_uint32 shift_u64 in
    if FStar.UInt32.v n32 >= 64 then None
    else
      match mem_fetch mem src_cell_idx with
      | None -> None
      | Some src_cell ->
        let shifted = shift_right src_cell n32 in
        let byte_val = shifted &^ 0xFFuL in
        match mem_fetch mem dst_cell_idx with
        | None -> None
        | Some dst_cell ->
          let mask = shift_left 0xFFuL n32 in
          let inv_mask = lognot mask in
          let cleared = dst_cell &^ inv_mask in
          let new_byte = shift_left byte_val n32 in
          let new_cell = cleared |^ new_byte in
          mem_store mem dst_cell_idx new_cell

(** Recursive byte copy with fuel *)
let rec bytes_copy_loop (mem: memory) (src_addr dst_addr offset remaining: nat)
  : Tot (option memory) (decreases remaining) =
  if remaining = 0 then Some mem
  else
    match copy_one_byte mem src_addr dst_addr offset with
    | None -> None
    | Some mem' -> bytes_copy_loop mem' src_addr dst_addr (offset + 1) (remaining - 1)

(** Execute bytes copy: ( src_addr dst_addr len -- ) *)
let exec_bytes_copy (u: universe) : exec_result =
  if not (is_live u) then ExecError "universe not live"
  else match u.stack with
    | len_cell :: dst_cell :: src_cell :: rest ->
      let src_addr = v src_cell in
      let dst_addr = v dst_cell in
      let len = v len_cell in
      if len = 0 then ExecOk { u with stack = rest }
      else
        (match bytes_copy_loop u.memory src_addr dst_addr 0 len with
         | None -> ExecError "bytes_copy: copy failed"
         | Some mem' -> ExecOk { u with stack = rest; memory = mem' })
    | _ -> ExecError "bytes_copy: stack underflow"

(** Execute any primitive *)
let exec_prim (op: prim_op) (u: universe) : exec_result =
  match op with
  (* Stack - 8 ops *)
  | PrimDup | PrimDrop | PrimSwap | PrimOver | PrimRot | PrimNip | PrimTuck | PrimPick ->
    exec_stack_prim op u
  (* Arithmetic - 9 ops *)
  | PrimAdd | PrimSub | PrimMul | PrimDivU | PrimDivS | PrimMod | PrimNeg | PrimMin | PrimMax ->
    exec_arith_prim op u
  (* Bitwise - 6 ops *)
  | PrimAnd | PrimOr | PrimXor | PrimNot | PrimShl | PrimShr ->
    exec_bitwise_prim op u
  (* Comparison - 6 ops *)
  | PrimEq | PrimNeq | PrimLtU | PrimGtU | PrimLtS | PrimGtS ->
    exec_compare_prim op u
  (* Memory - cell level - 3 ops *)
  | PrimFetch -> exec_mem_fetch u
  | PrimStore -> exec_mem_store u
  | PrimAlloc -> exec_mem_alloc u
  (* Memory - byte level - 5 ops (alloc/len routed through step_prim for metadata) *)
  | PrimBytesAlloc -> ExecError "bytes-alloc: routed through step_prim"
  | PrimBytesFetch -> exec_bytes_fetch u
  | PrimBytesStore -> exec_bytes_store u
  | PrimBytesLen -> ExecError "bytes-len: routed through step_prim"
  | PrimBytesCopy -> exec_bytes_copy u
  (* Borrowing - 8 ops - routed through step_borrow_prim *)
  | PrimBorrowPointer | PrimReturnPointer | PrimDropPointer | PrimFetchBorrowed
  | PrimStoreBorrowed | PrimFetchAndEnd | PrimStoreAndEnd | PrimOffsetBorrowed ->
    ExecError "borrow ops routed through step_prim"
  (* Warp - 7 ops - routed through step_warp_prim *)
  | PrimWarpFetch | PrimWarpStore | PrimWarpAdvance | PrimWarpFollow
  | PrimWarpPosition | PrimWarpRestore | PrimWarpNull ->
    ExecError "warp ops routed through step_prim"
  (* Text - 11 ops - routed through step_text_prim *)
  | PrimCreateText | PrimTextByteLength | PrimTextGraphemeCount | PrimTextIsSimple
  | PrimTextGraphemeAt | PrimTextGraphemeFirst | PrimTextGraphemeLast
  | PrimTextSlice | PrimTextConcat | PrimTextEqual | PrimTextCompare ->
    ExecError "text ops routed through step_prim"
  (* Text warp - 5 ops - routed through step_text_prim *)
  | PrimTextWarpHasGrapheme | PrimTextWarpCurrentGrapheme | PrimTextWarpNextGrapheme
  | PrimTextWarpGraphemeIndex | PrimTextWarpGotoGrapheme ->
    ExecError "text warp ops routed through step_prim"
  (* Grapheme - 3 ops - routed through step_text_prim *)
  | PrimGraphemeByteLength | PrimGraphemeIsAscii | PrimGraphemeCodePoints ->
    ExecError "grapheme ops routed through step_prim"
  (* Codepoint - 2 ops - routed through step_text_prim *)
  | PrimTextCodePointCount | PrimTextCodePointAt ->
    ExecError "codepoint ops routed through step_prim"
  (* I/O - 3 ops - routed through step_io_prim *)
  | PrimEmit | PrimKey | PrimEmitGrapheme ->
    ExecError "I/O ops routed through step_prim"
  (* System *)
  | PrimHalt -> ExecHalt
  (* Normalization - 4 ops - routed through step_text_prim *)
  | PrimTextNormalizeNfc | PrimTextNormalizeNfd | PrimTextNormalizeNfkc | PrimTextNormalizeNfkd ->
    ExecError "normalization ops routed through step_prim"
  (* Case mapping - 3 ops - routed through step_text_prim *)
  | PrimTextToUpper | PrimTextToLower | PrimTextToTitle ->
    ExecError "case mapping ops routed through step_prim"
