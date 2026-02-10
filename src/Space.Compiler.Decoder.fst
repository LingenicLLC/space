module Space.Compiler.Decoder

(** Bytecode decoder - converts bytecode to instruction sequences *)

open FStar.List.Tot
open FStar.UInt8
open FStar.UInt64
open FStar.Mul
open Space.Types
open Space.Instruction
open Space.Compiler.Bytecode

(** Decode result *)
noeq type decode_result =
  | DecOk of instruction & list UInt8.t   (* Decoded instruction and remaining bytes *)
  | DecError of string                     (* Error message *)
  | DecEnd                                 (* End of bytecode *)

(** Read unsigned 16-bit little-endian from bytes *)
let read_u16 (bytes: list UInt8.t) : option (nat & list UInt8.t) =
  match bytes with
  | b0 :: b1 :: rest ->
    let v = UInt8.v b0 + UInt8.v b1 * 256 in
    Some (v, rest)
  | _ -> None

(** Read signed 16-bit little-endian from bytes *)
let read_i16 (bytes: list UInt8.t) : option (int & list UInt8.t) =
  match read_u16 bytes with
  | Some (v, rest) ->
    let signed = if v >= 32768 then v - 65536 else v in
    Some (signed, rest)
  | None -> None

(** Read unsigned 32-bit little-endian from bytes *)
let read_u32 (bytes: list UInt8.t) : option (nat & list UInt8.t) =
  match bytes with
  | b0 :: b1 :: b2 :: b3 :: rest ->
    let v = UInt8.v b0 +
            UInt8.v b1 * 256 +
            UInt8.v b2 * 65536 +
            UInt8.v b3 * 16777216 in
    Some (v, rest)
  | _ -> None

(** Read unsigned 64-bit little-endian from bytes *)
let read_u64 (bytes: list UInt8.t) : option (UInt64.t & list UInt8.t) =
  match bytes with
  | b0 :: b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: rest ->
    let v0 : nat = UInt8.v b0 in
    let v1 : nat = UInt8.v b1 in
    let v2 : nat = UInt8.v b2 in
    let v3 : nat = UInt8.v b3 in
    let v4 : nat = UInt8.v b4 in
    let v5 : nat = UInt8.v b5 in
    let v6 : nat = UInt8.v b6 in
    let v7 : nat = UInt8.v b7 in
    (* Build 64-bit value from bytes *)
    let lo : nat = v0 + v1 * 256 + v2 * 65536 + v3 * 16777216 in
    let hi : nat = v4 + v5 * 256 + v6 * 65536 + v7 * 16777216 in
    (* Combine - need to be careful with 64-bit range *)
    if hi < 4294967296 && lo < 4294967296 then
      let combined = lo + hi * 4294967296 in
      if combined < 18446744073709551616 then
        Some (UInt64.uint_to_t combined, rest)
      else None
    else None
  | _ -> None

(** Convert nat to ip (instruction pointer) *)
let nat_to_ip (n: nat) : ip =
  if n < 18446744073709551616 then UInt64.uint_to_t n
  else 0uL

(** Decode stack operation opcodes (0x00-0x0F) *)
let decode_stack_op (op: UInt8.t) : option prim_op =
  let v = UInt8.v op in
  if v = 0x01 then Some PrimDup
  else if v = 0x02 then Some PrimDrop
  else if v = 0x03 then Some PrimSwap
  else if v = 0x04 then Some PrimOver
  else if v = 0x05 then Some PrimRot
  else None  (* nip, tuck, pick not in prim_op *)

(** Decode arithmetic operation opcodes (0x10-0x1F) *)
let decode_arith_op (op: UInt8.t) : option prim_op =
  let v = UInt8.v op in
  if v = 0x10 then Some PrimAdd
  else if v = 0x11 then Some PrimSub
  else if v = 0x12 then Some PrimMul
  else if v = 0x13 then Some PrimDivU
  else if v = 0x15 then Some PrimMod
  else None  (* divs, neg, min, max not in prim_op *)

(** Decode bitwise operation opcodes (0x20-0x2F) *)
let decode_bitwise_op (op: UInt8.t) : option prim_op =
  let v = UInt8.v op in
  if v = 0x20 then Some PrimAnd
  else if v = 0x21 then Some PrimOr
  else if v = 0x22 then Some PrimXor
  else if v = 0x23 then Some PrimNot
  else if v = 0x24 then Some PrimShl
  else if v = 0x25 then Some PrimShr
  else None

(** Decode comparison operation opcodes (0x30-0x3F) *)
let decode_compare_op (op: UInt8.t) : option prim_op =
  let v = UInt8.v op in
  if v = 0x30 then Some PrimEq
  else if v = 0x31 then Some PrimNeq
  else if v = 0x34 then Some PrimLtU
  else if v = 0x35 then Some PrimGtU
  else None  (* lt, gt (signed) not in prim_op *)

(** Decode memory operation opcodes (0x40-0x4F) *)
let decode_memory_op (op: UInt8.t) : option prim_op =
  let v = UInt8.v op in
  if v = 0x40 then Some PrimAlloc
  else if v = 0x41 then Some PrimFetch
  else if v = 0x42 then Some PrimStore
  else None  (* bytes_len, bytes_copy not in prim_op *)

(** Decode system operation opcodes (0xB0-0xBF) *)
let decode_system_op (op: UInt8.t) : option prim_op =
  let v = UInt8.v op in
  if v = 0xB0 then Some PrimHalt
  else if v = 0xB1 then Some PrimEmit
  else if v = 0xB2 then Some PrimKey
  else None

(** Decode discipline byte *)
let decode_discipline (b: UInt8.t) : discipline =
  let v = UInt8.v b in
  if v = 0x00 then Linear
  else if v = 0x01 then Affine
  else Unrestricted

(** Read null-terminated string from bytes *)
let rec read_string_aux (bytes: list UInt8.t) (acc: list UInt8.t) (fuel: nat)
  : Tot (option (string & list UInt8.t)) (decreases fuel) =
  if fuel = 0 then None
  else match bytes with
    | [] -> None
    | b :: rest ->
      if b = 0x00uy then
        let chars = List.Tot.map (fun b -> FStar.Char.char_of_int (UInt8.v b)) acc in
        Some (FStar.String.string_of_list chars, rest)
      else
        read_string_aux rest (acc @ [b]) (fuel - 1)

let read_string (bytes: list UInt8.t) : option (string & list UInt8.t) =
  read_string_aux bytes [] (List.Tot.length bytes)

(** Decode a single instruction from bytecode *)
let decode_one (bytes: list UInt8.t) : decode_result =
  match bytes with
  | [] -> DecEnd
  | op :: rest ->
    let v = UInt8.v op in

    (* NOP - skip *)
    if v = 0x00 then
      DecOk (IPrimitive PrimDup, rest)  (* NOP as dup;drop would be wasteful, just skip *)

    (* Stack operations: 0x01-0x0F *)
    else if v >= 0x01 && v <= 0x0F then
      (match decode_stack_op op with
       | Some prim -> DecOk (IPrimitive prim, rest)
       | None -> DecError "Unsupported stack operation")

    (* Arithmetic: 0x10-0x1F *)
    else if v >= 0x10 && v <= 0x1F then
      (match decode_arith_op op with
       | Some prim -> DecOk (IPrimitive prim, rest)
       | None -> DecError "Unsupported arithmetic operation")

    (* Bitwise: 0x20-0x2F *)
    else if v >= 0x20 && v <= 0x2F then
      (match decode_bitwise_op op with
       | Some prim -> DecOk (IPrimitive prim, rest)
       | None -> DecError "Unsupported bitwise operation")

    (* Comparison: 0x30-0x3F *)
    else if v >= 0x30 && v <= 0x3F then
      (match decode_compare_op op with
       | Some prim -> DecOk (IPrimitive prim, rest)
       | None -> DecError "Unsupported comparison operation")

    (* Memory: 0x40-0x4F *)
    else if v >= 0x40 && v <= 0x4F then
      (match decode_memory_op op with
       | Some prim -> DecOk (IPrimitive prim, rest)
       | None -> DecError "Unsupported memory operation")

    (* Control flow: 0x90-0x9F *)
    else if v = 0x90 then  (* call *)
      (match read_u16 rest with
       | Some (addr, rest') -> DecOk (ICall (nat_to_ip addr), rest')
       | None -> DecError "Missing address for call")

    else if v = 0x91 then  (* ret *)
      DecOk (IReturn, rest)

    else if v = 0x92 then  (* jmp *)
      (match read_i16 rest with
       | Some (offset, rest') -> DecOk (IBranch (nat_to_ip (if offset >= 0 then offset else 0)), rest')
       | None -> DecError "Missing offset for jump")

    else if v = 0x93 then  (* jz *)
      (match read_i16 rest with
       | Some (offset, rest') -> DecOk (IBranchZero (nat_to_ip (if offset >= 0 then offset else 0)), rest')
       | None -> DecError "Missing offset for jz")

    else if v = 0x94 then  (* jnz *)
      (match read_i16 rest with
       | Some (offset, rest') -> DecOk (IBranchNonZero (nat_to_ip (if offset >= 0 then offset else 0)), rest')
       | None -> DecError "Missing offset for jnz")

    else if v = 0x96 then  (* exit *)
      DecOk (IReturn, rest)  (* exit-word is like return *)

    (* Universe: 0xA0-0xAF *)
    else if v = 0xA0 then  (* create_univ *)
      (match rest with
       | disc_byte :: rest' ->
         (match read_string rest' with
          | Some (name, rest'') ->
            DecOk (ICreateUniverse (name, decode_discipline disc_byte), rest'')
          | None -> DecError "Missing universe name")
       | _ -> DecError "Missing discipline for create_univ")

    else if v = 0xA1 then  (* end_univ *)
      (match read_string rest with
       | Some (name, rest') -> DecOk (IEndUniverse name, rest')
       | None -> DecError "Missing universe name for end_univ")

    else if v = 0xA2 then  (* release_univ *)
      (match read_string rest with
       | Some (name, rest') -> DecOk (IReleaseUniverse name, rest')
       | None -> DecError "Missing universe name for release_univ")

    else if v = 0xA3 then  (* transfer *)
      (match read_string rest with
       | Some (name, rest') -> DecOk (ITransferTo name, rest')
       | None -> DecError "Missing universe name for transfer")

    (* System: 0xB0-0xBF *)
    else if v >= 0xB0 && v <= 0xBF then
      (match decode_system_op op with
       | Some prim -> DecOk (IPrimitive prim, rest)
       | None -> DecError "Unsupported system operation")

    (* Push instructions: 0xF0-0xFF *)
    else if v = 0xF0 then  (* push_i8 *)
      (match rest with
       | b :: rest' -> DecOk (IPush (UInt64.uint_to_t (UInt8.v b)), rest')
       | _ -> DecError "Missing byte for push_i8")

    else if v = 0xF1 then  (* push_i16 *)
      (match read_u16 rest with
       | Some (n, rest') -> DecOk (IPush (nat_to_ip n), rest')
       | None -> DecError "Missing bytes for push_i16")

    else if v = 0xF2 then  (* push_i32 *)
      (match read_u32 rest with
       | Some (n, rest') -> DecOk (IPush (nat_to_ip n), rest')
       | None -> DecError "Missing bytes for push_i32")

    else if v = 0xF3 then  (* push_i64 *)
      (match read_u64 rest with
       | Some (n, rest') -> DecOk (IPush n, rest')
       | None -> DecError "Missing bytes for push_i64")

    else
      DecError "Unknown opcode"

(** Decode all instructions from bytecode *)
let rec decode_all_aux (bytes: list UInt8.t) (acc: list instruction) (fuel: nat)
  : Tot (either (list instruction) string) (decreases fuel) =
  if fuel = 0 then Inl (List.Tot.rev acc)
  else match decode_one bytes with
    | DecOk (instr, rest) -> decode_all_aux rest (instr :: acc) (fuel - 1)
    | DecError msg -> Inr msg
    | DecEnd -> Inl (List.Tot.rev acc)

(** Decode bytecode to instruction list *)
let decode_bytecode (bytes: list UInt8.t) : either (list instruction) string =
  decode_all_aux bytes [] (List.Tot.length bytes + 1)

(** Decode a function from bytecode module *)
let decode_function (m: bytecode_module) (idx: nat) : either (list instruction) string =
  if idx >= List.Tot.length m.bm_functions then
    Inr "Function index out of bounds"
  else
    match List.Tot.nth m.bm_functions idx with
    | None -> Inr "Function not found"
    | Some func ->
      let start = func.fe_code_offset in
      let len = func.fe_code_length in
      (* Extract the function's bytecode *)
      let code = m.bm_code in
      if start + len <= List.Tot.length code then
        let rec skip_n (lst: list UInt8.t) (n: nat) : Tot (list UInt8.t) (decreases n) =
          if n = 0 then lst
          else match lst with
            | [] -> []
            | _ :: rest -> skip_n rest (n - 1)
        in
        let rec take_n (lst: list UInt8.t) (n: nat) : Tot (list UInt8.t) (decreases n) =
          if n = 0 then []
          else match lst with
            | [] -> []
            | x :: rest -> x :: take_n rest (n - 1)
        in
        let skipped = skip_n code start in
        let func_code = take_n skipped len in
        decode_bytecode func_code
      else
        Inr "Function code out of bounds"

(** Get function by name *)
let find_function (m: bytecode_module) (name: string) : option nat =
  let rec find_aux (funcs: list function_entry) (idx: nat) : option nat =
    match funcs with
    | [] -> None
    | f :: rest ->
      if f.fe_name = name then Some idx
      else find_aux rest (idx + 1)
  in
  find_aux m.bm_functions 0

(** Decode function by name *)
let decode_function_by_name (m: bytecode_module) (name: string) : either (list instruction) string =
  match find_function m name with
  | Some idx -> decode_function m idx
  | None -> Inr ("Function not found: " ^ name)
