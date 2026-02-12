module Space.Bytes

(** Byte-level operations: fetch, store, length, copy *)

open FStar.UInt8
open FStar.UInt64
open FStar.List.Tot
open Space.Types
open Space.Stack
open Space.Memory

(** Byte array representation *)
noeq type byte_array = {
  base: nat;        (* Base offset in memory *)
  len: nat;         (* Length in bytes *)
}

(** Allocate byte array (returns pointer and length) *)
let allocate_bytes (m: memory) (n: nat) : memory * byte_array =
  if n = 0 then (m, { base = 0; len = 0 })
  else
    (* Calculate cells needed (8 bytes per cell) *)
    let cells_needed = (n + 7) / 8 in
    let (m', base) = mem_alloc m cells_needed in
    (m', { base = base; len = n })

(** Shift amount for byte position *)
let shift_for_byte (byte_pos: nat{byte_pos < 8}) : FStar.UInt32.t =
  match byte_pos with
  | 0 -> 0ul | 1 -> 8ul | 2 -> 16ul | 3 -> 24ul
  | 4 -> 32ul | 5 -> 40ul | 6 -> 48ul | _ -> 56ul

(** Extract byte at position from cell (0-7) using UInt64 operations *)
let extract_byte (cell_val: cell) (byte_pos: nat{byte_pos < 8}) : UInt8.t =
  let shift = shift_for_byte byte_pos in
  let shifted = FStar.UInt64.shift_right cell_val shift in
  let masked = FStar.UInt64.logand shifted 0xFFuL in
  FStar.Int.Cast.uint64_to_uint8 masked

(** Insert byte at position in cell (0-7) *)
let insert_byte (cell_val: cell) (byte_pos: nat{byte_pos < 8}) (byte: UInt8.t) : cell =
  let shift = shift_for_byte byte_pos in
  let byte_extended = FStar.Int.Cast.uint8_to_uint64 byte in
  let byte_shifted = FStar.UInt64.shift_left byte_extended shift in
  let mask = FStar.UInt64.shift_left 0xFFuL shift in
  let inv_mask = FStar.UInt64.lognot mask in
  let cleared = FStar.UInt64.logand cell_val inv_mask in
  FStar.UInt64.logor cleared byte_shifted

(** Fetch single byte from byte array *)
let bytes_fetch (m: memory) (arr: byte_array) (offset: nat) : option UInt8.t =
  if offset >= arr.len then None
  else
    let cell_offset = offset / 8 in
    let byte_in_cell = offset % 8 in
    if byte_in_cell >= 8 then None  (* Should not happen, but needed for refinement *)
    else
      match mem_fetch m (arr.base + cell_offset) with
      | None -> None
      | Some cell_val -> Some (extract_byte cell_val byte_in_cell)

(** Store single byte into byte array *)
let bytes_store (m: memory) (arr: byte_array) (offset: nat) (byte: UInt8.t) : option memory =
  if offset >= arr.len then None
  else
    let cell_offset = offset / 8 in
    let byte_in_cell = offset % 8 in
    if byte_in_cell >= 8 then None  (* Should not happen *)
    else
      match mem_fetch m (arr.base + cell_offset) with
      | None -> None
      | Some cell_val ->
        let new_val = insert_byte cell_val byte_in_cell byte in
        mem_store m (arr.base + cell_offset) new_val

(** Get byte array length *)
let bytes_length (arr: byte_array) : nat =
  arr.len

(** Helper: minimum of two nats *)
let min_nat (a b: nat) : nat =
  if a <= b then a else b

(** Copy bytes between arrays (non-overlapping) *)
let rec bytes_copy_aux (m: memory) (src dst: byte_array) (src_off dst_off count: nat)
  : Tot (option memory) (decreases count) =
  if count = 0 then Some m
  else
    match bytes_fetch m src src_off with
    | None -> None
    | Some byte ->
      match bytes_store m dst dst_off byte with
      | None -> None
      | Some m' -> bytes_copy_aux m' src dst (src_off + 1) (dst_off + 1) (count - 1)

(** Copy n bytes from src to dst *)
let bytes_copy (m: memory) (src dst: byte_array) (len: nat) : option memory =
  if len > src.len then None
  else if len > dst.len then None
  else bytes_copy_aux m src dst 0 0 len

(** Stack operations for bytes *)

(** Bytes fetch: ( ptr offset -- byte ) *)
let stack_bytes_fetch (m: memory) (arr: byte_array) (s: stack) : option (UInt8.t * stack) =
  match s with
  | offset_cell :: rest ->
    let offset = v offset_cell in
    (match bytes_fetch m arr offset with
     | None -> None
     | Some byte -> Some (byte, rest))
  | _ -> None

(** Bytes store: ( byte ptr offset -- ) *)
let stack_bytes_store (m: memory) (arr: byte_array) (s: stack) : option (memory * stack) =
  match s with
  | offset_cell :: byte_cell :: rest ->
    let offset = v offset_cell in
    let byte = FStar.Int.Cast.uint64_to_uint8 byte_cell in
    (match bytes_store m arr offset byte with
     | None -> None
     | Some m' -> Some (m', rest))
  | _ -> None

