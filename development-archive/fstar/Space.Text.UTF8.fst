module Space.Text.UTF8

(** UTF-8 encoding and decoding *)

open FStar.UInt8

(** Check if byte is ASCII (0x00-0x7F) *)
let is_ascii (b: UInt8.t) : bool =
  UInt8.v b <= 0x7F

(** Check if byte is a continuation byte (10xxxxxx) *)
let is_continuation (b: UInt8.t) : bool =
  let v = UInt8.v b in
  v >= 0x80 && v <= 0xBF

(** Check if byte is a 2-byte lead (110xxxxx) *)
let is_lead_2 (b: UInt8.t) : bool =
  let v = UInt8.v b in
  v >= 0xC2 && v <= 0xDF

(** Check if byte is a 3-byte lead (1110xxxx) *)
let is_lead_3 (b: UInt8.t) : bool =
  let v = UInt8.v b in
  v >= 0xE0 && v <= 0xEF

(** Check if byte is a 4-byte lead (11110xxx) *)
let is_lead_4 (b: UInt8.t) : bool =
  let v = UInt8.v b in
  v >= 0xF0 && v <= 0xF4

(** Get expected sequence length from lead byte *)
let sequence_length (lead: UInt8.t) : nat =
  if is_ascii lead then 1
  else if is_lead_2 lead then 2
  else if is_lead_3 lead then 3
  else if is_lead_4 lead then 4
  else 0  (* Invalid lead byte *)

(** Decode codepoint from UTF-8 bytes - returns nat for simplicity *)
let decode_codepoint_1 (b0: UInt8.t) : nat =
  UInt8.v b0

let decode_codepoint_2 (b0 b1: UInt8.t) : nat =
  let v0 : nat = UInt8.v b0 in
  let v1 : nat = UInt8.v b1 in
  if v0 >= 0xC0 && v1 >= 0x80 then
    op_Multiply (v0 - 0xC0) 64 + (v1 - 0x80)
  else 0

let decode_codepoint_3 (b0 b1 b2: UInt8.t) : nat =
  let v0 : nat = UInt8.v b0 in
  let v1 : nat = UInt8.v b1 in
  let v2 : nat = UInt8.v b2 in
  if v0 >= 0xE0 && v1 >= 0x80 && v2 >= 0x80 then
    op_Multiply (v0 - 0xE0) 4096 + op_Multiply (v1 - 0x80) 64 + (v2 - 0x80)
  else 0

let decode_codepoint_4 (b0 b1 b2 b3: UInt8.t) : nat =
  let v0 : nat = UInt8.v b0 in
  let v1 : nat = UInt8.v b1 in
  let v2 : nat = UInt8.v b2 in
  let v3 : nat = UInt8.v b3 in
  if v0 >= 0xF0 && v1 >= 0x80 && v2 >= 0x80 && v3 >= 0x80 then
    op_Multiply (v0 - 0xF0) 262144 + op_Multiply (v1 - 0x80) 4096 + op_Multiply (v2 - 0x80) 64 + (v3 - 0x80)
  else 0

(** Check if codepoint is valid Unicode *)
let is_valid_codepoint (cp: nat) : bool =
  cp <= 0x10FFFF && not (cp >= 0xD800 && cp <= 0xDFFF)

(** Check if entire byte list is valid UTF-8 *)
let rec is_valid_utf8 (bytes: list UInt8.t) : Tot bool (decreases (List.Tot.length bytes)) =
  match bytes with
  | [] -> true
  | b0 :: rest ->
    let len = sequence_length b0 in
    if len = 0 then false
    else if len = 1 then is_valid_utf8 rest
    else if len = 2 then
      (match rest with
       | b1 :: rest' -> is_continuation b1 && is_valid_utf8 rest'
       | _ -> false)
    else if len = 3 then
      (match rest with
       | b1 :: b2 :: rest' -> is_continuation b1 && is_continuation b2 && is_valid_utf8 rest'
       | _ -> false)
    else (* len = 4 *)
      (match rest with
       | b1 :: b2 :: b3 :: rest' -> is_continuation b1 && is_continuation b2 && is_continuation b3 && is_valid_utf8 rest'
       | _ -> false)

(** Check if all bytes are ASCII *)
let rec is_all_ascii (bytes: list UInt8.t) : bool =
  match bytes with
  | [] -> true
  | b :: rest -> is_ascii b && is_all_ascii rest
