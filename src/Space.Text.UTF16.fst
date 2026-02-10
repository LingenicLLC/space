module Space.Text.UTF16

(** UTF-16 encoding/decoding for Windows/Java interop *)

open FStar.UInt8
open FStar.UInt16
open FStar.UInt32
open FStar.List.Tot
open FStar.Mul
open Space.Text.Types
open Space.Text.UTF8
open Space.Text.Create

(** UTF-16 code unit *)
type utf16_unit = UInt16.t

(** Check if code unit is high surrogate (0xD800-0xDBFF) *)
let is_high_surrogate (u: utf16_unit) : bool =
  let v = UInt16.v u in
  v >= 0xD800 && v <= 0xDBFF

(** Check if code unit is low surrogate (0xDC00-0xDFFF) *)
let is_low_surrogate (u: utf16_unit) : bool =
  let v = UInt16.v u in
  v >= 0xDC00 && v <= 0xDFFF

(** Check if code unit is any surrogate *)
let is_surrogate (u: utf16_unit) : bool =
  is_high_surrogate u || is_low_surrogate u

(** Check if code unit is BMP (not surrogate) *)
let is_bmp (u: utf16_unit) : bool =
  not (is_surrogate u)

(** Decode surrogate pair to codepoint *)
let decode_surrogate_pair (high low: utf16_unit) : nat =
  let h : nat = UInt16.v high in
  let l : nat = UInt16.v low in
  (* high is in range 0xD800-0xDBFF, low is in range 0xDC00-0xDFFF *)
  (* Result is always positive and in range 0x10000-0x10FFFF *)
  if h >= 0xD800 && l >= 0xDC00 then
    let high_offset : nat = h - 0xD800 in
    let low_offset : nat = l - 0xDC00 in
    0x10000 + high_offset * 1024 + low_offset
  else 0  (* Invalid pair *)

(** Encode codepoint to UTF-16 (1 or 2 code units) *)
let encode_codepoint_utf16 (cp: nat{cp <= 0x10FFFF && (cp < 0xD800 || cp > 0xDFFF)})
  : list utf16_unit =
  if cp < 0x10000 then
    [UInt16.uint_to_t cp]
  else
    let cp' = cp - 0x10000 in
    let high = 0xD800 + (cp' / 0x400) in
    let low = 0xDC00 + (cp' % 0x400) in
    [UInt16.uint_to_t high; UInt16.uint_to_t low]

(** Convert UTF-16 unit to two bytes (little-endian) *)
let utf16_to_bytes_le (u: utf16_unit) : list UInt8.t =
  let v = UInt16.v u in
  let low_byte = v % 256 in
  let high_byte = v / 256 in
  [UInt8.uint_to_t low_byte; UInt8.uint_to_t high_byte]

(** Convert UTF-16 unit to two bytes (big-endian) *)
let utf16_to_bytes_be (u: utf16_unit) : list UInt8.t =
  let v = UInt16.v u in
  let low_byte = v % 256 in
  let high_byte = v / 256 in
  [UInt8.uint_to_t high_byte; UInt8.uint_to_t low_byte]

(** Byte order for UTF-16 *)
type utf16_byte_order =
  | LittleEndian
  | BigEndian

(** Convert codepoint list to UTF-16 bytes *)
let rec codepoints_to_utf16_bytes (cps: list nat) (order: utf16_byte_order)
  : Tot (list UInt8.t) (decreases (length cps)) =
  match cps with
  | [] -> []
  | cp :: rest ->
    if cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF) then
      (* Invalid codepoint - skip *)
      codepoints_to_utf16_bytes rest order
    else
      let units = encode_codepoint_utf16 cp in
      let bytes =
        match order with
        | LittleEndian -> List.Tot.concatMap utf16_to_bytes_le units
        | BigEndian -> List.Tot.concatMap utf16_to_bytes_be units
      in
      bytes @ codepoints_to_utf16_bytes rest order

(** Extract codepoints from UTF-8 text data *)
let rec extract_codepoints (bytes: list UInt8.t) : Tot (list nat) (decreases (length bytes)) =
  match bytes with
  | [] -> []
  | b0 :: rest ->
    let len = sequence_length b0 in
    if len = 0 then extract_codepoints rest  (* Skip invalid *)
    else if len = 1 then
      UInt8.v b0 :: extract_codepoints rest
    else if len = 2 then
      match rest with
      | b1 :: rest' ->
        decode_codepoint_2 b0 b1 :: extract_codepoints rest'
      | _ -> []
    else if len = 3 then
      match rest with
      | b1 :: b2 :: rest' ->
        decode_codepoint_3 b0 b1 b2 :: extract_codepoints rest'
      | _ -> []
    else
      match rest with
      | b1 :: b2 :: b3 :: rest' ->
        decode_codepoint_4 b0 b1 b2 b3 :: extract_codepoints rest'
      | _ -> []

(** Convert text to UTF-16 bytes (little-endian, no BOM) *)
let text_to_utf16 (t: text) : list UInt8.t =
  let cps = extract_codepoints t.data in
  codepoints_to_utf16_bytes cps LittleEndian

(** Convert text to UTF-16 bytes (big-endian, no BOM) *)
let text_to_utf16_be (t: text) : list UInt8.t =
  let cps = extract_codepoints t.data in
  codepoints_to_utf16_bytes cps BigEndian

(** Convert text to UTF-16 bytes with BOM *)
let text_to_utf16_with_bom (t: text) (order: utf16_byte_order) : list UInt8.t =
  let bom = match order with
    | LittleEndian -> [0xFFuy; 0xFEuy]  (* LE BOM *)
    | BigEndian -> [0xFEuy; 0xFFuy]     (* BE BOM *)
  in
  bom @ (let cps = extract_codepoints t.data in codepoints_to_utf16_bytes cps order)

(** Read two bytes as UTF-16 unit (little-endian) *)
let bytes_to_utf16_le (b0 b1: UInt8.t) : utf16_unit =
  let low = UInt8.v b0 in
  let high = UInt8.v b1 in
  UInt16.uint_to_t (high * 256 + low)

(** Read two bytes as UTF-16 unit (big-endian) *)
let bytes_to_utf16_be (b0 b1: UInt8.t) : utf16_unit =
  let high = UInt8.v b0 in
  let low = UInt8.v b1 in
  UInt16.uint_to_t (high * 256 + low)

(** Encode codepoint to UTF-8 bytes *)
let encode_codepoint_utf8 (cp: nat) : list UInt8.t =
  if cp <= 0x7F then
    [UInt8.uint_to_t cp]
  else if cp <= 0x7FF then
    let b0 = 0xC0 + (cp / 64) in
    let b1 = 0x80 + (cp % 64) in
    [UInt8.uint_to_t b0; UInt8.uint_to_t b1]
  else if cp <= 0xFFFF then
    let b0 = 0xE0 + (cp / 4096) in
    let b1 = 0x80 + ((cp / 64) % 64) in
    let b2 = 0x80 + (cp % 64) in
    [UInt8.uint_to_t b0; UInt8.uint_to_t b1; UInt8.uint_to_t b2]
  else if cp <= 0x10FFFF then
    let b0 = 0xF0 + (cp / 262144) in
    let b1 = 0x80 + ((cp / 4096) % 64) in
    let b2 = 0x80 + ((cp / 64) % 64) in
    let b3 = 0x80 + (cp % 64) in
    [UInt8.uint_to_t b0; UInt8.uint_to_t b1; UInt8.uint_to_t b2; UInt8.uint_to_t b3]
  else
    []  (* Invalid *)

(** Decode UTF-16 bytes to codepoints (little-endian) *)
let rec decode_utf16_le (bytes: list UInt8.t) : Tot (list nat) (decreases (length bytes)) =
  match bytes with
  | [] -> []
  | [_] -> []  (* Incomplete *)
  | b0 :: b1 :: rest ->
    let unit = bytes_to_utf16_le b0 b1 in
    if is_high_surrogate unit then
      match rest with
      | b2 :: b3 :: rest' ->
        let low_unit = bytes_to_utf16_le b2 b3 in
        if is_low_surrogate low_unit then
          decode_surrogate_pair unit low_unit :: decode_utf16_le rest'
        else
          (* Invalid surrogate pair - skip high surrogate *)
          decode_utf16_le rest
      | _ -> []  (* Incomplete surrogate pair *)
    else if is_low_surrogate unit then
      (* Orphan low surrogate - skip *)
      decode_utf16_le rest
    else
      UInt16.v unit :: decode_utf16_le rest

(** Decode UTF-16 bytes to codepoints (big-endian) *)
let rec decode_utf16_be (bytes: list UInt8.t) : Tot (list nat) (decreases (length bytes)) =
  match bytes with
  | [] -> []
  | [_] -> []
  | b0 :: b1 :: rest ->
    let unit = bytes_to_utf16_be b0 b1 in
    if is_high_surrogate unit then
      match rest with
      | b2 :: b3 :: rest' ->
        let low_unit = bytes_to_utf16_be b2 b3 in
        if is_low_surrogate low_unit then
          decode_surrogate_pair unit low_unit :: decode_utf16_be rest'
        else
          decode_utf16_be rest
      | _ -> []
    else if is_low_surrogate unit then
      decode_utf16_be rest
    else
      UInt16.v unit :: decode_utf16_be rest

(** Convert codepoints to UTF-8 bytes *)
let rec codepoints_to_utf8 (cps: list nat) : Tot (list UInt8.t) (decreases (length cps)) =
  match cps with
  | [] -> []
  | cp :: rest ->
    if cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF) then
      codepoints_to_utf8 rest  (* Skip invalid *)
    else
      encode_codepoint_utf8 cp @ codepoints_to_utf8 rest

(** Convert UTF-16 bytes to text (little-endian) *)
let utf16_to_text (bytes: list UInt8.t) : option text =
  let cps = decode_utf16_le bytes in
  let utf8_bytes = codepoints_to_utf8 cps in
  text_from_bytes utf8_bytes

(** Convert UTF-16 bytes to text (big-endian) *)
let utf16_to_text_be (bytes: list UInt8.t) : option text =
  let cps = decode_utf16_be bytes in
  let utf8_bytes = codepoints_to_utf8 cps in
  text_from_bytes utf8_bytes

(** Detect byte order from BOM and convert *)
let utf16_to_text_auto (bytes: list UInt8.t) : option text =
  match bytes with
  | 0xFFuy :: 0xFEuy :: rest -> utf16_to_text rest      (* LE BOM *)
  | 0xFEuy :: 0xFFuy :: rest -> utf16_to_text_be rest   (* BE BOM *)
  | _ -> utf16_to_text bytes                             (* Default to LE *)

(** Get UTF-16 length (number of code units) *)
let text_utf16_length (t: text) : nat =
  let cps = extract_codepoints t.data in
  let rec count_units (cps: list nat) : nat =
    match cps with
    | [] -> 0
    | cp :: rest ->
      let units = if cp >= 0x10000 && cp <= 0x10FFFF then 2 else 1 in
      units + count_units rest
  in
  count_units cps
