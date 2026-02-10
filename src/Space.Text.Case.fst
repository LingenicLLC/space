module Space.Text.Case

(** Unicode case mapping: upper, lower, title *)

open FStar.UInt8
open FStar.List.Tot
open Space.Text.Types
open Space.Text.UTF8
open Space.Text.Create

(** Case mapping type *)
type case_type =
  | Uppercase
  | Lowercase
  | Titlecase

(** Simple case mapping entry (1-to-1) *)
noeq type simple_case_mapping = {
  codepoint: nat;
  uppercase: nat;
  lowercase: nat;
  titlecase: nat;
}

(** Special case mapping (1-to-many, context-dependent) *)
noeq type special_case_mapping = {
  codepoint: nat;
  condition: string;  (* e.g., "Final_Sigma", "After_I", etc. *)
  uppercase: list nat;
  lowercase: list nat;
  titlecase: list nat;
}

(** Get simple uppercase mapping (placeholder - uses UCD tables) *)
let simple_uppercase (cp: nat) : nat =
  (* ASCII range *)
  if cp >= 0x61 && cp <= 0x7A then cp - 32  (* a-z -> A-Z *)
  (* Latin-1 Supplement *)
  else if cp >= 0xE0 && cp <= 0xF6 then cp - 32  (* à-ö -> À-Ö *)
  else if cp >= 0xF8 && cp <= 0xFE then cp - 32  (* ø-þ -> Ø-Þ *)
  (* Latin Extended-A (selected) *)
  else if cp = 0x0101 then 0x0100  (* ā -> Ā *)
  else if cp = 0x0103 then 0x0102  (* ă -> Ă *)
  else if cp = 0x0105 then 0x0104  (* ą -> Ą *)
  else if cp = 0x0107 then 0x0106  (* ć -> Ć *)
  else if cp = 0x0109 then 0x0108  (* ĉ -> Ĉ *)
  (* Greek (selected) *)
  else if cp >= 0x03B1 && cp <= 0x03C1 then cp - 32  (* α-ρ -> Α-Ρ *)
  else if cp >= 0x03C3 && cp <= 0x03C9 then cp - 32  (* σ-ω -> Σ-Ω *)
  else if cp = 0x03C2 then 0x03A3  (* ς (final sigma) -> Σ *)
  (* Cyrillic (selected) *)
  else if cp >= 0x0430 && cp <= 0x044F then cp - 32  (* а-я -> А-Я *)
  (* German sharp s *)
  else if cp = 0x00DF then cp  (* ß - special case, handled separately *)
  else cp  (* No mapping *)

(** Get simple lowercase mapping (placeholder - uses UCD tables) *)
let simple_lowercase (cp: nat) : nat =
  (* ASCII range *)
  if cp >= 0x41 && cp <= 0x5A then cp + 32  (* A-Z -> a-z *)
  (* Latin-1 Supplement *)
  else if cp >= 0xC0 && cp <= 0xD6 then cp + 32  (* À-Ö -> à-ö *)
  else if cp >= 0xD8 && cp <= 0xDE then cp + 32  (* Ø-Þ -> ø-þ *)
  (* Latin Extended-A (selected) *)
  else if cp = 0x0100 then 0x0101  (* Ā -> ā *)
  else if cp = 0x0102 then 0x0103  (* Ă -> ă *)
  else if cp = 0x0104 then 0x0105  (* Ą -> ą *)
  else if cp = 0x0106 then 0x0107  (* Ć -> ć *)
  else if cp = 0x0108 then 0x0109  (* Ĉ -> ĉ *)
  (* Greek (selected) *)
  else if cp >= 0x0391 && cp <= 0x03A1 then cp + 32  (* Α-Ρ -> α-ρ *)
  else if cp >= 0x03A3 && cp <= 0x03A9 then cp + 32  (* Σ-Ω -> σ-ω *)
  (* Cyrillic (selected) *)
  else if cp >= 0x0410 && cp <= 0x042F then cp + 32  (* А-Я -> а-я *)
  else cp  (* No mapping *)

(** Get simple titlecase mapping (usually same as uppercase) *)
let simple_titlecase (cp: nat) : nat =
  (* Most titlecase = uppercase *)
  (* Special cases for digraphs like Dž, Lj, Nj *)
  if cp = 0x01C6 then 0x01C5  (* dž -> Dž *)
  else if cp = 0x01C9 then 0x01C8  (* lj -> Lj *)
  else if cp = 0x01CC then 0x01CB  (* nj -> Nj *)
  else if cp = 0x01F3 then 0x01F2  (* dz -> Dz *)
  else simple_uppercase cp

(** Check if codepoint has case *)
let has_case (cp: nat) : bool =
  simple_uppercase cp <> cp || simple_lowercase cp <> cp

(** Check if codepoint is uppercase *)
let is_uppercase (cp: nat) : bool =
  has_case cp && simple_uppercase cp = cp

(** Check if codepoint is lowercase *)
let is_lowercase (cp: nat) : bool =
  has_case cp && simple_lowercase cp = cp

(** Get full uppercase mapping (may expand to multiple codepoints) *)
let full_uppercase (cp: nat) : list nat =
  (* German sharp s expands *)
  if cp = 0x00DF then [0x0053; 0x0053]  (* ß -> SS *)
  (* Turkish dotted i *)
  else if cp = 0x0069 then [0x0049]  (* i -> I (default, not Turkish) *)
  (* Ligatures *)
  else if cp = 0xFB00 then [0x0046; 0x0046]  (* ff -> FF *)
  else if cp = 0xFB01 then [0x0046; 0x0049]  (* fi -> FI *)
  else if cp = 0xFB02 then [0x0046; 0x004C]  (* fl -> FL *)
  else if cp = 0xFB03 then [0x0046; 0x0046; 0x0049]  (* ffi -> FFI *)
  else if cp = 0xFB04 then [0x0046; 0x0046; 0x004C]  (* ffl -> FFL *)
  else [simple_uppercase cp]

(** Get full lowercase mapping *)
let full_lowercase (cp: nat) : list nat =
  (* Capital I with dot *)
  if cp = 0x0130 then [0x0069; 0x0307]  (* İ -> i + combining dot *)
  else [simple_lowercase cp]

(** Get full titlecase mapping *)
let full_titlecase (cp: nat) : list nat =
  (* Digraph titlecase *)
  if cp = 0x01C4 then [0x01C5]  (* DŽ -> Dž *)
  else if cp = 0x01C7 then [0x01C8]  (* LJ -> Lj *)
  else if cp = 0x01CA then [0x01CB]  (* NJ -> Nj *)
  else if cp = 0x01F1 then [0x01F2]  (* DZ -> Dz *)
  else [simple_titlecase cp]

(** Extract codepoints from UTF-8 bytes *)
let rec bytes_to_codepoints (bytes: list UInt8.t) : Tot (list nat) (decreases (length bytes)) =
  match bytes with
  | [] -> []
  | b0 :: rest ->
    let len = sequence_length b0 in
    if len = 0 then bytes_to_codepoints rest
    else if len = 1 then
      UInt8.v b0 :: bytes_to_codepoints rest
    else if len = 2 then
      match rest with
      | b1 :: rest' -> decode_codepoint_2 b0 b1 :: bytes_to_codepoints rest'
      | _ -> []
    else if len = 3 then
      match rest with
      | b1 :: b2 :: rest' -> decode_codepoint_3 b0 b1 b2 :: bytes_to_codepoints rest'
      | _ -> []
    else
      match rest with
      | b1 :: b2 :: b3 :: rest' -> decode_codepoint_4 b0 b1 b2 b3 :: bytes_to_codepoints rest'
      | _ -> []

(** Encode codepoint to UTF-8 bytes *)
let encode_codepoint (cp: nat) : list UInt8.t =
  if cp <= 0x7F then
    [UInt8.uint_to_t cp]
  else if cp <= 0x7FF then
    [UInt8.uint_to_t (0xC0 + cp / 64);
     UInt8.uint_to_t (0x80 + cp % 64)]
  else if cp <= 0xFFFF then
    [UInt8.uint_to_t (0xE0 + cp / 4096);
     UInt8.uint_to_t (0x80 + (cp / 64) % 64);
     UInt8.uint_to_t (0x80 + cp % 64)]
  else if cp <= 0x10FFFF then
    [UInt8.uint_to_t (0xF0 + cp / 262144);
     UInt8.uint_to_t (0x80 + (cp / 4096) % 64);
     UInt8.uint_to_t (0x80 + (cp / 64) % 64);
     UInt8.uint_to_t (0x80 + cp % 64)]
  else []

(** Encode codepoints to UTF-8 bytes *)
let codepoints_to_bytes (cps: list nat) : list UInt8.t =
  List.Tot.concatMap encode_codepoint cps

(** Map codepoints with given case function *)
let map_case (cps: list nat) (f: nat -> list nat) : list nat =
  List.Tot.concatMap f cps

(** Convert text to uppercase *)
let text_to_upper (t: text) : option text =
  let cps = bytes_to_codepoints t.data in
  let upper_cps = map_case cps full_uppercase in
  let bytes = codepoints_to_bytes upper_cps in
  text_from_bytes bytes

(** Convert text to lowercase *)
let text_to_lower (t: text) : option text =
  let cps = bytes_to_codepoints t.data in
  let lower_cps = map_case cps full_lowercase in
  let bytes = codepoints_to_bytes lower_cps in
  text_from_bytes bytes

(** Convert text to titlecase (first letter of each word) *)
let text_to_title (t: text) : option text =
  let cps = bytes_to_codepoints t.data in
  (* Simple titlecase: uppercase first, lowercase rest *)
  (* Real implementation would use word boundaries from UAX #29 *)
  let rec titlecase_aux (cps: list nat) (at_start: bool) : list nat =
    match cps with
    | [] -> []
    | cp :: rest ->
      let is_letter = has_case cp in
      let is_space = cp = 0x20 || cp = 0x09 || cp = 0x0A || cp = 0x0D in
      if at_start && is_letter then
        full_titlecase cp @ titlecase_aux rest false
      else if is_space then
        [cp] @ titlecase_aux rest true
      else if is_letter then
        full_lowercase cp @ titlecase_aux rest false
      else
        [cp] @ titlecase_aux rest at_start
  in
  let title_cps = titlecase_aux cps true in
  let bytes = codepoints_to_bytes title_cps in
  text_from_bytes bytes

(** Case-insensitive comparison (casefold both, then compare) *)
let text_casefold (t: text) : option text =
  (* Casefolding is similar to lowercase but with additional mappings *)
  (* For simplicity, use lowercase as approximation *)
  text_to_lower t

(** Case-insensitive equality *)
let text_equal_ignore_case (t1 t2: text) : bool =
  match (text_casefold t1, text_casefold t2) with
  | (Some f1, Some f2) ->
    f1.header.byte_length = f2.header.byte_length && f1.data = f2.data
  | _ -> false

(** Check if text is all uppercase *)
let text_is_upper (t: text) : bool =
  let cps = bytes_to_codepoints t.data in
  List.Tot.for_all (fun cp -> not (has_case cp) || is_uppercase cp) cps

(** Check if text is all lowercase *)
let text_is_lower (t: text) : bool =
  let cps = bytes_to_codepoints t.data in
  List.Tot.for_all (fun cp -> not (has_case cp) || is_lowercase cp) cps

(** Capitalize first character only *)
let text_capitalize (t: text) : option text =
  let cps = bytes_to_codepoints t.data in
  match cps with
  | [] -> Some t
  | first :: rest ->
    let cap_cps = full_titlecase first @ map_case rest full_lowercase in
    let bytes = codepoints_to_bytes cap_cps in
    text_from_bytes bytes

(** Swap case (upper -> lower, lower -> upper) *)
let text_swapcase (t: text) : option text =
  let cps = bytes_to_codepoints t.data in
  let swap cp =
    if is_uppercase cp then full_lowercase cp
    else if is_lowercase cp then full_uppercase cp
    else [cp]
  in
  let swapped = map_case cps swap in
  let bytes = codepoints_to_bytes swapped in
  text_from_bytes bytes
