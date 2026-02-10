module Space.Text.Case

(** Unicode case mapping: upper, lower, title *)

open FStar.UInt8
open FStar.List.Tot
open Space.Text.Types
open Space.Text.UTF8
open Space.Text.Create
open Space.Text.UCD.Types
open Space.Text.UCD.Case

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

(** Get simple uppercase mapping using UCD tables *)
let simple_uppercase (cp: nat) : nat =
  match lookup_mapping cp uppercase_mappings with
  | Some mapped -> mapped
  | None -> cp  (* No mapping = identity *)

(** Get simple lowercase mapping using UCD tables *)
let simple_lowercase (cp: nat) : nat =
  match lookup_mapping cp lowercase_mappings with
  | Some mapped -> mapped
  | None -> cp  (* No mapping = identity *)

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

(** Lookup special casing entry *)
let rec lookup_special_casing (cp: nat) (table: list special_case_entry) : option special_case_entry =
  match table with
  | [] -> None
  | entry :: rest ->
    if entry.codepoint = cp then Some entry
    else lookup_special_casing cp rest

(** Get full uppercase mapping using UCD special casing table *)
let full_uppercase (cp: nat) : list nat =
  match lookup_special_casing cp special_casing_table with
  | Some entry ->
    if Cons? entry.upper then entry.upper
    else [simple_uppercase cp]
  | None -> [simple_uppercase cp]

(** Get full lowercase mapping using UCD special casing table *)
let full_lowercase (cp: nat) : list nat =
  match lookup_special_casing cp special_casing_table with
  | Some entry ->
    if Cons? entry.lower then entry.lower
    else [simple_lowercase cp]
  | None -> [simple_lowercase cp]

(** Get full titlecase mapping using UCD special casing table *)
let full_titlecase (cp: nat) : list nat =
  match lookup_special_casing cp special_casing_table with
  | Some entry ->
    if Cons? entry.title then entry.title
    else [simple_titlecase cp]
  | None -> [simple_titlecase cp]

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

(** Check if codepoint is a word boundary character (simplified UAX #29) *)
let is_word_boundary (cp: nat) : bool =
  (* Whitespace *)
  cp = 0x20 || cp = 0x09 || cp = 0x0A || cp = 0x0D || cp = 0x0B || cp = 0x0C ||
  (* No-break space *)
  cp = 0xA0 || cp = 0x2007 || cp = 0x202F ||
  (* Various spaces *)
  (cp >= 0x2000 && cp <= 0x200A) ||
  (* Line/paragraph separators *)
  cp = 0x2028 || cp = 0x2029 ||
  (* Punctuation as word boundaries *)
  cp = 0x2D || cp = 0x2014 || cp = 0x2013 ||  (* Hyphens, dashes *)
  cp = 0x27 || cp = 0x2019 ||                   (* Apostrophes - usually not boundaries, but common *)
  (* Common punctuation *)
  cp = 0x2E || cp = 0x2C || cp = 0x3B || cp = 0x3A ||  (* . , ; : *)
  cp = 0x21 || cp = 0x3F ||                             (* ! ? *)
  cp = 0x28 || cp = 0x29 ||                             (* ( ) *)
  cp = 0x5B || cp = 0x5D ||                             (* [ ] *)
  cp = 0x7B || cp = 0x7D ||                             (* { } *)
  cp = 0x22 || cp = 0x201C || cp = 0x201D ||            (* Quotes *)
  cp = 0x2F || cp = 0x5C                                (* / \ *)

(** Check if character continues a word (letters and apostrophes) *)
let is_word_continue (cp: nat) : bool =
  has_case cp ||
  cp = 0x27 || cp = 0x2019 ||  (* Apostrophe can be mid-word *)
  (cp >= 0x30 && cp <= 0x39)   (* Digits can be in words *)

(** Convert text to titlecase (first letter of each word) *)
let text_to_title (t: text) : option text =
  let cps = bytes_to_codepoints t.data in
  (* Titlecase: uppercase first cased letter after word boundary, lowercase rest *)
  let rec titlecase_aux (cps: list nat) (at_word_start: bool) : list nat =
    match cps with
    | [] -> []
    | cp :: rest ->
      let is_cased = has_case cp in
      let is_boundary = is_word_boundary cp in
      let continues_word = is_word_continue cp in
      if at_word_start && is_cased then
        (* First cased letter after boundary: titlecase it *)
        full_titlecase cp @ titlecase_aux rest false
      else if is_boundary then
        (* At word boundary: next cased char starts new word *)
        [cp] @ titlecase_aux rest true
      else if is_cased then
        (* Mid-word cased letter: lowercase it *)
        full_lowercase cp @ titlecase_aux rest false
      else if continues_word then
        (* Non-cased character that continues word (digit, apostrophe) *)
        [cp] @ titlecase_aux rest false
      else
        (* Other characters: pass through, maintain word start state *)
        [cp] @ titlecase_aux rest at_word_start
  in
  let title_cps = titlecase_aux cps true in
  let bytes = codepoints_to_bytes title_cps in
  text_from_bytes bytes

(** Get case folding for a codepoint - differs from lowercase for special cases *)
let casefold_codepoint (cp: nat) : list nat =
  (* Special case folding mappings from CaseFolding.txt *)
  (* These differ from simple lowercase *)
  if cp = 0x00DF then [0x0073; 0x0073]  (* ß -> ss *)
  else if cp = 0x0130 then [0x0069; 0x0307]  (* İ -> i + combining dot above *)
  else if cp = 0x0149 then [0x02BC; 0x006E]  (* ʼn -> ʼn *)
  else if cp = 0x01F0 then [0x006A; 0x030C]  (* ǰ -> j + caron *)
  else if cp = 0x0390 then [0x03B9; 0x0308; 0x0301]  (* ΐ -> ι + diaeresis + acute *)
  else if cp = 0x03B0 then [0x03C5; 0x0308; 0x0301]  (* ΰ -> υ + diaeresis + acute *)
  else if cp = 0x1E96 then [0x0068; 0x0331]  (* ẖ -> h + line below *)
  else if cp = 0x1E97 then [0x0074; 0x0308]  (* ẗ -> t + diaeresis *)
  else if cp = 0x1E98 then [0x0077; 0x030A]  (* ẘ -> w + ring above *)
  else if cp = 0x1E99 then [0x0079; 0x030A]  (* ẙ -> y + ring above *)
  else if cp = 0x1E9A then [0x0061; 0x02BE]  (* ẚ -> a + right half ring *)
  else if cp = 0x1E9E then [0x0073; 0x0073]  (* ẞ -> ss (capital sharp s) *)
  else if cp = 0x1F50 then [0x03C5; 0x0313]  (* ὐ -> υ + comma above *)
  else if cp = 0x1F52 then [0x03C5; 0x0313; 0x0300]  (* ὒ *)
  else if cp = 0x1F54 then [0x03C5; 0x0313; 0x0301]  (* ὔ *)
  else if cp = 0x1F56 then [0x03C5; 0x0313; 0x0342]  (* ὖ *)
  else if cp = 0x1F80 then [0x1F00; 0x03B9]  (* ᾀ -> ἀι *)
  else if cp = 0x1F81 then [0x1F01; 0x03B9]  (* ᾁ -> ἁι *)
  else if cp = 0x1F82 then [0x1F02; 0x03B9]  (* ᾂ -> ἂι *)
  else if cp = 0x1F83 then [0x1F03; 0x03B9]  (* ᾃ -> ἃι *)
  else if cp = 0x1F84 then [0x1F04; 0x03B9]  (* ᾄ -> ἄι *)
  else if cp = 0x1F85 then [0x1F05; 0x03B9]  (* ᾅ -> ἅι *)
  else if cp = 0x1F86 then [0x1F06; 0x03B9]  (* ᾆ -> ἆι *)
  else if cp = 0x1F87 then [0x1F07; 0x03B9]  (* ᾇ -> ἇι *)
  else if cp = 0x1F88 then [0x1F00; 0x03B9]  (* ᾈ -> ἀι *)
  else if cp = 0x1F89 then [0x1F01; 0x03B9]  (* ᾉ -> ἁι *)
  else if cp = 0x1F8A then [0x1F02; 0x03B9]  (* ᾊ -> ἂι *)
  else if cp = 0x1F8B then [0x1F03; 0x03B9]  (* ᾋ -> ἃι *)
  else if cp = 0x1F8C then [0x1F04; 0x03B9]  (* ᾌ -> ἄι *)
  else if cp = 0x1F8D then [0x1F05; 0x03B9]  (* ᾍ -> ἅι *)
  else if cp = 0x1F8E then [0x1F06; 0x03B9]  (* ᾎ -> ἆι *)
  else if cp = 0x1F8F then [0x1F07; 0x03B9]  (* ᾏ -> ἇι *)
  else if cp = 0x1FB2 then [0x1F70; 0x03B9]  (* ᾲ -> ὰι *)
  else if cp = 0x1FB3 then [0x03B1; 0x03B9]  (* ᾳ -> αι *)
  else if cp = 0x1FB4 then [0x03AC; 0x03B9]  (* ᾴ -> άι *)
  else if cp = 0x1FB6 then [0x03B1; 0x0342]  (* ᾶ *)
  else if cp = 0x1FB7 then [0x03B1; 0x0342; 0x03B9]  (* ᾷ *)
  else if cp = 0x1FBC then [0x03B1; 0x03B9]  (* ᾼ -> αι *)
  else if cp = 0x1FC2 then [0x1F74; 0x03B9]  (* ῂ -> ὴι *)
  else if cp = 0x1FC3 then [0x03B7; 0x03B9]  (* ῃ -> ηι *)
  else if cp = 0x1FC4 then [0x03AE; 0x03B9]  (* ῄ -> ήι *)
  else if cp = 0x1FC6 then [0x03B7; 0x0342]  (* ῆ *)
  else if cp = 0x1FC7 then [0x03B7; 0x0342; 0x03B9]  (* ῇ *)
  else if cp = 0x1FCC then [0x03B7; 0x03B9]  (* ῌ -> ηι *)
  else if cp = 0x1FD2 then [0x03B9; 0x0308; 0x0300]  (* ῒ *)
  else if cp = 0x1FD3 then [0x03B9; 0x0308; 0x0301]  (* ΐ *)
  else if cp = 0x1FD6 then [0x03B9; 0x0342]  (* ῖ *)
  else if cp = 0x1FD7 then [0x03B9; 0x0308; 0x0342]  (* ῗ *)
  else if cp = 0x1FE2 then [0x03C5; 0x0308; 0x0300]  (* ῢ *)
  else if cp = 0x1FE3 then [0x03C5; 0x0308; 0x0301]  (* ΰ *)
  else if cp = 0x1FE4 then [0x03C1; 0x0313]  (* ῤ *)
  else if cp = 0x1FE6 then [0x03C5; 0x0342]  (* ῦ *)
  else if cp = 0x1FE7 then [0x03C5; 0x0308; 0x0342]  (* ῧ *)
  else if cp = 0x1FF2 then [0x1F7C; 0x03B9]  (* ῲ -> ὼι *)
  else if cp = 0x1FF3 then [0x03C9; 0x03B9]  (* ῳ -> ωι *)
  else if cp = 0x1FF4 then [0x03CE; 0x03B9]  (* ῴ -> ώι *)
  else if cp = 0x1FF6 then [0x03C9; 0x0342]  (* ῶ *)
  else if cp = 0x1FF7 then [0x03C9; 0x0342; 0x03B9]  (* ῷ *)
  else if cp = 0x1FFC then [0x03C9; 0x03B9]  (* ῼ -> ωι *)
  else if cp = 0xFB00 then [0x0066; 0x0066]  (* ﬀ -> ff *)
  else if cp = 0xFB01 then [0x0066; 0x0069]  (* ﬁ -> fi *)
  else if cp = 0xFB02 then [0x0066; 0x006C]  (* ﬂ -> fl *)
  else if cp = 0xFB03 then [0x0066; 0x0066; 0x0069]  (* ﬃ -> ffi *)
  else if cp = 0xFB04 then [0x0066; 0x0066; 0x006C]  (* ﬄ -> ffl *)
  else if cp = 0xFB05 then [0x0073; 0x0074]  (* ﬅ -> st *)
  else if cp = 0xFB06 then [0x0073; 0x0074]  (* ﬆ -> st *)
  else if cp = 0x017F then [0x0073]  (* ſ -> s (long s) *)
  else
    (* Fall back to simple lowercase for most characters *)
    [simple_lowercase cp]

(** Case-insensitive comparison (casefold both, then compare) *)
let text_casefold (t: text) : option text =
  let cps = bytes_to_codepoints t.data in
  let folded = List.Tot.concatMap casefold_codepoint cps in
  let bytes = codepoints_to_bytes folded in
  text_from_bytes bytes

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
