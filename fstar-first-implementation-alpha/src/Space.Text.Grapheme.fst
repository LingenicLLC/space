module Space.Text.Grapheme

(** Grapheme cluster break algorithm (UAX #29) *)

open FStar.UInt8
open Space.Text.UTF8
open Space.Text.UCD.Types
open Space.Text.UCD.CCC

(** Grapheme break property categories (from UAX #29) *)
type grapheme_break_property =
  | GBP_Other
  | GBP_CR
  | GBP_LF
  | GBP_Control
  | GBP_Extend
  | GBP_ZWJ
  | GBP_Regional_Indicator
  | GBP_Prepend
  | GBP_SpacingMark
  | GBP_L    (* Hangul Leading Jamo *)
  | GBP_V    (* Hangul Vowel Jamo *)
  | GBP_T    (* Hangul Trailing Jamo *)
  | GBP_LV   (* Hangul LV Syllable *)
  | GBP_LVT  (* Hangul LVT Syllable *)
  | GBP_Extended_Pictographic

(** Break state for stateful rules *)
noeq type break_state = {
  prev_gbp: grapheme_break_property;
  in_emoji_seq: bool;
  ri_count: nat;  (* Regional indicator count for pairs *)
}

(** Initial break state *)
let initial_state : break_state = {
  prev_gbp = GBP_Other;
  in_emoji_seq = false;
  ri_count = 0;
}

(** Check if codepoint has non-zero combining class using UCD table *)
let rec has_ccc_aux (table: list ccc_entry) (cp: nat) : bool =
  match table with
  | [] -> false
  | e :: rest -> if e.codepoint = cp then e.ccc > 0 else has_ccc_aux rest cp

let has_combining_class (cp: nat) : bool =
  has_ccc_aux combining_class_table cp

(** Check if codepoint is in Extend category (combining marks, etc.) *)
let is_extend (cp: nat) : bool =
  (* Combining Diacritical Marks *)
  (cp >= 0x0300 && cp <= 0x036F) ||
  (* Combining Diacritical Marks Extended *)
  (cp >= 0x1AB0 && cp <= 0x1AFF) ||
  (* Combining Diacritical Marks Supplement *)
  (cp >= 0x1DC0 && cp <= 0x1DFF) ||
  (* Combining Diacritical Marks for Symbols *)
  (cp >= 0x20D0 && cp <= 0x20FF) ||
  (* Combining Half Marks *)
  (cp >= 0xFE20 && cp <= 0xFE2F) ||
  (* Arabic combining marks *)
  (cp >= 0x064B && cp <= 0x065F) ||
  (* Hebrew combining marks *)
  (cp >= 0x0591 && cp <= 0x05BD) || cp = 0x05BF || (cp >= 0x05C1 && cp <= 0x05C2) ||
  cp = 0x05C4 || cp = 0x05C5 || cp = 0x05C7 ||
  (* Devanagari combining marks *)
  (cp >= 0x0901 && cp <= 0x0903) || (cp >= 0x093A && cp <= 0x094F) ||
  (cp >= 0x0951 && cp <= 0x0957) || (cp >= 0x0962 && cp <= 0x0963) ||
  (* Thai combining marks *)
  (cp >= 0x0E31 && cp <= 0x0E3A) || (cp >= 0x0E47 && cp <= 0x0E4E) ||
  (* Variation Selectors *)
  (cp >= 0xFE00 && cp <= 0xFE0F) ||
  (* Variation Selectors Supplement *)
  (cp >= 0xE0100 && cp <= 0xE01EF) ||
  (* Emoji Modifiers *)
  (cp >= 0x1F3FB && cp <= 0x1F3FF) ||
  (* CGJ and other format chars *)
  cp = 0x034F || cp = 0x200C || cp = 0x200D ||
  (* Fallback to CCC table for other combining marks *)
  has_combining_class cp

(** Check if codepoint is Extended_Pictographic *)
let is_extended_pictographic (cp: nat) : bool =
  (* Miscellaneous Symbols and Pictographs *)
  (cp >= 0x1F300 && cp <= 0x1F5FF) ||
  (* Emoticons *)
  (cp >= 0x1F600 && cp <= 0x1F64F) ||
  (* Transport and Map Symbols *)
  (cp >= 0x1F680 && cp <= 0x1F6FF) ||
  (* Supplemental Symbols and Pictographs *)
  (cp >= 0x1F900 && cp <= 0x1F9FF) ||
  (* Symbols and Pictographs Extended-A *)
  (cp >= 0x1FA00 && cp <= 0x1FA6F) ||
  (* Symbols and Pictographs Extended-B *)
  (cp >= 0x1FA70 && cp <= 0x1FAFF) ||
  (* Dingbats *)
  (cp >= 0x2700 && cp <= 0x27BF) ||
  (* Misc symbols *)
  (cp >= 0x2600 && cp <= 0x26FF) ||
  (* Common emoji *)
  cp = 0x2764 || cp = 0x2763 || cp = 0x2665 || cp = 0x2666 ||
  cp = 0x2660 || cp = 0x2663 || cp = 0x2615 || cp = 0x231A ||
  cp = 0x231B || cp = 0x23E9 || cp = 0x23EA || cp = 0x23F0 ||
  cp = 0x23F3 || cp = 0x2328 || cp = 0x260E

(** Check if codepoint is SpacingMark *)
let is_spacing_mark (cp: nat) : bool =
  (* Thai vowel signs that are spacing *)
  (cp >= 0x0E40 && cp <= 0x0E44) ||
  (* Lao vowel signs *)
  (cp >= 0x0EC0 && cp <= 0x0EC4) ||
  (* Tibetan *)
  (cp >= 0x0F3E && cp <= 0x0F3F) ||
  (* Myanmar *)
  (cp >= 0x1031 && cp <= 0x1031) ||
  (* Bengali, etc. spacing marks *)
  (cp >= 0x09BE && cp <= 0x09C4) ||
  (* Devanagari spacing marks *)
  (cp >= 0x093E && cp <= 0x0940)

(** Grapheme break property lookup using Unicode data *)
let gbp_from_codepoint (cp: nat) : grapheme_break_property =
  (* CR, LF *)
  if cp = 0x0D then GBP_CR
  else if cp = 0x0A then GBP_LF
  (* Control characters *)
  else if cp < 0x20 then GBP_Control
  else if (cp >= 0x7F && cp <= 0x9F) then GBP_Control
  else if cp = 0x2028 || cp = 0x2029 then GBP_Control  (* Line/Paragraph separator *)
  (* ZWJ *)
  else if cp = 0x200D then GBP_ZWJ
  (* Regional Indicators *)
  else if cp >= 0x1F1E6 && cp <= 0x1F1FF then GBP_Regional_Indicator
  (* Hangul Jamo *)
  else if cp >= 0x1100 && cp <= 0x115F then GBP_L
  else if cp >= 0xA960 && cp <= 0xA97C then GBP_L  (* Hangul Jamo Extended-A *)
  else if cp >= 0x1160 && cp <= 0x11A7 then GBP_V
  else if cp >= 0xD7B0 && cp <= 0xD7C6 then GBP_V  (* Hangul Jamo Extended-B *)
  else if cp >= 0x11A8 && cp <= 0x11FF then GBP_T
  else if cp >= 0xD7CB && cp <= 0xD7FB then GBP_T  (* Hangul Jamo Extended-B *)
  (* Hangul syllables *)
  else if cp >= 0xAC00 && cp <= 0xD7A3 then
    let idx = cp - 0xAC00 in
    if idx % 28 = 0 then GBP_LV else GBP_LVT
  (* Extended_Pictographic *)
  else if is_extended_pictographic cp then GBP_Extended_Pictographic
  (* SpacingMark *)
  else if is_spacing_mark cp then GBP_SpacingMark
  (* Extend *)
  else if is_extend cp then GBP_Extend
  (* Prepend - limited set *)
  else if (cp >= 0x0600 && cp <= 0x0605) || cp = 0x06DD || cp = 0x070F then GBP_Prepend
  else if cp = 0x0890 || cp = 0x0891 || cp = 0x08E2 then GBP_Prepend
  else if cp = 0x110BD || cp = 0x110CD then GBP_Prepend
  (* Default *)
  else GBP_Other

(** Should we break between two grapheme break properties? *)
let should_break (left right: grapheme_break_property) (state: break_state) : bool * break_state =
  (* GB1, GB2: Break at start and end - handled externally *)

  (* GB3: Do not break between CR and LF *)
  if left = GBP_CR && right = GBP_LF then
    (false, { state with prev_gbp = right })

  (* GB4, GB5: Break before and after controls *)
  else if left = GBP_Control || left = GBP_CR || left = GBP_LF then
    (true, { state with prev_gbp = right; in_emoji_seq = false; ri_count = 0 })
  else if right = GBP_Control || right = GBP_CR || right = GBP_LF then
    (true, { state with prev_gbp = right; in_emoji_seq = false; ri_count = 0 })

  (* GB6-8: Hangul syllable sequences *)
  else if left = GBP_L && (right = GBP_L || right = GBP_V || right = GBP_LV || right = GBP_LVT) then
    (false, { state with prev_gbp = right })
  else if (left = GBP_LV || left = GBP_V) && (right = GBP_V || right = GBP_T) then
    (false, { state with prev_gbp = right })
  else if (left = GBP_LVT || left = GBP_T) && right = GBP_T then
    (false, { state with prev_gbp = right })

  (* GB9: Do not break before extending characters or ZWJ *)
  else if right = GBP_Extend || right = GBP_ZWJ then
    (false, { state with prev_gbp = right; in_emoji_seq = state.in_emoji_seq || left = GBP_Extended_Pictographic })

  (* GB9a: Do not break before SpacingMarks *)
  else if right = GBP_SpacingMark then
    (false, { state with prev_gbp = right })

  (* GB9b: Do not break after Prepend *)
  else if left = GBP_Prepend then
    (false, { state with prev_gbp = right })

  (* GB11: Extended Pictographic + (Extend | ZWJ)* + ZWJ + Extended Pictographic *)
  else if state.in_emoji_seq && left = GBP_ZWJ && right = GBP_Extended_Pictographic then
    (false, { state with prev_gbp = right; in_emoji_seq = true })

  (* GB12, GB13: Regional Indicators - break after pairs *)
  else if left = GBP_Regional_Indicator && right = GBP_Regional_Indicator then
    let new_count = state.ri_count + 1 in
    if new_count % 2 = 0 then
      (true, { state with prev_gbp = right; ri_count = 0 })
    else
      (false, { state with prev_gbp = right; ri_count = new_count })

  (* GB999: Otherwise, break *)
  else
    (true, { state with prev_gbp = right; in_emoji_seq = right = GBP_Extended_Pictographic; ri_count = 0 })
