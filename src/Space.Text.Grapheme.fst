module Space.Text.Grapheme

(** Grapheme cluster break algorithm (UAX #29 simplified) *)

open FStar.UInt8
open Space.Text.UTF8

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

(** Simple grapheme break property lookup (for common cases) *)
let gbp_from_codepoint (cp: nat) : grapheme_break_property =
  (* Simplified - real implementation needs full Unicode tables *)
  if cp = 0x0D then GBP_CR
  else if cp = 0x0A then GBP_LF
  else if cp < 0x20 then GBP_Control
  else if cp = 0x200D then GBP_ZWJ
  else if cp >= 0x1F1E6 && cp <= 0x1F1FF then GBP_Regional_Indicator
  else if cp >= 0x0300 && cp <= 0x036F then GBP_Extend  (* Combining diacriticals *)
  else if cp >= 0x1F600 && cp <= 0x1F64F then GBP_Extended_Pictographic  (* Emoticons *)
  else if cp >= 0x1F300 && cp <= 0x1F5FF then GBP_Extended_Pictographic  (* Misc symbols *)
  else if cp >= 0xAC00 && cp <= 0xD7A3 then  (* Hangul syllables *)
    let idx = cp - 0xAC00 in
    if idx % 28 = 0 then GBP_LV else GBP_LVT
  else if cp >= 0x1100 && cp <= 0x115F then GBP_L
  else if cp >= 0x1160 && cp <= 0x11A7 then GBP_V
  else if cp >= 0x11A8 && cp <= 0x11FF then GBP_T
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
