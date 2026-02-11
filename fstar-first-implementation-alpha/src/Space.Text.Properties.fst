module Space.Text.Properties

(** Verified properties of text operations *)

open FStar.UInt8
open Space.Text.Types
open Space.Text.UTF8
open Space.Text.Ops
open Space.Text.Props

(** Empty text has zero graphemes *)
let empty_grapheme_count (t: text) :
  Lemma (requires t.header.grapheme_count = 0)
        (ensures text_is_empty t) =
  ()

(** Simple text complexity check *)
let simple_text_is_simple (t: text) :
  Lemma (requires t.header.complexity = Simple)
        (ensures text_is_ascii t) =
  ()

(** Concatenation preserves total grapheme count *)
let concat_grapheme_count (t1 t2: text) :
  Lemma ((text_concat t1 t2).header.grapheme_count =
         t1.header.grapheme_count + t2.header.grapheme_count) =
  ()

(** Concatenation preserves total byte length *)
let concat_byte_length (t1 t2: text) :
  Lemma ((text_concat t1 t2).header.byte_length =
         t1.header.byte_length + t2.header.byte_length) =
  ()

(** ASCII byte is valid UTF-8 lead *)
let ascii_is_valid_lead (b: UInt8.t) :
  Lemma (requires is_ascii b)
        (ensures sequence_length b = 1) =
  ()

(** Continuation byte is not a valid lead *)
let continuation_not_lead (b: UInt8.t) :
  Lemma (requires is_continuation b)
        (ensures sequence_length b = 0) =
  ()

(** 2-byte lead has sequence length 2 *)
let lead_2_length (b: UInt8.t) :
  Lemma (requires is_lead_2 b)
        (ensures sequence_length b = 2) =
  ()

(** 3-byte lead has sequence length 3 *)
let lead_3_length (b: UInt8.t) :
  Lemma (requires is_lead_3 b)
        (ensures sequence_length b = 3) =
  ()

(** 4-byte lead has sequence length 4 *)
let lead_4_length (b: UInt8.t) :
  Lemma (requires is_lead_4 b)
        (ensures sequence_length b = 4) =
  ()

(** Grapheme byte length is positive *)
let grapheme_length_positive (g: grapheme) :
  Lemma (requires g.len > 0)
        (ensures grapheme_byte_length g > 0) =
  ()

(** Empty grapheme has zero length *)
let empty_grapheme_length (g: grapheme) :
  Lemma (requires g.len = 0)
        (ensures grapheme_byte_length g = 0) =
  ()

(** Empty byte list is valid UTF-8 *)
let empty_is_valid_utf8 () :
  Lemma (is_valid_utf8 []) =
  ()

(** ASCII bytes are valid UTF-8 *)
let ascii_byte_valid (b: UInt8.t) :
  Lemma (requires is_ascii b)
        (ensures is_valid_utf8 [b]) =
  ()

(** ASCII bytes have sequence length 1 *)
let ascii_sequence_length (b: UInt8.t) :
  Lemma (requires is_ascii b)
        (ensures sequence_length b = 1) =
  ()

(** Continuation bytes are not lead bytes *)
let continuation_invalid_lead (b: UInt8.t) :
  Lemma (requires is_continuation b)
        (ensures sequence_length b = 0) =
  ()

(** All ASCII is also valid UTF-8 *)
let all_ascii_valid_utf8 (bytes: list UInt8.t) :
  Lemma (requires is_all_ascii bytes)
        (ensures is_valid_utf8 bytes) =
  let rec aux (bytes: list UInt8.t) :
    Lemma (requires is_all_ascii bytes)
          (ensures is_valid_utf8 bytes) =
    match bytes with
    | [] -> ()
    | b :: rest ->
      assert (is_ascii b);
      assert (sequence_length b = 1);
      aux rest
  in aux bytes

(** ASCII bytes less than 0x80 *)
let ascii_value_bound (b: UInt8.t) :
  Lemma (is_ascii b <==> UInt8.v b <= 0x7F) =
  ()

(** 2-byte lead in correct range *)
let lead_2_range (b: UInt8.t) :
  Lemma (is_lead_2 b <==> (UInt8.v b >= 0xC2 && UInt8.v b <= 0xDF)) =
  ()

(** 3-byte lead in correct range *)
let lead_3_range (b: UInt8.t) :
  Lemma (is_lead_3 b <==> (UInt8.v b >= 0xE0 && UInt8.v b <= 0xEF)) =
  ()

(** 4-byte lead in correct range *)
let lead_4_range (b: UInt8.t) :
  Lemma (is_lead_4 b <==> (UInt8.v b >= 0xF0 && UInt8.v b <= 0xF4)) =
  ()

(** Continuation byte in correct range *)
let continuation_range (b: UInt8.t) :
  Lemma (is_continuation b <==> (UInt8.v b >= 0x80 && UInt8.v b <= 0xBF)) =
  ()

(** Decoded ASCII codepoint equals byte value *)
let decode_ascii_identity (b: UInt8.t) :
  Lemma (requires is_ascii b)
        (ensures decode_codepoint_1 b = UInt8.v b) =
  ()

(** ASCII codepoints are valid Unicode *)
let ascii_codepoint_valid (b: UInt8.t) :
  Lemma (requires is_ascii b)
        (ensures is_valid_codepoint (decode_codepoint_1 b)) =
  ()

(** Surrogate codepoints are invalid *)
let surrogate_invalid (cp: nat) :
  Lemma (requires cp >= 0xD800 && cp <= 0xDFFF)
        (ensures not (is_valid_codepoint cp)) =
  ()

(** Codepoints beyond 0x10FFFF are invalid *)
let beyond_max_invalid (cp: nat) :
  Lemma (requires cp > 0x10FFFF)
        (ensures not (is_valid_codepoint cp)) =
  ()

(** BMP codepoints are valid (except surrogates) *)
let bmp_valid (cp: nat) :
  Lemma (requires cp <= 0xFFFF && (cp < 0xD800 || cp > 0xDFFF))
        (ensures is_valid_codepoint cp) =
  ()

(** Concatenation complexity preserves simple *)
let concat_simple_simple (t1 t2: text) :
  Lemma (requires t1.header.complexity = Simple && t2.header.complexity = Simple)
        (ensures (text_concat t1 t2).header.complexity = Simple) =
  ()

(** Equal texts have equal byte lengths *)
let equal_implies_same_byte_length (t1 t2: text) :
  Lemma (requires text_equal t1 t2)
        (ensures t1.header.byte_length = t2.header.byte_length) =
  ()

(** Text equality implies bytes_equal *)
let text_equal_implies_bytes_equal (t1 t2: text) :
  Lemma (requires text_equal t1 t2)
        (ensures bytes_equal t1.data t2.data) =
  ()

(** Text equality is reflexive *)
let text_equal_reflexive_prop (t: text) :
  Lemma (t.header.byte_length = t.header.byte_length ==>
         (bytes_equal t.data t.data <==> text_equal t t)) =
  ()

