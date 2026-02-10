module Space.Text.Types

(** Text types for grapheme-indexed strings *)

open FStar.UInt8
open FStar.UInt64
open Space.Types

(** Text complexity classification *)
type text_complexity =
  | Simple   (* ASCII only: grapheme = byte *)
  | Complex  (* Non-ASCII or multi-codepoint graphemes *)

(** Text header *)
noeq type text_header = {
  grapheme_count: nat;
  byte_length: nat;
  complexity: text_complexity;
}

(** Grapheme index entry - for Complex text *)
noeq type grapheme_entry = {
  byte_offset: nat;
  byte_len: nat;
}

(** A grapheme (single user-perceived character) *)
noeq type grapheme = {
  bytes: list UInt8.t;
  len: nat;
}

(** Empty grapheme *)
let empty_grapheme : grapheme = {
  bytes = [];
  len = 0;
}

(** Text representation *)
noeq type text = {
  header: text_header;
  index: list grapheme_entry;  (* Empty for Simple text *)
  data: list UInt8.t;          (* UTF-8 encoded data *)
}

(** Create empty text *)
let empty_text : text = {
  header = { grapheme_count = 0; byte_length = 0; complexity = Simple };
  index = [];
  data = [];
}

(** Check if text is simple (ASCII only) *)
let is_simple (t: text) : bool =
  t.header.complexity = Simple

(** Check if text is complex *)
let is_complex (t: text) : bool =
  t.header.complexity = Complex

(** Get grapheme count *)
let grapheme_count (t: text) : nat =
  t.header.grapheme_count

(** Get byte length *)
let byte_length (t: text) : nat =
  t.header.byte_length
