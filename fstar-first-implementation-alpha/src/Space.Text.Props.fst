module Space.Text.Props

(** Grapheme and text properties *)

open FStar.UInt8
open Space.Text.Types
open Space.Text.UTF8

(** Get byte length of a grapheme *)
let grapheme_byte_length (g: grapheme) : nat =
  g.len

(** Check if grapheme is single ASCII character *)
let grapheme_is_ascii (g: grapheme) : bool =
  g.len = 1 &&
  (match g.bytes with
   | [b] -> is_ascii b
   | _ -> false)

(** Drop first n elements from list *)
let rec list_drop (n: nat) (xs: list UInt8.t) : Tot (list UInt8.t) (decreases n) =
  if n = 0 then xs
  else match xs with
    | [] -> []
    | _ :: rest -> list_drop (n - 1) rest

(** Count codepoints in grapheme bytes using fuel *)
let rec count_codepoints_aux (bytes: list UInt8.t) (fuel: nat) : Tot nat (decreases fuel) =
  if fuel = 0 then 0
  else match bytes with
  | [] -> 0
  | b0 :: rest ->
    let len = sequence_length b0 in
    if len = 0 then 0  (* Invalid *)
    else 1 + count_codepoints_aux (list_drop len (b0 :: rest)) (fuel - 1)

(** Count codepoints in grapheme bytes *)
let count_codepoints (bytes: list UInt8.t) : nat =
  count_codepoints_aux bytes (List.Tot.length bytes)

(** Get codepoint count in grapheme *)
let grapheme_codepoint_count (g: grapheme) : nat =
  count_codepoints g.bytes

(** Check if text is empty *)
let text_is_empty (t: text) : bool =
  t.header.grapheme_count = 0

(** Check if text contains only ASCII *)
let text_is_ascii (t: text) : bool =
  t.header.complexity = Simple

(** Get first byte of text (if non-empty) *)
let text_first_byte (t: text) : option UInt8.t =
  match t.data with
  | [] -> None
  | b :: _ -> Some b

(** Get last byte of text (if non-empty) *)
let rec list_last (xs: list UInt8.t) : option UInt8.t =
  match xs with
  | [] -> None
  | [x] -> Some x
  | _ :: rest -> list_last rest

let text_last_byte (t: text) : option UInt8.t =
  list_last t.data
