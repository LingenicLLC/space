module Space.Text.Create

(** Text creation and grapheme indexing *)

open FStar.UInt8
open Space.Text.Types
open Space.Text.UTF8
open Space.Text.Grapheme

(** Drop first n elements from list *)
let rec drop_n (n: nat) (xs: list UInt8.t) : Tot (list UInt8.t) (decreases n) =
  if n = 0 then xs
  else match xs with
    | [] -> []
    | _ :: rest -> drop_n (n - 1) rest

(** Take first n elements from list *)
let rec take_n (n: nat) (xs: list UInt8.t) : Tot (list UInt8.t) (decreases n) =
  if n = 0 then []
  else match xs with
    | [] -> []
    | x :: rest -> x :: take_n (n - 1) rest

(** Result of analyzing bytes: grapheme count, is_simple, and index entries *)
noeq type analyze_result = {
  gcount: nat;
  is_simple: bool;
  entries: list grapheme_entry;
}

(** Build grapheme index while analyzing UTF-8 data *)
let rec build_index_aux (bytes: list UInt8.t) (state: break_state)
                        (byte_pos: nat) (current_start: nat) (current_len: nat)
                        (acc_entries: list grapheme_entry) (acc_count: nat) (acc_simple: bool)
  : Tot analyze_result (decreases (List.Tot.length bytes)) =
  match bytes with
  | [] ->
    (* End of data - emit final grapheme if any *)
    if current_len > 0 then
      let entry = { byte_offset = current_start; byte_len = current_len } in
      { gcount = acc_count + 1; is_simple = acc_simple; entries = List.Tot.append acc_entries [entry] }
    else
      { gcount = acc_count; is_simple = acc_simple; entries = acc_entries }
  | b0 :: rest ->
    let still_simple = acc_simple && is_ascii b0 in
    let len = sequence_length b0 in
    if len = 0 then
      (* Invalid - return what we have *)
      { gcount = acc_count; is_simple = false; entries = acc_entries }
    else
      (* Decode codepoint *)
      let cp =
        if len = 1 then UInt8.v b0
        else if len = 2 then
          match rest with
          | b1 :: _ -> decode_codepoint_2 b0 b1
          | _ -> 0
        else if len = 3 then
          match rest with
          | b1 :: b2 :: _ -> decode_codepoint_3 b0 b1 b2
          | _ -> 0
        else
          match rest with
          | b1 :: b2 :: b3 :: _ -> decode_codepoint_4 b0 b1 b2 b3
          | _ -> 0
      in
      let gbp = gbp_from_codepoint cp in
      let (do_break, new_state) = should_break state.prev_gbp gbp state in
      let remaining = drop_n len bytes in
      if do_break && current_len > 0 then
        (* Break here - emit current grapheme and start new one *)
        let entry = { byte_offset = current_start; byte_len = current_len } in
        let new_entries = List.Tot.append acc_entries [entry] in
        build_index_aux remaining new_state (byte_pos + len) byte_pos len
                        new_entries (acc_count + 1) still_simple
      else
        (* Continue current grapheme *)
        build_index_aux remaining new_state (byte_pos + len) current_start (current_len + len)
                        acc_entries acc_count still_simple

(** Build grapheme index from bytes *)
let build_index (bytes: list UInt8.t) : analyze_result =
  build_index_aux bytes initial_state 0 0 0 [] 0 true

(** Create text from byte list *)
let text_from_bytes (bytes: list UInt8.t) : option text =
  if not (is_valid_utf8 bytes) then None
  else
    let result = build_index bytes in
    let header = {
      grapheme_count = result.gcount;
      byte_length = List.Tot.length bytes;
      complexity = if result.is_simple then Simple else Complex;
    } in
    Some {
      header = header;
      index = if result.is_simple then [] else result.entries;
      data = bytes;
    }

(** Get byte at position *)
let byte_at (t: text) (pos: nat) : option UInt8.t =
  List.Tot.nth t.data pos

(** O(1) grapheme access for Simple text *)
let simple_grapheme_at (t: text) (idx: nat) : option grapheme =
  if idx >= t.header.grapheme_count then None
  else if t.header.complexity <> Simple then None
  else
    match byte_at t idx with
    | None -> None
    | Some b -> Some { bytes = [b]; len = 1 }

(** Get grapheme entry at index from index list *)
let rec get_entry (entries: list grapheme_entry) (idx: nat) : option grapheme_entry =
  match entries with
  | [] -> None
  | e :: rest ->
    if idx = 0 then Some e
    else get_entry rest (idx - 1)

(** Extract bytes from data at offset with length *)
let rec extract_bytes (data: list UInt8.t) (offset: nat) (len: nat) : list UInt8.t =
  if offset > 0 then
    match data with
    | [] -> []
    | _ :: rest -> extract_bytes rest (offset - 1) len
  else
    take_n len data

(** O(1) grapheme access for Complex text using index *)
let complex_grapheme_at (t: text) (idx: nat) : option grapheme =
  match get_entry t.index idx with
  | None -> None
  | Some entry ->
    let bytes = extract_bytes t.data entry.byte_offset entry.byte_len in
    Some { bytes = bytes; len = entry.byte_len }

(** Get grapheme at index (works for both Simple and Complex) - O(1) *)
let grapheme_at (t: text) (idx: nat) : option grapheme =
  if idx >= t.header.grapheme_count then None
  else if t.header.complexity = Simple then
    simple_grapheme_at t idx
  else
    (* Complex text - use index for O(1) lookup *)
    complex_grapheme_at t idx

(** Count graphemes in text *)
let text_grapheme_count (t: text) : nat =
  t.header.grapheme_count

(** Get byte length of text *)
let text_byte_length (t: text) : nat =
  t.header.byte_length

(** Check if text is simple (ASCII only) *)
let text_is_simple (t: text) : bool =
  t.header.complexity = Simple

(** Get first grapheme *)
let text_grapheme_first (t: text) : option grapheme =
  if t.header.grapheme_count = 0 then None
  else grapheme_at t 0

(** Get last grapheme *)
let text_grapheme_last (t: text) : option grapheme =
  if t.header.grapheme_count = 0 then None
  else grapheme_at t (t.header.grapheme_count - 1)
