module Space.Text.Iter

(** Text iteration / warp for sequential access *)

open FStar.UInt8
open Space.Text.Types
open Space.Text.UTF8
open Space.Text.Grapheme

(** Text iterator state *)
noeq type text_iter = {
  text: text;
  byte_pos: nat;
  grapheme_pos: nat;
  break_state: break_state;
}

(** Create iterator at start of text *)
let iter_begin (t: text) : text_iter = {
  text = t;
  byte_pos = 0;
  grapheme_pos = 0;
  break_state = initial_state;
}

(** Check if at end *)
let iter_at_end (it: text_iter) : bool =
  it.grapheme_pos >= it.text.header.grapheme_count

(** Get current position *)
let iter_position (it: text_iter) : nat =
  it.grapheme_pos

(** Helper: extract bytes for current codepoint *)
let rec take_bytes (bytes: list UInt8.t) (n: nat) : list UInt8.t =
  if n = 0 then []
  else match bytes with
    | [] -> []
    | b :: rest -> b :: take_bytes rest (n - 1)

let rec drop_bytes (bytes: list UInt8.t) (n: nat) : list UInt8.t =
  if n = 0 then bytes
  else match bytes with
    | [] -> []
    | _ :: rest -> drop_bytes rest (n - 1)

(** Advance iterator and return current grapheme *)
let rec scan_grapheme (bytes: list UInt8.t) (state: break_state) (acc: list UInt8.t)
  : Tot (option (grapheme * list UInt8.t * break_state)) (decreases (List.Tot.length bytes)) =
  match bytes with
  | [] ->
    if List.Tot.length acc > 0 then
      Some ({ bytes = acc; len = List.Tot.length acc }, [], state)
    else None
  | b0 :: rest ->
    let len = sequence_length b0 in
    if len = 0 then None
    else
      let cp_bytes = take_bytes bytes len in
      let remaining = drop_bytes bytes len in
      let cp = if len = 1 then decode_codepoint_1 b0
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
      let new_acc = List.Tot.append acc cp_bytes in
      if do_break && List.Tot.length acc > 0 then
        (* Break before this codepoint - return accumulated grapheme *)
        Some ({ bytes = acc; len = List.Tot.length acc }, bytes, { state with prev_gbp = gbp })
      else
        (* Continue accumulating *)
        scan_grapheme remaining new_state new_acc

(** Get next grapheme and advance iterator *)
let iter_next (it: text_iter) : option (grapheme * text_iter) =
  if iter_at_end it then None
  else
    let remaining = drop_bytes it.text.data it.byte_pos in
    match scan_grapheme remaining it.break_state [] with
    | None -> None
    | Some (g, rest, new_state) ->
      let new_pos = it.byte_pos + g.len in
      Some (g, {
        text = it.text;
        byte_pos = new_pos;
        grapheme_pos = it.grapheme_pos + 1;
        break_state = new_state;
      })

(** Collect all graphemes *)
let rec collect_all (it: text_iter) : Tot (list grapheme) (decreases (it.text.header.grapheme_count - it.grapheme_pos)) =
  if iter_at_end it then []
  else
    match iter_next it with
    | None -> []
    | Some (g, it') ->
      if it'.grapheme_pos > it.grapheme_pos then
        g :: collect_all it'
      else []  (* Safety: ensure progress *)
