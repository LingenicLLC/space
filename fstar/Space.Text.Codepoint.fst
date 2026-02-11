module Space.Text.Codepoint

(** Code point access for text *)

open FStar.UInt8
open FStar.UInt32
open Space.Text.Types
open Space.Text.UTF8

(** Code point as nat (Unicode scalar value 0-0x10FFFF) *)
type codepoint = nat

(** Drop n elements from list *)
let rec drop_n (n: nat) (xs: list UInt8.t) : Tot (list UInt8.t) (decreases n) =
  if n = 0 then xs
  else match xs with
    | [] -> []
    | _ :: rest -> drop_n (n - 1) rest

(** Count code points in UTF-8 byte sequence *)
let rec count_codepoints_aux (bytes: list UInt8.t) (fuel: nat) : Tot nat (decreases fuel) =
  if fuel = 0 then 0
  else match bytes with
  | [] -> 0
  | b0 :: rest ->
    let len = sequence_length b0 in
    if len = 0 then 0  (* Invalid sequence *)
    else
      let remaining = drop_n len (b0 :: rest) in
      1 + count_codepoints_aux remaining (fuel - 1)

(** Count code points in text *)
let text_codepoint_count (t: text) : nat =
  count_codepoints_aux t.data (List.Tot.length t.data)

(** Decode codepoint at position in byte list *)
let decode_at (bytes: list UInt8.t) : option (codepoint * nat) =
  match bytes with
  | [] -> None
  | b0 :: rest ->
    let len = sequence_length b0 in
    if len = 0 then None
    else if len = 1 then
      Some (decode_codepoint_1 b0, 1)
    else if len = 2 then
      match rest with
      | b1 :: _ -> Some (decode_codepoint_2 b0 b1, 2)
      | _ -> None
    else if len = 3 then
      match rest with
      | b1 :: b2 :: _ -> Some (decode_codepoint_3 b0 b1 b2, 3)
      | _ -> None
    else
      match rest with
      | b1 :: b2 :: b3 :: _ -> Some (decode_codepoint_4 b0 b1 b2 b3, 4)
      | _ -> None

(** Get nth code point from byte list - O(n) *)
let rec get_codepoint_at (bytes: list UInt8.t) (idx: nat) (fuel: nat)
  : Tot (option codepoint) (decreases fuel) =
  if fuel = 0 then None
  else match decode_at bytes with
  | None -> None
  | Some (cp, len) ->
    if idx = 0 then Some cp
    else
      let remaining = drop_n len bytes in
      get_codepoint_at remaining (idx - 1) (fuel - 1)

(** Get code point at index from text - O(n) *)
let text_codepoint_at (t: text) (idx: nat) : option codepoint =
  get_codepoint_at t.data idx (List.Tot.length t.data)

(** Iterator for code point access *)
noeq type codepoint_iter = {
  bytes: list UInt8.t;
  position: nat;
}

(** Create iterator at start of text *)
let codepoint_iter_begin (t: text) : codepoint_iter = {
  bytes = t.data;
  position = 0;
}

(** Check if at end *)
let codepoint_iter_at_end (it: codepoint_iter) : bool =
  match it.bytes with
  | [] -> true
  | _ -> false

(** Get current position *)
let codepoint_iter_position (it: codepoint_iter) : nat =
  it.position

(** Helper to drop n bytes *)
let rec iter_drop (n: nat) (xs: list UInt8.t) : Tot (list UInt8.t) (decreases n) =
  if n = 0 then xs
  else match xs with
    | [] -> []
    | _ :: rest -> iter_drop (n - 1) rest

(** Get next code point and advance iterator *)
let codepoint_iter_next (it: codepoint_iter) : option (codepoint * codepoint_iter) =
  match decode_at it.bytes with
  | None -> None
  | Some (cp, len) ->
    let remaining = iter_drop len it.bytes in
    Some (cp, {
      bytes = remaining;
      position = it.position + 1;
    })

(** Collect all code points *)
let rec collect_codepoints (it: codepoint_iter) (fuel: nat)
  : Tot (list codepoint) (decreases fuel) =
  if fuel = 0 then []
  else if codepoint_iter_at_end it then []
  else
    match codepoint_iter_next it with
    | None -> []
    | Some (cp, it') -> cp :: collect_codepoints it' (fuel - 1)

(** Get all code points from text *)
let text_codepoints (t: text) : list codepoint =
  let it = codepoint_iter_begin t in
  collect_codepoints it (List.Tot.length t.data)

