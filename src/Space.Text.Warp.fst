module Space.Text.Warp

(** Text warps for structured traversal *)

open FStar.UInt8
open FStar.UInt64
open Space.Types
open Space.Text.Types
open Space.Text.Iter

(** Text warp direction *)
type text_direction =
  | Forward
  | Backward

(** Text warp state *)
noeq type text_warp = {
  iter: text_iter;
  direction: text_direction;
  start_pos: nat;
  end_pos: nat;
}

(** Create forward warp over entire text *)
let text_warp_begin (t: text) : text_warp = {
  iter = iter_begin t;
  direction = Forward;
  start_pos = 0;
  end_pos = t.header.grapheme_count;
}

(** Create warp over range [start, finish) *)
let text_warp_range (t: text) (start finish: nat) : option text_warp =
  if finish < start then None
  else if finish > t.header.grapheme_count then None
  else Some {
    iter = iter_begin t;  (* Would need to advance to start *)
    direction = Forward;
    start_pos = start;
    end_pos = finish;
  }

(** Check if warp is exhausted *)
let text_warp_done (w: text_warp) : bool =
  match w.direction with
  | Forward -> iter_position w.iter >= w.end_pos
  | Backward -> iter_position w.iter <= w.start_pos

(** Get current position in warp *)
let text_warp_position (w: text_warp) : nat =
  iter_position w.iter

(** Advance warp and get current grapheme *)
let text_warp_next (w: text_warp) : option (grapheme * text_warp) =
  if text_warp_done w then None
  else match w.direction with
  | Forward ->
    (match iter_next w.iter with
     | None -> None
     | Some (g, it') -> Some (g, { w with iter = it' }))
  | Backward -> None  (* Backward iteration not yet implemented *)

(** Get remaining grapheme count *)
let text_warp_remaining (w: text_warp) : nat =
  if text_warp_done w then 0
  else match w.direction with
  | Forward -> w.end_pos - iter_position w.iter
  | Backward -> iter_position w.iter - w.start_pos

(** Collect graphemes from warp *)
let rec text_warp_collect (w: text_warp) (fuel: nat) : Tot (list grapheme) (decreases fuel) =
  if fuel = 0 then []
  else if text_warp_done w then []
  else match text_warp_next w with
    | None -> []
    | Some (g, w') -> g :: text_warp_collect w' (fuel - 1)

(** Convert text warp result to cell (for stack) *)
let grapheme_to_cell (g: grapheme) : cell =
  (* Return first codepoint as cell value, or 0 if empty *)
  match g.bytes with
  | [] -> UInt64.uint_to_t 0
  | b :: _ -> UInt64.uint_to_t (UInt8.v b)

(** Get current grapheme without advancing (warp-current-grapheme) *)
let text_warp_current (w: text_warp) : option grapheme =
  if text_warp_done w then None
  else match iter_next w.iter with
  | None -> None
  | Some (g, _) -> Some g  (* Return grapheme but don't update warp *)

(** Advance warp to specific position - O(n) sequential, O(1) for Simple text *)
let rec advance_to_position (it: text_iter) (target: nat) (fuel: nat)
  : Tot (option text_iter) (decreases fuel) =
  if fuel = 0 then None
  else if iter_position it >= target then Some it
  else match iter_next it with
    | None -> None
    | Some (_, it') -> advance_to_position it' target (fuel - 1)

(** Seek to grapheme position (warp-goto-grapheme) *)
let text_warp_goto (w: text_warp) (pos: nat) : option text_warp =
  if pos < w.start_pos then None
  else if pos > w.end_pos then None
  else
    let t = w.iter.text in
    (* For Simple text, grapheme index = byte index, so this is O(1) conceptually *)
    (* For Complex text with index, would also be O(1) with proper index lookup *)
    (* Current implementation: restart from beginning and advance *)
    let fresh_iter = iter_begin t in
    match advance_to_position fresh_iter pos (t.header.grapheme_count + 1) with
    | None -> None
    | Some it' -> Some { w with iter = it' }

(** Check if warp has more graphemes (warp-has-grapheme) *)
let text_warp_has_grapheme (w: text_warp) : bool =
  not (text_warp_done w)

