module Space.Warp

(** Warps for structured traversal of universes *)

open FStar.UInt64
open Space.Types
open Space.Stack
open Space.Universe

(** Warp identifier *)
type warp_id = nat

(** Warp state *)
noeq type warp = {
  id: warp_id;
  name: string;
  target_id: universe_id;
  position: cell;
  saved_positions: list cell;
  readonly: bool;
  active: bool;
}

(** Create a new warp *)
let create_warp (id: warp_id) (name: string) (target: universe_id) (pos: cell) (ro: bool) : warp = {
  id = id;
  name = name;
  target_id = target;
  position = pos;
  saved_positions = [];
  readonly = ro;
  active = true;
}

(** Check if warp is active *)
let warp_active (w: warp) : bool =
  w.active

(** Close a warp *)
let close_warp (w: warp) : warp =
  { w with active = false }

(** Get current position *)
let warp_position (w: warp) : cell =
  w.position

(** Set position *)
let set_position (w: warp) (pos: cell) : warp =
  { w with position = pos }

(** Advance position by offset *)
let advance_position (w: warp) (offset: cell) : warp =
  { w with position = add_mod w.position offset }

(** Save current position *)
let save_position (w: warp) : warp =
  { w with saved_positions = w.position :: w.saved_positions }

(** Restore a saved position *)
let rec find_saved (positions: list cell) (pos: cell) : bool =
  match positions with
  | [] -> false
  | p :: rest -> if p = pos then true else find_saved rest pos

let restore_position (w: warp) (pos: cell) : option warp =
  if find_saved w.saved_positions pos
  then Some { w with position = pos }
  else None

(** Check if position is null (zero) *)
let is_null_position (w: warp) : bool =
  w.position = 0uL

(** Warp table for managing multiple warps *)
noeq type warp_table = {
  warps: list warp;
  next_id: warp_id;
}

(** Empty warp table *)
let empty_warp_table : warp_table = {
  warps = [];
  next_id = 0;
}

(** Find warp by ID *)
let rec find_warp_by_id (ws: list warp) (id: warp_id) : option warp =
  match ws with
  | [] -> None
  | w :: rest -> if w.id = id && w.active then Some w else find_warp_by_id rest id

(** Find warp by name *)
let rec find_warp_by_name (ws: list warp) (name: string) : option warp =
  match ws with
  | [] -> None
  | w :: rest -> if w.name = name && w.active then Some w else find_warp_by_name rest name

(** Add warp to table *)
let add_warp (wt: warp_table) (name: string) (target: universe_id) (pos: cell) (ro: bool) : warp_table * warp_id =
  let id = wt.next_id in
  let w = create_warp id name target pos ro in
  ({ warps = w :: wt.warps; next_id = id + 1 }, id)

(** Update warp in table *)
let rec update_warp_in_list (ws: list warp) (w: warp) : list warp =
  match ws with
  | [] -> []
  | x :: rest ->
    if x.id = w.id then w :: rest
    else x :: update_warp_in_list rest w

let update_warp (wt: warp_table) (w: warp) : warp_table =
  { wt with warps = update_warp_in_list wt.warps w }

(** Count active warps targeting a universe *)
let rec count_warps_to (ws: list warp) (target: universe_id) : nat =
  match ws with
  | [] -> 0
  | w :: rest ->
    let count = count_warps_to rest target in
    if w.target_id = target && w.active then count + 1 else count

(** Check if universe has any active warps *)
let has_active_warps (wt: warp_table) (target: universe_id) : bool =
  count_warps_to wt.warps target > 0

(** Get implicit warp (if only one active) *)
let get_implicit_warp (wt: warp_table) : option warp =
  let rec get_single_active (ws: list warp) (found: option warp) : option warp =
    match ws with
    | [] -> found
    | w :: rest ->
      if w.active then
        match found with
        | None -> get_single_active rest (Some w)
        | Some _ -> None  (* More than one active *)
      else get_single_active rest found
  in
  get_single_active wt.warps None
