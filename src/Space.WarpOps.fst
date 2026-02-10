module Space.WarpOps

(** Memory operations through warps *)

open FStar.UInt64
open Space.Types
open Space.Stack
open Space.Universe
open Space.Memory
open Space.Warp

(** Universe with memory for warp operations *)
noeq type target_universe = {
  universe: universe;
  memory: memory;
}

(** Fetch at current warp position *)
let warp_fetch (w: warp) (tu: target_universe) : option cell =
  if not w.active then None
  else if tu.universe.id <> w.target_id then None
  else mem_fetch tu.memory (v w.position)

(** Store at current warp position *)
let warp_store (w: warp) (value: cell) (tu: target_universe) : option target_universe =
  if not w.active then None
  else if w.readonly then None
  else if tu.universe.id <> w.target_id then None
  else
    match mem_store tu.memory (v w.position) value with
    | None -> None
    | Some mem' -> Some { tu with memory = mem' }

(** Advance warp position by offset cells *)
let warp_advance (w: warp) (offset: cell) : warp =
  advance_position w offset

(** Follow pointer at current position (for linked structures) *)
let warp_follow (w: warp) (tu: target_universe) : option warp =
  if not w.active then None
  else
    match mem_fetch tu.memory (v w.position) with
    | None -> None
    | Some ptr -> Some (set_position w ptr)

(** Follow pointer at offset from current position *)
let warp_follow_offset (w: warp) (offset: cell) (tu: target_universe) : option warp =
  if not w.active then None
  else
    let addr = v (add_mod w.position offset) in
    match mem_fetch tu.memory addr with
    | None -> None
    | Some ptr -> Some (set_position w ptr)

(** Check if at null position *)
let warp_at_null (w: warp) : bool =
  is_null_position w

(** Save position and return token *)
let warp_save (w: warp) : warp * cell =
  (save_position w, w.position)

(** Restore position from token *)
let warp_restore (w: warp) (pos: cell) : option warp =
  restore_position w pos

(** Create warp from universe stack *)
let warp_into (name: string) (src: universe) (target_id: universe_id) (readonly: bool) (wt: warp_table)
  : option (warp_table * warp_id * universe) =
  if not (is_live src) then None
  else
    match universe_pop src with
    | None -> None
    | Some (pos, src') ->
      let (wt', wid) = add_warp wt name target_id pos readonly in
      Some (wt', wid, src')

(** End warp and return updated table *)
let end_warp (wid: warp_id) (wt: warp_table) : option warp_table =
  match find_warp_by_id wt.warps wid with
  | None -> None
  | Some w ->
    let w' = close_warp w in
    Some (update_warp wt w')
