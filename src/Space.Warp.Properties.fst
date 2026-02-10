module Space.Warp.Properties

(** Verified properties of warps *)

open FStar.UInt64
open Space.Types
open Space.Warp

(** Created warp is active *)
let create_is_active (id: warp_id) (name: string) (target: universe_id) (pos: cell) (ro: bool) :
  Lemma (warp_active (create_warp id name target pos ro)) =
  ()

(** Closed warp is not active *)
let close_not_active (w: warp) :
  Lemma (not (warp_active (close_warp w))) =
  ()

(** Create preserves target *)
let create_target (id: warp_id) (name: string) (target: universe_id) (pos: cell) (ro: bool) :
  Lemma ((create_warp id name target pos ro).target_id = target) =
  ()

(** Create preserves position *)
let create_position (id: warp_id) (name: string) (target: universe_id) (pos: cell) (ro: bool) :
  Lemma ((create_warp id name target pos ro).position = pos) =
  ()

(** Advance changes position *)
let advance_changes_position (w: warp) (offset: cell) :
  Lemma ((advance_position w offset).position = add_mod w.position offset) =
  ()

(** Set position works *)
let set_position_works (w: warp) (pos: cell) :
  Lemma ((set_position w pos).position = pos) =
  ()

(** Save adds to saved positions *)
let save_adds_position (w: warp) :
  Lemma ((save_position w).saved_positions = w.position :: w.saved_positions) =
  ()

(** Close preserves target *)
let close_preserves_target (w: warp) :
  Lemma ((close_warp w).target_id = w.target_id) =
  ()

(** Null check is correct *)
let null_check_correct (w: warp) :
  Lemma (is_null_position w = (w.position = 0uL)) =
  ()

(** Empty warp table has no active warps *)
let empty_has_no_warps (target: universe_id) :
  Lemma (not (has_active_warps empty_warp_table target)) =
  ()

(** Close preserves readonly *)
let close_preserves_readonly (w: warp) :
  Lemma ((close_warp w).readonly = w.readonly) =
  ()

(** Close preserves id *)
let close_preserves_id (w: warp) :
  Lemma ((close_warp w).id = w.id) =
  ()

(** Create preserves readonly setting *)
let create_readonly (id: warp_id) (name: string) (target: universe_id) (pos: cell) (ro: bool) :
  Lemma ((create_warp id name target pos ro).readonly = ro) =
  ()

(** Advance preserves activity *)
let advance_preserves_active (w: warp) (offset: cell) :
  Lemma ((advance_position w offset).active = w.active) =
  ()

(** Advance preserves target *)
let advance_preserves_target (w: warp) (offset: cell) :
  Lemma ((advance_position w offset).target_id = w.target_id) =
  ()

(** Set position preserves activity *)
let set_position_preserves_active (w: warp) (pos: cell) :
  Lemma ((set_position w pos).active = w.active) =
  ()

(** Set position preserves target *)
let set_position_preserves_target (w: warp) (pos: cell) :
  Lemma ((set_position w pos).target_id = w.target_id) =
  ()

(** Save preserves position *)
let save_preserves_position (w: warp) :
  Lemma ((save_position w).position = w.position) =
  ()

(** Save preserves activity *)
let save_preserves_active (w: warp) :
  Lemma ((save_position w).active = w.active) =
  ()

(** Restore to saved position succeeds *)
let restore_saved_succeeds (w: warp) :
  Lemma (let w' = save_position w in
         Some? (restore_position w' w.position)) =
  ()

(** Count warps to different target is zero when none exist *)
let count_empty_is_zero (target: universe_id) :
  Lemma (count_warps_to [] target = 0) =
  ()

(** Add warp increases next_id *)
let add_warp_increases_id (wt: warp_table) (name: string) (target: universe_id) (pos: cell) (ro: bool) :
  Lemma (let (wt', _) = add_warp wt name target pos ro in
         wt'.next_id = wt.next_id + 1) =
  ()

(** Find by ID requires matching ID *)
let find_by_id_correct (w: warp) :
  Lemma (requires w.active)
        (ensures Some? (find_warp_by_id [w] w.id)) =
  ()

(** Find by name requires matching name *)
let find_by_name_correct (w: warp) :
  Lemma (requires w.active)
        (ensures Some? (find_warp_by_name [w] w.name)) =
  ()

(** Closed warp not found by ID *)
let closed_not_found_by_id (w: warp) :
  Lemma (let w' = close_warp w in
         None? (find_warp_by_id [w'] w'.id)) =
  ()

(** Closed warp not found by name *)
let closed_not_found_by_name (w: warp) :
  Lemma (let w' = close_warp w in
         None? (find_warp_by_name [w'] w'.name)) =
  ()

(** Count warps decreases when warp closed *)
let close_decreases_count (ws: list warp) (w: warp) (target: universe_id) :
  Lemma (requires w.active && w.target_id = target)
        (ensures count_warps_to (close_warp w :: ws) target =
                 count_warps_to ws target) =
  ()
