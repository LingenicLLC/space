module Space.Universe.Properties

(** Verified properties of universes and disciplines *)

open Space.Types
open Space.Stack
open Space.Discipline
open Space.Universe

(** Unrestricted discipline allows duplication *)
let unrestricted_can_copy (d: discipline) :
  Lemma (d = Unrestricted ==> can_copy d) =
  ()

(** Unrestricted discipline allows dropping *)
let unrestricted_can_drop (d: discipline) :
  Lemma (d = Unrestricted ==> can_drop d) =
  ()

(** Linear discipline forbids copying *)
let linear_cannot_copy (d: discipline) :
  Lemma (d = Linear ==> not (can_copy d)) =
  ()

(** Linear discipline forbids dropping *)
let linear_cannot_drop (d: discipline) :
  Lemma (d = Linear ==> not (can_drop d)) =
  ()

(** Affine discipline forbids copying *)
let affine_cannot_copy (d: discipline) :
  Lemma (d = Affine ==> not (can_copy d)) =
  ()

(** Affine discipline allows dropping *)
let affine_can_drop (d: discipline) :
  Lemma (d = Affine ==> can_drop d) =
  ()

(** Created universe is live *)
let create_is_live (id: universe_id) (name: universe_name) (cap: nat) (disc: discipline) :
  Lemma (is_live (create id name cap disc)) =
  ()

(** Created universe has empty stack *)
let create_empty_stack (id: universe_id) (name: universe_name) (cap: nat) (disc: discipline) :
  Lemma (stack_empty (create id name cap disc)) =
  ()

(** Destroyed universe is not live *)
let destroy_not_live (u: universe) :
  Lemma (not (is_live (destroy u))) =
  ()

(** Linear universe self-destructs when empty *)
let linear_self_destruct_condition (u: universe) :
  Lemma (should_self_destruct u <==>
         (u.discipline = Linear && stack_empty u && is_live u)) =
  ()

(** Push to live universe with space succeeds *)
let push_succeeds (u: universe) (v: cell) :
  Lemma (requires is_live u && has_space u)
        (ensures Some? (universe_push u v)) =
  ()

(** Pop from live non-empty universe succeeds *)
let pop_succeeds (u: universe) :
  Lemma (requires is_live u && not (stack_empty u))
        (ensures Some? (universe_pop u)) =
  ()

(** Release only works on affine universes *)
let release_affine_only (u: universe) :
  Lemma (requires u.discipline <> Affine)
        (ensures None? (release u)) =
  ()

(** Release on live affine universe succeeds *)
let release_affine_succeeds (u: universe) :
  Lemma (requires u.discipline = Affine && is_live u)
        (ensures Some? (release u)) =
  ()

(** Universe discipline is preserved through push *)
let push_preserves_discipline (u: universe) (v: cell) :
  Lemma (requires Some? (universe_push u v))
        (ensures (Some?.v (universe_push u v)).discipline = u.discipline) =
  ()

(** Universe discipline is preserved through pop *)
let pop_preserves_discipline (u: universe) :
  Lemma (requires Some? (universe_pop u))
        (ensures (match universe_pop u with
                  | Some (_, u') -> u'.discipline = u.discipline
                  | None -> True)) =
  ()

