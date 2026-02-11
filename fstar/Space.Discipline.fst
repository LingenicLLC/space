module Space.Discipline

(** Discipline rules and enforcement *)

open Space.Types

(** Can values of this discipline be copied? *)
let can_copy (d: discipline) : bool =
  match d with
  | Unrestricted -> true
  | Affine -> false
  | Linear -> false

(** Can values of this discipline be dropped? *)
let can_drop (d: discipline) : bool =
  match d with
  | Unrestricted -> true
  | Affine -> true
  | Linear -> false

(** Does this discipline require explicit release? *)
let requires_release (d: discipline) : bool =
  match d with
  | Unrestricted -> false
  | Affine -> true
  | Linear -> false

(** Does this discipline self-destruct when empty? *)
let self_destructs (d: discipline) : bool =
  match d with
  | Unrestricted -> false
  | Affine -> false
  | Linear -> true

(** Is discipline more restrictive than another? *)
let more_restrictive (d1 d2: discipline) : bool =
  match d1, d2 with
  | Linear, _ -> true
  | Affine, Unrestricted -> true
  | _, _ -> false
