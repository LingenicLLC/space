open Prims
let can_copy (d : Space_Types.discipline) : Prims.bool=
  match d with
  | Space_Types.Unrestricted -> true
  | Space_Types.Affine -> false
  | Space_Types.Linear -> false
let can_drop (d : Space_Types.discipline) : Prims.bool=
  match d with
  | Space_Types.Unrestricted -> true
  | Space_Types.Affine -> true
  | Space_Types.Linear -> false
let requires_release (d : Space_Types.discipline) : Prims.bool=
  match d with
  | Space_Types.Unrestricted -> false
  | Space_Types.Affine -> true
  | Space_Types.Linear -> false
let self_destructs (d : Space_Types.discipline) : Prims.bool=
  match d with
  | Space_Types.Unrestricted -> false
  | Space_Types.Affine -> false
  | Space_Types.Linear -> true
let more_restrictive (d1 : Space_Types.discipline)
  (d2 : Space_Types.discipline) : Prims.bool=
  match (d1, d2) with
  | (Space_Types.Linear, uu___) -> true
  | (Space_Types.Affine, Space_Types.Unrestricted) -> true
  | (uu___, uu___1) -> false
