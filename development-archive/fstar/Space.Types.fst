module Space.Types

(** Basic types for Space language *)

open FStar.UInt64

(** A cell is 64 bits on all platforms *)
type cell = UInt64.t

(** Universe identifier *)
type universe_id = nat

(** Universe name - lexically scoped identifier *)
type universe_name = string

(** Discipline controls how values can be used *)
type discipline =
  | Unrestricted  (* Can copy and drop freely *)
  | Affine        (* Cannot copy, can drop *)
  | Linear        (* Cannot copy, cannot drop - must consume exactly once *)
