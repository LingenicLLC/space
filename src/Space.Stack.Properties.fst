module Space.Stack.Properties

(** Verified properties of stack operations *)

open Space.Types
open Space.Stack

(** Push then pop returns original value and stack *)
let push_pop_identity (s: stack) (v: cell) :
  Lemma (pop (push s v) == Some (v, s)) =
  ()

(** Push increases size by 1 *)
let push_size (s: stack) (v: cell) :
  Lemma (size (push s v) == size s + 1) =
  ()

(** Pop decreases size by 1 *)
let pop_size (s: stack) :
  Lemma (requires not (is_empty s))
        (ensures (match pop s with
                  | Some (_, s') -> size s' == size s - 1
                  | None -> False)) =
  ()

(** Empty stack has size 0 *)
let empty_size () :
  Lemma (size empty == 0) =
  ()

(** Dup increases size by 1 *)
let dup_size (s: stack) :
  Lemma (requires not (is_empty s))
        (ensures (match dup s with
                  | Some s' -> size s' == size s + 1
                  | None -> False)) =
  ()

(** Swap preserves size *)
let swap_size (s: stack) :
  Lemma (requires size s >= 2)
        (ensures (match swap s with
                  | Some s' -> size s' == size s
                  | None -> False)) =
  ()
