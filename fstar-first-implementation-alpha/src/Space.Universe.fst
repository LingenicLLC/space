module Space.Universe

(** Universe: isolated memory region with its own stack *)

open Space.Types
open Space.Stack
open Space.Memory

(** Universe state *)
type universe_state =
  | Live
  | Destroyed

(** A universe contains a stack, memory, discipline, and state *)
noeq type universe = {
  id: universe_id;
  name: universe_name;
  discipline: discipline;
  stack: stack;
  memory: memory;
  capacity: nat;
  state: universe_state;
}

(** Create a new universe *)
let create (id: universe_id) (name: universe_name) (cap: nat) (disc: discipline) : universe = {
  id = id;
  name = name;
  discipline = disc;
  stack = empty;
  memory = empty_memory;
  capacity = cap;
  state = Live;
}

(** Check if universe is live *)
let is_live (u: universe) : bool =
  match u.state with
  | Live -> true
  | Destroyed -> false

(** Check if universe stack has space *)
let has_space (u: universe) : bool =
  size u.stack < u.capacity

(** Check if universe stack is empty *)
let stack_empty (u: universe) : bool =
  is_empty u.stack

(** Push value to universe stack *)
let universe_push (u: universe) (v: cell) : option universe =
  if not (is_live u) then None
  else if not (has_space u) then None
  else Some { u with stack = push u.stack v }

(** Pop value from universe stack *)
let universe_pop (u: universe) : option (cell * universe) =
  if not (is_live u) then None
  else match pop u.stack with
  | None -> None
  | Some (v, s') -> Some (v, { u with stack = s' })

(** Check if linear universe should self-destruct *)
let should_self_destruct (u: universe) : bool =
  u.discipline = Linear && stack_empty u && is_live u

(** Destroy a universe *)
let destroy (u: universe) : universe =
  { u with state = Destroyed }

(** Release an affine universe (explicit destruction) *)
let release (u: universe) : option universe =
  match u.discipline with
  | Affine -> if is_live u then Some (destroy u) else None
  | _ -> None
