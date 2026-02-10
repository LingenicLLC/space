module Space.Borrow

(** Borrowed pointers for cross-universe access *)

open FStar.UInt64
open Space.Types
open Space.Stack
open Space.Universe

(** Borrowed pointer - tracks source universe *)
noeq type borrowed = {
  address: cell;
  source_id: universe_id;
  is_active: bool;
}

(** Create a borrowed pointer *)
let create_borrowed (addr: cell) (src: universe_id) : borrowed = {
  address = addr;
  source_id = src;
  is_active = true;
}

(** Deactivate a borrowed pointer *)
let deactivate (b: borrowed) : borrowed =
  { b with is_active = false }

(** Check if borrow is active *)
let borrow_active (b: borrowed) : bool =
  b.is_active

(** Borrow state - tracks all active borrows *)
noeq type borrow_state = {
  borrows: list borrowed;
}

(** Empty borrow state *)
let empty_borrow_state : borrow_state = {
  borrows = [];
}

(** Add a borrow *)
let add_borrow (bs: borrow_state) (b: borrowed) : borrow_state =
  { borrows = b :: bs.borrows }

(** Find borrow by address and source *)
let rec find_borrow (bs: list borrowed) (addr: cell) (src: universe_id) : option borrowed =
  match bs with
  | [] -> None
  | b :: rest ->
    if b.address = addr && b.source_id = src && b.is_active
    then Some b
    else find_borrow rest addr src

(** Remove a borrow (mark inactive) *)
let rec remove_borrow (bs: list borrowed) (addr: cell) (src: universe_id) : list borrowed =
  match bs with
  | [] -> []
  | b :: rest ->
    if b.address = addr && b.source_id = src
    then deactivate b :: rest
    else b :: remove_borrow rest addr src

(** Count active borrows from a universe *)
let rec count_borrows_from (bs: list borrowed) (src: universe_id) : nat =
  match bs with
  | [] -> 0
  | b :: rest ->
    let count = count_borrows_from rest src in
    if b.source_id = src && b.is_active then count + 1 else count

(** Check if universe has any active borrows *)
let has_active_borrows (bs: borrow_state) (src: universe_id) : bool =
  count_borrows_from bs.borrows src > 0

(** Borrow a pointer from source universe *)
let borrow_pointer (src: universe) : option (borrowed * universe) =
  if not (is_live src) then None
  else
    match universe_pop src with
    | None -> None
    | Some (addr, src') ->
      let b = create_borrowed addr src.id in
      Some (b, src')

(** Drop borrowed pointer (end borrow) *)
let drop_borrowed (bs: borrow_state) (b: borrowed) : borrow_state =
  { borrows = remove_borrow bs.borrows b.address b.source_id }

(** Return borrowed pointer to source stack *)
let return_borrowed (b: borrowed) (src: universe) : option universe =
  if not (is_live src) then None
  else if not b.is_active then None
  else if src.id <> b.source_id then None
  else universe_push src b.address
