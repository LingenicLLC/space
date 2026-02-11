module Space.BorrowOps

(** Memory operations through borrowed pointers *)

open FStar.UInt64
open Space.Types
open Space.Stack
open Space.Universe
open Space.Memory
open Space.Borrow

(** Universe with memory *)
noeq type universe_mem = {
  universe: universe;
  memory: memory;
}

(** Fetch through borrowed pointer *)
let fetch_borrowed (b: borrowed) (um: universe_mem) : option cell =
  if not b.is_active then None
  else if um.universe.id <> b.source_id then None
  else mem_fetch um.memory (v b.address)

(** Store through borrowed pointer *)
let store_borrowed (b: borrowed) (value: cell) (um: universe_mem) : option universe_mem =
  if not b.is_active then None
  else if um.universe.id <> b.source_id then None
  else
    match mem_store um.memory (v b.address) value with
    | None -> None
    | Some mem' -> Some { um with memory = mem' }

(** Offset a borrowed pointer *)
let offset_borrowed (b: borrowed) (offset: cell) : borrowed =
  { b with address = add_mod b.address offset }

(** Fetch and end borrow - returns value and deactivated borrow *)
let fetch_and_end (b: borrowed) (um: universe_mem) : option (cell * borrowed) =
  match fetch_borrowed b um with
  | None -> None
  | Some v -> Some (v, deactivate b)

(** Store and end borrow - writes value and ends borrow *)
let store_and_end (b: borrowed) (value: cell) (um: universe_mem) : option (universe_mem * borrowed) =
  match store_borrowed b value um with
  | None -> None
  | Some um' -> Some (um', deactivate b)
