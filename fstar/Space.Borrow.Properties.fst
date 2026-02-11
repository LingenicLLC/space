module Space.Borrow.Properties

(** Verified properties of borrowing *)

open Space.Types
open Space.Universe
open Space.Borrow

(** Deactivated borrow is not active *)
let deactivate_not_active (b: borrowed) :
  Lemma (not (borrow_active (deactivate b))) =
  ()

(** Created borrow is active *)
let create_is_active (addr: cell) (src: universe_id) :
  Lemma (borrow_active (create_borrowed addr src)) =
  ()

(** Borrow source is preserved *)
let borrow_source_preserved (addr: cell) (src: universe_id) :
  Lemma ((create_borrowed addr src).source_id = src) =
  ()

(** Borrow address is preserved *)
let borrow_address_preserved (addr: cell) (src: universe_id) :
  Lemma ((create_borrowed addr src).address = addr) =
  ()

(** Deactivation preserves address *)
let deactivate_preserves_address (b: borrowed) :
  Lemma ((deactivate b).address = b.address) =
  ()

(** Deactivation preserves source *)
let deactivate_preserves_source (b: borrowed) :
  Lemma ((deactivate b).source_id = b.source_id) =
  ()

(** Empty borrow state has no active borrows *)
let empty_has_no_borrows (src: universe_id) :
  Lemma (not (has_active_borrows empty_borrow_state src)) =
  ()

(** Adding borrow increases count *)
let add_borrow_count (bs: borrow_state) (b: borrowed) :
  Lemma (requires b.is_active)
        (ensures count_borrows_from (add_borrow bs b).borrows b.source_id >=
                 count_borrows_from bs.borrows b.source_id) =
  ()

(** Finding created borrow succeeds *)
let find_created_borrow (addr: cell) (src: universe_id) :
  Lemma (let b = create_borrowed addr src in
         let bs = add_borrow empty_borrow_state b in
         Some? (find_borrow bs.borrows addr src)) =
  ()

(** Deactivated borrow cannot be found *)
let deactivated_not_found (b: borrowed) (bs: borrow_state) :
  Lemma (requires not b.is_active)
        (ensures None? (find_borrow [b] b.address b.source_id)) =
  ()

(** Remove borrow decreases or equals count *)
let remove_decreases_count (bs: list borrowed) (addr: cell) (src: universe_id) :
  Lemma (count_borrows_from (remove_borrow bs addr src) src <=
         count_borrows_from bs src) =
  let rec aux (bs: list borrowed) :
    Lemma (count_borrows_from (remove_borrow bs addr src) src <=
           count_borrows_from bs src) =
    match bs with
    | [] -> ()
    | _ :: rest -> aux rest
  in aux bs

(** Borrow from different source doesn't affect count *)
let count_different_source (bs: list borrowed) (src1 src2: universe_id) (b: borrowed) :
  Lemma (requires b.source_id = src1 && src1 <> src2)
        (ensures count_borrows_from (b :: bs) src2 = count_borrows_from bs src2) =
  ()

(** Return requires matching source *)
let return_requires_source_match (b: borrowed) (src: universe) :
  Lemma (requires b.source_id <> src.id)
        (ensures None? (return_borrowed b src)) =
  ()

(** Return requires active borrow *)
let return_requires_active (b: borrowed) (src: universe) :
  Lemma (requires not b.is_active)
        (ensures None? (return_borrowed b src)) =
  ()

(** Borrow pointer requires live universe *)
let borrow_requires_live (src: universe) :
  Lemma (requires not (is_live src))
        (ensures None? (borrow_pointer src)) =
  ()
