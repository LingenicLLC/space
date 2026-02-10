module Space.Memory.Properties

(** Verified memory safety properties *)

open FStar.UInt64
open Space.Types
open Space.Stack
open Space.Memory

(** Empty memory has size 0 *)
let empty_memory_size () :
  Lemma (mem_size empty_memory = 0) =
  ()

(** Allocation increases memory size *)
let alloc_increases_size (m: memory) (n: nat) :
  Lemma (requires n > 0)
        (ensures (let (m', _) = mem_alloc m n in
                  mem_size m' = mem_size m + n)) =
  ()

(** Fetch from invalid address returns None *)
let invalid_fetch_none (m: memory) (addr: nat) :
  Lemma (requires not (valid_addr m addr))
        (ensures None? (mem_fetch m addr)) =
  ()

(** Store to invalid address returns None *)
let invalid_store_none (m: memory) (addr: nat) (v: cell) :
  Lemma (requires not (valid_addr m addr))
        (ensures None? (mem_store m addr v)) =
  ()

(** Valid address within bounds *)
let valid_addr_bounds (m: memory) (addr: nat) :
  Lemma (requires valid_addr m addr)
        (ensures addr >= m.base && addr < m.base + mem_size m) =
  ()

(** Allocation returns fresh base address *)
let alloc_fresh_base (m: memory) (n: nat) :
  Lemma (let (m', base) = mem_alloc m n in
         base = m.base + mem_size m) =
  ()

(** Store then fetch returns stored value *)
let store_fetch_identity (m: memory) (addr: nat) (v: cell) :
  Lemma (requires valid_addr m addr)
        (ensures (match mem_store m addr v with
                  | Some m' -> mem_fetch m' addr = Some v
                  | None -> True)) =
  ()

(** Store preserves other addresses *)
let store_preserves_other (m: memory) (addr1 addr2: nat) (v: cell) :
  Lemma (requires valid_addr m addr1 && valid_addr m addr2 && addr1 <> addr2)
        (ensures (match mem_store m addr1 v with
                  | Some m' -> mem_fetch m' addr2 = mem_fetch m addr2
                  | None -> True)) =
  ()

(** Zeros creates correct length *)
let zeros_length (n: nat) :
  Lemma (List.Tot.length (zeros n) = n) =
  let rec aux (k: nat) : Lemma (List.Tot.length (zeros k) = k) [SMTPat (zeros k)] =
    if k = 0 then () else aux (k - 1)
  in aux n

(** Pointer conversion round-trip (within bounds) *)
let ptr_cell_roundtrip (p: ptr) :
  Lemma (requires p.addr < pow2 64)
        (ensures (ptr_of_cell (cell_of_ptr p)).addr = p.addr) =
  ()

