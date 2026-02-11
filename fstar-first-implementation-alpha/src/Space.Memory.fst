module Space.Memory

(** Memory operations within a universe *)

open FStar.UInt64
open Space.Types
open Space.Stack

(** Memory is a list of cells with base address *)
type memory = {
  base: nat;
  cells: list cell;
}

(** Empty memory *)
let empty_memory : memory = {
  base = 0;
  cells = [];
}

(** Memory size *)
let mem_size (m: memory) : nat =
  List.Tot.length m.cells

(** Check if address is valid *)
let valid_addr (m: memory) (addr: nat) : bool =
  addr >= m.base && addr < m.base + mem_size m

(** Fetch cell at address *)
let mem_fetch (m: memory) (addr: nat) : option cell =
  if not (valid_addr m addr)
  then None
  else
    let idx = addr - m.base in
    List.Tot.nth m.cells idx

(** Store cell at address - returns updated memory *)
let rec update_at (xs: list cell) (idx: nat) (v: cell) : option (list cell) =
  match xs, idx with
  | [], _ -> None
  | _ :: rest, 0 -> Some (v :: rest)
  | x :: rest, n ->
    match update_at rest (n - 1) v with
    | None -> None
    | Some rest' -> Some (x :: rest')

let mem_store (m: memory) (addr: nat) (v: cell) : option memory =
  if not (valid_addr m addr)
  then None
  else
    let idx = addr - m.base in
    match update_at m.cells idx v with
    | None -> None
    | Some cells' -> Some { m with cells = cells' }

(** Create a list of n zero cells *)
let rec zeros (n: nat) : Tot (list cell) (decreases n) =
  if n = 0 then []
  else 0uL :: zeros (n - 1)

(** Allocate n cells, returns base address of allocation *)
let mem_alloc (m: memory) (n: nat) : memory * nat =
  let new_base = m.base + mem_size m in
  let new_cells = List.Tot.append m.cells (zeros n) in
  ({ base = m.base; cells = new_cells }, new_base)

(** Pointer type - address with validity proof *)
type ptr = {
  addr: nat;
}

(** Create pointer from stack value *)
let ptr_of_cell (c: cell) : ptr =
  { addr = UInt64.v c }

(** Convert pointer to cell *)
let cell_of_ptr (p: ptr) : cell =
  if p.addr < pow2 64
  then UInt64.uint_to_t p.addr
  else 0uL  (* overflow protection *)
