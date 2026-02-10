module Space.Interpreter

(** Space interpreter - fetch/execute cycle *)

open Space.Types
open Space.Stack
open Space.Universe
open Space.World
open Space.Instruction
open Space.Borrow
open Space.Warp
open Space.Text.Types
open Space.Text.Warp

(** Text handle - index into text table *)
type text_handle = nat

(** Text table - stores allocated text objects *)
noeq type text_table = {
  texts: list text;
  next_handle: text_handle;
}

(** Empty text table *)
let empty_text_table : text_table = {
  texts = [];
  next_handle = 0;
}

(** Add text to table, return handle *)
let add_text (tt: text_table) (t: text) : text_table * text_handle =
  let h = tt.next_handle in
  ({ texts = List.Tot.append tt.texts [t]; next_handle = h + 1 }, h)

(** Get text by handle *)
let rec get_text_aux (ts: list text) (h: nat) : option text =
  match ts with
  | [] -> None
  | t :: rest -> if h = 0 then Some t else get_text_aux rest (h - 1)

let get_text (tt: text_table) (h: text_handle) : option text =
  if h >= tt.next_handle then None
  else get_text_aux tt.texts h

(** Update text in table *)
let rec update_text_aux (ts: list text) (h: nat) (t: text) : list text =
  match ts with
  | [] -> []
  | old :: rest -> if h = 0 then t :: rest else old :: update_text_aux rest (h - 1) t

let update_text (tt: text_table) (h: text_handle) (t: text) : text_table =
  { tt with texts = update_text_aux tt.texts h t }

(** Text warp handle - index into text warp table *)
type text_warp_handle = nat

(** Text warp table - stores active text warps *)
noeq type text_warp_table = {
  warps: list text_warp;
  next_handle: text_warp_handle;
}

(** Empty text warp table *)
let empty_text_warp_table : text_warp_table = {
  warps = [];
  next_handle = 0;
}

(** Add text warp to table, return handle *)
let add_text_warp (twt: text_warp_table) (tw: text_warp) : text_warp_table * text_warp_handle =
  let h = twt.next_handle in
  ({ warps = List.Tot.append twt.warps [tw]; next_handle = h + 1 }, h)

(** Get text warp by handle *)
let rec get_text_warp_aux (tws: list text_warp) (h: nat) : option text_warp =
  match tws with
  | [] -> None
  | tw :: rest -> if h = 0 then Some tw else get_text_warp_aux rest (h - 1)

let get_text_warp (twt: text_warp_table) (h: text_warp_handle) : option text_warp =
  if h >= twt.next_handle then None
  else get_text_warp_aux twt.warps h

(** Update text warp in table *)
let rec update_text_warp_aux (tws: list text_warp) (h: nat) (tw: text_warp) : list text_warp =
  match tws with
  | [] -> []
  | old :: rest -> if h = 0 then tw :: rest else old :: update_text_warp_aux rest (h - 1) tw

let update_text_warp (twt: text_warp_table) (h: text_warp_handle) (tw: text_warp) : text_warp_table =
  { twt with warps = update_text_warp_aux twt.warps h tw }

(** Get implicit text warp (most recent) *)
let get_implicit_text_warp (twt: text_warp_table) : option (text_warp * text_warp_handle) =
  if twt.next_handle = 0 then None
  else
    let h = twt.next_handle - 1 in
    match get_text_warp twt h with
    | None -> None
    | Some tw -> Some (tw, h)

(** Grapheme handle - index into grapheme table *)
type grapheme_handle = nat

(** Grapheme table - stores graphemes for stack operations *)
noeq type grapheme_table = {
  graphemes: list grapheme;
  next_handle: grapheme_handle;
}

(** Empty grapheme table *)
let empty_grapheme_table : grapheme_table = {
  graphemes = [];
  next_handle = 0;
}

(** Add grapheme to table, return handle *)
let add_grapheme (gt: grapheme_table) (g: grapheme) : grapheme_table * grapheme_handle =
  let h = gt.next_handle in
  ({ graphemes = List.Tot.append gt.graphemes [g]; next_handle = h + 1 }, h)

(** Get grapheme by handle *)
let rec get_grapheme_aux (gs: list grapheme) (h: nat) : option grapheme =
  match gs with
  | [] -> None
  | g :: rest -> if h = 0 then Some g else get_grapheme_aux rest (h - 1)

let get_grapheme (gt: grapheme_table) (h: grapheme_handle) : option grapheme =
  if h >= gt.next_handle then None
  else get_grapheme_aux gt.graphemes h

(** Bytes allocation metadata - tracks sizes of byte allocations *)
noeq type bytes_metadata_entry = {
  base_addr: nat;
  byte_length: nat;
}

noeq type bytes_metadata = {
  entries: list bytes_metadata_entry;
}

let empty_bytes_metadata : bytes_metadata = {
  entries = [];
}

(** Record a byte allocation *)
let record_bytes_alloc (bm: bytes_metadata) (addr len: nat) : bytes_metadata =
  { entries = { base_addr = addr; byte_length = len } :: bm.entries }

(** Lookup byte allocation size *)
let rec lookup_bytes_len (entries: list bytes_metadata_entry) (addr: nat) : option nat =
  match entries with
  | [] -> None
  | e :: rest ->
    if e.base_addr = addr then Some e.byte_length
    else lookup_bytes_len rest addr

let get_bytes_len (bm: bytes_metadata) (addr: nat) : option nat =
  lookup_bytes_len bm.entries addr

(** Machine state *)
noeq type machine = {
  mworld: world;
  borrows: borrow_state;     (* Active borrowed pointers *)
  warps: warp_table;         (* Active memory warps *)
  texts: text_table;         (* Allocated text objects *)
  text_warps: text_warp_table; (* Active text warps *)
  graphemes: grapheme_table; (* Graphemes for stack ops *)
  bytes_meta: bytes_metadata; (* Byte allocation sizes *)
  output: list nat;          (* Output buffer - emitted characters *)
  input: list nat;           (* Input buffer - pending input *)
  inst_ptr: ip;              (* Instruction pointer *)
  return_stack: list ip;     (* Return addresses *)
  running: bool;
  error: option string;
}

(** Initial machine state *)
let initial_machine : machine = {
  mworld = empty_world;
  borrows = empty_borrow_state;
  warps = empty_warp_table;
  texts = empty_text_table;
  text_warps = empty_text_warp_table;
  graphemes = empty_grapheme_table;
  bytes_meta = empty_bytes_metadata;
  output = [];
  input = [];
  inst_ptr = 0uL;
  return_stack = [];
  running = false;
  error = None;
}

(** Machine with error *)
let machine_error (m: machine) (msg: string) : machine =
  { m with running = false; error = Some msg }

(** Halt machine *)
let halt (m: machine) : machine =
  { m with running = false }

(** Advance instruction pointer *)
let advance_ip (m: machine) : machine =
  { m with inst_ptr = FStar.UInt64.add_mod m.inst_ptr 1uL }

(** Jump to address *)
let jump (m: machine) (target: ip) : machine =
  { m with inst_ptr = target }

(** Call: push return address and jump *)
let call (m: machine) (target: ip) : machine =
  let return_addr = FStar.UInt64.add_mod m.inst_ptr 1uL in
  { m with
    inst_ptr = target;
    return_stack = return_addr :: m.return_stack }

(** Return: pop return address and jump *)
let do_return (m: machine) : machine =
  match m.return_stack with
  | [] -> machine_error m "return stack underflow"
  | addr :: rest -> { m with inst_ptr = addr; return_stack = rest }

(** Get current universe from machine *)
let current_universe (m: machine) : option universe =
  get_current m.mworld

(** Update current universe in machine *)
let update_current (m: machine) (u: universe) : machine =
  { m with mworld = update_universe m.mworld u }
