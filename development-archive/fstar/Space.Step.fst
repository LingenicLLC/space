module Space.Step

(** Single-step execution of the interpreter *)

open FStar.UInt64
open Space.Types
open Space.Stack
open Space.Universe
open Space.World
open Space.Instruction
open Space.Interpreter
open Space.Execute
open Space.Control
open Space.Text.Types
open Space.Text.Create
open Space.Text.Ops
open Space.Text.Warp
open Space.Text.Normalize
open Space.Text.Case
open Space.Borrow
open Space.BorrowOps
open Space.Warp
open Space.WarpOps

(** Step result *)
noeq type step_result =
  | StepOk of machine
  | StepHalt of machine
  | StepError of machine * string

(** Execute push instruction *)
let step_push (m: machine) (v: cell) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match universe_push u v with
    | None -> StepError (m, "push failed")
    | Some u' -> StepOk (advance_ip (update_current m u'))

(** Execute branch if zero *)
let step_branch_zero (m: machine) (target: ip) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match if_true u.stack with
    | None -> StepError (m, "branch: stack underflow")
    | Some (TakeBranch, s') ->
      let u' = { u with stack = s' } in
      StepOk (advance_ip (update_current m u'))
    | Some (SkipBranch, s') ->
      let u' = { u with stack = s' } in
      StepOk (jump (update_current m u') target)

(** Execute branch if non-zero *)
let step_branch_nonzero (m: machine) (target: ip) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match if_false u.stack with
    | None -> StepError (m, "branch: stack underflow")
    | Some (TakeBranch, s') ->
      let u' = { u with stack = s' } in
      StepOk (advance_ip (update_current m u'))
    | Some (SkipBranch, s') ->
      let u' = { u with stack = s' } in
      StepOk (jump (update_current m u') target)

(** Execute call instruction *)
let step_call (m: machine) (target: ip) : step_result =
  StepOk (call m target)

(** Execute return instruction *)
let step_return (m: machine) : step_result =
  let m' = do_return m in
  match m'.error with
  | Some msg -> StepError (m', msg)
  | None -> StepOk m'

(** Execute unconditional branch *)
let step_branch (m: machine) (target: ip) : step_result =
  StepOk (jump m target)

(** Create universe instruction *)
let step_create_universe (m: machine) (name: universe_name) (cap: nat) (disc: discipline) : step_result =
  let (w', _uid) = create_universe m.mworld name cap disc in
  StepOk (advance_ip { m with mworld = w' })

(** End universe instruction *)
let step_end_universe (m: machine) (name: universe_name) : step_result =
  match find_by_name m.mworld name with
  | None -> StepError (m, "universe not found")
  | Some u ->
    if u.discipline = Linear && not (stack_empty u)
    then StepError (m, "linear universe not empty")
    else
      let u' = destroy u in
      let w' = update_universe m.mworld u' in
      StepOk (advance_ip { m with mworld = w' })

(** Release universe instruction *)
let step_release_universe (m: machine) (name: universe_name) : step_result =
  match find_by_name m.mworld name with
  | None -> StepError (m, "universe not found")
  | Some u ->
    match release u with
    | None -> StepError (m, "release failed: not affine or not live")
    | Some u' ->
      let w' = update_universe m.mworld u' in
      StepOk (advance_ip { m with mworld = w' })

(** Transfer to universe instruction *)
let step_transfer (m: machine) (name: universe_name) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some src ->
    match find_by_name m.mworld name with
    | None -> StepError (m, "target universe not found")
    | Some dst ->
      match src.stack with
      | [] -> StepError (m, "transfer: stack underflow")
      | v :: rest ->
        let src' = { src with stack = rest } in
        let dst' = { dst with stack = v :: dst.stack } in
        let w' = update_universe (update_universe m.mworld src') dst' in
        StepOk (advance_ip { m with mworld = w' })

(* ============== Text Operations ============== *)

(** Read a single byte from packed memory using UInt64 operations
    Bytes are packed 8 per 64-bit cell. offset selects which byte within the cell. *)
let read_byte_from_mem (mem: Space.Memory.memory) (base_addr: nat) (offset: nat) : option FStar.UInt8.t =
  if offset >= 18446744073709551616 then None  (* offset too large *)
  else
    (* Use UInt64 for arithmetic to avoid nat/int issues *)
    let offset64 = FStar.UInt64.uint_to_t offset in
    let cell_offset = FStar.UInt64.div offset64 8uL in
    let cell_idx = base_addr + FStar.UInt64.v cell_offset in
    let byte_pos = FStar.UInt64.rem offset64 8uL in
    let shift_amt = FStar.UInt64.mul byte_pos 8uL in
    match Space.Memory.mem_fetch mem cell_idx with
    | None -> None
    | Some cell_val ->
      if FStar.UInt64.v shift_amt >= 64 then None
      else
        let n32 = FStar.UInt32.uint_to_t (FStar.UInt64.v shift_amt) in
        let shifted = FStar.UInt64.shift_right cell_val n32 in
        let byte_val = FStar.UInt64.v (FStar.UInt64.logand shifted 0xFFuL) in
        if byte_val < 256 then Some (FStar.UInt8.uint_to_t byte_val)
        else None

(** Read n bytes from memory starting at addr *)
let rec read_bytes_from_mem (mem: Space.Memory.memory) (addr: nat) (remaining: nat)
  : Tot (option (list FStar.UInt8.t)) (decreases remaining) =
  if remaining = 0 then Some []
  else
    match read_byte_from_mem mem addr 0 with
    | None -> None
    | Some b ->
      match read_bytes_from_mem mem addr (remaining - 1) with
      | None -> None
      | Some rest -> Some (b :: rest)

(** Read bytes sequentially from packed memory *)
let rec read_bytes_seq (mem: Space.Memory.memory) (base_addr offset remaining: nat)
  : Tot (option (list FStar.UInt8.t)) (decreases remaining) =
  if remaining = 0 then Some []
  else
    match read_byte_from_mem mem base_addr offset with
    | None -> None
    | Some b ->
      match read_bytes_seq mem base_addr (offset + 1) (remaining - 1) with
      | None -> None
      | Some rest -> Some (b :: rest)

(** Create text from bytes in memory: ( addr len -- text_handle ) *)
let step_create_text (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | len_cell :: addr_cell :: rest ->
      let len = v len_cell in
      let addr = v addr_cell in
      if len = 0 then
        (* Empty text *)
        let (tt', handle) = add_text m.texts empty_text in
        let handle_cell = if handle < 18446744073709551616 then uint_to_t handle else 0uL in
        let u' = { u with stack = handle_cell :: rest } in
        StepOk (advance_ip { (update_current m u') with texts = tt' })
      else
        (* Read bytes from memory *)
        (match read_bytes_seq u.memory addr 0 len with
         | None -> StepError (m, "create-text: failed to read memory")
         | Some bytes ->
           match text_from_bytes bytes with
           | None -> StepError (m, "create-text: invalid UTF-8")
           | Some t ->
             let (tt', handle) = add_text m.texts t in
             let handle_cell = if handle < 18446744073709551616 then uint_to_t handle else 0uL in
             let u' = { u with stack = handle_cell :: rest } in
             StepOk (advance_ip { (update_current m u') with texts = tt' }))
    | _ -> StepError (m, "create-text: stack underflow")

(** Get text byte length: ( text_handle -- length ) *)
let step_text_byte_length (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | handle_cell :: rest ->
      let handle = v handle_cell in
      (match get_text m.texts handle with
       | None -> StepError (m, "text-byte-length: invalid handle")
       | Some t ->
         let len = text_byte_length t in
         let len_cell = if len < 18446744073709551616 then uint_to_t len else 0uL in
         let u' = { u with stack = len_cell :: rest } in
         StepOk (advance_ip (update_current m u')))
    | _ -> StepError (m, "text-byte-length: stack underflow")

(** Get text grapheme count: ( text_handle -- count ) *)
let step_text_grapheme_count (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | handle_cell :: rest ->
      let handle = v handle_cell in
      (match get_text m.texts handle with
       | None -> StepError (m, "text-grapheme-count: invalid handle")
       | Some t ->
         let count = Space.Text.Create.text_grapheme_count t in
         let count_cell = if count < 18446744073709551616 then uint_to_t count else 0uL in
         let u' = { u with stack = count_cell :: rest } in
         StepOk (advance_ip (update_current m u')))
    | _ -> StepError (m, "text-grapheme-count: stack underflow")

(** Check if text is simple: ( text_handle -- flag ) *)
let step_text_is_simple (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | handle_cell :: rest ->
      let handle = v handle_cell in
      (match get_text m.texts handle with
       | None -> StepError (m, "text-is-simple: invalid handle")
       | Some t ->
         let flag = if Space.Text.Create.text_is_simple t then 1uL else 0uL in
         let u' = { u with stack = flag :: rest } in
         StepOk (advance_ip (update_current m u')))
    | _ -> StepError (m, "text-is-simple: stack underflow")

(** Text equal: ( h1 h2 -- flag ) *)
let step_text_equal (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | h2_cell :: h1_cell :: rest ->
      let h1 = v h1_cell in
      let h2 = v h2_cell in
      (match get_text m.texts h1, get_text m.texts h2 with
       | Some t1, Some t2 ->
         let eq = if text_equal t1 t2 then 1uL else 0uL in
         let u' = { u with stack = eq :: rest } in
         StepOk (advance_ip (update_current m u'))
       | _, _ -> StepError (m, "text-equal: invalid handle"))
    | _ -> StepError (m, "text-equal: stack underflow")

(** Text concat: ( h1 h2 -- h3 ) *)
let step_text_concat (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | h2_cell :: h1_cell :: rest ->
      let h1 = v h1_cell in
      let h2 = v h2_cell in
      (match get_text m.texts h1, get_text m.texts h2 with
       | Some t1, Some t2 ->
         let t3 = text_concat t1 t2 in
         let (tt', h3) = add_text m.texts t3 in
         let h3_cell = if h3 < 18446744073709551616 then uint_to_t h3 else 0uL in
         let u' = { u with stack = h3_cell :: rest } in
         StepOk (advance_ip { (update_current m u') with texts = tt' })
       | _, _ -> StepError (m, "text-concat: invalid handle"))
    | _ -> StepError (m, "text-concat: stack underflow")

(** Text slice: ( handle start end -- new_handle ) *)
let step_text_slice (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | end_cell :: start_cell :: handle_cell :: rest ->
      let handle = v handle_cell in
      let start = v start_cell in
      let finish = v end_cell in
      (match get_text m.texts handle with
       | None -> StepError (m, "text-slice: invalid handle")
       | Some t ->
         (match text_slice t start finish with
          | None -> StepError (m, "text-slice: invalid range")
          | Some t' ->
            let (tt', h') = add_text m.texts t' in
            let h_cell = if h' < 18446744073709551616 then uint_to_t h' else 0uL in
            let u' = { u with stack = h_cell :: rest } in
            StepOk (advance_ip { (update_current m u') with texts = tt' })))
    | _ -> StepError (m, "text-slice: stack underflow")

(* ============== Bytes Operations with Metadata ============== *)

(** Bytes alloc with metadata: ( n -- addr ) *)
let step_bytes_alloc (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | n_cell :: rest ->
      let n = v n_cell in
      if n = 0 then StepError (m, "bytes-alloc: cannot allocate zero bytes")
      else
        (* Bytes need (n+7)/8 cells *)
        let cells_needed = (n + 7) / 8 in
        let (mem', base_addr) = Space.Memory.mem_alloc u.memory cells_needed in
        let addr_cell = if base_addr < 18446744073709551616 then uint_to_t base_addr else 0uL in
        let u' = { u with stack = addr_cell :: rest; memory = mem' } in
        let bm' = record_bytes_alloc m.bytes_meta base_addr n in
        StepOk (advance_ip { (update_current m u') with bytes_meta = bm' })
    | _ -> StepError (m, "bytes-alloc: stack underflow")

(** Bytes len: ( addr -- len ) *)
let step_bytes_len (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | addr_cell :: rest ->
      let addr = v addr_cell in
      (match get_bytes_len m.bytes_meta addr with
       | None -> StepError (m, "bytes-len: unknown allocation")
       | Some len ->
         let len_cell = if len < 18446744073709551616 then uint_to_t len else 0uL in
         let u' = { u with stack = len_cell :: rest } in
         StepOk (advance_ip (update_current m u')))
    | _ -> StepError (m, "bytes-len: stack underflow")

(** Check if primitive is a bytes operation needing machine state *)
let is_bytes_meta_prim (op: prim_op) : bool =
  match op with
  | PrimBytesAlloc | PrimBytesLen -> true
  | _ -> false

(** Execute bytes primitive with metadata *)
let step_bytes_meta_prim (m: machine) (op: prim_op) : step_result =
  match op with
  | PrimBytesAlloc -> step_bytes_alloc m
  | PrimBytesLen -> step_bytes_len m
  | _ -> StepError (m, "unknown bytes primitive")

(* ============== I/O Operations ============== *)

(** Emit character: ( char -- ) - appends to output buffer *)
let step_emit (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | char_cell :: rest ->
      let char_val = FStar.UInt64.v char_cell in
      let u' = { u with stack = rest } in
      let m' = { (update_current m u') with output = m.output @ [char_val] } in
      StepOk (advance_ip m')
    | _ -> StepError (m, "emit: stack underflow")

(** Read character: ( -- char ) - reads from input buffer *)
let step_key (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match m.input with
    | char_val :: rest_input ->
      let char_cell = if char_val < 18446744073709551616 then FStar.UInt64.uint_to_t char_val else 0uL in
      let u' = { u with stack = char_cell :: u.stack } in
      let m' = { (update_current m u') with input = rest_input } in
      StepOk (advance_ip m')
    | [] ->
      (* No input available, push -1 (EOF) *)
      let u' = { u with stack = 0xFFFFFFFFFFFFFFFFuL :: u.stack } in
      StepOk (advance_ip (update_current m u'))

(** Check if primitive is an I/O operation *)
let is_io_prim (op: prim_op) : bool =
  match op with
  | PrimEmit | PrimKey -> true
  | _ -> false

(** Execute I/O primitive with machine state *)
let step_io_prim (m: machine) (op: prim_op) : step_result =
  match op with
  | PrimEmit -> step_emit m
  | PrimKey -> step_key m
  | _ -> StepError (m, "unknown I/O primitive")

(* ============== Borrow Operations ============== *)

(** Borrow pointer: ( addr -- ) - creates borrowed pointer from current universe *)
let step_borrow_pointer (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match borrow_pointer u with
    | None -> StepError (m, "borrow-pointer: failed")
    | Some (b, u') ->
      let bs' = add_borrow m.borrows b in
      StepOk (advance_ip { (update_current m u') with borrows = bs' })

(** Return pointer: ( -- addr ) - returns borrowed pointer to source universe *)
let step_return_pointer (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match m.borrows.borrows with
    | [] -> StepError (m, "return-pointer: no active borrow")
    | b :: rest ->
      match return_borrowed b u with
      | None -> StepError (m, "return-pointer: failed")
      | Some u' ->
        let bs' = { borrows = rest } in
        StepOk (advance_ip { (update_current m u') with borrows = bs' })

(** Drop pointer: ( -- ) - ends borrow without returning *)
let step_drop_pointer (m: machine) : step_result =
  match m.borrows.borrows with
  | [] -> StepError (m, "drop-pointer: no active borrow")
  | b :: rest ->
    let bs' = { borrows = rest } in
    StepOk (advance_ip { m with borrows = bs' })

(** Fetch borrowed: ( -- value ) - fetch through current borrowed pointer *)
let step_fetch_borrowed (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match m.borrows.borrows with
    | [] -> StepError (m, "fetch-borrowed: no active borrow")
    | b :: _ ->
      let um : Space.BorrowOps.universe_mem = { universe = u; memory = u.memory } in
      match fetch_borrowed b um with
      | None -> StepError (m, "fetch-borrowed: failed")
      | Some v ->
        let u' = { u with stack = v :: u.stack } in
        StepOk (advance_ip (update_current m u'))

(** Store borrowed: ( value -- ) - store through current borrowed pointer *)
let step_store_borrowed (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | [] -> StepError (m, "store-borrowed: stack underflow")
    | v :: rest ->
      match m.borrows.borrows with
      | [] -> StepError (m, "store-borrowed: no active borrow")
      | b :: _ ->
        let um : Space.BorrowOps.universe_mem = { universe = u; memory = u.memory } in
        match store_borrowed b v um with
        | None -> StepError (m, "store-borrowed: failed")
        | Some um' ->
          let u' = { um'.universe with stack = rest; memory = um'.memory } in
          StepOk (advance_ip (update_current m u'))

(** Fetch and end: ( -- value ) - fetch and end borrow *)
let step_fetch_and_end (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match m.borrows.borrows with
    | [] -> StepError (m, "fetch-and-end: no active borrow")
    | b :: rest ->
      let um : Space.BorrowOps.universe_mem = { universe = u; memory = u.memory } in
      match fetch_and_end b um with
      | None -> StepError (m, "fetch-and-end: failed")
      | Some (v, _) ->
        let u' = { u with stack = v :: u.stack } in
        let bs' = { borrows = rest } in
        StepOk (advance_ip { (update_current m u') with borrows = bs' })

(** Store and end: ( value -- ) - store and end borrow *)
let step_store_and_end (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | [] -> StepError (m, "store-and-end: stack underflow")
    | v :: rest ->
      match m.borrows.borrows with
      | [] -> StepError (m, "store-and-end: no active borrow")
      | b :: brest ->
        let um : Space.BorrowOps.universe_mem = { universe = u; memory = u.memory } in
        match store_and_end b v um with
        | None -> StepError (m, "store-and-end: failed")
        | Some (um', _) ->
          let u' = { um'.universe with stack = rest; memory = um'.memory } in
          let bs' = { borrows = brest } in
          StepOk (advance_ip { (update_current m u') with borrows = bs' })

(** Offset borrowed: ( offset -- ) - offset current borrowed pointer *)
let step_offset_borrowed (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | [] -> StepError (m, "offset-borrowed: stack underflow")
    | offset :: rest ->
      match m.borrows.borrows with
      | [] -> StepError (m, "offset-borrowed: no active borrow")
      | b :: brest ->
        let b' = offset_borrowed b offset in
        let u' = { u with stack = rest } in
        let bs' = { borrows = b' :: brest } in
        StepOk (advance_ip { (update_current m u') with borrows = bs' })

(** Check if primitive is a borrow operation *)
let is_borrow_prim (op: prim_op) : bool =
  match op with
  | PrimBorrowPointer | PrimReturnPointer | PrimDropPointer
  | PrimFetchBorrowed | PrimStoreBorrowed | PrimFetchAndEnd
  | PrimStoreAndEnd | PrimOffsetBorrowed -> true
  | _ -> false

(** Execute borrow primitive *)
let step_borrow_prim (m: machine) (op: prim_op) : step_result =
  match op with
  | PrimBorrowPointer -> step_borrow_pointer m
  | PrimReturnPointer -> step_return_pointer m
  | PrimDropPointer -> step_drop_pointer m
  | PrimFetchBorrowed -> step_fetch_borrowed m
  | PrimStoreBorrowed -> step_store_borrowed m
  | PrimFetchAndEnd -> step_fetch_and_end m
  | PrimStoreAndEnd -> step_store_and_end m
  | PrimOffsetBorrowed -> step_offset_borrowed m
  | _ -> StepError (m, "unknown borrow primitive")

(* ============== Warp Operations ============== *)

(** Warp fetch: ( -- value ) - fetch at current warp position *)
let step_warp_fetch (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match get_implicit_warp m.warps with
    | None -> StepError (m, "warp-fetch: no active warp")
    | Some w ->
      match find_universe m.mworld w.target_id with
      | None -> StepError (m, "warp-fetch: target universe not found")
      | Some target ->
        let tu : Space.WarpOps.target_universe = { universe = target; memory = target.memory } in
        match warp_fetch w tu with
        | None -> StepError (m, "warp-fetch: failed")
        | Some v ->
          let u' = { u with stack = v :: u.stack } in
          StepOk (advance_ip (update_current m u'))

(** Warp store: ( value -- ) - store at current warp position *)
let step_warp_store (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | [] -> StepError (m, "warp-store: stack underflow")
    | v :: rest ->
      match get_implicit_warp m.warps with
      | None -> StepError (m, "warp-store: no active warp")
      | Some w ->
        match find_universe m.mworld w.target_id with
        | None -> StepError (m, "warp-store: target universe not found")
        | Some target ->
          let tu : Space.WarpOps.target_universe = { universe = target; memory = target.memory } in
          match warp_store w v tu with
          | None -> StepError (m, "warp-store: failed")
          | Some tu' ->
            let target' = { tu'.universe with memory = tu'.memory } in
            let w' = update_universe m.mworld target' in
            let u' = { u with stack = rest } in
            StepOk (advance_ip { (update_current { m with mworld = w' } u') with mworld = w' })

(** Warp advance: ( offset -- ) - advance warp position *)
let step_warp_advance (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | [] -> StepError (m, "warp-advance: stack underflow")
    | offset :: rest ->
      match get_implicit_warp m.warps with
      | None -> StepError (m, "warp-advance: no active warp")
      | Some w ->
        let w' = warp_advance w offset in
        let wt' = update_warp m.warps w' in
        let u' = { u with stack = rest } in
        StepOk (advance_ip { (update_current m u') with warps = wt' })

(** Warp follow: ( -- ) - follow pointer at current position *)
let step_warp_follow (m: machine) : step_result =
  match get_implicit_warp m.warps with
  | None -> StepError (m, "warp-follow: no active warp")
  | Some w ->
    match find_universe m.mworld w.target_id with
    | None -> StepError (m, "warp-follow: target universe not found")
    | Some target ->
      let tu : Space.WarpOps.target_universe = { universe = target; memory = target.memory } in
      match warp_follow w tu with
      | None -> StepError (m, "warp-follow: failed")
      | Some w' ->
        let wt' = update_warp m.warps w' in
        StepOk (advance_ip { m with warps = wt' })

(** Warp position: ( -- pos ) - get current warp position *)
let step_warp_position (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match get_implicit_warp m.warps with
    | None -> StepError (m, "warp-position: no active warp")
    | Some w ->
      let pos = Space.Warp.warp_position w in
      let u' = { u with stack = pos :: u.stack } in
      StepOk (advance_ip (update_current m u'))

(** Warp restore: ( pos -- ) - restore saved position *)
let step_warp_restore (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | [] -> StepError (m, "warp-restore: stack underflow")
    | pos :: rest ->
      match get_implicit_warp m.warps with
      | None -> StepError (m, "warp-restore: no active warp")
      | Some w ->
        match Space.WarpOps.warp_restore w pos with
        | None -> StepError (m, "warp-restore: position not saved")
        | Some w' ->
          let wt' = update_warp m.warps w' in
          let u' = { u with stack = rest } in
          StepOk (advance_ip { (update_current m u') with warps = wt' })

(** Warp null: ( -- flag ) - check if at null position *)
let step_warp_null (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match get_implicit_warp m.warps with
    | None -> StepError (m, "warp-null: no active warp")
    | Some w ->
      let flag = if warp_at_null w then 1uL else 0uL in
      let u' = { u with stack = flag :: u.stack } in
      StepOk (advance_ip (update_current m u'))

(** Check if primitive is a warp operation *)
let is_warp_prim (op: prim_op) : bool =
  match op with
  | PrimWarpFetch | PrimWarpStore | PrimWarpAdvance | PrimWarpFollow
  | PrimWarpPosition | PrimWarpRestore | PrimWarpNull -> true
  | _ -> false

(** Execute warp primitive *)
let step_warp_prim (m: machine) (op: prim_op) : step_result =
  match op with
  | PrimWarpFetch -> step_warp_fetch m
  | PrimWarpStore -> step_warp_store m
  | PrimWarpAdvance -> step_warp_advance m
  | PrimWarpFollow -> step_warp_follow m
  | PrimWarpPosition -> step_warp_position m
  | PrimWarpRestore -> step_warp_restore m
  | PrimWarpNull -> step_warp_null m
  | _ -> StepError (m, "unknown warp primitive")

(** Check if primitive is a text operation that needs machine state *)
let is_text_prim (op: prim_op) : bool =
  match op with
  (* Core text ops *)
  | PrimCreateText | PrimTextByteLength | PrimTextGraphemeCount | PrimTextIsSimple
  | PrimTextGraphemeAt | PrimTextGraphemeFirst | PrimTextGraphemeLast
  | PrimTextSlice | PrimTextConcat | PrimTextEqual | PrimTextCompare
  (* Text warp ops *)
  | PrimTextWarpHasGrapheme | PrimTextWarpCurrentGrapheme | PrimTextWarpNextGrapheme
  | PrimTextWarpGraphemeIndex | PrimTextWarpGotoGrapheme
  (* Grapheme ops *)
  | PrimGraphemeByteLength | PrimGraphemeIsAscii | PrimGraphemeCodePoints
  (* Codepoint ops *)
  | PrimTextCodePointCount | PrimTextCodePointAt
  (* Normalization ops *)
  | PrimTextNormalizeNfc | PrimTextNormalizeNfd | PrimTextNormalizeNfkc | PrimTextNormalizeNfkd
  (* Case mapping ops *)
  | PrimTextToUpper | PrimTextToLower | PrimTextToTitle
  (* Grapheme I/O *)
  | PrimEmitGrapheme -> true
  | _ -> false

(** Text normalize NFC: ( handle -- handle' ) *)
let step_text_normalize_nfc (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | handle_cell :: rest ->
      let handle = v handle_cell in
      (match get_text m.texts handle with
       | None -> StepError (m, "text-normalize-nfc: invalid handle")
       | Some t ->
         match normalize_nfc t with
         | None -> StepError (m, "text-normalize-nfc: normalization failed")
         | Some t' ->
           let (tt', h') = add_text m.texts t' in
           let h_cell = if h' < 18446744073709551616 then uint_to_t h' else 0uL in
           let u' = { u with stack = h_cell :: rest } in
           StepOk (advance_ip { (update_current m u') with texts = tt' }))
    | _ -> StepError (m, "text-normalize-nfc: stack underflow")

(** Text normalize NFD: ( handle -- handle' ) *)
let step_text_normalize_nfd (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | handle_cell :: rest ->
      let handle = v handle_cell in
      (match get_text m.texts handle with
       | None -> StepError (m, "text-normalize-nfd: invalid handle")
       | Some t ->
         match normalize_nfd t with
         | None -> StepError (m, "text-normalize-nfd: normalization failed")
         | Some t' ->
           let (tt', h') = add_text m.texts t' in
           let h_cell = if h' < 18446744073709551616 then uint_to_t h' else 0uL in
           let u' = { u with stack = h_cell :: rest } in
           StepOk (advance_ip { (update_current m u') with texts = tt' }))
    | _ -> StepError (m, "text-normalize-nfd: stack underflow")

(** Text normalize NFKC: ( handle -- handle' ) *)
let step_text_normalize_nfkc (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | handle_cell :: rest ->
      let handle = v handle_cell in
      (match get_text m.texts handle with
       | None -> StepError (m, "text-normalize-nfkc: invalid handle")
       | Some t ->
         match normalize_nfkc t with
         | None -> StepError (m, "text-normalize-nfkc: normalization failed")
         | Some t' ->
           let (tt', h') = add_text m.texts t' in
           let h_cell = if h' < 18446744073709551616 then uint_to_t h' else 0uL in
           let u' = { u with stack = h_cell :: rest } in
           StepOk (advance_ip { (update_current m u') with texts = tt' }))
    | _ -> StepError (m, "text-normalize-nfkc: stack underflow")

(** Text normalize NFKD: ( handle -- handle' ) *)
let step_text_normalize_nfkd (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | handle_cell :: rest ->
      let handle = v handle_cell in
      (match get_text m.texts handle with
       | None -> StepError (m, "text-normalize-nfkd: invalid handle")
       | Some t ->
         match normalize_nfkd t with
         | None -> StepError (m, "text-normalize-nfkd: normalization failed")
         | Some t' ->
           let (tt', h') = add_text m.texts t' in
           let h_cell = if h' < 18446744073709551616 then uint_to_t h' else 0uL in
           let u' = { u with stack = h_cell :: rest } in
           StepOk (advance_ip { (update_current m u') with texts = tt' }))
    | _ -> StepError (m, "text-normalize-nfkd: stack underflow")

(** Text to upper: ( handle -- handle' ) *)
let step_text_to_upper (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | handle_cell :: rest ->
      let handle = v handle_cell in
      (match get_text m.texts handle with
       | None -> StepError (m, "text-to-upper: invalid handle")
       | Some t ->
         match text_to_upper t with
         | None -> StepError (m, "text-to-upper: conversion failed")
         | Some t' ->
           let (tt', h') = add_text m.texts t' in
           let h_cell = if h' < 18446744073709551616 then uint_to_t h' else 0uL in
           let u' = { u with stack = h_cell :: rest } in
           StepOk (advance_ip { (update_current m u') with texts = tt' }))
    | _ -> StepError (m, "text-to-upper: stack underflow")

(** Text to lower: ( handle -- handle' ) *)
let step_text_to_lower (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | handle_cell :: rest ->
      let handle = v handle_cell in
      (match get_text m.texts handle with
       | None -> StepError (m, "text-to-lower: invalid handle")
       | Some t ->
         match text_to_lower t with
         | None -> StepError (m, "text-to-lower: conversion failed")
         | Some t' ->
           let (tt', h') = add_text m.texts t' in
           let h_cell = if h' < 18446744073709551616 then uint_to_t h' else 0uL in
           let u' = { u with stack = h_cell :: rest } in
           StepOk (advance_ip { (update_current m u') with texts = tt' }))
    | _ -> StepError (m, "text-to-lower: stack underflow")

(** Text to title: ( handle -- handle' ) *)
let step_text_to_title (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | handle_cell :: rest ->
      let handle = v handle_cell in
      (match get_text m.texts handle with
       | None -> StepError (m, "text-to-title: invalid handle")
       | Some t ->
         match text_to_title t with
         | None -> StepError (m, "text-to-title: conversion failed")
         | Some t' ->
           let (tt', h') = add_text m.texts t' in
           let h_cell = if h' < 18446744073709551616 then uint_to_t h' else 0uL in
           let u' = { u with stack = h_cell :: rest } in
           StepOk (advance_ip { (update_current m u') with texts = tt' }))
    | _ -> StepError (m, "text-to-title: stack underflow")

(* ============== Text Warp Operations ============== *)

(** Text warp has grapheme: ( warp_handle -- flag ) *)
let step_text_warp_has_grapheme (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | wh_cell :: rest ->
      let wh = v wh_cell in
      (match get_text_warp m.text_warps wh with
       | None -> StepError (m, "text-warp-has-grapheme: invalid warp handle")
       | Some tw ->
         let flag = if text_warp_has_grapheme tw then 1uL else 0uL in
         let u' = { u with stack = flag :: rest } in
         StepOk (advance_ip (update_current m u')))
    | _ -> StepError (m, "text-warp-has-grapheme: stack underflow")

(** Text warp current grapheme: ( warp_handle -- grapheme_handle ) *)
let step_text_warp_current_grapheme (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | wh_cell :: rest ->
      let wh = v wh_cell in
      (match get_text_warp m.text_warps wh with
       | None -> StepError (m, "text-warp-current-grapheme: invalid warp handle")
       | Some tw ->
         match text_warp_current tw with
         | None -> StepError (m, "text-warp-current-grapheme: no current grapheme")
         | Some g ->
           let (gt', gh) = add_grapheme m.graphemes g in
           let gh_cell = if gh < 18446744073709551616 then uint_to_t gh else 0uL in
           let u' = { u with stack = gh_cell :: rest } in
           StepOk (advance_ip { (update_current m u') with graphemes = gt' }))
    | _ -> StepError (m, "text-warp-current-grapheme: stack underflow")

(** Text warp next grapheme: ( warp_handle -- grapheme_handle warp_handle' ) *)
let step_text_warp_next_grapheme (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | wh_cell :: rest ->
      let wh = v wh_cell in
      (match get_text_warp m.text_warps wh with
       | None -> StepError (m, "text-warp-next-grapheme: invalid warp handle")
       | Some tw ->
         match text_warp_next tw with
         | None -> StepError (m, "text-warp-next-grapheme: no next grapheme")
         | Some (g, tw') ->
           let (gt', gh) = add_grapheme m.graphemes g in
           let twt' = update_text_warp m.text_warps wh tw' in
           let gh_cell = if gh < 18446744073709551616 then uint_to_t gh else 0uL in
           let u' = { u with stack = wh_cell :: gh_cell :: rest } in
           StepOk (advance_ip { (update_current { m with text_warps = twt' } u') with graphemes = gt' }))
    | _ -> StepError (m, "text-warp-next-grapheme: stack underflow")

(** Text warp grapheme index: ( warp_handle -- index ) *)
let step_text_warp_grapheme_index (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | wh_cell :: rest ->
      let wh = v wh_cell in
      (match get_text_warp m.text_warps wh with
       | None -> StepError (m, "text-warp-grapheme-index: invalid warp handle")
       | Some tw ->
         let pos = text_warp_position tw in
         let pos_cell = if pos < 18446744073709551616 then uint_to_t pos else 0uL in
         let u' = { u with stack = pos_cell :: rest } in
         StepOk (advance_ip (update_current m u')))
    | _ -> StepError (m, "text-warp-grapheme-index: stack underflow")

(** Text warp goto grapheme: ( warp_handle index -- warp_handle' ) *)
let step_text_warp_goto_grapheme (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | idx_cell :: wh_cell :: rest ->
      let wh = v wh_cell in
      let idx = v idx_cell in
      (match get_text_warp m.text_warps wh with
       | None -> StepError (m, "text-warp-goto-grapheme: invalid warp handle")
       | Some tw ->
         match text_warp_goto tw idx with
         | None -> StepError (m, "text-warp-goto-grapheme: invalid index")
         | Some tw' ->
           let twt' = update_text_warp m.text_warps wh tw' in
           let u' = { u with stack = wh_cell :: rest } in
           StepOk (advance_ip { (update_current m u') with text_warps = twt' }))
    | _ -> StepError (m, "text-warp-goto-grapheme: stack underflow")

(* ============== Grapheme Operations ============== *)

(** Grapheme byte length: ( grapheme_handle -- length ) *)
let step_grapheme_byte_length (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | gh_cell :: rest ->
      let gh = v gh_cell in
      (match get_grapheme m.graphemes gh with
       | None -> StepError (m, "grapheme-byte-length: invalid handle")
       | Some g ->
         let len = g.len in
         let len_cell = if len < 18446744073709551616 then uint_to_t len else 0uL in
         let u' = { u with stack = len_cell :: rest } in
         StepOk (advance_ip (update_current m u')))
    | _ -> StepError (m, "grapheme-byte-length: stack underflow")

(** Grapheme is ASCII: ( grapheme_handle -- flag ) *)
let step_grapheme_is_ascii (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | gh_cell :: rest ->
      let gh = v gh_cell in
      (match get_grapheme m.graphemes gh with
       | None -> StepError (m, "grapheme-is-ascii: invalid handle")
       | Some g ->
         let is_ascii = g.len = 1 && (match g.bytes with | [b] -> FStar.UInt8.v b < 128 | _ -> false) in
         let flag = if is_ascii then 1uL else 0uL in
         let u' = { u with stack = flag :: rest } in
         StepOk (advance_ip (update_current m u')))
    | _ -> StepError (m, "grapheme-is-ascii: stack underflow")

(** Grapheme code points: ( grapheme_handle -- count ) - returns number of codepoints *)
let step_grapheme_code_points (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | gh_cell :: rest ->
      let gh = v gh_cell in
      (match get_grapheme m.graphemes gh with
       | None -> StepError (m, "grapheme-code-points: invalid handle")
       | Some g ->
         (* Count UTF-8 lead bytes to get codepoint count *)
         let rec count_codepoints (bs: list FStar.UInt8.t) : nat =
           match bs with
           | [] -> 0
           | b :: rest' ->
             let bv = FStar.UInt8.v b in
             if bv < 128 || bv >= 192 then 1 + count_codepoints rest'
             else count_codepoints rest'  (* continuation byte *)
         in
         let count = count_codepoints g.bytes in
         let count_cell = if count < 18446744073709551616 then uint_to_t count else 0uL in
         let u' = { u with stack = count_cell :: rest } in
         StepOk (advance_ip (update_current m u')))
    | _ -> StepError (m, "grapheme-code-points: stack underflow")

(** Text grapheme at: ( text_handle index -- grapheme_handle ) *)
let step_text_grapheme_at (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | idx_cell :: th_cell :: rest ->
      let th = v th_cell in
      let idx = v idx_cell in
      (match get_text m.texts th with
       | None -> StepError (m, "text-grapheme-at: invalid text handle")
       | Some t ->
         if idx >= t.header.grapheme_count then
           StepError (m, "text-grapheme-at: index out of bounds")
         else
           (* Create a text warp, advance to index, get grapheme *)
           let tw = text_warp_begin t in
           match text_warp_goto tw idx with
           | None -> StepError (m, "text-grapheme-at: seek failed")
           | Some tw' ->
             match text_warp_current tw' with
             | None -> StepError (m, "text-grapheme-at: no grapheme at index")
             | Some g ->
               let (gt', gh) = add_grapheme m.graphemes g in
               let gh_cell = if gh < 18446744073709551616 then uint_to_t gh else 0uL in
               let u' = { u with stack = gh_cell :: rest } in
               StepOk (advance_ip { (update_current m u') with graphemes = gt' }))
    | _ -> StepError (m, "text-grapheme-at: stack underflow")

(** Text grapheme first: ( text_handle -- grapheme_handle ) *)
let step_text_grapheme_first (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | th_cell :: rest ->
      let th = v th_cell in
      (match get_text m.texts th with
       | None -> StepError (m, "text-grapheme-first: invalid text handle")
       | Some t ->
         if t.header.grapheme_count = 0 then
           StepError (m, "text-grapheme-first: empty text")
         else
           let tw = text_warp_begin t in
           match text_warp_current tw with
           | None -> StepError (m, "text-grapheme-first: no grapheme")
           | Some g ->
             let (gt', gh) = add_grapheme m.graphemes g in
             let gh_cell = if gh < 18446744073709551616 then uint_to_t gh else 0uL in
             let u' = { u with stack = gh_cell :: rest } in
             StepOk (advance_ip { (update_current m u') with graphemes = gt' }))
    | _ -> StepError (m, "text-grapheme-first: stack underflow")

(** Text grapheme last: ( text_handle -- grapheme_handle ) *)
let step_text_grapheme_last (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | th_cell :: rest ->
      let th = v th_cell in
      (match get_text m.texts th with
       | None -> StepError (m, "text-grapheme-last: invalid text handle")
       | Some t ->
         if t.header.grapheme_count = 0 then
           StepError (m, "text-grapheme-last: empty text")
         else
           let tw = text_warp_begin t in
           let last_idx = t.header.grapheme_count - 1 in
           match text_warp_goto tw last_idx with
           | None -> StepError (m, "text-grapheme-last: seek failed")
           | Some tw' ->
             match text_warp_current tw' with
             | None -> StepError (m, "text-grapheme-last: no grapheme")
             | Some g ->
               let (gt', gh) = add_grapheme m.graphemes g in
               let gh_cell = if gh < 18446744073709551616 then uint_to_t gh else 0uL in
               let u' = { u with stack = gh_cell :: rest } in
               StepOk (advance_ip { (update_current m u') with graphemes = gt' }))
    | _ -> StepError (m, "text-grapheme-last: stack underflow")

(** Text compare: ( h1 h2 -- result ) - lexicographic comparison, -1/0/1 *)
let step_text_compare (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | h2_cell :: h1_cell :: rest ->
      let h1 = v h1_cell in
      let h2 = v h2_cell in
      (match get_text m.texts h1, get_text m.texts h2 with
       | Some t1, Some t2 ->
         (* Lexicographic byte comparison *)
         let rec compare_bytes (b1 b2: list FStar.UInt8.t) : int =
           match b1, b2 with
           | [], [] -> 0
           | [], _ -> -1
           | _, [] -> 1
           | x :: xs, y :: ys ->
             let xv = FStar.UInt8.v x in
             let yv = FStar.UInt8.v y in
             if xv < yv then -1
             else if xv > yv then 1
             else compare_bytes xs ys
         in
         let cmp = compare_bytes t1.data t2.data in
         let cmp_cell = if cmp = -1 then 0xFFFFFFFFFFFFFFFFuL
                        else if cmp = 0 then 0uL
                        else 1uL in
         let u' = { u with stack = cmp_cell :: rest } in
         StepOk (advance_ip (update_current m u'))
       | _, _ -> StepError (m, "text-compare: invalid handle"))
    | _ -> StepError (m, "text-compare: stack underflow")

(** Text code point count: ( text_handle -- count ) *)
let step_text_code_point_count (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | th_cell :: rest ->
      let th = v th_cell in
      (match get_text m.texts th with
       | None -> StepError (m, "text-code-point-count: invalid handle")
       | Some t ->
         (* Count UTF-8 lead bytes *)
         let rec count_codepoints (bs: list FStar.UInt8.t) : nat =
           match bs with
           | [] -> 0
           | b :: rest' ->
             let bv = FStar.UInt8.v b in
             if bv < 128 || bv >= 192 then 1 + count_codepoints rest'
             else count_codepoints rest'
         in
         let count = count_codepoints t.data in
         let count_cell = if count < 18446744073709551616 then uint_to_t count else 0uL in
         let u' = { u with stack = count_cell :: rest } in
         StepOk (advance_ip (update_current m u')))
    | _ -> StepError (m, "text-code-point-count: stack underflow")

(** Text code point at: ( text_handle index -- codepoint ) *)
let step_text_code_point_at (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | idx_cell :: th_cell :: rest ->
      let th = v th_cell in
      let idx = v idx_cell in
      (match get_text m.texts th with
       | None -> StepError (m, "text-code-point-at: invalid handle")
       | Some t ->
         (* Use existing UTF-8 decoding from Text module *)
         let cps = Space.Text.Normalize.bytes_to_codepoints t.data in
         if idx >= List.Tot.length cps then StepError (m, "text-code-point-at: index out of bounds")
         else
           match List.Tot.nth cps idx with
           | None -> StepError (m, "text-code-point-at: index out of bounds")
           | Some cp ->
             let cp_cell = if cp < 18446744073709551616 then uint_to_t cp else 0uL in
             let u' = { u with stack = cp_cell :: rest } in
             StepOk (advance_ip (update_current m u')))
    | _ -> StepError (m, "text-code-point-at: stack underflow")

(** Emit grapheme: ( grapheme_handle -- ) - outputs grapheme bytes *)
let step_emit_grapheme (m: machine) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match u.stack with
    | gh_cell :: rest ->
      let gh = v gh_cell in
      (match get_grapheme m.graphemes gh with
       | None -> StepError (m, "emit-grapheme: invalid handle")
       | Some g ->
         (* Append grapheme bytes (as nats) to output *)
         let byte_to_nat (b:FStar.UInt8.t) : nat = FStar.UInt8.v b in
         let byte_vals = List.Tot.map byte_to_nat g.bytes in
         let u' = { u with stack = rest } in
         let m' = { (update_current m u') with output = List.Tot.append m.output byte_vals } in
         StepOk (advance_ip m'))
    | _ -> StepError (m, "emit-grapheme: stack underflow")

(** Execute text primitive with machine state *)
let step_text_prim (m: machine) (op: prim_op) : step_result =
  match op with
  (* Core text ops *)
  | PrimCreateText -> step_create_text m
  | PrimTextByteLength -> step_text_byte_length m
  | PrimTextGraphemeCount -> step_text_grapheme_count m
  | PrimTextIsSimple -> step_text_is_simple m
  | PrimTextEqual -> step_text_equal m
  | PrimTextConcat -> step_text_concat m
  | PrimTextSlice -> step_text_slice m
  (* Normalization ops *)
  | PrimTextNormalizeNfc -> step_text_normalize_nfc m
  | PrimTextNormalizeNfd -> step_text_normalize_nfd m
  | PrimTextNormalizeNfkc -> step_text_normalize_nfkc m
  | PrimTextNormalizeNfkd -> step_text_normalize_nfkd m
  (* Case mapping ops *)
  | PrimTextToUpper -> step_text_to_upper m
  | PrimTextToLower -> step_text_to_lower m
  | PrimTextToTitle -> step_text_to_title m
  (* Text warp ops *)
  | PrimTextWarpHasGrapheme -> step_text_warp_has_grapheme m
  | PrimTextWarpCurrentGrapheme -> step_text_warp_current_grapheme m
  | PrimTextWarpNextGrapheme -> step_text_warp_next_grapheme m
  | PrimTextWarpGraphemeIndex -> step_text_warp_grapheme_index m
  | PrimTextWarpGotoGrapheme -> step_text_warp_goto_grapheme m
  (* Grapheme ops *)
  | PrimGraphemeByteLength -> step_grapheme_byte_length m
  | PrimGraphemeIsAscii -> step_grapheme_is_ascii m
  | PrimGraphemeCodePoints -> step_grapheme_code_points m
  (* Grapheme access ops *)
  | PrimTextGraphemeAt -> step_text_grapheme_at m
  | PrimTextGraphemeFirst -> step_text_grapheme_first m
  | PrimTextGraphemeLast -> step_text_grapheme_last m
  (* Text comparison *)
  | PrimTextCompare -> step_text_compare m
  (* Codepoint ops *)
  | PrimTextCodePointCount -> step_text_code_point_count m
  | PrimTextCodePointAt -> step_text_code_point_at m
  (* Grapheme I/O *)
  | PrimEmitGrapheme -> step_emit_grapheme m
  | _ -> StepError (m, "unknown text primitive")

(** Execute primitive instruction - routes to appropriate handler *)
let step_prim (m: machine) (op: prim_op) : step_result =
  if is_text_prim op then
    step_text_prim m op
  else if is_io_prim op then
    step_io_prim m op
  else if is_borrow_prim op then
    step_borrow_prim m op
  else if is_warp_prim op then
    step_warp_prim m op
  else if is_bytes_meta_prim op then
    step_bytes_meta_prim m op
  else
    match current_universe m with
    | None -> StepError (m, "no current universe")
    | Some u ->
      match exec_prim op u with
      | ExecOk u' -> StepOk (advance_ip (update_current m u'))
      | ExecHalt -> StepHalt (halt m)
      | ExecError msg -> StepError (m, msg)

(** Execute one instruction *)
let step_one (m: machine) (instr: instruction) : step_result =
  match instr with
  | IPush v -> step_push m v
  | ICall target -> step_call m target
  | IReturn -> step_return m
  | IPrimitive op -> step_prim m op
  | IBranch target -> step_branch m target
  | IBranchZero target -> step_branch_zero m target
  | IBranchNonZero target -> step_branch_nonzero m target
  | ICreateUniverse (name, disc) -> step_create_universe m name 1024 disc
  | IEndUniverse name -> step_end_universe m name
  | IReleaseUniverse name -> step_release_universe m name
  | ITransferTo name -> step_transfer m name

(** Fetch instruction at current IP from program *)
let fetch_instr (prog: list instruction) (ip: ip) : option instruction =
  let idx = FStar.UInt64.v ip in
  if idx < List.Tot.length prog then List.Tot.nth prog idx
  else None

(** Run execution loop with fuel *)
let rec run_loop (m: machine) (prog: list instruction) (fuel: nat)
  : Tot step_result (decreases fuel) =
  if fuel = 0 then StepError (m, "fuel exhausted")
  else if not m.running then StepOk m
  else
    match fetch_instr prog m.inst_ptr with
    | None -> StepError (m, "invalid instruction pointer")
    | Some instr ->
      match step_one m instr with
      | StepOk m' -> run_loop m' prog (fuel - 1)
      | StepHalt m' -> StepHalt m'
      | StepError (m', msg) -> StepError (m', msg)

(** Run a program from initial state *)
let run_program (prog: list instruction) (fuel: nat) : step_result =
  let m = { initial_machine with running = true } in
  (* Create default data universe *)
  let (w', _) = create_universe m.mworld "data" 65536 Unrestricted in
  match set_current w' "data" with
  | None -> StepError (m, "failed to set current universe")
  | Some w'' -> run_loop { m with mworld = w'' } prog fuel
