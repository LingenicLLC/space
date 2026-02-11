open Prims
type memory = {
  base: Prims.nat ;
  cells: Space_Types.cell Prims.list }
let __proj__Mkmemory__item__base (projectee : memory) : Prims.nat=
  match projectee with | { base; cells;_} -> base
let __proj__Mkmemory__item__cells (projectee : memory) :
  Space_Types.cell Prims.list=
  match projectee with | { base; cells;_} -> cells
let empty_memory : memory= { base = Prims.int_zero; cells = [] }
let mem_size (m : memory) : Prims.nat= FStar_List_Tot_Base.length m.cells
let valid_addr (m : memory) (addr : Prims.nat) : Prims.bool=
  (addr >= m.base) && (addr < (m.base + (mem_size m)))
let mem_fetch (m : memory) (addr : Prims.nat) :
  Space_Types.cell FStar_Pervasives_Native.option=
  if Prims.op_Negation (valid_addr m addr)
  then FStar_Pervasives_Native.None
  else (let idx = addr - m.base in FStar_List_Tot_Base.nth m.cells idx)
let rec update_at (xs : Space_Types.cell Prims.list) (idx : Prims.nat)
  (v : Space_Types.cell) :
  Space_Types.cell Prims.list FStar_Pervasives_Native.option=
  match (xs, idx) with
  | ([], uu___) -> FStar_Pervasives_Native.None
  | (uu___::rest, uu___1) when uu___1 = Prims.int_zero ->
      FStar_Pervasives_Native.Some (v :: rest)
  | (x::rest, n) ->
      (match update_at rest (n - Prims.int_one) v with
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
       | FStar_Pervasives_Native.Some rest' ->
           FStar_Pervasives_Native.Some (x :: rest'))
let mem_store (m : memory) (addr : Prims.nat) (v : Space_Types.cell) :
  memory FStar_Pervasives_Native.option=
  if Prims.op_Negation (valid_addr m addr)
  then FStar_Pervasives_Native.None
  else
    (let idx = addr - m.base in
     match update_at m.cells idx v with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some cells' ->
         FStar_Pervasives_Native.Some { base = (m.base); cells = cells' })
let rec zeros (n : Prims.nat) : Space_Types.cell Prims.list=
  if n = Prims.int_zero
  then []
  else Stdint.Uint64.zero :: (zeros (n - Prims.int_one))
let mem_alloc (m : memory) (n : Prims.nat) : (memory * Prims.nat)=
  let new_base = m.base + (mem_size m) in
  let new_cells = FStar_List_Tot_Base.append m.cells (zeros n) in
  ({ base = (m.base); cells = new_cells }, new_base)
type ptr = {
  addr: Prims.nat }
let __proj__Mkptr__item__addr (projectee : ptr) : Prims.nat=
  match projectee with | { addr;_} -> addr
let ptr_of_cell (c : Space_Types.cell) : ptr= { addr = (FStar_UInt64.v c) }
let cell_of_ptr (p : ptr) : Space_Types.cell=
  if p.addr < (Prims.pow2 (Prims.of_int (64)))
  then FStar_UInt64.uint_to_t p.addr
  else Stdint.Uint64.zero
