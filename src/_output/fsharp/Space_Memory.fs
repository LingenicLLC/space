#light "off"
module Space_Memory
type memory =
{base1 : Prims.nat; cells : Space_Types.cell Prims.list}


let __proj__Mkmemory__item__base : memory  ->  Prims.nat = (fun ( projectee  :  memory ) -> (match (projectee) with
| {base1 = base1; cells = cells} -> begin
base1
end))


let __proj__Mkmemory__item__cells : memory  ->  Space_Types.cell Prims.list = (fun ( projectee  :  memory ) -> (match (projectee) with
| {base1 = base1; cells = cells} -> begin
cells
end))


let empty_memory : memory = {base1 = (Prims.parse_int "0"); cells = []}


let mem_size : memory  ->  Prims.nat = (fun ( m  :  memory ) -> (FStar_List_Tot_Base.length m.cells))


let valid_addr : memory  ->  Prims.nat  ->  Prims.bool = (fun ( m  :  memory ) ( addr  :  Prims.nat ) -> ((addr >= m.base1) && (addr < (m.base1 + (mem_size m)))))


let mem_fetch : memory  ->  Prims.nat  ->  Space_Types.cell FStar_Pervasives_Native.option = (fun ( m  :  memory ) ( addr  :  Prims.nat ) -> (match ((not ((valid_addr m addr)))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(

let idx = (addr - m.base1)
in (FStar_List_Tot_Base.nth m.cells idx))
end))


let rec update_at : Space_Types.cell Prims.list  ->  Prims.nat  ->  Space_Types.cell  ->  Space_Types.cell Prims.list FStar_Pervasives_Native.option = (fun ( xs  :  Space_Types.cell Prims.list ) ( idx  :  Prims.nat ) ( v  :  Space_Types.cell ) -> (match (((xs), (idx))) with
| ([], uu___) -> begin
FStar_Pervasives_Native.None
end
| ((uu___)::rest, uu___1) when (uu___1 = (Prims.parse_int "0")) -> begin
FStar_Pervasives_Native.Some ((v)::rest)
end
| ((x)::rest, n) -> begin
(match ((update_at rest (n - (Prims.parse_int "1")) v)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (rest') -> begin
FStar_Pervasives_Native.Some ((x)::rest')
end)
end))


let mem_store : memory  ->  Prims.nat  ->  Space_Types.cell  ->  memory FStar_Pervasives_Native.option = (fun ( m  :  memory ) ( addr  :  Prims.nat ) ( v  :  Space_Types.cell ) -> (match ((not ((valid_addr m addr)))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(

let idx = (addr - m.base1)
in (match ((update_at m.cells idx v)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (cells') -> begin
FStar_Pervasives_Native.Some ({base1 = m.base1; cells = cells'})
end))
end))


let rec zeros : Prims.nat  ->  Space_Types.cell Prims.list = (fun ( n  :  Prims.nat ) -> (match ((Prims.op_Equality n (Prims.parse_int "0"))) with
| true -> begin
[]
end
| uu___ -> begin
((FStar_UInt64.uint_to_t ((Prims.parse_int "0"))))::(zeros (n - (Prims.parse_int "1")))
end))


let mem_alloc : memory  ->  Prims.nat  ->  (memory * Prims.nat) = (fun ( m  :  memory ) ( n  :  Prims.nat ) -> (

let new_base = (m.base1 + (mem_size m))
in (

let new_cells = (FStar_List_Tot_Base.append m.cells (zeros n))
in (({base1 = m.base1; cells = new_cells}), (new_base)))))

type ptr =
{addr : Prims.nat}


let __proj__Mkptr__item__addr : ptr  ->  Prims.nat = (fun ( projectee  :  ptr ) -> (match (projectee) with
| {addr = addr} -> begin
addr
end))


let ptr_of_cell : Space_Types.cell  ->  ptr = (fun ( c  :  Space_Types.cell ) -> {addr = (FStar_UInt64.v c)})


let cell_of_ptr : ptr  ->  Space_Types.cell = (fun ( p  :  ptr ) -> (match ((p.addr < (Prims.pow2 (Prims.parse_int "64")))) with
| true -> begin
(FStar_UInt64.uint_to_t p.addr)
end
| uu___ -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end))




