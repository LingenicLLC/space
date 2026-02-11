#light "off"
module Space_BorrowOps
type universe_mem =
{universe : Space_Universe.universe; memory : Space_Memory.memory}


let __proj__Mkuniverse_mem__item__universe : universe_mem  ->  Space_Universe.universe = (fun ( projectee  :  universe_mem ) -> (match (projectee) with
| {universe = universe; memory = memory} -> begin
universe
end))


let __proj__Mkuniverse_mem__item__memory : universe_mem  ->  Space_Memory.memory = (fun ( projectee  :  universe_mem ) -> (match (projectee) with
| {universe = universe; memory = memory} -> begin
memory
end))


let fetch_borrowed : Space_Borrow.borrowed  ->  universe_mem  ->  Space_Types.cell FStar_Pervasives_Native.option = (fun ( b  :  Space_Borrow.borrowed ) ( um  :  universe_mem ) -> (match ((not (b.is_active))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((Prims.op_disEquality um.universe.id b.source_id)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
(Space_Memory.mem_fetch um.memory (FStar_UInt64.v b.address))
end)
end))


let store_borrowed : Space_Borrow.borrowed  ->  Space_Types.cell  ->  universe_mem  ->  universe_mem FStar_Pervasives_Native.option = (fun ( b  :  Space_Borrow.borrowed ) ( value  :  Space_Types.cell ) ( um  :  universe_mem ) -> (match ((not (b.is_active))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((Prims.op_disEquality um.universe.id b.source_id)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
(match ((Space_Memory.mem_store um.memory (FStar_UInt64.v b.address) value)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (mem') -> begin
FStar_Pervasives_Native.Some ({universe = um.universe; memory = mem'})
end)
end)
end))


let offset_borrowed : Space_Borrow.borrowed  ->  Space_Types.cell  ->  Space_Borrow.borrowed = (fun ( b  :  Space_Borrow.borrowed ) ( offset  :  Space_Types.cell ) -> {Space_Borrow.address = (FStar_UInt64.add_mod b.address offset); Space_Borrow.source_id = b.source_id; Space_Borrow.is_active = b.is_active})


let fetch_and_end : Space_Borrow.borrowed  ->  universe_mem  ->  (Space_Types.cell * Space_Borrow.borrowed) FStar_Pervasives_Native.option = (fun ( b  :  Space_Borrow.borrowed ) ( um  :  universe_mem ) -> (match ((fetch_borrowed b um)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (v) -> begin
FStar_Pervasives_Native.Some (((v), ((Space_Borrow.deactivate b))))
end))


let store_and_end : Space_Borrow.borrowed  ->  Space_Types.cell  ->  universe_mem  ->  (universe_mem * Space_Borrow.borrowed) FStar_Pervasives_Native.option = (fun ( b  :  Space_Borrow.borrowed ) ( value  :  Space_Types.cell ) ( um  :  universe_mem ) -> (match ((store_borrowed b value um)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (um') -> begin
FStar_Pervasives_Native.Some (((um'), ((Space_Borrow.deactivate b))))
end))




