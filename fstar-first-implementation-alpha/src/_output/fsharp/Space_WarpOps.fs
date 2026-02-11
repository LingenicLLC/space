#light "off"
module Space_WarpOps
type target_universe =
{universe : Space_Universe.universe; memory : Space_Memory.memory}


let __proj__Mktarget_universe__item__universe : target_universe  ->  Space_Universe.universe = (fun ( projectee  :  target_universe ) -> (match (projectee) with
| {universe = universe; memory = memory} -> begin
universe
end))


let __proj__Mktarget_universe__item__memory : target_universe  ->  Space_Memory.memory = (fun ( projectee  :  target_universe ) -> (match (projectee) with
| {universe = universe; memory = memory} -> begin
memory
end))


let warp_fetch : Space_Warp.warp  ->  target_universe  ->  Space_Types.cell FStar_Pervasives_Native.option = (fun ( w  :  Space_Warp.warp ) ( tu  :  target_universe ) -> (match ((not (w.active))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((Prims.op_disEquality tu.universe.id w.target_id)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
(Space_Memory.mem_fetch tu.memory (FStar_UInt64.v w.position))
end)
end))


let warp_store : Space_Warp.warp  ->  Space_Types.cell  ->  target_universe  ->  target_universe FStar_Pervasives_Native.option = (fun ( w  :  Space_Warp.warp ) ( value  :  Space_Types.cell ) ( tu  :  target_universe ) -> (match ((not (w.active))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match (w.readonly) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
(match ((Prims.op_disEquality tu.universe.id w.target_id)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___2 -> begin
(match ((Space_Memory.mem_store tu.memory (FStar_UInt64.v w.position) value)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (mem') -> begin
FStar_Pervasives_Native.Some ({universe = tu.universe; memory = mem'})
end)
end)
end)
end))


let warp_advance : Space_Warp.warp  ->  Space_Types.cell  ->  Space_Warp.warp = (fun ( w  :  Space_Warp.warp ) ( offset  :  Space_Types.cell ) -> (Space_Warp.advance_position w offset))


let warp_follow : Space_Warp.warp  ->  target_universe  ->  Space_Warp.warp FStar_Pervasives_Native.option = (fun ( w  :  Space_Warp.warp ) ( tu  :  target_universe ) -> (match ((not (w.active))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((Space_Memory.mem_fetch tu.memory (FStar_UInt64.v w.position))) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (ptr) -> begin
FStar_Pervasives_Native.Some ((Space_Warp.set_position w ptr))
end)
end))


let warp_follow_offset : Space_Warp.warp  ->  Space_Types.cell  ->  target_universe  ->  Space_Warp.warp FStar_Pervasives_Native.option = (fun ( w  :  Space_Warp.warp ) ( offset  :  Space_Types.cell ) ( tu  :  target_universe ) -> (match ((not (w.active))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(

let addr = (FStar_UInt64.v (FStar_UInt64.add_mod w.position offset))
in (match ((Space_Memory.mem_fetch tu.memory addr)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (ptr) -> begin
FStar_Pervasives_Native.Some ((Space_Warp.set_position w ptr))
end))
end))


let warp_at_null : Space_Warp.warp  ->  Prims.bool = (fun ( w  :  Space_Warp.warp ) -> (Space_Warp.is_null_position w))


let warp_save : Space_Warp.warp  ->  (Space_Warp.warp * Space_Types.cell) = (fun ( w  :  Space_Warp.warp ) -> (((Space_Warp.save_position w)), (w.position)))


let warp_restore : Space_Warp.warp  ->  Space_Types.cell  ->  Space_Warp.warp FStar_Pervasives_Native.option = (fun ( w  :  Space_Warp.warp ) ( pos  :  Space_Types.cell ) -> (Space_Warp.restore_position w pos))


let warp_into : Prims.string  ->  Space_Universe.universe  ->  Space_Types.universe_id  ->  Prims.bool  ->  Space_Warp.warp_table  ->  (Space_Warp.warp_table * Space_Warp.warp_id * Space_Universe.universe) FStar_Pervasives_Native.option = (fun ( name  :  Prims.string ) ( src  :  Space_Universe.universe ) ( target_id  :  Space_Types.universe_id ) ( readonly  :  Prims.bool ) ( wt  :  Space_Warp.warp_table ) -> (match ((not ((Space_Universe.is_live src)))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((Space_Universe.universe_pop src)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (pos, src') -> begin
(

let uu___1 = (Space_Warp.add_warp wt name target_id pos readonly)
in (match (uu___1) with
| (wt', wid) -> begin
FStar_Pervasives_Native.Some (((wt'), (wid), (src')))
end))
end)
end))


let end_warp : Space_Warp.warp_id  ->  Space_Warp.warp_table  ->  Space_Warp.warp_table FStar_Pervasives_Native.option = (fun ( wid  :  Space_Warp.warp_id ) ( wt  :  Space_Warp.warp_table ) -> (match ((Space_Warp.find_warp_by_id wt.warps wid)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (w) -> begin
(

let w' = (Space_Warp.close_warp w)
in FStar_Pervasives_Native.Some ((Space_Warp.update_warp wt w')))
end))




