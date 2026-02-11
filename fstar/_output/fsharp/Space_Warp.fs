#light "off"
module Space_Warp

type warp_id =
Prims.nat

type warp =
{id : warp_id; name : Prims.string; target_id : Space_Types.universe_id; position : Space_Types.cell; saved_positions : Space_Types.cell Prims.list; readonly : Prims.bool; active : Prims.bool}


let __proj__Mkwarp__item__id : warp  ->  warp_id = (fun ( projectee  :  warp ) -> (match (projectee) with
| {id = id; name = name; target_id = target_id; position = position; saved_positions = saved_positions; readonly = readonly; active = active} -> begin
id
end))


let __proj__Mkwarp__item__name : warp  ->  Prims.string = (fun ( projectee  :  warp ) -> (match (projectee) with
| {id = id; name = name; target_id = target_id; position = position; saved_positions = saved_positions; readonly = readonly; active = active} -> begin
name
end))


let __proj__Mkwarp__item__target_id : warp  ->  Space_Types.universe_id = (fun ( projectee  :  warp ) -> (match (projectee) with
| {id = id; name = name; target_id = target_id; position = position; saved_positions = saved_positions; readonly = readonly; active = active} -> begin
target_id
end))


let __proj__Mkwarp__item__position : warp  ->  Space_Types.cell = (fun ( projectee  :  warp ) -> (match (projectee) with
| {id = id; name = name; target_id = target_id; position = position; saved_positions = saved_positions; readonly = readonly; active = active} -> begin
position
end))


let __proj__Mkwarp__item__saved_positions : warp  ->  Space_Types.cell Prims.list = (fun ( projectee  :  warp ) -> (match (projectee) with
| {id = id; name = name; target_id = target_id; position = position; saved_positions = saved_positions; readonly = readonly; active = active} -> begin
saved_positions
end))


let __proj__Mkwarp__item__readonly : warp  ->  Prims.bool = (fun ( projectee  :  warp ) -> (match (projectee) with
| {id = id; name = name; target_id = target_id; position = position; saved_positions = saved_positions; readonly = readonly; active = active} -> begin
readonly
end))


let __proj__Mkwarp__item__active : warp  ->  Prims.bool = (fun ( projectee  :  warp ) -> (match (projectee) with
| {id = id; name = name; target_id = target_id; position = position; saved_positions = saved_positions; readonly = readonly; active = active} -> begin
active
end))


let create_warp : warp_id  ->  Prims.string  ->  Space_Types.universe_id  ->  Space_Types.cell  ->  Prims.bool  ->  warp = (fun ( id  :  warp_id ) ( name  :  Prims.string ) ( target  :  Space_Types.universe_id ) ( pos  :  Space_Types.cell ) ( ro  :  Prims.bool ) -> {id = id; name = name; target_id = target; position = pos; saved_positions = []; readonly = ro; active = true})


let warp_active : warp  ->  Prims.bool = (fun ( w  :  warp ) -> w.active)


let close_warp : warp  ->  warp = (fun ( w  :  warp ) -> {id = w.id; name = w.name; target_id = w.target_id; position = w.position; saved_positions = w.saved_positions; readonly = w.readonly; active = false})


let warp_position : warp  ->  Space_Types.cell = (fun ( w  :  warp ) -> w.position)


let set_position : warp  ->  Space_Types.cell  ->  warp = (fun ( w  :  warp ) ( pos  :  Space_Types.cell ) -> {id = w.id; name = w.name; target_id = w.target_id; position = pos; saved_positions = w.saved_positions; readonly = w.readonly; active = w.active})


let advance_position : warp  ->  Space_Types.cell  ->  warp = (fun ( w  :  warp ) ( offset  :  Space_Types.cell ) -> {id = w.id; name = w.name; target_id = w.target_id; position = (FStar_UInt64.add_mod w.position offset); saved_positions = w.saved_positions; readonly = w.readonly; active = w.active})


let save_position : warp  ->  warp = (fun ( w  :  warp ) -> {id = w.id; name = w.name; target_id = w.target_id; position = w.position; saved_positions = (w.position)::w.saved_positions; readonly = w.readonly; active = w.active})


let rec find_saved : Space_Types.cell Prims.list  ->  Space_Types.cell  ->  Prims.bool = (fun ( positions  :  Space_Types.cell Prims.list ) ( pos  :  Space_Types.cell ) -> (match (positions) with
| [] -> begin
false
end
| (p)::rest -> begin
(match ((Prims.op_Equality p pos)) with
| true -> begin
true
end
| uu___ -> begin
(find_saved rest pos)
end)
end))


let restore_position : warp  ->  Space_Types.cell  ->  warp FStar_Pervasives_Native.option = (fun ( w  :  warp ) ( pos  :  Space_Types.cell ) -> (match ((find_saved w.saved_positions pos)) with
| true -> begin
FStar_Pervasives_Native.Some ({id = w.id; name = w.name; target_id = w.target_id; position = pos; saved_positions = w.saved_positions; readonly = w.readonly; active = w.active})
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let is_null_position : warp  ->  Prims.bool = (fun ( w  :  warp ) -> (Prims.op_Equality w.position (FStar_UInt64.uint_to_t ((Prims.parse_int "0")))))

type warp_table =
{warps : warp Prims.list; next_id : warp_id}


let __proj__Mkwarp_table__item__warps : warp_table  ->  warp Prims.list = (fun ( projectee  :  warp_table ) -> (match (projectee) with
| {warps = warps; next_id = next_id} -> begin
warps
end))


let __proj__Mkwarp_table__item__next_id : warp_table  ->  warp_id = (fun ( projectee  :  warp_table ) -> (match (projectee) with
| {warps = warps; next_id = next_id} -> begin
next_id
end))


let empty_warp_table : warp_table = {warps = []; next_id = (Prims.parse_int "0")}


let rec find_warp_by_id : warp Prims.list  ->  warp_id  ->  warp FStar_Pervasives_Native.option = (fun ( ws  :  warp Prims.list ) ( id  :  warp_id ) -> (match (ws) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (w)::rest -> begin
(match (((Prims.op_Equality w.id id) && w.active)) with
| true -> begin
FStar_Pervasives_Native.Some (w)
end
| uu___ -> begin
(find_warp_by_id rest id)
end)
end))


let rec find_warp_by_name : warp Prims.list  ->  Prims.string  ->  warp FStar_Pervasives_Native.option = (fun ( ws  :  warp Prims.list ) ( name  :  Prims.string ) -> (match (ws) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (w)::rest -> begin
(match (((Prims.op_Equality w.name name) && w.active)) with
| true -> begin
FStar_Pervasives_Native.Some (w)
end
| uu___ -> begin
(find_warp_by_name rest name)
end)
end))


let add_warp : warp_table  ->  Prims.string  ->  Space_Types.universe_id  ->  Space_Types.cell  ->  Prims.bool  ->  (warp_table * warp_id) = (fun ( wt  :  warp_table ) ( name  :  Prims.string ) ( target  :  Space_Types.universe_id ) ( pos  :  Space_Types.cell ) ( ro  :  Prims.bool ) -> (

let id = wt.next_id
in (

let w = (create_warp id name target pos ro)
in (({warps = (w)::wt.warps; next_id = (id + (Prims.parse_int "1"))}), (id)))))


let rec update_warp_in_list : warp Prims.list  ->  warp  ->  warp Prims.list = (fun ( ws  :  warp Prims.list ) ( w  :  warp ) -> (match (ws) with
| [] -> begin
[]
end
| (x)::rest -> begin
(match ((Prims.op_Equality x.id w.id)) with
| true -> begin
(w)::rest
end
| uu___ -> begin
(x)::(update_warp_in_list rest w)
end)
end))


let update_warp : warp_table  ->  warp  ->  warp_table = (fun ( wt  :  warp_table ) ( w  :  warp ) -> {warps = (update_warp_in_list wt.warps w); next_id = wt.next_id})


let rec count_warps_to : warp Prims.list  ->  Space_Types.universe_id  ->  Prims.nat = (fun ( ws  :  warp Prims.list ) ( target  :  Space_Types.universe_id ) -> (match (ws) with
| [] -> begin
(Prims.parse_int "0")
end
| (w)::rest -> begin
(

let count = (count_warps_to rest target)
in (match (((Prims.op_Equality w.target_id target) && w.active)) with
| true -> begin
(count + (Prims.parse_int "1"))
end
| uu___ -> begin
count
end))
end))


let has_active_warps : warp_table  ->  Space_Types.universe_id  ->  Prims.bool = (fun ( wt  :  warp_table ) ( target  :  Space_Types.universe_id ) -> ((count_warps_to wt.warps target) > (Prims.parse_int "0")))


let get_implicit_warp : warp_table  ->  warp FStar_Pervasives_Native.option = (fun ( wt  :  warp_table ) -> (

let rec get_single_active : warp Prims.list  ->  warp FStar_Pervasives_Native.option  ->  warp FStar_Pervasives_Native.option = (fun ( ws  :  warp Prims.list ) ( found  :  warp FStar_Pervasives_Native.option ) -> (match (ws) with
| [] -> begin
found
end
| (w)::rest -> begin
(match (w.active) with
| true -> begin
(match (found) with
| FStar_Pervasives_Native.None -> begin
(get_single_active rest (FStar_Pervasives_Native.Some (w)))
end
| FStar_Pervasives_Native.Some (uu___) -> begin
FStar_Pervasives_Native.None
end)
end
| uu___ -> begin
(get_single_active rest found)
end)
end))
in (get_single_active wt.warps FStar_Pervasives_Native.None)))




