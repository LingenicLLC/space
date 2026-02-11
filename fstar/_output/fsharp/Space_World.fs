#light "off"
module Space_World
type world =
{universes : Space_Universe.universe Prims.list; next_id : Space_Types.universe_id; current : Space_Types.universe_id FStar_Pervasives_Native.option}


let __proj__Mkworld__item__universes : world  ->  Space_Universe.universe Prims.list = (fun ( projectee  :  world ) -> (match (projectee) with
| {universes = universes; next_id = next_id; current = current} -> begin
universes
end))


let __proj__Mkworld__item__next_id : world  ->  Space_Types.universe_id = (fun ( projectee  :  world ) -> (match (projectee) with
| {universes = universes; next_id = next_id; current = current} -> begin
next_id
end))


let __proj__Mkworld__item__current : world  ->  Space_Types.universe_id FStar_Pervasives_Native.option = (fun ( projectee  :  world ) -> (match (projectee) with
| {universes = universes; next_id = next_id; current = current} -> begin
current
end))


let empty_world : world = {universes = []; next_id = (Prims.parse_int "0"); current = FStar_Pervasives_Native.None}


let rec find_by_id_in_list : Space_Universe.universe Prims.list  ->  Space_Types.universe_id  ->  Space_Universe.universe FStar_Pervasives_Native.option = (fun ( us  :  Space_Universe.universe Prims.list ) ( id  :  Space_Types.universe_id ) -> (match (us) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (u)::rest -> begin
(match ((Prims.op_Equality u.id id)) with
| true -> begin
FStar_Pervasives_Native.Some (u)
end
| uu___ -> begin
(find_by_id_in_list rest id)
end)
end))


let find_universe : world  ->  Space_Types.universe_id  ->  Space_Universe.universe FStar_Pervasives_Native.option = (fun ( w  :  world ) ( id  :  Space_Types.universe_id ) -> (find_by_id_in_list w.universes id))


let rec find_live_by_name_in_list : Space_Universe.universe Prims.list  ->  Space_Types.universe_name  ->  Space_Universe.universe FStar_Pervasives_Native.option = (fun ( us  :  Space_Universe.universe Prims.list ) ( name  :  Space_Types.universe_name ) -> (match (us) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (u)::rest -> begin
(match (((Prims.op_Equality u.name name) && (Space_Universe.is_live u))) with
| true -> begin
FStar_Pervasives_Native.Some (u)
end
| uu___ -> begin
(find_live_by_name_in_list rest name)
end)
end))


let find_by_name : world  ->  Space_Types.universe_name  ->  Space_Universe.universe FStar_Pervasives_Native.option = (fun ( w  :  world ) ( name  :  Space_Types.universe_name ) -> (find_live_by_name_in_list w.universes name))


let update_universe : world  ->  Space_Universe.universe  ->  world = (fun ( w  :  world ) ( u  :  Space_Universe.universe ) -> (

let universes' = (FStar_List_Tot_Base.map (fun ( u'  :  Space_Universe.universe ) -> (match ((Prims.op_Equality u'.id u.id)) with
| true -> begin
u
end
| uu___ -> begin
u'
end)) w.universes)
in {universes = universes'; next_id = w.next_id; current = w.current}))


let create_universe : world  ->  Space_Types.universe_name  ->  Prims.nat  ->  Space_Types.discipline  ->  (world * Space_Types.universe_id) = (fun ( w  :  world ) ( name  :  Space_Types.universe_name ) ( cap  :  Prims.nat ) ( disc  :  Space_Types.discipline ) -> (

let id = w.next_id
in (

let u = (Space_Universe.create id name cap disc)
in (

let w' = {universes = (u)::w.universes; next_id = (id + (Prims.parse_int "1")); current = FStar_Pervasives_Native.Some (id)}
in ((w'), (id))))))


let get_current : world  ->  Space_Universe.universe FStar_Pervasives_Native.option = (fun ( w  :  world ) -> (match (w.current) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (id) -> begin
(find_universe w id)
end))


let set_current : world  ->  Space_Types.universe_name  ->  world FStar_Pervasives_Native.option = (fun ( w  :  world ) ( name  :  Space_Types.universe_name ) -> (match ((find_by_name w name)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (u) -> begin
FStar_Pervasives_Native.Some ({universes = w.universes; next_id = w.next_id; current = FStar_Pervasives_Native.Some (u.id)})
end))


let end_current : world  ->  world FStar_Pervasives_Native.option = (fun ( w  :  world ) -> (match ((get_current w)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (((Prims.op_Equality u.discipline Space_Types.Linear) && (not ((Space_Universe.stack_empty u))))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(

let u' = (Space_Universe.destroy u)
in FStar_Pervasives_Native.Some ((

let uu___1 = (update_universe w u')
in {universes = uu___1.universes; next_id = uu___1.next_id; current = FStar_Pervasives_Native.None})))
end)
end))




