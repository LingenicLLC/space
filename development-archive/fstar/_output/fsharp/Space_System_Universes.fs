#light "off"
module Space_System_Universes

let data_universe_id : Space_Types.universe_id = (Prims.parse_int "0")


let return_universe_id : Space_Types.universe_id = (Prims.parse_int "1")


let first_user_universe_id : Space_Types.universe_id = (Prims.parse_int "2")

type system_universe_config =
{data_size : Prims.nat; return_size : Prims.nat}


let __proj__Mksystem_universe_config__item__data_size : system_universe_config  ->  Prims.nat = (fun ( projectee  :  system_universe_config ) -> (match (projectee) with
| {data_size = data_size; return_size = return_size} -> begin
data_size
end))


let __proj__Mksystem_universe_config__item__return_size : system_universe_config  ->  Prims.nat = (fun ( projectee  :  system_universe_config ) -> (match (projectee) with
| {data_size = data_size; return_size = return_size} -> begin
return_size
end))


let default_config : system_universe_config = {data_size = (Prims.parse_int "1024"); return_size = (Prims.parse_int "256")}

type data_universe =
{stack : Space_Stack.stack; max_depth : Prims.nat}


let __proj__Mkdata_universe__item__stack : data_universe  ->  Space_Stack.stack = (fun ( projectee  :  data_universe ) -> (match (projectee) with
| {stack = stack; max_depth = max_depth} -> begin
stack
end))


let __proj__Mkdata_universe__item__max_depth : data_universe  ->  Prims.nat = (fun ( projectee  :  data_universe ) -> (match (projectee) with
| {stack = stack; max_depth = max_depth} -> begin
max_depth
end))

type return_universe =
{stack1 : Space_Stack.stack; max_depth1 : Prims.nat; obligation_count : Prims.nat}


let __proj__Mkreturn_universe__item__stack : return_universe  ->  Space_Stack.stack = (fun ( projectee  :  return_universe ) -> (match (projectee) with
| {stack1 = stack; max_depth1 = max_depth; obligation_count = obligation_count} -> begin
stack
end))


let __proj__Mkreturn_universe__item__max_depth : return_universe  ->  Prims.nat = (fun ( projectee  :  return_universe ) -> (match (projectee) with
| {stack1 = stack; max_depth1 = max_depth; obligation_count = obligation_count} -> begin
max_depth
end))


let __proj__Mkreturn_universe__item__obligation_count : return_universe  ->  Prims.nat = (fun ( projectee  :  return_universe ) -> (match (projectee) with
| {stack1 = stack; max_depth1 = max_depth; obligation_count = obligation_count} -> begin
obligation_count
end))


let create_data_universe : Prims.nat  ->  data_universe = (fun ( size  :  Prims.nat ) -> {stack = []; max_depth = size})


let create_return_universe : Prims.nat  ->  return_universe = (fun ( size  :  Prims.nat ) -> {stack1 = []; max_depth1 = size; obligation_count = (Prims.parse_int "0")})


let data_push : data_universe  ->  Space_Types.cell  ->  data_universe FStar_Pervasives_Native.option = (fun ( u  :  data_universe ) ( v  :  Space_Types.cell ) -> (match (((FStar_List_Tot_Base.length u.stack) >= u.max_depth)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
FStar_Pervasives_Native.Some ({stack = (v)::u.stack; max_depth = u.max_depth})
end))


let data_pop : data_universe  ->  (Space_Types.cell * data_universe) FStar_Pervasives_Native.option = (fun ( u  :  data_universe ) -> (match (u.stack) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (x)::rest -> begin
FStar_Pervasives_Native.Some (((x), ({stack = rest; max_depth = u.max_depth})))
end))


let data_dup : data_universe  ->  data_universe FStar_Pervasives_Native.option = (fun ( u  :  data_universe ) -> (match (u.stack) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (x)::uu___ -> begin
(match (((FStar_List_Tot_Base.length u.stack) >= u.max_depth)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
FStar_Pervasives_Native.Some ({stack = (x)::u.stack; max_depth = u.max_depth})
end)
end))


let data_drop : data_universe  ->  data_universe FStar_Pervasives_Native.option = (fun ( u  :  data_universe ) -> (match (u.stack) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (uu___)::rest -> begin
FStar_Pervasives_Native.Some ({stack = rest; max_depth = u.max_depth})
end))


let return_push : return_universe  ->  Space_Types.cell  ->  return_universe FStar_Pervasives_Native.option = (fun ( u  :  return_universe ) ( v  :  Space_Types.cell ) -> (match (((FStar_List_Tot_Base.length u.stack1) >= u.max_depth1)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
FStar_Pervasives_Native.Some ({stack1 = (v)::u.stack1; max_depth1 = u.max_depth1; obligation_count = (u.obligation_count + (Prims.parse_int "1"))})
end))


let return_pop : return_universe  ->  (Space_Types.cell * return_universe) FStar_Pervasives_Native.option = (fun ( u  :  return_universe ) -> (match (u.stack1) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (x)::rest -> begin
FStar_Pervasives_Native.Some (((x), ({stack1 = rest; max_depth1 = u.max_depth1; obligation_count = (match ((u.obligation_count > (Prims.parse_int "0"))) with
| true -> begin
(u.obligation_count - (Prims.parse_int "1"))
end
| uu___ -> begin
(Prims.parse_int "0")
end)})))
end))


let return_is_balanced : return_universe  ->  Prims.bool = (fun ( u  :  return_universe ) -> (Prims.op_Equality u.obligation_count (Prims.parse_int "0")))

type system_universes =
{data : data_universe; return1 : return_universe}


let __proj__Mksystem_universes__item__data : system_universes  ->  data_universe = (fun ( projectee  :  system_universes ) -> (match (projectee) with
| {data = data; return1 = return1} -> begin
data
end))


let __proj__Mksystem_universes__item__return : system_universes  ->  return_universe = (fun ( projectee  :  system_universes ) -> (match (projectee) with
| {data = data; return1 = return1} -> begin
return1
end))


let init_system_universes : system_universe_config  ->  system_universes = (fun ( cfg  :  system_universe_config ) -> {data = (create_data_universe cfg.data_size); return1 = (create_return_universe cfg.return_size)})


let init_default : system_universes = (init_system_universes default_config)


let call_push_return : system_universes  ->  Space_Types.cell  ->  system_universes FStar_Pervasives_Native.option = (fun ( sys  :  system_universes ) ( addr  :  Space_Types.cell ) -> (match ((return_push sys.return1 addr)) with
| FStar_Pervasives_Native.Some (ret') -> begin
FStar_Pervasives_Native.Some ({data = sys.data; return1 = ret'})
end
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end))


let call_pop_return : system_universes  ->  (Space_Types.cell * system_universes) FStar_Pervasives_Native.option = (fun ( sys  :  system_universes ) -> (match ((return_pop sys.return1)) with
| FStar_Pervasives_Native.Some (addr, ret') -> begin
FStar_Pervasives_Native.Some (((addr), ({data = sys.data; return1 = ret'})))
end
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end))




