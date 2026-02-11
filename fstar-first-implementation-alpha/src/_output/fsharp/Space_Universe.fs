#light "off"
module Space_Universe
type universe_state =
| Live
| Destroyed


let uu___is_Live : universe_state  ->  Prims.bool = (fun ( projectee  :  universe_state ) -> (match (projectee) with
| Live -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_Destroyed : universe_state  ->  Prims.bool = (fun ( projectee  :  universe_state ) -> (match (projectee) with
| Destroyed -> begin
true
end
| uu___ -> begin
false
end))

type universe =
{id : Space_Types.universe_id; name : Space_Types.universe_name; discipline : Space_Types.discipline; stack : Space_Stack.stack; memory : Space_Memory.memory; capacity : Prims.nat; state : universe_state}


let __proj__Mkuniverse__item__id : universe  ->  Space_Types.universe_id = (fun ( projectee  :  universe ) -> (match (projectee) with
| {id = id; name = name; discipline = discipline; stack = stack; memory = memory; capacity = capacity; state = state} -> begin
id
end))


let __proj__Mkuniverse__item__name : universe  ->  Space_Types.universe_name = (fun ( projectee  :  universe ) -> (match (projectee) with
| {id = id; name = name; discipline = discipline; stack = stack; memory = memory; capacity = capacity; state = state} -> begin
name
end))


let __proj__Mkuniverse__item__discipline : universe  ->  Space_Types.discipline = (fun ( projectee  :  universe ) -> (match (projectee) with
| {id = id; name = name; discipline = discipline; stack = stack; memory = memory; capacity = capacity; state = state} -> begin
discipline
end))


let __proj__Mkuniverse__item__stack : universe  ->  Space_Stack.stack = (fun ( projectee  :  universe ) -> (match (projectee) with
| {id = id; name = name; discipline = discipline; stack = stack; memory = memory; capacity = capacity; state = state} -> begin
stack
end))


let __proj__Mkuniverse__item__memory : universe  ->  Space_Memory.memory = (fun ( projectee  :  universe ) -> (match (projectee) with
| {id = id; name = name; discipline = discipline; stack = stack; memory = memory; capacity = capacity; state = state} -> begin
memory
end))


let __proj__Mkuniverse__item__capacity : universe  ->  Prims.nat = (fun ( projectee  :  universe ) -> (match (projectee) with
| {id = id; name = name; discipline = discipline; stack = stack; memory = memory; capacity = capacity; state = state} -> begin
capacity
end))


let __proj__Mkuniverse__item__state : universe  ->  universe_state = (fun ( projectee  :  universe ) -> (match (projectee) with
| {id = id; name = name; discipline = discipline; stack = stack; memory = memory; capacity = capacity; state = state} -> begin
state
end))


let create : Space_Types.universe_id  ->  Space_Types.universe_name  ->  Prims.nat  ->  Space_Types.discipline  ->  universe = (fun ( id  :  Space_Types.universe_id ) ( name  :  Space_Types.universe_name ) ( cap  :  Prims.nat ) ( disc  :  Space_Types.discipline ) -> {id = id; name = name; discipline = disc; stack = Space_Stack.empty; memory = Space_Memory.empty_memory; capacity = cap; state = Live})


let is_live : universe  ->  Prims.bool = (fun ( u  :  universe ) -> (match (u.state) with
| Live -> begin
true
end
| Destroyed -> begin
false
end))


let has_space : universe  ->  Prims.bool = (fun ( u  :  universe ) -> ((Space_Stack.size u.stack) < u.capacity))


let stack_empty : universe  ->  Prims.bool = (fun ( u  :  universe ) -> (Space_Stack.is_empty u.stack))


let universe_push : universe  ->  Space_Types.cell  ->  universe FStar_Pervasives_Native.option = (fun ( u  :  universe ) ( v  :  Space_Types.cell ) -> (match ((not ((is_live u)))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((not ((has_space u)))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
FStar_Pervasives_Native.Some ({id = u.id; name = u.name; discipline = u.discipline; stack = (Space_Stack.push u.stack v); memory = u.memory; capacity = u.capacity; state = u.state})
end)
end))


let universe_pop : universe  ->  (Space_Types.cell * universe) FStar_Pervasives_Native.option = (fun ( u  :  universe ) -> (match ((not ((is_live u)))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((Space_Stack.pop u.stack)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (v, s') -> begin
FStar_Pervasives_Native.Some (((v), ({id = u.id; name = u.name; discipline = u.discipline; stack = s'; memory = u.memory; capacity = u.capacity; state = u.state})))
end)
end))


let should_self_destruct : universe  ->  Prims.bool = (fun ( u  :  universe ) -> (((Prims.op_Equality u.discipline Space_Types.Linear) && (stack_empty u)) && (is_live u)))


let destroy : universe  ->  universe = (fun ( u  :  universe ) -> {id = u.id; name = u.name; discipline = u.discipline; stack = u.stack; memory = u.memory; capacity = u.capacity; state = Destroyed})


let release : universe  ->  universe FStar_Pervasives_Native.option = (fun ( u  :  universe ) -> (match (u.discipline) with
| Space_Types.Affine -> begin
(match ((is_live u)) with
| true -> begin
FStar_Pervasives_Native.Some ((destroy u))
end
| uu___ -> begin
FStar_Pervasives_Native.None
end)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))




