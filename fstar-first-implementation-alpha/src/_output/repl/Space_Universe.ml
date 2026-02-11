open Prims
type universe_state =
  | Live 
  | Destroyed 
let uu___is_Live (projectee : universe_state) : Prims.bool=
  match projectee with | Live -> true | uu___ -> false
let uu___is_Destroyed (projectee : universe_state) : Prims.bool=
  match projectee with | Destroyed -> true | uu___ -> false
type universe =
  {
  id: Space_Types.universe_id ;
  name: Space_Types.universe_name ;
  discipline: Space_Types.discipline ;
  stack: Space_Stack.stack ;
  memory: Space_Memory.memory ;
  capacity: Prims.nat ;
  state: universe_state }
let __proj__Mkuniverse__item__id (projectee : universe) :
  Space_Types.universe_id=
  match projectee with
  | { id; name; discipline; stack; memory; capacity; state;_} -> id
let __proj__Mkuniverse__item__name (projectee : universe) :
  Space_Types.universe_name=
  match projectee with
  | { id; name; discipline; stack; memory; capacity; state;_} -> name
let __proj__Mkuniverse__item__discipline (projectee : universe) :
  Space_Types.discipline=
  match projectee with
  | { id; name; discipline; stack; memory; capacity; state;_} -> discipline
let __proj__Mkuniverse__item__stack (projectee : universe) :
  Space_Stack.stack=
  match projectee with
  | { id; name; discipline; stack; memory; capacity; state;_} -> stack
let __proj__Mkuniverse__item__memory (projectee : universe) :
  Space_Memory.memory=
  match projectee with
  | { id; name; discipline; stack; memory; capacity; state;_} -> memory
let __proj__Mkuniverse__item__capacity (projectee : universe) : Prims.nat=
  match projectee with
  | { id; name; discipline; stack; memory; capacity; state;_} -> capacity
let __proj__Mkuniverse__item__state (projectee : universe) : universe_state=
  match projectee with
  | { id; name; discipline; stack; memory; capacity; state;_} -> state
let create (id : Space_Types.universe_id) (name : Space_Types.universe_name)
  (cap : Prims.nat) (disc : Space_Types.discipline) : universe=
  {
    id;
    name;
    discipline = disc;
    stack = Space_Stack.empty;
    memory = Space_Memory.empty_memory;
    capacity = cap;
    state = Live
  }
let is_live (u : universe) : Prims.bool=
  match u.state with | Live -> true | Destroyed -> false
let has_space (u : universe) : Prims.bool=
  (Space_Stack.size u.stack) < u.capacity
let stack_empty (u : universe) : Prims.bool= Space_Stack.is_empty u.stack
let universe_push (u : universe) (v : Space_Types.cell) :
  universe FStar_Pervasives_Native.option=
  if Prims.op_Negation (is_live u)
  then FStar_Pervasives_Native.None
  else
    if Prims.op_Negation (has_space u)
    then FStar_Pervasives_Native.None
    else
      FStar_Pervasives_Native.Some
        {
          id = (u.id);
          name = (u.name);
          discipline = (u.discipline);
          stack = (Space_Stack.push u.stack v);
          memory = (u.memory);
          capacity = (u.capacity);
          state = (u.state)
        }
let universe_pop (u : universe) :
  (Space_Types.cell * universe) FStar_Pervasives_Native.option=
  if Prims.op_Negation (is_live u)
  then FStar_Pervasives_Native.None
  else
    (match Space_Stack.pop u.stack with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some (v, s') ->
         FStar_Pervasives_Native.Some
           (v,
             {
               id = (u.id);
               name = (u.name);
               discipline = (u.discipline);
               stack = s';
               memory = (u.memory);
               capacity = (u.capacity);
               state = (u.state)
             }))
let should_self_destruct (u : universe) : Prims.bool=
  ((u.discipline = Space_Types.Linear) && (stack_empty u)) && (is_live u)
let destroy (u : universe) : universe=
  {
    id = (u.id);
    name = (u.name);
    discipline = (u.discipline);
    stack = (u.stack);
    memory = (u.memory);
    capacity = (u.capacity);
    state = Destroyed
  }
let release (u : universe) : universe FStar_Pervasives_Native.option=
  match u.discipline with
  | Space_Types.Affine ->
      if is_live u
      then FStar_Pervasives_Native.Some (destroy u)
      else FStar_Pervasives_Native.None
  | uu___ -> FStar_Pervasives_Native.None
