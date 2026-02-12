open Prims
let data_universe_id : Space_Types.universe_id= Prims.int_zero
let return_universe_id : Space_Types.universe_id= Prims.int_one
let first_user_universe_id : Space_Types.universe_id= (Prims.of_int (2))
type system_universe_config = {
  data_size: Prims.nat ;
  return_size: Prims.nat }
let __proj__Mksystem_universe_config__item__data_size
  (projectee : system_universe_config) : Prims.nat=
  match projectee with | { data_size; return_size;_} -> data_size
let __proj__Mksystem_universe_config__item__return_size
  (projectee : system_universe_config) : Prims.nat=
  match projectee with | { data_size; return_size;_} -> return_size
let default_config : system_universe_config=
  { data_size = (Prims.of_int (1024)); return_size = (Prims.of_int (256)) }
type data_universe = {
  stack: Space_Stack.stack ;
  max_depth: Prims.nat }
let __proj__Mkdata_universe__item__stack (projectee : data_universe) :
  Space_Stack.stack= match projectee with | { stack; max_depth;_} -> stack
let __proj__Mkdata_universe__item__max_depth (projectee : data_universe) :
  Prims.nat= match projectee with | { stack; max_depth;_} -> max_depth
type return_universe =
  {
  stack1: Space_Stack.stack ;
  max_depth1: Prims.nat ;
  obligation_count: Prims.nat }
let __proj__Mkreturn_universe__item__stack (projectee : return_universe) :
  Space_Stack.stack=
  match projectee with
  | { stack1 = stack; max_depth1 = max_depth; obligation_count;_} -> stack
let __proj__Mkreturn_universe__item__max_depth (projectee : return_universe)
  : Prims.nat=
  match projectee with
  | { stack1 = stack; max_depth1 = max_depth; obligation_count;_} ->
      max_depth
let __proj__Mkreturn_universe__item__obligation_count
  (projectee : return_universe) : Prims.nat=
  match projectee with
  | { stack1 = stack; max_depth1 = max_depth; obligation_count;_} ->
      obligation_count
let create_data_universe (size : Prims.nat) : data_universe=
  { stack = []; max_depth = size }
let create_return_universe (size : Prims.nat) : return_universe=
  { stack1 = []; max_depth1 = size; obligation_count = Prims.int_zero }
let data_push (u : data_universe) (v : Space_Types.cell) :
  data_universe FStar_Pervasives_Native.option=
  if (FStar_List_Tot_Base.length u.stack) >= u.max_depth
  then FStar_Pervasives_Native.None
  else
    FStar_Pervasives_Native.Some
      { stack = (v :: (u.stack)); max_depth = (u.max_depth) }
let data_pop (u : data_universe) :
  (Space_Types.cell * data_universe) FStar_Pervasives_Native.option=
  match u.stack with
  | [] -> FStar_Pervasives_Native.None
  | x::rest ->
      FStar_Pervasives_Native.Some
        (x, { stack = rest; max_depth = (u.max_depth) })
let data_dup (u : data_universe) :
  data_universe FStar_Pervasives_Native.option=
  match u.stack with
  | [] -> FStar_Pervasives_Native.None
  | x::uu___ ->
      if (FStar_List_Tot_Base.length u.stack) >= u.max_depth
      then FStar_Pervasives_Native.None
      else
        FStar_Pervasives_Native.Some
          { stack = (x :: (u.stack)); max_depth = (u.max_depth) }
let data_drop (u : data_universe) :
  data_universe FStar_Pervasives_Native.option=
  match u.stack with
  | [] -> FStar_Pervasives_Native.None
  | uu___::rest ->
      FStar_Pervasives_Native.Some
        { stack = rest; max_depth = (u.max_depth) }
let return_push (u : return_universe) (v : Space_Types.cell) :
  return_universe FStar_Pervasives_Native.option=
  if (FStar_List_Tot_Base.length u.stack1) >= u.max_depth1
  then FStar_Pervasives_Native.None
  else
    FStar_Pervasives_Native.Some
      {
        stack1 = (v :: (u.stack1));
        max_depth1 = (u.max_depth1);
        obligation_count = (u.obligation_count + Prims.int_one)
      }
let return_pop (u : return_universe) :
  (Space_Types.cell * return_universe) FStar_Pervasives_Native.option=
  match u.stack1 with
  | [] -> FStar_Pervasives_Native.None
  | x::rest ->
      FStar_Pervasives_Native.Some
        (x,
          {
            stack1 = rest;
            max_depth1 = (u.max_depth1);
            obligation_count =
              (if u.obligation_count > Prims.int_zero
               then u.obligation_count - Prims.int_one
               else Prims.int_zero)
          })
let return_is_balanced (u : return_universe) : Prims.bool=
  u.obligation_count = Prims.int_zero
type system_universes = {
  data: data_universe ;
  return: return_universe }
let __proj__Mksystem_universes__item__data (projectee : system_universes) :
  data_universe= match projectee with | { data; return;_} -> data
let __proj__Mksystem_universes__item__return (projectee : system_universes) :
  return_universe= match projectee with | { data; return;_} -> return
let init_system_universes (cfg : system_universe_config) : system_universes=
  {
    data = (create_data_universe cfg.data_size);
    return = (create_return_universe cfg.return_size)
  }
let init_default : system_universes= init_system_universes default_config
let call_push_return (sys : system_universes) (addr : Space_Types.cell) :
  system_universes FStar_Pervasives_Native.option=
  match return_push sys.return addr with
  | FStar_Pervasives_Native.Some ret' ->
      FStar_Pervasives_Native.Some { data = (sys.data); return = ret' }
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
let call_pop_return (sys : system_universes) :
  (Space_Types.cell * system_universes) FStar_Pervasives_Native.option=
  match return_pop sys.return with
  | FStar_Pervasives_Native.Some (addr, ret') ->
      FStar_Pervasives_Native.Some
        (addr, { data = (sys.data); return = ret' })
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
