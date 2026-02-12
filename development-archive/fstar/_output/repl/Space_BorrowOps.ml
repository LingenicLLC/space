open Prims
type universe_mem =
  {
  universe: Space_Universe.universe ;
  memory: Space_Memory.memory }
let __proj__Mkuniverse_mem__item__universe (projectee : universe_mem) :
  Space_Universe.universe=
  match projectee with | { universe; memory;_} -> universe
let __proj__Mkuniverse_mem__item__memory (projectee : universe_mem) :
  Space_Memory.memory= match projectee with | { universe; memory;_} -> memory
let fetch_borrowed (b : Space_Borrow.borrowed) (um : universe_mem) :
  Space_Types.cell FStar_Pervasives_Native.option=
  if Prims.op_Negation b.Space_Borrow.is_active
  then FStar_Pervasives_Native.None
  else
    if (um.universe).Space_Universe.id <> b.Space_Borrow.source_id
    then FStar_Pervasives_Native.None
    else
      Space_Memory.mem_fetch um.memory
        (FStar_UInt64.v b.Space_Borrow.address)
let store_borrowed (b : Space_Borrow.borrowed) (value : Space_Types.cell)
  (um : universe_mem) : universe_mem FStar_Pervasives_Native.option=
  if Prims.op_Negation b.Space_Borrow.is_active
  then FStar_Pervasives_Native.None
  else
    if (um.universe).Space_Universe.id <> b.Space_Borrow.source_id
    then FStar_Pervasives_Native.None
    else
      (match Space_Memory.mem_store um.memory
               (FStar_UInt64.v b.Space_Borrow.address) value
       with
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
       | FStar_Pervasives_Native.Some mem' ->
           FStar_Pervasives_Native.Some
             { universe = (um.universe); memory = mem' })
let offset_borrowed (b : Space_Borrow.borrowed) (offset : Space_Types.cell) :
  Space_Borrow.borrowed=
  {
    Space_Borrow.address =
      (FStar_UInt64.add_mod b.Space_Borrow.address offset);
    Space_Borrow.source_id = (b.Space_Borrow.source_id);
    Space_Borrow.is_active = (b.Space_Borrow.is_active)
  }
let fetch_and_end (b : Space_Borrow.borrowed) (um : universe_mem) :
  (Space_Types.cell * Space_Borrow.borrowed) FStar_Pervasives_Native.option=
  match fetch_borrowed b um with
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
  | FStar_Pervasives_Native.Some v ->
      FStar_Pervasives_Native.Some (v, (Space_Borrow.deactivate b))
let store_and_end (b : Space_Borrow.borrowed) (value : Space_Types.cell)
  (um : universe_mem) :
  (universe_mem * Space_Borrow.borrowed) FStar_Pervasives_Native.option=
  match store_borrowed b value um with
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
  | FStar_Pervasives_Native.Some um' ->
      FStar_Pervasives_Native.Some (um', (Space_Borrow.deactivate b))
