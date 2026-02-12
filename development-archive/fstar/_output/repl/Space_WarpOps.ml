open Prims
type target_universe =
  {
  universe: Space_Universe.universe ;
  memory: Space_Memory.memory }
let __proj__Mktarget_universe__item__universe (projectee : target_universe) :
  Space_Universe.universe=
  match projectee with | { universe; memory;_} -> universe
let __proj__Mktarget_universe__item__memory (projectee : target_universe) :
  Space_Memory.memory= match projectee with | { universe; memory;_} -> memory
let warp_fetch (w : Space_Warp.warp) (tu : target_universe) :
  Space_Types.cell FStar_Pervasives_Native.option=
  if Prims.op_Negation w.Space_Warp.active
  then FStar_Pervasives_Native.None
  else
    if (tu.universe).Space_Universe.id <> w.Space_Warp.target_id
    then FStar_Pervasives_Native.None
    else
      Space_Memory.mem_fetch tu.memory (FStar_UInt64.v w.Space_Warp.position)
let warp_store (w : Space_Warp.warp) (value : Space_Types.cell)
  (tu : target_universe) : target_universe FStar_Pervasives_Native.option=
  if Prims.op_Negation w.Space_Warp.active
  then FStar_Pervasives_Native.None
  else
    if w.Space_Warp.readonly
    then FStar_Pervasives_Native.None
    else
      if (tu.universe).Space_Universe.id <> w.Space_Warp.target_id
      then FStar_Pervasives_Native.None
      else
        (match Space_Memory.mem_store tu.memory
                 (FStar_UInt64.v w.Space_Warp.position) value
         with
         | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
         | FStar_Pervasives_Native.Some mem' ->
             FStar_Pervasives_Native.Some
               { universe = (tu.universe); memory = mem' })
let warp_advance (w : Space_Warp.warp) (offset : Space_Types.cell) :
  Space_Warp.warp= Space_Warp.advance_position w offset
let warp_follow (w : Space_Warp.warp) (tu : target_universe) :
  Space_Warp.warp FStar_Pervasives_Native.option=
  if Prims.op_Negation w.Space_Warp.active
  then FStar_Pervasives_Native.None
  else
    (match Space_Memory.mem_fetch tu.memory
             (FStar_UInt64.v w.Space_Warp.position)
     with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some ptr ->
         FStar_Pervasives_Native.Some (Space_Warp.set_position w ptr))
let warp_follow_offset (w : Space_Warp.warp) (offset : Space_Types.cell)
  (tu : target_universe) : Space_Warp.warp FStar_Pervasives_Native.option=
  if Prims.op_Negation w.Space_Warp.active
  then FStar_Pervasives_Native.None
  else
    (let addr =
       FStar_UInt64.v (FStar_UInt64.add_mod w.Space_Warp.position offset) in
     match Space_Memory.mem_fetch tu.memory addr with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some ptr ->
         FStar_Pervasives_Native.Some (Space_Warp.set_position w ptr))
let warp_at_null (w : Space_Warp.warp) : Prims.bool=
  Space_Warp.is_null_position w
let warp_save (w : Space_Warp.warp) : (Space_Warp.warp * Space_Types.cell)=
  ((Space_Warp.save_position w), (w.Space_Warp.position))
let warp_restore (w : Space_Warp.warp) (pos : Space_Types.cell) :
  Space_Warp.warp FStar_Pervasives_Native.option=
  Space_Warp.restore_position w pos
let warp_into (name : Prims.string) (src : Space_Universe.universe)
  (target_id : Space_Types.universe_id) (readonly : Prims.bool)
  (wt : Space_Warp.warp_table) :
  (Space_Warp.warp_table * Space_Warp.warp_id * Space_Universe.universe)
    FStar_Pervasives_Native.option=
  if Prims.op_Negation (Space_Universe.is_live src)
  then FStar_Pervasives_Native.None
  else
    (match Space_Universe.universe_pop src with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some (pos, src') ->
         let uu___1 = Space_Warp.add_warp wt name target_id pos readonly in
         (match uu___1 with
          | (wt', wid) -> FStar_Pervasives_Native.Some (wt', wid, src')))
let end_warp (wid : Space_Warp.warp_id) (wt : Space_Warp.warp_table) :
  Space_Warp.warp_table FStar_Pervasives_Native.option=
  match Space_Warp.find_warp_by_id wt.Space_Warp.warps wid with
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
  | FStar_Pervasives_Native.Some w ->
      let w' = Space_Warp.close_warp w in
      FStar_Pervasives_Native.Some (Space_Warp.update_warp wt w')
