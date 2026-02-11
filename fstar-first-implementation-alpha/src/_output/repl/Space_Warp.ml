open Prims
type warp_id = Prims.nat
type warp =
  {
  id: warp_id ;
  name: Prims.string ;
  target_id: Space_Types.universe_id ;
  position: Space_Types.cell ;
  saved_positions: Space_Types.cell Prims.list ;
  readonly: Prims.bool ;
  active: Prims.bool }
let __proj__Mkwarp__item__id (projectee : warp) : warp_id=
  match projectee with
  | { id; name; target_id; position; saved_positions; readonly; active;_} ->
      id
let __proj__Mkwarp__item__name (projectee : warp) : Prims.string=
  match projectee with
  | { id; name; target_id; position; saved_positions; readonly; active;_} ->
      name
let __proj__Mkwarp__item__target_id (projectee : warp) :
  Space_Types.universe_id=
  match projectee with
  | { id; name; target_id; position; saved_positions; readonly; active;_} ->
      target_id
let __proj__Mkwarp__item__position (projectee : warp) : Space_Types.cell=
  match projectee with
  | { id; name; target_id; position; saved_positions; readonly; active;_} ->
      position
let __proj__Mkwarp__item__saved_positions (projectee : warp) :
  Space_Types.cell Prims.list=
  match projectee with
  | { id; name; target_id; position; saved_positions; readonly; active;_} ->
      saved_positions
let __proj__Mkwarp__item__readonly (projectee : warp) : Prims.bool=
  match projectee with
  | { id; name; target_id; position; saved_positions; readonly; active;_} ->
      readonly
let __proj__Mkwarp__item__active (projectee : warp) : Prims.bool=
  match projectee with
  | { id; name; target_id; position; saved_positions; readonly; active;_} ->
      active
let create_warp (id : warp_id) (name : Prims.string)
  (target : Space_Types.universe_id) (pos : Space_Types.cell)
  (ro : Prims.bool) : warp=
  {
    id;
    name;
    target_id = target;
    position = pos;
    saved_positions = [];
    readonly = ro;
    active = true
  }
let warp_active (w : warp) : Prims.bool= w.active
let close_warp (w : warp) : warp=
  {
    id = (w.id);
    name = (w.name);
    target_id = (w.target_id);
    position = (w.position);
    saved_positions = (w.saved_positions);
    readonly = (w.readonly);
    active = false
  }
let warp_position (w : warp) : Space_Types.cell= w.position
let set_position (w : warp) (pos : Space_Types.cell) : warp=
  {
    id = (w.id);
    name = (w.name);
    target_id = (w.target_id);
    position = pos;
    saved_positions = (w.saved_positions);
    readonly = (w.readonly);
    active = (w.active)
  }
let advance_position (w : warp) (offset : Space_Types.cell) : warp=
  {
    id = (w.id);
    name = (w.name);
    target_id = (w.target_id);
    position = (FStar_UInt64.add_mod w.position offset);
    saved_positions = (w.saved_positions);
    readonly = (w.readonly);
    active = (w.active)
  }
let save_position (w : warp) : warp=
  {
    id = (w.id);
    name = (w.name);
    target_id = (w.target_id);
    position = (w.position);
    saved_positions = ((w.position) :: (w.saved_positions));
    readonly = (w.readonly);
    active = (w.active)
  }
let rec find_saved (positions : Space_Types.cell Prims.list)
  (pos : Space_Types.cell) : Prims.bool=
  match positions with
  | [] -> false
  | p::rest -> if p = pos then true else find_saved rest pos
let restore_position (w : warp) (pos : Space_Types.cell) :
  warp FStar_Pervasives_Native.option=
  if find_saved w.saved_positions pos
  then
    FStar_Pervasives_Native.Some
      {
        id = (w.id);
        name = (w.name);
        target_id = (w.target_id);
        position = pos;
        saved_positions = (w.saved_positions);
        readonly = (w.readonly);
        active = (w.active)
      }
  else FStar_Pervasives_Native.None
let is_null_position (w : warp) : Prims.bool= w.position = Stdint.Uint64.zero
type warp_table = {
  warps: warp Prims.list ;
  next_id: warp_id }
let __proj__Mkwarp_table__item__warps (projectee : warp_table) :
  warp Prims.list= match projectee with | { warps; next_id;_} -> warps
let __proj__Mkwarp_table__item__next_id (projectee : warp_table) : warp_id=
  match projectee with | { warps; next_id;_} -> next_id
let empty_warp_table : warp_table= { warps = []; next_id = Prims.int_zero }
let rec find_warp_by_id (ws : warp Prims.list) (id : warp_id) :
  warp FStar_Pervasives_Native.option=
  match ws with
  | [] -> FStar_Pervasives_Native.None
  | w::rest ->
      if (w.id = id) && w.active
      then FStar_Pervasives_Native.Some w
      else find_warp_by_id rest id
let rec find_warp_by_name (ws : warp Prims.list) (name : Prims.string) :
  warp FStar_Pervasives_Native.option=
  match ws with
  | [] -> FStar_Pervasives_Native.None
  | w::rest ->
      if (w.name = name) && w.active
      then FStar_Pervasives_Native.Some w
      else find_warp_by_name rest name
let add_warp (wt : warp_table) (name : Prims.string)
  (target : Space_Types.universe_id) (pos : Space_Types.cell)
  (ro : Prims.bool) : (warp_table * warp_id)=
  let id = wt.next_id in
  let w = create_warp id name target pos ro in
  ({ warps = (w :: (wt.warps)); next_id = (id + Prims.int_one) }, id)
let rec update_warp_in_list (ws : warp Prims.list) (w : warp) :
  warp Prims.list=
  match ws with
  | [] -> []
  | x::rest ->
      if x.id = w.id then w :: rest else x :: (update_warp_in_list rest w)
let update_warp (wt : warp_table) (w : warp) : warp_table=
  { warps = (update_warp_in_list wt.warps w); next_id = (wt.next_id) }
let rec count_warps_to (ws : warp Prims.list)
  (target : Space_Types.universe_id) : Prims.nat=
  match ws with
  | [] -> Prims.int_zero
  | w::rest ->
      let count = count_warps_to rest target in
      if (w.target_id = target) && w.active
      then count + Prims.int_one
      else count
let has_active_warps (wt : warp_table) (target : Space_Types.universe_id) :
  Prims.bool= (count_warps_to wt.warps target) > Prims.int_zero
let get_implicit_warp (wt : warp_table) :
  warp FStar_Pervasives_Native.option=
  let rec get_single_active ws found =
    match ws with
    | [] -> found
    | w::rest ->
        if w.active
        then
          (match found with
           | FStar_Pervasives_Native.None ->
               get_single_active rest (FStar_Pervasives_Native.Some w)
           | FStar_Pervasives_Native.Some uu___ ->
               FStar_Pervasives_Native.None)
        else get_single_active rest found in
  get_single_active wt.warps FStar_Pervasives_Native.None
