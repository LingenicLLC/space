open Prims
type world =
  {
  universes: Space_Universe.universe Prims.list ;
  next_id: Space_Types.universe_id ;
  current: Space_Types.universe_id FStar_Pervasives_Native.option }
let __proj__Mkworld__item__universes (projectee : world) :
  Space_Universe.universe Prims.list=
  match projectee with | { universes; next_id; current;_} -> universes
let __proj__Mkworld__item__next_id (projectee : world) :
  Space_Types.universe_id=
  match projectee with | { universes; next_id; current;_} -> next_id
let __proj__Mkworld__item__current (projectee : world) :
  Space_Types.universe_id FStar_Pervasives_Native.option=
  match projectee with | { universes; next_id; current;_} -> current
let empty_world : world=
  {
    universes = [];
    next_id = Prims.int_zero;
    current = FStar_Pervasives_Native.None
  }
let rec find_by_id_in_list (us : Space_Universe.universe Prims.list)
  (id : Space_Types.universe_id) :
  Space_Universe.universe FStar_Pervasives_Native.option=
  match us with
  | [] -> FStar_Pervasives_Native.None
  | u::rest ->
      if u.Space_Universe.id = id
      then FStar_Pervasives_Native.Some u
      else find_by_id_in_list rest id
let find_universe (w : world) (id : Space_Types.universe_id) :
  Space_Universe.universe FStar_Pervasives_Native.option=
  find_by_id_in_list w.universes id
let rec find_live_by_name_in_list (us : Space_Universe.universe Prims.list)
  (name : Space_Types.universe_name) :
  Space_Universe.universe FStar_Pervasives_Native.option=
  match us with
  | [] -> FStar_Pervasives_Native.None
  | u::rest ->
      if (u.Space_Universe.name = name) && (Space_Universe.is_live u)
      then FStar_Pervasives_Native.Some u
      else find_live_by_name_in_list rest name
let find_by_name (w : world) (name : Space_Types.universe_name) :
  Space_Universe.universe FStar_Pervasives_Native.option=
  find_live_by_name_in_list w.universes name
let update_universe (w : world) (u : Space_Universe.universe) : world=
  let universes' =
    FStar_List_Tot_Base.map
      (fun u' -> if u'.Space_Universe.id = u.Space_Universe.id then u else u')
      w.universes in
  { universes = universes'; next_id = (w.next_id); current = (w.current) }
let create_universe (w : world) (name : Space_Types.universe_name)
  (cap : Prims.nat) (disc : Space_Types.discipline) :
  (world * Space_Types.universe_id)=
  let id = w.next_id in
  let u = Space_Universe.create id name cap disc in
  let w' =
    {
      universes = (u :: (w.universes));
      next_id = (id + Prims.int_one);
      current = (FStar_Pervasives_Native.Some id)
    } in
  (w', id)
let get_current (w : world) :
  Space_Universe.universe FStar_Pervasives_Native.option=
  match w.current with
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
  | FStar_Pervasives_Native.Some id -> find_universe w id
let set_current (w : world) (name : Space_Types.universe_name) :
  world FStar_Pervasives_Native.option=
  match find_by_name w name with
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
  | FStar_Pervasives_Native.Some u ->
      FStar_Pervasives_Native.Some
        {
          universes = (w.universes);
          next_id = (w.next_id);
          current = (FStar_Pervasives_Native.Some (u.Space_Universe.id))
        }
let end_current (w : world) : world FStar_Pervasives_Native.option=
  match get_current w with
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
  | FStar_Pervasives_Native.Some u ->
      if
        (u.Space_Universe.discipline = Space_Types.Linear) &&
          (Prims.op_Negation (Space_Universe.stack_empty u))
      then FStar_Pervasives_Native.None
      else
        (let u' = Space_Universe.destroy u in
         FStar_Pervasives_Native.Some
           (let uu___1 = update_universe w u' in
            {
              universes = (uu___1.universes);
              next_id = (uu___1.next_id);
              current = FStar_Pervasives_Native.None
            }))
