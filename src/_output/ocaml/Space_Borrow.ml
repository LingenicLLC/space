open Prims
type borrowed =
  {
  address: Space_Types.cell ;
  source_id: Space_Types.universe_id ;
  is_active: Prims.bool }
let __proj__Mkborrowed__item__address (projectee : borrowed) :
  Space_Types.cell=
  match projectee with | { address; source_id; is_active;_} -> address
let __proj__Mkborrowed__item__source_id (projectee : borrowed) :
  Space_Types.universe_id=
  match projectee with | { address; source_id; is_active;_} -> source_id
let __proj__Mkborrowed__item__is_active (projectee : borrowed) : Prims.bool=
  match projectee with | { address; source_id; is_active;_} -> is_active
let create_borrowed (addr : Space_Types.cell) (src : Space_Types.universe_id)
  : borrowed= { address = addr; source_id = src; is_active = true }
let deactivate (b : borrowed) : borrowed=
  { address = (b.address); source_id = (b.source_id); is_active = false }
let borrow_active (b : borrowed) : Prims.bool= b.is_active
type borrow_state = {
  borrows: borrowed Prims.list }
let __proj__Mkborrow_state__item__borrows (projectee : borrow_state) :
  borrowed Prims.list= match projectee with | { borrows;_} -> borrows
let empty_borrow_state : borrow_state= { borrows = [] }
let add_borrow (bs : borrow_state) (b : borrowed) : borrow_state=
  { borrows = (b :: (bs.borrows)) }
let rec find_borrow (bs : borrowed Prims.list) (addr : Space_Types.cell)
  (src : Space_Types.universe_id) : borrowed FStar_Pervasives_Native.option=
  match bs with
  | [] -> FStar_Pervasives_Native.None
  | b::rest ->
      if ((b.address = addr) && (b.source_id = src)) && b.is_active
      then FStar_Pervasives_Native.Some b
      else find_borrow rest addr src
let rec remove_borrow (bs : borrowed Prims.list) (addr : Space_Types.cell)
  (src : Space_Types.universe_id) : borrowed Prims.list=
  match bs with
  | [] -> []
  | b::rest ->
      if (b.address = addr) && (b.source_id = src)
      then (deactivate b) :: rest
      else b :: (remove_borrow rest addr src)
let rec count_borrows_from (bs : borrowed Prims.list)
  (src : Space_Types.universe_id) : Prims.nat=
  match bs with
  | [] -> Prims.int_zero
  | b::rest ->
      let count = count_borrows_from rest src in
      if (b.source_id = src) && b.is_active
      then count + Prims.int_one
      else count
let has_active_borrows (bs : borrow_state) (src : Space_Types.universe_id) :
  Prims.bool= (count_borrows_from bs.borrows src) > Prims.int_zero
let borrow_pointer (src : Space_Universe.universe) :
  (borrowed * Space_Universe.universe) FStar_Pervasives_Native.option=
  if Prims.op_Negation (Space_Universe.is_live src)
  then FStar_Pervasives_Native.None
  else
    (match Space_Universe.universe_pop src with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some (addr, src') ->
         let b = create_borrowed addr src.Space_Universe.id in
         FStar_Pervasives_Native.Some (b, src'))
let drop_borrowed (bs : borrow_state) (b : borrowed) : borrow_state=
  { borrows = (remove_borrow bs.borrows b.address b.source_id) }
let return_borrowed (b : borrowed) (src : Space_Universe.universe) :
  Space_Universe.universe FStar_Pervasives_Native.option=
  if Prims.op_Negation (Space_Universe.is_live src)
  then FStar_Pervasives_Native.None
  else
    if Prims.op_Negation b.is_active
    then FStar_Pervasives_Native.None
    else
      if src.Space_Universe.id <> b.source_id
      then FStar_Pervasives_Native.None
      else Space_Universe.universe_push src b.address
