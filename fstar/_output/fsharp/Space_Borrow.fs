#light "off"
module Space_Borrow
type borrowed =
{address : Space_Types.cell; source_id : Space_Types.universe_id; is_active : Prims.bool}


let __proj__Mkborrowed__item__address : borrowed  ->  Space_Types.cell = (fun ( projectee  :  borrowed ) -> (match (projectee) with
| {address = address; source_id = source_id; is_active = is_active} -> begin
address
end))


let __proj__Mkborrowed__item__source_id : borrowed  ->  Space_Types.universe_id = (fun ( projectee  :  borrowed ) -> (match (projectee) with
| {address = address; source_id = source_id; is_active = is_active} -> begin
source_id
end))


let __proj__Mkborrowed__item__is_active : borrowed  ->  Prims.bool = (fun ( projectee  :  borrowed ) -> (match (projectee) with
| {address = address; source_id = source_id; is_active = is_active} -> begin
is_active
end))


let create_borrowed : Space_Types.cell  ->  Space_Types.universe_id  ->  borrowed = (fun ( addr  :  Space_Types.cell ) ( src  :  Space_Types.universe_id ) -> {address = addr; source_id = src; is_active = true})


let deactivate : borrowed  ->  borrowed = (fun ( b  :  borrowed ) -> {address = b.address; source_id = b.source_id; is_active = false})


let borrow_active : borrowed  ->  Prims.bool = (fun ( b  :  borrowed ) -> b.is_active)

type borrow_state =
{borrows : borrowed Prims.list}


let __proj__Mkborrow_state__item__borrows : borrow_state  ->  borrowed Prims.list = (fun ( projectee  :  borrow_state ) -> (match (projectee) with
| {borrows = borrows} -> begin
borrows
end))


let empty_borrow_state : borrow_state = {borrows = []}


let add_borrow : borrow_state  ->  borrowed  ->  borrow_state = (fun ( bs  :  borrow_state ) ( b  :  borrowed ) -> {borrows = (b)::bs.borrows})


let rec find_borrow : borrowed Prims.list  ->  Space_Types.cell  ->  Space_Types.universe_id  ->  borrowed FStar_Pervasives_Native.option = (fun ( bs  :  borrowed Prims.list ) ( addr  :  Space_Types.cell ) ( src  :  Space_Types.universe_id ) -> (match (bs) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (b)::rest -> begin
(match ((((Prims.op_Equality b.address addr) && (Prims.op_Equality b.source_id src)) && b.is_active)) with
| true -> begin
FStar_Pervasives_Native.Some (b)
end
| uu___ -> begin
(find_borrow rest addr src)
end)
end))


let rec remove_borrow : borrowed Prims.list  ->  Space_Types.cell  ->  Space_Types.universe_id  ->  borrowed Prims.list = (fun ( bs  :  borrowed Prims.list ) ( addr  :  Space_Types.cell ) ( src  :  Space_Types.universe_id ) -> (match (bs) with
| [] -> begin
[]
end
| (b)::rest -> begin
(match (((Prims.op_Equality b.address addr) && (Prims.op_Equality b.source_id src))) with
| true -> begin
((deactivate b))::rest
end
| uu___ -> begin
(b)::(remove_borrow rest addr src)
end)
end))


let rec count_borrows_from : borrowed Prims.list  ->  Space_Types.universe_id  ->  Prims.nat = (fun ( bs  :  borrowed Prims.list ) ( src  :  Space_Types.universe_id ) -> (match (bs) with
| [] -> begin
(Prims.parse_int "0")
end
| (b)::rest -> begin
(

let count = (count_borrows_from rest src)
in (match (((Prims.op_Equality b.source_id src) && b.is_active)) with
| true -> begin
(count + (Prims.parse_int "1"))
end
| uu___ -> begin
count
end))
end))


let has_active_borrows : borrow_state  ->  Space_Types.universe_id  ->  Prims.bool = (fun ( bs  :  borrow_state ) ( src  :  Space_Types.universe_id ) -> ((count_borrows_from bs.borrows src) > (Prims.parse_int "0")))


let borrow_pointer : Space_Universe.universe  ->  (borrowed * Space_Universe.universe) FStar_Pervasives_Native.option = (fun ( src  :  Space_Universe.universe ) -> (match ((not ((Space_Universe.is_live src)))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((Space_Universe.universe_pop src)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (addr, src') -> begin
(

let b = (create_borrowed addr src.id)
in FStar_Pervasives_Native.Some (((b), (src'))))
end)
end))


let drop_borrowed : borrow_state  ->  borrowed  ->  borrow_state = (fun ( bs  :  borrow_state ) ( b  :  borrowed ) -> {borrows = (remove_borrow bs.borrows b.address b.source_id)})


let return_borrowed : borrowed  ->  Space_Universe.universe  ->  Space_Universe.universe FStar_Pervasives_Native.option = (fun ( b  :  borrowed ) ( src  :  Space_Universe.universe ) -> (match ((not ((Space_Universe.is_live src)))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((not (b.is_active))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
(match ((Prims.op_disEquality src.id b.source_id)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___2 -> begin
(Space_Universe.universe_push src b.address)
end)
end)
end))




