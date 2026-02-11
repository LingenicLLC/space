#light "off"
module Space_Stack

type stack =
Space_Types.cell Prims.list


let empty : stack = []


let is_empty : stack  ->  Prims.bool = (fun ( s  :  stack ) -> (match (s) with
| [] -> begin
true
end
| uu___ -> begin
false
end))


let size : stack  ->  Prims.nat = (fun ( s  :  stack ) -> (FStar_List_Tot_Base.length s))


let push : stack  ->  Space_Types.cell  ->  stack = (fun ( s  :  stack ) ( v  :  Space_Types.cell ) -> (v)::s)


let pop : stack  ->  (Space_Types.cell * stack) FStar_Pervasives_Native.option = (fun ( s  :  stack ) -> (match (s) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (x)::xs -> begin
FStar_Pervasives_Native.Some (((x), (xs)))
end))


let peek : stack  ->  Space_Types.cell FStar_Pervasives_Native.option = (fun ( s  :  stack ) -> (match (s) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (x)::uu___ -> begin
FStar_Pervasives_Native.Some (x)
end))


let dup : stack  ->  stack FStar_Pervasives_Native.option = (fun ( s  :  stack ) -> (match (s) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (x)::xs -> begin
FStar_Pervasives_Native.Some ((x)::(x)::xs)
end))


let drop : stack  ->  stack FStar_Pervasives_Native.option = (fun ( s  :  stack ) -> (match (s) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (uu___)::xs -> begin
FStar_Pervasives_Native.Some (xs)
end))


let swap : stack  ->  stack FStar_Pervasives_Native.option = (fun ( s  :  stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some ((b)::(a)::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let over : stack  ->  stack FStar_Pervasives_Native.option = (fun ( s  :  stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some ((b)::(a)::(b)::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let rot : stack  ->  stack FStar_Pervasives_Native.option = (fun ( s  :  stack ) -> (match (s) with
| (a)::(b)::(c)::xs -> begin
FStar_Pervasives_Native.Some ((c)::(a)::(b)::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let nip : stack  ->  stack FStar_Pervasives_Native.option = (fun ( s  :  stack ) -> (match (s) with
| (a)::(uu___)::xs -> begin
FStar_Pervasives_Native.Some ((a)::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let tuck : stack  ->  stack FStar_Pervasives_Native.option = (fun ( s  :  stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some ((a)::(b)::(a)::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let rec nth_opt : stack  ->  Prims.nat  ->  Space_Types.cell FStar_Pervasives_Native.option = (fun ( s  :  stack ) ( n  :  Prims.nat ) -> (match (((s), (n))) with
| ([], uu___) -> begin
FStar_Pervasives_Native.None
end
| ((x)::uu___, uu___1) when (uu___1 = (Prims.parse_int "0")) -> begin
FStar_Pervasives_Native.Some (x)
end
| ((uu___)::xs, n1) -> begin
(nth_opt xs (n1 - (Prims.parse_int "1")))
end))


let pick : stack  ->  stack FStar_Pervasives_Native.option = (fun ( s  :  stack ) -> (match (s) with
| (n_cell)::rest -> begin
(

let n = (FStar_UInt64.v n_cell)
in (match ((nth_opt rest n)) with
| FStar_Pervasives_Native.Some (x) -> begin
FStar_Pervasives_Native.Some ((x)::rest)
end
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end))
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))




