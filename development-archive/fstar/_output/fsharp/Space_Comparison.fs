#light "off"
module Space_Comparison

let bool_to_cell : Prims.bool  ->  Space_Types.cell = (fun ( b  :  Prims.bool ) -> (match (b) with
| true -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "1")))
end
| uu___ -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end))


let cell_eq : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (bool_to_cell (Prims.op_Equality a b)))


let cell_neq : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (bool_to_cell (Prims.op_disEquality a b)))


let cell_lt : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (bool_to_cell (FStar_UInt64.lt a b)))


let cell_lte : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (bool_to_cell (FStar_UInt64.lte a b)))


let cell_gt : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (bool_to_cell (FStar_UInt64.gt a b)))


let cell_gte : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (bool_to_cell (FStar_UInt64.gte a b)))


let stack_eq : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some (((cell_eq b a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let stack_neq : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some (((cell_neq b a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let stack_lt : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some (((cell_lt b a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let stack_gt : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some (((cell_gt b a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let is_truthy : Space_Types.cell  ->  Prims.bool = (fun ( c  :  Space_Types.cell ) -> (Prims.op_disEquality c (FStar_UInt64.uint_to_t ((Prims.parse_int "0")))))


let is_negative : Space_Types.cell  ->  Prims.bool = (fun ( a  :  Space_Types.cell ) -> ((FStar_UInt64.v a) >= (Prims.parse_int "0x8000000000000000")))


let cell_lt_signed : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (

let a_neg = (is_negative a)
in (

let b_neg = (is_negative b)
in (match ((a_neg && (not (b_neg)))) with
| true -> begin
(bool_to_cell true)
end
| uu___ -> begin
(match (((not (a_neg)) && b_neg)) with
| true -> begin
(bool_to_cell false)
end
| uu___1 -> begin
(bool_to_cell (FStar_UInt64.lt a b))
end)
end))))


let cell_gt_signed : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (

let a_neg = (is_negative a)
in (

let b_neg = (is_negative b)
in (match ((a_neg && (not (b_neg)))) with
| true -> begin
(bool_to_cell false)
end
| uu___ -> begin
(match (((not (a_neg)) && b_neg)) with
| true -> begin
(bool_to_cell true)
end
| uu___1 -> begin
(bool_to_cell (FStar_UInt64.gt a b))
end)
end))))


let stack_lt_signed : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some (((cell_lt_signed b a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let stack_gt_signed : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some (((cell_gt_signed b a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))




