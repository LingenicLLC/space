#light "off"
module Space_Arithmetic

let add_cells : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (FStar_UInt64.add_mod a b))


let sub_cells : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (FStar_UInt64.sub_mod a b))


let mul_cells : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (FStar_UInt64.mul_mod a b))


let div_cells : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell FStar_Pervasives_Native.option = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (match ((Prims.op_Equality (FStar_UInt64.v b) (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
FStar_Pervasives_Native.Some ((FStar_UInt64.div a b))
end))


let mod_cells : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell FStar_Pervasives_Native.option = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (match ((Prims.op_Equality (FStar_UInt64.v b) (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
FStar_Pervasives_Native.Some ((FStar_UInt64.rem a b))
end))


let stack_add : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some (((add_cells b a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let stack_sub : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some (((sub_cells b a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let stack_mul : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some (((mul_cells b a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let stack_div : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
(match ((div_cells b a)) with
| FStar_Pervasives_Native.Some (r) -> begin
FStar_Pervasives_Native.Some ((r)::xs)
end
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let stack_mod : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
(match ((mod_cells b a)) with
| FStar_Pervasives_Native.Some (r) -> begin
FStar_Pervasives_Native.Some ((r)::xs)
end
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let negate_cell : Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) -> (FStar_UInt64.sub_mod (FStar_UInt64.uint_to_t ((Prims.parse_int "0"))) a))


let stack_negate : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::xs -> begin
FStar_Pervasives_Native.Some (((negate_cell a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let is_negative : Space_Types.cell  ->  Prims.bool = (fun ( a  :  Space_Types.cell ) -> ((FStar_UInt64.v a) >= (Prims.parse_int "0x8000000000000000")))


let abs_signed : Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) -> (match ((is_negative a)) with
| true -> begin
(negate_cell a)
end
| uu___ -> begin
a
end))


let div_signed : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell FStar_Pervasives_Native.option = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (match ((Prims.op_Equality (FStar_UInt64.v b) (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(

let a_neg = (is_negative a)
in (

let b_neg = (is_negative b)
in (

let a_abs = (abs_signed a)
in (

let b_abs = (abs_signed b)
in (

let quot = (FStar_UInt64.div a_abs b_abs)
in (match ((Prims.op_disEquality a_neg b_neg)) with
| true -> begin
FStar_Pervasives_Native.Some ((negate_cell quot))
end
| uu___1 -> begin
FStar_Pervasives_Native.Some (quot)
end))))))
end))


let stack_div_signed : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
(match ((div_signed b a)) with
| FStar_Pervasives_Native.Some (r) -> begin
FStar_Pervasives_Native.Some ((r)::xs)
end
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let min_cells : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (match (((FStar_UInt64.v a) <= (FStar_UInt64.v b))) with
| true -> begin
a
end
| uu___ -> begin
b
end))


let max_cells : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (match (((FStar_UInt64.v a) >= (FStar_UInt64.v b))) with
| true -> begin
a
end
| uu___ -> begin
b
end))


let stack_min : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some (((min_cells b a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let stack_max : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some (((max_cells b a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))




