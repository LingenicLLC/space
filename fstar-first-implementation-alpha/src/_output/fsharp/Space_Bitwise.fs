#light "off"
module Space_Bitwise

let bit_and : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (FStar_UInt64.logand a b))


let bit_or : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (FStar_UInt64.logor a b))


let bit_xor : Space_Types.cell  ->  Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( b  :  Space_Types.cell ) -> (FStar_UInt64.logxor a b))


let bit_not : Space_Types.cell  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) -> (FStar_UInt64.lognot a))


let shift_left : Space_Types.cell  ->  FStar_UInt32.t  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( n  :  FStar_UInt32.t ) -> (FStar_UInt64.shift_left a n))


let shift_right : Space_Types.cell  ->  FStar_UInt32.t  ->  Space_Types.cell = (fun ( a  :  Space_Types.cell ) ( n  :  FStar_UInt32.t ) -> (FStar_UInt64.shift_right a n))


let stack_and : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some (((bit_and b a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let stack_or : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some (((bit_or b a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let stack_xor : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::(b)::xs -> begin
FStar_Pervasives_Native.Some (((bit_xor b a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let stack_not : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (a)::xs -> begin
FStar_Pervasives_Native.Some (((bit_not a))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let stack_shl : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (n_cell)::(a)::xs -> begin
(

let n = (FStar_UInt64.v n_cell)
in (

let shift = (match ((n >= (Prims.parse_int "64"))) with
| true -> begin
(Prims.parse_int "63")
end
| uu___ -> begin
n
end)
in (

let n32 = (FStar_Int_Cast.uint64_to_uint32 (FStar_UInt64.uint_to_t shift))
in (match (((FStar_UInt32.v n32) < (Prims.parse_int "64"))) with
| true -> begin
FStar_Pervasives_Native.Some (((shift_left a n32))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.Some (((FStar_UInt64.uint_to_t ((Prims.parse_int "0"))))::xs)
end))))
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let stack_shr : Space_Stack.stack  ->  Space_Stack.stack FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (n_cell)::(a)::xs -> begin
(

let n = (FStar_UInt64.v n_cell)
in (

let shift = (match ((n >= (Prims.parse_int "64"))) with
| true -> begin
(Prims.parse_int "63")
end
| uu___ -> begin
n
end)
in (

let n32 = (FStar_Int_Cast.uint64_to_uint32 (FStar_UInt64.uint_to_t shift))
in (match (((FStar_UInt32.v n32) < (Prims.parse_int "64"))) with
| true -> begin
FStar_Pervasives_Native.Some (((shift_right a n32))::xs)
end
| uu___ -> begin
FStar_Pervasives_Native.Some (((FStar_UInt64.uint_to_t ((Prims.parse_int "0"))))::xs)
end))))
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))




