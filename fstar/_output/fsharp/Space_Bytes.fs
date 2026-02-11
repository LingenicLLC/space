#light "off"
module Space_Bytes
type byte_array =
{base1 : Prims.nat; len : Prims.nat}


let __proj__Mkbyte_array__item__base : byte_array  ->  Prims.nat = (fun ( projectee  :  byte_array ) -> (match (projectee) with
| {base1 = base1; len = len} -> begin
base1
end))


let __proj__Mkbyte_array__item__len : byte_array  ->  Prims.nat = (fun ( projectee  :  byte_array ) -> (match (projectee) with
| {base1 = base1; len = len} -> begin
len
end))


let allocate_bytes : Space_Memory.memory  ->  Prims.nat  ->  (Space_Memory.memory * byte_array) = (fun ( m  :  Space_Memory.memory ) ( n  :  Prims.nat ) -> (match ((Prims.op_Equality n (Prims.parse_int "0"))) with
| true -> begin
((m), ({base1 = (Prims.parse_int "0"); len = (Prims.parse_int "0")}))
end
| uu___ -> begin
(

let cells_needed = ((n + (Prims.parse_int "7")) / (Prims.parse_int "8"))
in (

let uu___1 = (Space_Memory.mem_alloc m cells_needed)
in (match (uu___1) with
| (m', base1) -> begin
((m'), ({base1 = base1; len = n}))
end)))
end))


let shift_for_byte : Prims.nat  ->  FStar_UInt32.t = (fun ( byte_pos  :  Prims.nat ) -> (match (byte_pos) with
| uu___ when (uu___ = (Prims.parse_int "0")) -> begin
(FStar_UInt32.uint_to_t ((Prims.parse_int "0")))
end
| uu___ when (uu___ = (Prims.parse_int "1")) -> begin
(FStar_UInt32.uint_to_t ((Prims.parse_int "8")))
end
| uu___ when (uu___ = (Prims.parse_int "2")) -> begin
(FStar_UInt32.uint_to_t ((Prims.parse_int "16")))
end
| uu___ when (uu___ = (Prims.parse_int "3")) -> begin
(FStar_UInt32.uint_to_t ((Prims.parse_int "24")))
end
| uu___ when (uu___ = (Prims.parse_int "4")) -> begin
(FStar_UInt32.uint_to_t ((Prims.parse_int "32")))
end
| uu___ when (uu___ = (Prims.parse_int "5")) -> begin
(FStar_UInt32.uint_to_t ((Prims.parse_int "40")))
end
| uu___ when (uu___ = (Prims.parse_int "6")) -> begin
(FStar_UInt32.uint_to_t ((Prims.parse_int "48")))
end
| uu___ -> begin
(FStar_UInt32.uint_to_t ((Prims.parse_int "56")))
end))


let extract_byte : Space_Types.cell  ->  Prims.nat  ->  FStar_UInt8.t = (fun ( cell_val  :  Space_Types.cell ) ( byte_pos  :  Prims.nat ) -> (

let shift = (shift_for_byte byte_pos)
in (

let shifted = (FStar_UInt64.shift_right cell_val shift)
in (

let masked = (FStar_UInt64.logand shifted (FStar_UInt64.uint_to_t ((Prims.parse_int "0xFF"))))
in (FStar_Int_Cast.uint64_to_uint8 masked)))))


let insert_byte : Space_Types.cell  ->  Prims.nat  ->  FStar_UInt8.t  ->  Space_Types.cell = (fun ( cell_val  :  Space_Types.cell ) ( byte_pos  :  Prims.nat ) ( byte  :  FStar_UInt8.t ) -> (

let shift = (shift_for_byte byte_pos)
in (

let byte_extended = (FStar_Int_Cast.uint8_to_uint64 byte)
in (

let byte_shifted = (FStar_UInt64.shift_left byte_extended shift)
in (

let mask = (FStar_UInt64.shift_left (FStar_UInt64.uint_to_t ((Prims.parse_int "0xFF"))) shift)
in (

let inv_mask = (FStar_UInt64.lognot mask)
in (

let cleared = (FStar_UInt64.logand cell_val inv_mask)
in (FStar_UInt64.logor cleared byte_shifted))))))))


let bytes_fetch : Space_Memory.memory  ->  byte_array  ->  Prims.nat  ->  FStar_UInt8.t FStar_Pervasives_Native.option = (fun ( m  :  Space_Memory.memory ) ( arr  :  byte_array ) ( offset  :  Prims.nat ) -> (match ((offset >= arr.len)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(

let cell_offset = (offset / (Prims.parse_int "8"))
in (

let byte_in_cell = (Prims.mod_f offset (Prims.parse_int "8"))
in (match ((byte_in_cell >= (Prims.parse_int "8"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
(match ((Space_Memory.mem_fetch m (arr.base1 + cell_offset))) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (cell_val) -> begin
FStar_Pervasives_Native.Some ((extract_byte cell_val byte_in_cell))
end)
end)))
end))


let bytes_store : Space_Memory.memory  ->  byte_array  ->  Prims.nat  ->  FStar_UInt8.t  ->  Space_Memory.memory FStar_Pervasives_Native.option = (fun ( m  :  Space_Memory.memory ) ( arr  :  byte_array ) ( offset  :  Prims.nat ) ( byte  :  FStar_UInt8.t ) -> (match ((offset >= arr.len)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(

let cell_offset = (offset / (Prims.parse_int "8"))
in (

let byte_in_cell = (Prims.mod_f offset (Prims.parse_int "8"))
in (match ((byte_in_cell >= (Prims.parse_int "8"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
(match ((Space_Memory.mem_fetch m (arr.base1 + cell_offset))) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (cell_val) -> begin
(

let new_val = (insert_byte cell_val byte_in_cell byte)
in (Space_Memory.mem_store m (arr.base1 + cell_offset) new_val))
end)
end)))
end))


let bytes_length : byte_array  ->  Prims.nat = (fun ( arr  :  byte_array ) -> arr.len)


let min_nat : Prims.nat  ->  Prims.nat  ->  Prims.nat = (fun ( a  :  Prims.nat ) ( b  :  Prims.nat ) -> (match ((a <= b)) with
| true -> begin
a
end
| uu___ -> begin
b
end))


let rec bytes_copy_aux : Space_Memory.memory  ->  byte_array  ->  byte_array  ->  Prims.nat  ->  Prims.nat  ->  Prims.nat  ->  Space_Memory.memory FStar_Pervasives_Native.option = (fun ( m  :  Space_Memory.memory ) ( src  :  byte_array ) ( dst  :  byte_array ) ( src_off  :  Prims.nat ) ( dst_off  :  Prims.nat ) ( count  :  Prims.nat ) -> (match ((Prims.op_Equality count (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.Some (m)
end
| uu___ -> begin
(match ((bytes_fetch m src src_off)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (byte) -> begin
(match ((bytes_store m dst dst_off byte)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (m') -> begin
(bytes_copy_aux m' src dst (src_off + (Prims.parse_int "1")) (dst_off + (Prims.parse_int "1")) (count - (Prims.parse_int "1")))
end)
end)
end))


let bytes_copy : Space_Memory.memory  ->  byte_array  ->  byte_array  ->  Prims.nat  ->  Space_Memory.memory FStar_Pervasives_Native.option = (fun ( m  :  Space_Memory.memory ) ( src  :  byte_array ) ( dst  :  byte_array ) ( len  :  Prims.nat ) -> (match ((len > src.len)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((len > dst.len)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
(bytes_copy_aux m src dst (Prims.parse_int "0") (Prims.parse_int "0") len)
end)
end))


let stack_bytes_fetch : Space_Memory.memory  ->  byte_array  ->  Space_Stack.stack  ->  (FStar_UInt8.t * Space_Stack.stack) FStar_Pervasives_Native.option = (fun ( m  :  Space_Memory.memory ) ( arr  :  byte_array ) ( s  :  Space_Stack.stack ) -> (match (s) with
| (offset_cell)::rest -> begin
(

let offset = (FStar_UInt64.v offset_cell)
in (match ((bytes_fetch m arr offset)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (byte) -> begin
FStar_Pervasives_Native.Some (((byte), (rest)))
end))
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))


let stack_bytes_store : Space_Memory.memory  ->  byte_array  ->  Space_Stack.stack  ->  (Space_Memory.memory * Space_Stack.stack) FStar_Pervasives_Native.option = (fun ( m  :  Space_Memory.memory ) ( arr  :  byte_array ) ( s  :  Space_Stack.stack ) -> (match (s) with
| (offset_cell)::(byte_cell)::rest -> begin
(

let offset = (FStar_UInt64.v offset_cell)
in (

let byte = (FStar_Int_Cast.uint64_to_uint8 byte_cell)
in (match ((bytes_store m arr offset byte)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (m') -> begin
FStar_Pervasives_Native.Some (((m'), (rest)))
end)))
end
| uu___ -> begin
FStar_Pervasives_Native.None
end))




