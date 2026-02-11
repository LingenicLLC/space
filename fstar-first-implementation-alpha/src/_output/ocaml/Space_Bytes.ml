open Prims
type byte_array = {
  base: Prims.nat ;
  len: Prims.nat }
let __proj__Mkbyte_array__item__base (projectee : byte_array) : Prims.nat=
  match projectee with | { base; len;_} -> base
let __proj__Mkbyte_array__item__len (projectee : byte_array) : Prims.nat=
  match projectee with | { base; len;_} -> len
let allocate_bytes (m : Space_Memory.memory) (n : Prims.nat) :
  (Space_Memory.memory * byte_array)=
  if n = Prims.int_zero
  then (m, { base = Prims.int_zero; len = Prims.int_zero })
  else
    (let cells_needed = (n + (Prims.of_int (7))) / (Prims.of_int (8)) in
     let uu___1 = Space_Memory.mem_alloc m cells_needed in
     match uu___1 with | (m', base) -> (m', { base; len = n }))
let shift_for_byte (byte_pos : Prims.nat) : FStar_UInt32.t=
  match byte_pos with
  | uu___ when uu___ = Prims.int_zero -> Stdint.Uint32.zero
  | uu___ when uu___ = Prims.int_one -> (Stdint.Uint32.of_int (8))
  | uu___ when uu___ = (Prims.of_int (2)) -> (Stdint.Uint32.of_int (16))
  | uu___ when uu___ = (Prims.of_int (3)) -> (Stdint.Uint32.of_int (24))
  | uu___ when uu___ = (Prims.of_int (4)) -> (Stdint.Uint32.of_int (32))
  | uu___ when uu___ = (Prims.of_int (5)) -> (Stdint.Uint32.of_int (40))
  | uu___ when uu___ = (Prims.of_int (6)) -> (Stdint.Uint32.of_int (48))
  | uu___ -> (Stdint.Uint32.of_int (56))
let extract_byte (cell_val : Space_Types.cell) (byte_pos : Prims.nat) :
  FStar_UInt8.t=
  let shift = shift_for_byte byte_pos in
  let shifted = FStar_UInt64.shift_right cell_val shift in
  let masked = FStar_UInt64.logand shifted (Stdint.Uint64.of_int (0xFF)) in
  FStar_Int_Cast.uint64_to_uint8 masked
let insert_byte (cell_val : Space_Types.cell) (byte_pos : Prims.nat)
  (byte : FStar_UInt8.t) : Space_Types.cell=
  let shift = shift_for_byte byte_pos in
  let byte_extended = FStar_Int_Cast.uint8_to_uint64 byte in
  let byte_shifted = FStar_UInt64.shift_left byte_extended shift in
  let mask = FStar_UInt64.shift_left (Stdint.Uint64.of_int (0xFF)) shift in
  let inv_mask = FStar_UInt64.lognot mask in
  let cleared = FStar_UInt64.logand cell_val inv_mask in
  FStar_UInt64.logor cleared byte_shifted
let bytes_fetch (m : Space_Memory.memory) (arr : byte_array)
  (offset : Prims.nat) : FStar_UInt8.t FStar_Pervasives_Native.option=
  if offset >= arr.len
  then FStar_Pervasives_Native.None
  else
    (let cell_offset = offset / (Prims.of_int (8)) in
     let byte_in_cell = (mod) offset (Prims.of_int (8)) in
     if byte_in_cell >= (Prims.of_int (8))
     then FStar_Pervasives_Native.None
     else
       (match Space_Memory.mem_fetch m (arr.base + cell_offset) with
        | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
        | FStar_Pervasives_Native.Some cell_val ->
            FStar_Pervasives_Native.Some (extract_byte cell_val byte_in_cell)))
let bytes_store (m : Space_Memory.memory) (arr : byte_array)
  (offset : Prims.nat) (byte : FStar_UInt8.t) :
  Space_Memory.memory FStar_Pervasives_Native.option=
  if offset >= arr.len
  then FStar_Pervasives_Native.None
  else
    (let cell_offset = offset / (Prims.of_int (8)) in
     let byte_in_cell = (mod) offset (Prims.of_int (8)) in
     if byte_in_cell >= (Prims.of_int (8))
     then FStar_Pervasives_Native.None
     else
       (match Space_Memory.mem_fetch m (arr.base + cell_offset) with
        | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
        | FStar_Pervasives_Native.Some cell_val ->
            let new_val = insert_byte cell_val byte_in_cell byte in
            Space_Memory.mem_store m (arr.base + cell_offset) new_val))
let bytes_length (arr : byte_array) : Prims.nat= arr.len
let min_nat (a : Prims.nat) (b : Prims.nat) : Prims.nat=
  if a <= b then a else b
let rec bytes_copy_aux (m : Space_Memory.memory) (src : byte_array)
  (dst : byte_array) (src_off : Prims.nat) (dst_off : Prims.nat)
  (count : Prims.nat) : Space_Memory.memory FStar_Pervasives_Native.option=
  if count = Prims.int_zero
  then FStar_Pervasives_Native.Some m
  else
    (match bytes_fetch m src src_off with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some byte ->
         (match bytes_store m dst dst_off byte with
          | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
          | FStar_Pervasives_Native.Some m' ->
              bytes_copy_aux m' src dst (src_off + Prims.int_one)
                (dst_off + Prims.int_one) (count - Prims.int_one)))
let bytes_copy (m : Space_Memory.memory) (src : byte_array)
  (dst : byte_array) (len : Prims.nat) :
  Space_Memory.memory FStar_Pervasives_Native.option=
  if len > src.len
  then FStar_Pervasives_Native.None
  else
    if len > dst.len
    then FStar_Pervasives_Native.None
    else bytes_copy_aux m src dst Prims.int_zero Prims.int_zero len
let stack_bytes_fetch (m : Space_Memory.memory) (arr : byte_array)
  (s : Space_Stack.stack) :
  (FStar_UInt8.t * Space_Stack.stack) FStar_Pervasives_Native.option=
  match s with
  | offset_cell::rest ->
      let offset = FStar_UInt64.v offset_cell in
      (match bytes_fetch m arr offset with
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
       | FStar_Pervasives_Native.Some byte ->
           FStar_Pervasives_Native.Some (byte, rest))
  | uu___ -> FStar_Pervasives_Native.None
let stack_bytes_store (m : Space_Memory.memory) (arr : byte_array)
  (s : Space_Stack.stack) :
  (Space_Memory.memory * Space_Stack.stack) FStar_Pervasives_Native.option=
  match s with
  | offset_cell::byte_cell::rest ->
      let offset = FStar_UInt64.v offset_cell in
      let byte = FStar_Int_Cast.uint64_to_uint8 byte_cell in
      (match bytes_store m arr offset byte with
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
       | FStar_Pervasives_Native.Some m' ->
           FStar_Pervasives_Native.Some (m', rest))
  | uu___ -> FStar_Pervasives_Native.None
