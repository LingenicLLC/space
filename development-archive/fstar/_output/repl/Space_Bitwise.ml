open Prims
let bit_and (a : Space_Types.cell) (b : Space_Types.cell) : Space_Types.cell=
  FStar_UInt64.logand a b
let bit_or (a : Space_Types.cell) (b : Space_Types.cell) : Space_Types.cell=
  FStar_UInt64.logor a b
let bit_xor (a : Space_Types.cell) (b : Space_Types.cell) : Space_Types.cell=
  FStar_UInt64.logxor a b
let bit_not (a : Space_Types.cell) : Space_Types.cell= FStar_UInt64.lognot a
let shift_left (a : Space_Types.cell) (n : FStar_UInt32.t) :
  Space_Types.cell= FStar_UInt64.shift_left a n
let shift_right (a : Space_Types.cell) (n : FStar_UInt32.t) :
  Space_Types.cell= FStar_UInt64.shift_right a n
let stack_and (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some ((bit_and b a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let stack_or (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some ((bit_or b a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let stack_xor (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some ((bit_xor b a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let stack_not (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::xs -> FStar_Pervasives_Native.Some ((bit_not a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let stack_shl (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | n_cell::a::xs ->
      let n = FStar_UInt64.v n_cell in
      let shift = if n >= (Prims.of_int (64)) then (Prims.of_int (63)) else n in
      let n32 =
        FStar_Int_Cast.uint64_to_uint32 (FStar_UInt64.uint_to_t shift) in
      if (FStar_UInt32.v n32) < (Prims.of_int (64))
      then FStar_Pervasives_Native.Some ((shift_left a n32) :: xs)
      else FStar_Pervasives_Native.Some (Stdint.Uint64.zero :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let stack_shr (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | n_cell::a::xs ->
      let n = FStar_UInt64.v n_cell in
      let shift = if n >= (Prims.of_int (64)) then (Prims.of_int (63)) else n in
      let n32 =
        FStar_Int_Cast.uint64_to_uint32 (FStar_UInt64.uint_to_t shift) in
      if (FStar_UInt32.v n32) < (Prims.of_int (64))
      then FStar_Pervasives_Native.Some ((shift_right a n32) :: xs)
      else FStar_Pervasives_Native.Some (Stdint.Uint64.zero :: xs)
  | uu___ -> FStar_Pervasives_Native.None
