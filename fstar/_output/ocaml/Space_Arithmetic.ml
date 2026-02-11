open Prims
let add_cells (a : Space_Types.cell) (b : Space_Types.cell) :
  Space_Types.cell= FStar_UInt64.add_mod a b
let sub_cells (a : Space_Types.cell) (b : Space_Types.cell) :
  Space_Types.cell= FStar_UInt64.sub_mod a b
let mul_cells (a : Space_Types.cell) (b : Space_Types.cell) :
  Space_Types.cell= FStar_UInt64.mul_mod a b
let div_cells (a : Space_Types.cell) (b : Space_Types.cell) :
  Space_Types.cell FStar_Pervasives_Native.option=
  if (FStar_UInt64.v b) = Prims.int_zero
  then FStar_Pervasives_Native.None
  else FStar_Pervasives_Native.Some (FStar_UInt64.div a b)
let mod_cells (a : Space_Types.cell) (b : Space_Types.cell) :
  Space_Types.cell FStar_Pervasives_Native.option=
  if (FStar_UInt64.v b) = Prims.int_zero
  then FStar_Pervasives_Native.None
  else FStar_Pervasives_Native.Some (FStar_UInt64.rem a b)
let stack_add (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some ((add_cells b a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let stack_sub (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some ((sub_cells b a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let stack_mul (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some ((mul_cells b a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let stack_div (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs ->
      (match div_cells b a with
       | FStar_Pervasives_Native.Some r ->
           FStar_Pervasives_Native.Some (r :: xs)
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
  | uu___ -> FStar_Pervasives_Native.None
let stack_mod (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs ->
      (match mod_cells b a with
       | FStar_Pervasives_Native.Some r ->
           FStar_Pervasives_Native.Some (r :: xs)
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
  | uu___ -> FStar_Pervasives_Native.None
let negate_cell (a : Space_Types.cell) : Space_Types.cell=
  FStar_UInt64.sub_mod Stdint.Uint64.zero a
let stack_negate (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::xs -> FStar_Pervasives_Native.Some ((negate_cell a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let is_negative (a : Space_Types.cell) : Prims.bool=
  (FStar_UInt64.v a) >= (Prims.parse_int "0x8000000000000000")
let abs_signed (a : Space_Types.cell) : Space_Types.cell=
  if is_negative a then negate_cell a else a
let div_signed (a : Space_Types.cell) (b : Space_Types.cell) :
  Space_Types.cell FStar_Pervasives_Native.option=
  if (FStar_UInt64.v b) = Prims.int_zero
  then FStar_Pervasives_Native.None
  else
    (let a_neg = is_negative a in
     let b_neg = is_negative b in
     let a_abs = abs_signed a in
     let b_abs = abs_signed b in
     let quot = FStar_UInt64.div a_abs b_abs in
     if a_neg <> b_neg
     then FStar_Pervasives_Native.Some (negate_cell quot)
     else FStar_Pervasives_Native.Some quot)
let stack_div_signed (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs ->
      (match div_signed b a with
       | FStar_Pervasives_Native.Some r ->
           FStar_Pervasives_Native.Some (r :: xs)
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
  | uu___ -> FStar_Pervasives_Native.None
let min_cells (a : Space_Types.cell) (b : Space_Types.cell) :
  Space_Types.cell= if (FStar_UInt64.v a) <= (FStar_UInt64.v b) then a else b
let max_cells (a : Space_Types.cell) (b : Space_Types.cell) :
  Space_Types.cell= if (FStar_UInt64.v a) >= (FStar_UInt64.v b) then a else b
let stack_min (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some ((min_cells b a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let stack_max (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some ((max_cells b a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
