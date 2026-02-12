open Prims
let bool_to_cell (b : Prims.bool) : Space_Types.cell=
  if b then Stdint.Uint64.one else Stdint.Uint64.zero
let cell_eq (a : Space_Types.cell) (b : Space_Types.cell) : Space_Types.cell=
  bool_to_cell (a = b)
let cell_neq (a : Space_Types.cell) (b : Space_Types.cell) :
  Space_Types.cell= bool_to_cell (a <> b)
let cell_lt (a : Space_Types.cell) (b : Space_Types.cell) : Space_Types.cell=
  bool_to_cell (FStar_UInt64.lt a b)
let cell_lte (a : Space_Types.cell) (b : Space_Types.cell) :
  Space_Types.cell= bool_to_cell (FStar_UInt64.lte a b)
let cell_gt (a : Space_Types.cell) (b : Space_Types.cell) : Space_Types.cell=
  bool_to_cell (FStar_UInt64.gt a b)
let cell_gte (a : Space_Types.cell) (b : Space_Types.cell) :
  Space_Types.cell= bool_to_cell (FStar_UInt64.gte a b)
let stack_eq (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some ((cell_eq b a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let stack_neq (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some ((cell_neq b a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let stack_lt (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some ((cell_lt b a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let stack_gt (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some ((cell_gt b a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let is_truthy (c : Space_Types.cell) : Prims.bool= c <> Stdint.Uint64.zero
let is_negative (a : Space_Types.cell) : Prims.bool=
  (FStar_UInt64.v a) >= (Prims.parse_int "0x8000000000000000")
let cell_lt_signed (a : Space_Types.cell) (b : Space_Types.cell) :
  Space_Types.cell=
  let a_neg = is_negative a in
  let b_neg = is_negative b in
  if a_neg && (Prims.op_Negation b_neg)
  then bool_to_cell true
  else
    if (Prims.op_Negation a_neg) && b_neg
    then bool_to_cell false
    else bool_to_cell (FStar_UInt64.lt a b)
let cell_gt_signed (a : Space_Types.cell) (b : Space_Types.cell) :
  Space_Types.cell=
  let a_neg = is_negative a in
  let b_neg = is_negative b in
  if a_neg && (Prims.op_Negation b_neg)
  then bool_to_cell false
  else
    if (Prims.op_Negation a_neg) && b_neg
    then bool_to_cell true
    else bool_to_cell (FStar_UInt64.gt a b)
let stack_lt_signed (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some ((cell_lt_signed b a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let stack_gt_signed (s : Space_Stack.stack) :
  Space_Stack.stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some ((cell_gt_signed b a) :: xs)
  | uu___ -> FStar_Pervasives_Native.None
