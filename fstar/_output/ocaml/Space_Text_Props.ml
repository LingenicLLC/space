open Prims
let grapheme_byte_length (g : Space_Text_Types.grapheme) : Prims.nat=
  g.Space_Text_Types.len
let grapheme_is_ascii (g : Space_Text_Types.grapheme) : Prims.bool=
  (g.Space_Text_Types.len = Prims.int_one) &&
    (match g.Space_Text_Types.bytes with
     | b::[] -> Space_Text_UTF8.is_ascii b
     | uu___ -> false)
let rec list_drop (n : Prims.nat) (xs : FStar_UInt8.t Prims.list) :
  FStar_UInt8.t Prims.list=
  if n = Prims.int_zero
  then xs
  else
    (match xs with
     | [] -> []
     | uu___1::rest -> list_drop (n - Prims.int_one) rest)
let rec count_codepoints_aux (bytes : FStar_UInt8.t Prims.list)
  (fuel : Prims.nat) : Prims.nat=
  if fuel = Prims.int_zero
  then Prims.int_zero
  else
    (match bytes with
     | [] -> Prims.int_zero
     | b0::rest ->
         let len = Space_Text_UTF8.sequence_length b0 in
         if len = Prims.int_zero
         then Prims.int_zero
         else
           Prims.int_one +
             (count_codepoints_aux (list_drop len (b0 :: rest))
                (fuel - Prims.int_one)))
let count_codepoints (bytes : FStar_UInt8.t Prims.list) : Prims.nat=
  count_codepoints_aux bytes (FStar_List_Tot_Base.length bytes)
let grapheme_codepoint_count (g : Space_Text_Types.grapheme) : Prims.nat=
  count_codepoints g.Space_Text_Types.bytes
let text_is_empty (t : Space_Text_Types.text) : Prims.bool=
  (t.Space_Text_Types.header).Space_Text_Types.grapheme_count =
    Prims.int_zero
let text_is_ascii (t : Space_Text_Types.text) : Prims.bool=
  (t.Space_Text_Types.header).Space_Text_Types.complexity =
    Space_Text_Types.Simple
let text_first_byte (t : Space_Text_Types.text) :
  FStar_UInt8.t FStar_Pervasives_Native.option=
  match t.Space_Text_Types.data with
  | [] -> FStar_Pervasives_Native.None
  | b::uu___ -> FStar_Pervasives_Native.Some b
let rec list_last (xs : FStar_UInt8.t Prims.list) :
  FStar_UInt8.t FStar_Pervasives_Native.option=
  match xs with
  | [] -> FStar_Pervasives_Native.None
  | x::[] -> FStar_Pervasives_Native.Some x
  | uu___::rest -> list_last rest
let text_last_byte (t : Space_Text_Types.text) :
  FStar_UInt8.t FStar_Pervasives_Native.option=
  list_last t.Space_Text_Types.data
