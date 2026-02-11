open Prims
type codepoint = Prims.nat
let rec drop_n (n : Prims.nat) (xs : FStar_UInt8.t Prims.list) :
  FStar_UInt8.t Prims.list=
  if n = Prims.int_zero
  then xs
  else
    (match xs with
     | [] -> []
     | uu___1::rest -> drop_n (n - Prims.int_one) rest)
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
           (let remaining = drop_n len (b0 :: rest) in
            Prims.int_one +
              (count_codepoints_aux remaining (fuel - Prims.int_one))))
let text_codepoint_count (t : Space_Text_Types.text) : Prims.nat=
  count_codepoints_aux t.Space_Text_Types.data
    (FStar_List_Tot_Base.length t.Space_Text_Types.data)
let decode_at (bytes : FStar_UInt8.t Prims.list) :
  (codepoint * Prims.nat) FStar_Pervasives_Native.option=
  match bytes with
  | [] -> FStar_Pervasives_Native.None
  | b0::rest ->
      let len = Space_Text_UTF8.sequence_length b0 in
      if len = Prims.int_zero
      then FStar_Pervasives_Native.None
      else
        if len = Prims.int_one
        then
          FStar_Pervasives_Native.Some
            ((Space_Text_UTF8.decode_codepoint_1 b0), Prims.int_one)
        else
          if len = (Prims.of_int (2))
          then
            (match rest with
             | b1::uu___2 ->
                 FStar_Pervasives_Native.Some
                   ((Space_Text_UTF8.decode_codepoint_2 b0 b1),
                     (Prims.of_int (2)))
             | uu___2 -> FStar_Pervasives_Native.None)
          else
            if len = (Prims.of_int (3))
            then
              (match rest with
               | b1::b2::uu___3 ->
                   FStar_Pervasives_Native.Some
                     ((Space_Text_UTF8.decode_codepoint_3 b0 b1 b2),
                       (Prims.of_int (3)))
               | uu___3 -> FStar_Pervasives_Native.None)
            else
              (match rest with
               | b1::b2::b3::uu___4 ->
                   FStar_Pervasives_Native.Some
                     ((Space_Text_UTF8.decode_codepoint_4 b0 b1 b2 b3),
                       (Prims.of_int (4)))
               | uu___4 -> FStar_Pervasives_Native.None)
let rec get_codepoint_at (bytes : FStar_UInt8.t Prims.list) (idx : Prims.nat)
  (fuel : Prims.nat) : codepoint FStar_Pervasives_Native.option=
  if fuel = Prims.int_zero
  then FStar_Pervasives_Native.None
  else
    (match decode_at bytes with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some (cp, len) ->
         if idx = Prims.int_zero
         then FStar_Pervasives_Native.Some cp
         else
           (let remaining = drop_n len bytes in
            get_codepoint_at remaining (idx - Prims.int_one)
              (fuel - Prims.int_one)))
let text_codepoint_at (t : Space_Text_Types.text) (idx : Prims.nat) :
  codepoint FStar_Pervasives_Native.option=
  get_codepoint_at t.Space_Text_Types.data idx
    (FStar_List_Tot_Base.length t.Space_Text_Types.data)
type codepoint_iter = {
  bytes: FStar_UInt8.t Prims.list ;
  position: Prims.nat }
let __proj__Mkcodepoint_iter__item__bytes (projectee : codepoint_iter) :
  FStar_UInt8.t Prims.list=
  match projectee with | { bytes; position;_} -> bytes
let __proj__Mkcodepoint_iter__item__position (projectee : codepoint_iter) :
  Prims.nat= match projectee with | { bytes; position;_} -> position
let codepoint_iter_begin (t : Space_Text_Types.text) : codepoint_iter=
  { bytes = (t.Space_Text_Types.data); position = Prims.int_zero }
let codepoint_iter_at_end (it : codepoint_iter) : Prims.bool=
  match it.bytes with | [] -> true | uu___ -> false
let codepoint_iter_position (it : codepoint_iter) : Prims.nat= it.position
let rec iter_drop (n : Prims.nat) (xs : FStar_UInt8.t Prims.list) :
  FStar_UInt8.t Prims.list=
  if n = Prims.int_zero
  then xs
  else
    (match xs with
     | [] -> []
     | uu___1::rest -> iter_drop (n - Prims.int_one) rest)
let codepoint_iter_next (it : codepoint_iter) :
  (codepoint * codepoint_iter) FStar_Pervasives_Native.option=
  match decode_at it.bytes with
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
  | FStar_Pervasives_Native.Some (cp, len) ->
      let remaining = iter_drop len it.bytes in
      FStar_Pervasives_Native.Some
        (cp, { bytes = remaining; position = (it.position + Prims.int_one) })
let rec collect_codepoints (it : codepoint_iter) (fuel : Prims.nat) :
  codepoint Prims.list=
  if fuel = Prims.int_zero
  then []
  else
    if codepoint_iter_at_end it
    then []
    else
      (match codepoint_iter_next it with
       | FStar_Pervasives_Native.None -> []
       | FStar_Pervasives_Native.Some (cp, it') -> cp ::
           (collect_codepoints it' (fuel - Prims.int_one)))
let text_codepoints (t : Space_Text_Types.text) : codepoint Prims.list=
  let it = codepoint_iter_begin t in
  collect_codepoints it (FStar_List_Tot_Base.length t.Space_Text_Types.data)
