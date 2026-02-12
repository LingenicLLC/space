open Prims
type text_iter =
  {
  text: Space_Text_Types.text ;
  byte_pos: Prims.nat ;
  grapheme_pos: Prims.nat ;
  break_state: Space_Text_Grapheme.break_state }
let __proj__Mktext_iter__item__text (projectee : text_iter) :
  Space_Text_Types.text=
  match projectee with
  | { text; byte_pos; grapheme_pos; break_state;_} -> text
let __proj__Mktext_iter__item__byte_pos (projectee : text_iter) : Prims.nat=
  match projectee with
  | { text; byte_pos; grapheme_pos; break_state;_} -> byte_pos
let __proj__Mktext_iter__item__grapheme_pos (projectee : text_iter) :
  Prims.nat=
  match projectee with
  | { text; byte_pos; grapheme_pos; break_state;_} -> grapheme_pos
let __proj__Mktext_iter__item__break_state (projectee : text_iter) :
  Space_Text_Grapheme.break_state=
  match projectee with
  | { text; byte_pos; grapheme_pos; break_state;_} -> break_state
let iter_begin (t : Space_Text_Types.text) : text_iter=
  {
    text = t;
    byte_pos = Prims.int_zero;
    grapheme_pos = Prims.int_zero;
    break_state = Space_Text_Grapheme.initial_state
  }
let iter_at_end (it : text_iter) : Prims.bool=
  it.grapheme_pos >=
    ((it.text).Space_Text_Types.header).Space_Text_Types.grapheme_count
let iter_position (it : text_iter) : Prims.nat= it.grapheme_pos
let rec take_bytes (bytes : FStar_UInt8.t Prims.list) (n : Prims.nat) :
  FStar_UInt8.t Prims.list=
  if n = Prims.int_zero
  then []
  else
    (match bytes with
     | [] -> []
     | b::rest -> b :: (take_bytes rest (n - Prims.int_one)))
let rec drop_bytes (bytes : FStar_UInt8.t Prims.list) (n : Prims.nat) :
  FStar_UInt8.t Prims.list=
  if n = Prims.int_zero
  then bytes
  else
    (match bytes with
     | [] -> []
     | uu___1::rest -> drop_bytes rest (n - Prims.int_one))
let rec scan_grapheme (bytes : FStar_UInt8.t Prims.list)
  (state : Space_Text_Grapheme.break_state) (acc : FStar_UInt8.t Prims.list)
  :
  (Space_Text_Types.grapheme * FStar_UInt8.t Prims.list *
    Space_Text_Grapheme.break_state) FStar_Pervasives_Native.option=
  match bytes with
  | [] ->
      if (FStar_List_Tot_Base.length acc) > Prims.int_zero
      then
        FStar_Pervasives_Native.Some
          ({
             Space_Text_Types.bytes = acc;
             Space_Text_Types.len = (FStar_List_Tot_Base.length acc)
           }, [], state)
      else FStar_Pervasives_Native.None
  | b0::rest ->
      let len = Space_Text_UTF8.sequence_length b0 in
      if len = Prims.int_zero
      then FStar_Pervasives_Native.None
      else
        (let cp_bytes = take_bytes bytes len in
         let remaining = drop_bytes bytes len in
         let cp =
           if len = Prims.int_one
           then Space_Text_UTF8.decode_codepoint_1 b0
           else
             if len = (Prims.of_int (2))
             then
               (match rest with
                | b1::uu___2 -> Space_Text_UTF8.decode_codepoint_2 b0 b1
                | uu___2 -> Prims.int_zero)
             else
               if len = (Prims.of_int (3))
               then
                 (match rest with
                  | b1::b2::uu___3 ->
                      Space_Text_UTF8.decode_codepoint_3 b0 b1 b2
                  | uu___3 -> Prims.int_zero)
               else
                 (match rest with
                  | b1::b2::b3::uu___4 ->
                      Space_Text_UTF8.decode_codepoint_4 b0 b1 b2 b3
                  | uu___4 -> Prims.int_zero) in
         let gbp = Space_Text_Grapheme.gbp_from_codepoint cp in
         let uu___1 =
           Space_Text_Grapheme.should_break
             state.Space_Text_Grapheme.prev_gbp gbp state in
         match uu___1 with
         | (do_break, new_state) ->
             let new_acc = FStar_List_Tot_Base.append acc cp_bytes in
             if
               do_break &&
                 ((FStar_List_Tot_Base.length acc) > Prims.int_zero)
             then
               FStar_Pervasives_Native.Some
                 ({
                    Space_Text_Types.bytes = acc;
                    Space_Text_Types.len = (FStar_List_Tot_Base.length acc)
                  }, bytes,
                   {
                     Space_Text_Grapheme.prev_gbp = gbp;
                     Space_Text_Grapheme.in_emoji_seq =
                       (state.Space_Text_Grapheme.in_emoji_seq);
                     Space_Text_Grapheme.ri_count =
                       (state.Space_Text_Grapheme.ri_count)
                   })
             else scan_grapheme remaining new_state new_acc)
let iter_next (it : text_iter) :
  (Space_Text_Types.grapheme * text_iter) FStar_Pervasives_Native.option=
  if iter_at_end it
  then FStar_Pervasives_Native.None
  else
    (let remaining = drop_bytes (it.text).Space_Text_Types.data it.byte_pos in
     match scan_grapheme remaining it.break_state [] with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some (g, rest, new_state) ->
         let new_pos = it.byte_pos + g.Space_Text_Types.len in
         FStar_Pervasives_Native.Some
           (g,
             {
               text = (it.text);
               byte_pos = new_pos;
               grapheme_pos = (it.grapheme_pos + Prims.int_one);
               break_state = new_state
             }))
let rec collect_all (it : text_iter) : Space_Text_Types.grapheme Prims.list=
  if iter_at_end it
  then []
  else
    (match iter_next it with
     | FStar_Pervasives_Native.None -> []
     | FStar_Pervasives_Native.Some (g, it') ->
         if it'.grapheme_pos > it.grapheme_pos
         then g :: (collect_all it')
         else [])
