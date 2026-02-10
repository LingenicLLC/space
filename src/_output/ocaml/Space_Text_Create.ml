open Prims
let rec drop_n (n : Prims.nat) (xs : FStar_UInt8.t Prims.list) :
  FStar_UInt8.t Prims.list=
  if n = Prims.int_zero
  then xs
  else
    (match xs with
     | [] -> []
     | uu___1::rest -> drop_n (n - Prims.int_one) rest)
let rec take_n (n : Prims.nat) (xs : FStar_UInt8.t Prims.list) :
  FStar_UInt8.t Prims.list=
  if n = Prims.int_zero
  then []
  else
    (match xs with
     | [] -> []
     | x::rest -> x :: (take_n (n - Prims.int_one) rest))
type analyze_result =
  {
  gcount: Prims.nat ;
  is_simple: Prims.bool ;
  entries: Space_Text_Types.grapheme_entry Prims.list }
let __proj__Mkanalyze_result__item__gcount (projectee : analyze_result) :
  Prims.nat= match projectee with | { gcount; is_simple; entries;_} -> gcount
let __proj__Mkanalyze_result__item__is_simple (projectee : analyze_result) :
  Prims.bool=
  match projectee with | { gcount; is_simple; entries;_} -> is_simple
let __proj__Mkanalyze_result__item__entries (projectee : analyze_result) :
  Space_Text_Types.grapheme_entry Prims.list=
  match projectee with | { gcount; is_simple; entries;_} -> entries
let rec build_index_aux (bytes : FStar_UInt8.t Prims.list)
  (state : Space_Text_Grapheme.break_state) (byte_pos : Prims.nat)
  (current_start : Prims.nat) (current_len : Prims.nat)
  (acc_entries : Space_Text_Types.grapheme_entry Prims.list)
  (acc_count : Prims.nat) (acc_simple : Prims.bool) : analyze_result=
  match bytes with
  | [] ->
      if current_len > Prims.int_zero
      then
        let entry =
          {
            Space_Text_Types.byte_offset = current_start;
            Space_Text_Types.byte_len = current_len
          } in
        {
          gcount = (acc_count + Prims.int_one);
          is_simple = acc_simple;
          entries = (FStar_List_Tot_Base.append acc_entries [entry])
        }
      else
        { gcount = acc_count; is_simple = acc_simple; entries = acc_entries }
  | b0::rest ->
      let still_simple = acc_simple && (Space_Text_UTF8.is_ascii b0) in
      let len = Space_Text_UTF8.sequence_length b0 in
      if len = Prims.int_zero
      then { gcount = acc_count; is_simple = false; entries = acc_entries }
      else
        (let cp =
           if len = Prims.int_one
           then FStar_UInt8.v b0
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
             let remaining = drop_n len bytes in
             if do_break && (current_len > Prims.int_zero)
             then
               let entry =
                 {
                   Space_Text_Types.byte_offset = current_start;
                   Space_Text_Types.byte_len = current_len
                 } in
               let new_entries =
                 FStar_List_Tot_Base.append acc_entries [entry] in
               build_index_aux remaining new_state (byte_pos + len) byte_pos
                 len new_entries (acc_count + Prims.int_one) still_simple
             else
               build_index_aux remaining new_state (byte_pos + len)
                 current_start (current_len + len) acc_entries acc_count
                 still_simple)
let build_index (bytes : FStar_UInt8.t Prims.list) : analyze_result=
  build_index_aux bytes Space_Text_Grapheme.initial_state Prims.int_zero
    Prims.int_zero Prims.int_zero [] Prims.int_zero true
let text_from_bytes (bytes : FStar_UInt8.t Prims.list) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  if Prims.op_Negation (Space_Text_UTF8.is_valid_utf8 bytes)
  then FStar_Pervasives_Native.None
  else
    (let result = build_index bytes in
     let header =
       {
         Space_Text_Types.grapheme_count = (result.gcount);
         Space_Text_Types.byte_length = (FStar_List_Tot_Base.length bytes);
         Space_Text_Types.complexity =
           (if result.is_simple
            then Space_Text_Types.Simple
            else Space_Text_Types.Complex)
       } in
     FStar_Pervasives_Native.Some
       {
         Space_Text_Types.header = header;
         Space_Text_Types.index =
           (if result.is_simple then [] else result.entries);
         Space_Text_Types.data = bytes
       })
let byte_at (t : Space_Text_Types.text) (pos : Prims.nat) :
  FStar_UInt8.t FStar_Pervasives_Native.option=
  FStar_List_Tot_Base.nth t.Space_Text_Types.data pos
let simple_grapheme_at (t : Space_Text_Types.text) (idx : Prims.nat) :
  Space_Text_Types.grapheme FStar_Pervasives_Native.option=
  if idx >= (t.Space_Text_Types.header).Space_Text_Types.grapheme_count
  then FStar_Pervasives_Native.None
  else
    if
      (t.Space_Text_Types.header).Space_Text_Types.complexity <>
        Space_Text_Types.Simple
    then FStar_Pervasives_Native.None
    else
      (match byte_at t idx with
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
       | FStar_Pervasives_Native.Some b ->
           FStar_Pervasives_Native.Some
             {
               Space_Text_Types.bytes = [b];
               Space_Text_Types.len = Prims.int_one
             })
let rec get_entry (entries : Space_Text_Types.grapheme_entry Prims.list)
  (idx : Prims.nat) :
  Space_Text_Types.grapheme_entry FStar_Pervasives_Native.option=
  match entries with
  | [] -> FStar_Pervasives_Native.None
  | e::rest ->
      if idx = Prims.int_zero
      then FStar_Pervasives_Native.Some e
      else get_entry rest (idx - Prims.int_one)
let rec extract_bytes (data : FStar_UInt8.t Prims.list) (offset : Prims.nat)
  (len : Prims.nat) : FStar_UInt8.t Prims.list=
  if offset > Prims.int_zero
  then
    match data with
    | [] -> []
    | uu___::rest -> extract_bytes rest (offset - Prims.int_one) len
  else take_n len data
let complex_grapheme_at (t : Space_Text_Types.text) (idx : Prims.nat) :
  Space_Text_Types.grapheme FStar_Pervasives_Native.option=
  match get_entry t.Space_Text_Types.index idx with
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
  | FStar_Pervasives_Native.Some entry ->
      let bytes =
        extract_bytes t.Space_Text_Types.data
          entry.Space_Text_Types.byte_offset entry.Space_Text_Types.byte_len in
      FStar_Pervasives_Native.Some
        {
          Space_Text_Types.bytes = bytes;
          Space_Text_Types.len = (entry.Space_Text_Types.byte_len)
        }
let grapheme_at (t : Space_Text_Types.text) (idx : Prims.nat) :
  Space_Text_Types.grapheme FStar_Pervasives_Native.option=
  if idx >= (t.Space_Text_Types.header).Space_Text_Types.grapheme_count
  then FStar_Pervasives_Native.None
  else
    if
      (t.Space_Text_Types.header).Space_Text_Types.complexity =
        Space_Text_Types.Simple
    then simple_grapheme_at t idx
    else complex_grapheme_at t idx
let text_grapheme_count (t : Space_Text_Types.text) : Prims.nat=
  (t.Space_Text_Types.header).Space_Text_Types.grapheme_count
let text_byte_length (t : Space_Text_Types.text) : Prims.nat=
  (t.Space_Text_Types.header).Space_Text_Types.byte_length
let text_is_simple (t : Space_Text_Types.text) : Prims.bool=
  (t.Space_Text_Types.header).Space_Text_Types.complexity =
    Space_Text_Types.Simple
let text_grapheme_first (t : Space_Text_Types.text) :
  Space_Text_Types.grapheme FStar_Pervasives_Native.option=
  if
    (t.Space_Text_Types.header).Space_Text_Types.grapheme_count =
      Prims.int_zero
  then FStar_Pervasives_Native.None
  else grapheme_at t Prims.int_zero
let text_grapheme_last (t : Space_Text_Types.text) :
  Space_Text_Types.grapheme FStar_Pervasives_Native.option=
  if
    (t.Space_Text_Types.header).Space_Text_Types.grapheme_count =
      Prims.int_zero
  then FStar_Pervasives_Native.None
  else
    grapheme_at t
      ((t.Space_Text_Types.header).Space_Text_Types.grapheme_count -
         Prims.int_one)
