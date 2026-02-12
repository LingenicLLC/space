open Prims
let rec bytes_equal (a : FStar_UInt8.t Prims.list)
  (b : FStar_UInt8.t Prims.list) : Prims.bool=
  match (a, b) with
  | ([], []) -> true
  | (x::xs, y::ys) -> (x = y) && (bytes_equal xs ys)
  | (uu___, uu___1) -> false
let text_equal (t1 : Space_Text_Types.text) (t2 : Space_Text_Types.text) :
  Prims.bool=
  ((t1.Space_Text_Types.header).Space_Text_Types.byte_length =
     (t2.Space_Text_Types.header).Space_Text_Types.byte_length)
    && (bytes_equal t1.Space_Text_Types.data t2.Space_Text_Types.data)
type ordering =
  | Less 
  | Equal 
  | Greater 
let uu___is_Less (projectee : ordering) : Prims.bool=
  match projectee with | Less -> true | uu___ -> false
let uu___is_Equal (projectee : ordering) : Prims.bool=
  match projectee with | Equal -> true | uu___ -> false
let uu___is_Greater (projectee : ordering) : Prims.bool=
  match projectee with | Greater -> true | uu___ -> false
let rec bytes_compare (a : FStar_UInt8.t Prims.list)
  (b : FStar_UInt8.t Prims.list) : ordering=
  match (a, b) with
  | ([], []) -> Equal
  | ([], uu___) -> Less
  | (uu___, []) -> Greater
  | (x::xs, y::ys) ->
      if (FStar_UInt8.v x) < (FStar_UInt8.v y)
      then Less
      else
        if (FStar_UInt8.v x) > (FStar_UInt8.v y)
        then Greater
        else bytes_compare xs ys
let text_compare (t1 : Space_Text_Types.text) (t2 : Space_Text_Types.text) :
  ordering= bytes_compare t1.Space_Text_Types.data t2.Space_Text_Types.data
let bytes_concat (a : FStar_UInt8.t Prims.list)
  (b : FStar_UInt8.t Prims.list) : FStar_UInt8.t Prims.list=
  FStar_List_Tot_Base.append a b
let rec generate_simple_index (count : Prims.nat) (offset : Prims.nat) :
  Space_Text_Types.grapheme_entry Prims.list=
  if count = Prims.int_zero
  then []
  else
    {
      Space_Text_Types.byte_offset = offset;
      Space_Text_Types.byte_len = Prims.int_one
    } ::
    (generate_simple_index (count - Prims.int_one) (offset + Prims.int_one))
let get_text_index (t : Space_Text_Types.text) :
  Space_Text_Types.grapheme_entry Prims.list=
  if
    (t.Space_Text_Types.header).Space_Text_Types.complexity =
      Space_Text_Types.Simple
  then
    generate_simple_index
      (t.Space_Text_Types.header).Space_Text_Types.grapheme_count
      Prims.int_zero
  else t.Space_Text_Types.index
let rec offset_index (idx : Space_Text_Types.grapheme_entry Prims.list)
  (offset : Prims.nat) : Space_Text_Types.grapheme_entry Prims.list=
  match idx with
  | [] -> []
  | e::rest ->
      {
        Space_Text_Types.byte_offset =
          (e.Space_Text_Types.byte_offset + offset);
        Space_Text_Types.byte_len = (e.Space_Text_Types.byte_len)
      } :: (offset_index rest offset)
let text_concat (t1 : Space_Text_Types.text) (t2 : Space_Text_Types.text) :
  Space_Text_Types.text=
  let new_data =
    bytes_concat t1.Space_Text_Types.data t2.Space_Text_Types.data in
  let new_complexity =
    if
      ((t1.Space_Text_Types.header).Space_Text_Types.complexity =
         Space_Text_Types.Complex)
        ||
        ((t2.Space_Text_Types.header).Space_Text_Types.complexity =
           Space_Text_Types.Complex)
    then Space_Text_Types.Complex
    else Space_Text_Types.Simple in
  let new_index =
    if new_complexity = Space_Text_Types.Simple
    then []
    else
      (let idx1 = get_text_index t1 in
       let idx2 = get_text_index t2 in
       let idx2_offset =
         offset_index idx2
           (t1.Space_Text_Types.header).Space_Text_Types.byte_length in
       FStar_List_Tot_Base.append idx1 idx2_offset) in
  {
    Space_Text_Types.header =
      {
        Space_Text_Types.grapheme_count =
          ((t1.Space_Text_Types.header).Space_Text_Types.grapheme_count +
             (t2.Space_Text_Types.header).Space_Text_Types.grapheme_count);
        Space_Text_Types.byte_length =
          ((t1.Space_Text_Types.header).Space_Text_Types.byte_length +
             (t2.Space_Text_Types.header).Space_Text_Types.byte_length);
        Space_Text_Types.complexity = new_complexity
      };
    Space_Text_Types.index = new_index;
    Space_Text_Types.data = new_data
  }
let rec list_take (n : Prims.nat) (xs : FStar_UInt8.t Prims.list) :
  FStar_UInt8.t Prims.list=
  if n = Prims.int_zero
  then []
  else
    (match xs with
     | [] -> []
     | x::rest -> x :: (list_take (n - Prims.int_one) rest))
let rec list_drop (n : Prims.nat) (xs : FStar_UInt8.t Prims.list) :
  FStar_UInt8.t Prims.list=
  if n = Prims.int_zero
  then xs
  else
    (match xs with
     | [] -> []
     | uu___1::rest -> list_drop (n - Prims.int_one) rest)
let bytes_slice_nat (bytes : FStar_UInt8.t Prims.list) (start : Prims.nat)
  (finish : Prims.nat) : FStar_UInt8.t Prims.list=
  if finish >= start
  then list_take (finish - start) (list_drop start bytes)
  else []
let bytes_slice (bytes : FStar_UInt8.t Prims.list) (start : Prims.nat)
  (finish : Prims.nat) : FStar_UInt8.t Prims.list=
  list_take (finish - start) (list_drop start bytes)
let simple_grapheme_offset (t : Space_Text_Types.text) (idx : Prims.nat) :
  Prims.nat=
  if
    (t.Space_Text_Types.header).Space_Text_Types.complexity =
      Space_Text_Types.Simple
  then idx
  else Prims.int_zero
let rec get_index_entry (idx : Space_Text_Types.grapheme_entry Prims.list)
  (n : Prims.nat) :
  Space_Text_Types.grapheme_entry FStar_Pervasives_Native.option=
  match idx with
  | [] -> FStar_Pervasives_Native.None
  | e::rest ->
      if n = Prims.int_zero
      then FStar_Pervasives_Native.Some e
      else get_index_entry rest (n - Prims.int_one)
let rec sum_index_bytes (idx : Space_Text_Types.grapheme_entry Prims.list)
  (start : Prims.nat) (finish : Prims.nat) (current : Prims.nat) : Prims.nat=
  if current >= finish
  then Prims.int_zero
  else
    (match idx with
     | [] -> Prims.int_zero
     | e::rest ->
         if current < start
         then sum_index_bytes rest start finish (current + Prims.int_one)
         else
           e.Space_Text_Types.byte_len +
             (sum_index_bytes rest start finish (current + Prims.int_one)))
let rec slice_index (idx : Space_Text_Types.grapheme_entry Prims.list)
  (start : Prims.nat) (finish : Prims.nat) (current : Prims.nat)
  (base_offset : Prims.nat) : Space_Text_Types.grapheme_entry Prims.list=
  if current >= finish
  then []
  else
    (match idx with
     | [] -> []
     | e::rest ->
         if current < start
         then
           slice_index rest start finish (current + Prims.int_one)
             base_offset
         else
           (let new_offset =
              if e.Space_Text_Types.byte_offset >= base_offset
              then e.Space_Text_Types.byte_offset - base_offset
              else Prims.int_zero in
            let new_entry =
              {
                Space_Text_Types.byte_offset = new_offset;
                Space_Text_Types.byte_len = (e.Space_Text_Types.byte_len)
              } in
            new_entry ::
              (slice_index rest start finish (current + Prims.int_one)
                 base_offset)))
let text_slice (t : Space_Text_Types.text) (start : Prims.nat)
  (finish : Prims.nat) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  if finish < start
  then FStar_Pervasives_Native.None
  else
    if finish > (t.Space_Text_Types.header).Space_Text_Types.grapheme_count
    then FStar_Pervasives_Native.None
    else
      if start = finish
      then FStar_Pervasives_Native.Some Space_Text_Types.empty_text
      else
        if
          (t.Space_Text_Types.header).Space_Text_Types.complexity =
            Space_Text_Types.Simple
        then
          (let new_data = bytes_slice t.Space_Text_Types.data start finish in
           let new_count = finish - start in
           FStar_Pervasives_Native.Some
             {
               Space_Text_Types.header =
                 {
                   Space_Text_Types.grapheme_count = new_count;
                   Space_Text_Types.byte_length = new_count;
                   Space_Text_Types.complexity = Space_Text_Types.Simple
                 };
               Space_Text_Types.index = [];
               Space_Text_Types.data = new_data
             })
        else
          (match get_index_entry t.Space_Text_Types.index start with
           | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
           | FStar_Pervasives_Native.Some start_entry ->
               let start_byte = start_entry.Space_Text_Types.byte_offset in
               let end_byte =
                 if
                   finish =
                     (t.Space_Text_Types.header).Space_Text_Types.grapheme_count
                 then
                   (t.Space_Text_Types.header).Space_Text_Types.byte_length
                 else
                   (match get_index_entry t.Space_Text_Types.index finish
                    with
                    | FStar_Pervasives_Native.None ->
                        (t.Space_Text_Types.header).Space_Text_Types.byte_length
                    | FStar_Pervasives_Native.Some end_entry ->
                        end_entry.Space_Text_Types.byte_offset) in
               if end_byte < start_byte
               then FStar_Pervasives_Native.None
               else
                 (let new_byte_len = end_byte - start_byte in
                  let new_data =
                    bytes_slice_nat t.Space_Text_Types.data start_byte
                      end_byte in
                  let new_index =
                    slice_index t.Space_Text_Types.index start finish
                      Prims.int_zero start_byte in
                  let new_count = finish - start in
                  FStar_Pervasives_Native.Some
                    {
                      Space_Text_Types.header =
                        {
                          Space_Text_Types.grapheme_count = new_count;
                          Space_Text_Types.byte_length = new_byte_len;
                          Space_Text_Types.complexity =
                            Space_Text_Types.Complex
                        };
                      Space_Text_Types.index = new_index;
                      Space_Text_Types.data = new_data
                    }))
let text_slice_simple (t : Space_Text_Types.text) (start : Prims.nat)
  (finish : Prims.nat) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  if
    (t.Space_Text_Types.header).Space_Text_Types.complexity <>
      Space_Text_Types.Simple
  then FStar_Pervasives_Native.None
  else text_slice t start finish
