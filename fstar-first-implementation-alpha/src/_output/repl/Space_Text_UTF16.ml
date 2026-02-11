open Prims
type utf16_unit = FStar_UInt16.t
let is_high_surrogate (u : utf16_unit) : Prims.bool=
  let v = FStar_UInt16.v u in
  (v >= (Prims.of_int (0xD800))) && (v <= (Prims.of_int (0xDBFF)))
let is_low_surrogate (u : utf16_unit) : Prims.bool=
  let v = FStar_UInt16.v u in
  (v >= (Prims.of_int (0xDC00))) && (v <= (Prims.of_int (0xDFFF)))
let is_surrogate (u : utf16_unit) : Prims.bool=
  (is_high_surrogate u) || (is_low_surrogate u)
let is_bmp (u : utf16_unit) : Prims.bool= Prims.op_Negation (is_surrogate u)
let decode_surrogate_pair (high : utf16_unit) (low : utf16_unit) : Prims.nat=
  let h = FStar_UInt16.v high in
  let l = FStar_UInt16.v low in
  if (h >= (Prims.of_int (0xD800))) && (l >= (Prims.of_int (0xDC00)))
  then
    let high_offset = h - (Prims.of_int (0xD800)) in
    let low_offset = l - (Prims.of_int (0xDC00)) in
    ((Prims.parse_int "0x10000") + (high_offset * (Prims.of_int (1024)))) +
      low_offset
  else Prims.int_zero
let encode_codepoint_utf16 (cp : Prims.nat) : utf16_unit Prims.list=
  if cp < (Prims.parse_int "0x10000")
  then [FStar_UInt16.uint_to_t cp]
  else
    (let cp' = cp - (Prims.parse_int "0x10000") in
     let high = (Prims.of_int (0xD800)) + (cp' / (Prims.of_int (0x400))) in
     let low = (Prims.of_int (0xDC00)) + ((mod) cp' (Prims.of_int (0x400))) in
     [FStar_UInt16.uint_to_t high; FStar_UInt16.uint_to_t low])
let utf16_to_bytes_le (u : utf16_unit) : FStar_UInt8.t Prims.list=
  let v = FStar_UInt16.v u in
  let low_byte = (mod) v (Prims.of_int (256)) in
  let high_byte = v / (Prims.of_int (256)) in
  [FStar_UInt8.uint_to_t low_byte; FStar_UInt8.uint_to_t high_byte]
let utf16_to_bytes_be (u : utf16_unit) : FStar_UInt8.t Prims.list=
  let v = FStar_UInt16.v u in
  let low_byte = (mod) v (Prims.of_int (256)) in
  let high_byte = v / (Prims.of_int (256)) in
  [FStar_UInt8.uint_to_t high_byte; FStar_UInt8.uint_to_t low_byte]
type utf16_byte_order =
  | LittleEndian 
  | BigEndian 
let uu___is_LittleEndian (projectee : utf16_byte_order) : Prims.bool=
  match projectee with | LittleEndian -> true | uu___ -> false
let uu___is_BigEndian (projectee : utf16_byte_order) : Prims.bool=
  match projectee with | BigEndian -> true | uu___ -> false
let rec codepoints_to_utf16_bytes (cps : Prims.nat Prims.list)
  (order : utf16_byte_order) : FStar_UInt8.t Prims.list=
  match cps with
  | [] -> []
  | cp::rest ->
      if
        (cp > (Prims.parse_int "0x10FFFF")) ||
          ((cp >= (Prims.of_int (0xD800))) && (cp <= (Prims.of_int (0xDFFF))))
      then codepoints_to_utf16_bytes rest order
      else
        (let units = encode_codepoint_utf16 cp in
         let bytes =
           match order with
           | LittleEndian ->
               FStar_List_Tot_Base.concatMap utf16_to_bytes_le units
           | BigEndian ->
               FStar_List_Tot_Base.concatMap utf16_to_bytes_be units in
         FStar_List_Tot_Base.op_At bytes
           (codepoints_to_utf16_bytes rest order))
let rec extract_codepoints (bytes : FStar_UInt8.t Prims.list) :
  Prims.nat Prims.list=
  match bytes with
  | [] -> []
  | b0::rest ->
      let len = Space_Text_UTF8.sequence_length b0 in
      if len = Prims.int_zero
      then extract_codepoints rest
      else
        if len = Prims.int_one
        then (FStar_UInt8.v b0) :: (extract_codepoints rest)
        else
          if len = (Prims.of_int (2))
          then
            (match rest with
             | b1::rest' -> (Space_Text_UTF8.decode_codepoint_2 b0 b1) ::
                 (extract_codepoints rest')
             | uu___2 -> [])
          else
            if len = (Prims.of_int (3))
            then
              (match rest with
               | b1::b2::rest' ->
                   (Space_Text_UTF8.decode_codepoint_3 b0 b1 b2) ::
                   (extract_codepoints rest')
               | uu___3 -> [])
            else
              (match rest with
               | b1::b2::b3::rest' ->
                   (Space_Text_UTF8.decode_codepoint_4 b0 b1 b2 b3) ::
                   (extract_codepoints rest')
               | uu___4 -> [])
let text_to_utf16 (t : Space_Text_Types.text) : FStar_UInt8.t Prims.list=
  let cps = extract_codepoints t.Space_Text_Types.data in
  codepoints_to_utf16_bytes cps LittleEndian
let text_to_utf16_be (t : Space_Text_Types.text) : FStar_UInt8.t Prims.list=
  let cps = extract_codepoints t.Space_Text_Types.data in
  codepoints_to_utf16_bytes cps BigEndian
let text_to_utf16_with_bom (t : Space_Text_Types.text)
  (order : utf16_byte_order) : FStar_UInt8.t Prims.list=
  let bom =
    match order with
    | LittleEndian -> [0xFF; 0xFE]
    | BigEndian -> [0xFE; 0xFF] in
  FStar_List_Tot_Base.op_At bom
    (let cps = extract_codepoints t.Space_Text_Types.data in
     codepoints_to_utf16_bytes cps order)
let bytes_to_utf16_le (b0 : FStar_UInt8.t) (b1 : FStar_UInt8.t) : utf16_unit=
  let low = FStar_UInt8.v b0 in
  let high = FStar_UInt8.v b1 in
  FStar_UInt16.uint_to_t ((high * (Prims.of_int (256))) + low)
let bytes_to_utf16_be (b0 : FStar_UInt8.t) (b1 : FStar_UInt8.t) : utf16_unit=
  let high = FStar_UInt8.v b0 in
  let low = FStar_UInt8.v b1 in
  FStar_UInt16.uint_to_t ((high * (Prims.of_int (256))) + low)
let encode_codepoint_utf8 (cp : Prims.nat) : FStar_UInt8.t Prims.list=
  if cp <= (Prims.of_int (0x7F))
  then [FStar_UInt8.uint_to_t cp]
  else
    if cp <= (Prims.of_int (0x7FF))
    then
      (let b0 = (Prims.of_int (0xC0)) + (cp / (Prims.of_int (64))) in
       let b1 = (Prims.of_int (0x80)) + ((mod) cp (Prims.of_int (64))) in
       [FStar_UInt8.uint_to_t b0; FStar_UInt8.uint_to_t b1])
    else
      if cp <= (Prims.parse_int "0xFFFF")
      then
        (let b0 = (Prims.of_int (0xE0)) + (cp / (Prims.of_int (4096))) in
         let b1 =
           (Prims.of_int (0x80)) +
             ((mod) (cp / (Prims.of_int (64))) (Prims.of_int (64))) in
         let b2 = (Prims.of_int (0x80)) + ((mod) cp (Prims.of_int (64))) in
         [FStar_UInt8.uint_to_t b0;
         FStar_UInt8.uint_to_t b1;
         FStar_UInt8.uint_to_t b2])
      else
        if cp <= (Prims.parse_int "0x10FFFF")
        then
          (let b0 = (Prims.of_int (0xF0)) + (cp / (Prims.parse_int "262144")) in
           let b1 =
             (Prims.of_int (0x80)) +
               ((mod) (cp / (Prims.of_int (4096))) (Prims.of_int (64))) in
           let b2 =
             (Prims.of_int (0x80)) +
               ((mod) (cp / (Prims.of_int (64))) (Prims.of_int (64))) in
           let b3 = (Prims.of_int (0x80)) + ((mod) cp (Prims.of_int (64))) in
           [FStar_UInt8.uint_to_t b0;
           FStar_UInt8.uint_to_t b1;
           FStar_UInt8.uint_to_t b2;
           FStar_UInt8.uint_to_t b3])
        else []
let rec decode_utf16_le (bytes : FStar_UInt8.t Prims.list) :
  Prims.nat Prims.list=
  match bytes with
  | [] -> []
  | uu___::[] -> []
  | b0::b1::rest ->
      let unit = bytes_to_utf16_le b0 b1 in
      if is_high_surrogate unit
      then
        (match rest with
         | b2::b3::rest' ->
             let low_unit = bytes_to_utf16_le b2 b3 in
             if is_low_surrogate low_unit
             then (decode_surrogate_pair unit low_unit) ::
               (decode_utf16_le rest')
             else decode_utf16_le rest
         | uu___ -> [])
      else
        if is_low_surrogate unit
        then decode_utf16_le rest
        else (FStar_UInt16.v unit) :: (decode_utf16_le rest)
let rec decode_utf16_be (bytes : FStar_UInt8.t Prims.list) :
  Prims.nat Prims.list=
  match bytes with
  | [] -> []
  | uu___::[] -> []
  | b0::b1::rest ->
      let unit = bytes_to_utf16_be b0 b1 in
      if is_high_surrogate unit
      then
        (match rest with
         | b2::b3::rest' ->
             let low_unit = bytes_to_utf16_be b2 b3 in
             if is_low_surrogate low_unit
             then (decode_surrogate_pair unit low_unit) ::
               (decode_utf16_be rest')
             else decode_utf16_be rest
         | uu___ -> [])
      else
        if is_low_surrogate unit
        then decode_utf16_be rest
        else (FStar_UInt16.v unit) :: (decode_utf16_be rest)
let rec codepoints_to_utf8 (cps : Prims.nat Prims.list) :
  FStar_UInt8.t Prims.list=
  match cps with
  | [] -> []
  | cp::rest ->
      if
        (cp > (Prims.parse_int "0x10FFFF")) ||
          ((cp >= (Prims.of_int (0xD800))) && (cp <= (Prims.of_int (0xDFFF))))
      then codepoints_to_utf8 rest
      else
        FStar_List_Tot_Base.op_At (encode_codepoint_utf8 cp)
          (codepoints_to_utf8 rest)
let utf16_to_text (bytes : FStar_UInt8.t Prims.list) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  let cps = decode_utf16_le bytes in
  let utf8_bytes = codepoints_to_utf8 cps in
  Space_Text_Create.text_from_bytes utf8_bytes
let utf16_to_text_be (bytes : FStar_UInt8.t Prims.list) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  let cps = decode_utf16_be bytes in
  let utf8_bytes = codepoints_to_utf8 cps in
  Space_Text_Create.text_from_bytes utf8_bytes
let utf16_to_text_auto (bytes : FStar_UInt8.t Prims.list) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  match bytes with
  | uu___::uu___1::rest when (uu___ = 0xFF) && (uu___1 = 0xFE) ->
      utf16_to_text rest
  | uu___::uu___1::rest when (uu___ = 0xFE) && (uu___1 = 0xFF) ->
      utf16_to_text_be rest
  | uu___ -> utf16_to_text bytes
let text_utf16_length (t : Space_Text_Types.text) : Prims.nat=
  let cps = extract_codepoints t.Space_Text_Types.data in
  let rec count_units cps1 =
    match cps1 with
    | [] -> Prims.int_zero
    | cp::rest ->
        let units =
          if
            (cp >= (Prims.parse_int "0x10000")) &&
              (cp <= (Prims.parse_int "0x10FFFF"))
          then (Prims.of_int (2))
          else Prims.int_one in
        units + (count_units rest) in
  count_units cps
