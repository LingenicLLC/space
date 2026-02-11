open Prims
let is_ascii (b : FStar_UInt8.t) : Prims.bool=
  (FStar_UInt8.v b) <= (Prims.of_int (0x7F))
let is_continuation (b : FStar_UInt8.t) : Prims.bool=
  let v = FStar_UInt8.v b in
  (v >= (Prims.of_int (0x80))) && (v <= (Prims.of_int (0xBF)))
let is_lead_2 (b : FStar_UInt8.t) : Prims.bool=
  let v = FStar_UInt8.v b in
  (v >= (Prims.of_int (0xC2))) && (v <= (Prims.of_int (0xDF)))
let is_lead_3 (b : FStar_UInt8.t) : Prims.bool=
  let v = FStar_UInt8.v b in
  (v >= (Prims.of_int (0xE0))) && (v <= (Prims.of_int (0xEF)))
let is_lead_4 (b : FStar_UInt8.t) : Prims.bool=
  let v = FStar_UInt8.v b in
  (v >= (Prims.of_int (0xF0))) && (v <= (Prims.of_int (0xF4)))
let sequence_length (lead : FStar_UInt8.t) : Prims.nat=
  if is_ascii lead
  then Prims.int_one
  else
    if is_lead_2 lead
    then (Prims.of_int (2))
    else
      if is_lead_3 lead
      then (Prims.of_int (3))
      else if is_lead_4 lead then (Prims.of_int (4)) else Prims.int_zero
let decode_codepoint_1 (b0 : FStar_UInt8.t) : Prims.nat= FStar_UInt8.v b0
let decode_codepoint_2 (b0 : FStar_UInt8.t) (b1 : FStar_UInt8.t) : Prims.nat=
  let v0 = FStar_UInt8.v b0 in
  let v1 = FStar_UInt8.v b1 in
  if (v0 >= (Prims.of_int (0xC0))) && (v1 >= (Prims.of_int (0x80)))
  then
    ((v0 - (Prims.of_int (0xC0))) * (Prims.of_int (64))) +
      (v1 - (Prims.of_int (0x80)))
  else Prims.int_zero
let decode_codepoint_3 (b0 : FStar_UInt8.t) (b1 : FStar_UInt8.t)
  (b2 : FStar_UInt8.t) : Prims.nat=
  let v0 = FStar_UInt8.v b0 in
  let v1 = FStar_UInt8.v b1 in
  let v2 = FStar_UInt8.v b2 in
  if
    ((v0 >= (Prims.of_int (0xE0))) && (v1 >= (Prims.of_int (0x80)))) &&
      (v2 >= (Prims.of_int (0x80)))
  then
    (((v0 - (Prims.of_int (0xE0))) * (Prims.of_int (4096))) +
       ((v1 - (Prims.of_int (0x80))) * (Prims.of_int (64))))
      + (v2 - (Prims.of_int (0x80)))
  else Prims.int_zero
let decode_codepoint_4 (b0 : FStar_UInt8.t) (b1 : FStar_UInt8.t)
  (b2 : FStar_UInt8.t) (b3 : FStar_UInt8.t) : Prims.nat=
  let v0 = FStar_UInt8.v b0 in
  let v1 = FStar_UInt8.v b1 in
  let v2 = FStar_UInt8.v b2 in
  let v3 = FStar_UInt8.v b3 in
  if
    (((v0 >= (Prims.of_int (0xF0))) && (v1 >= (Prims.of_int (0x80)))) &&
       (v2 >= (Prims.of_int (0x80))))
      && (v3 >= (Prims.of_int (0x80)))
  then
    ((((v0 - (Prims.of_int (0xF0))) * (Prims.parse_int "262144")) +
        ((v1 - (Prims.of_int (0x80))) * (Prims.of_int (4096))))
       + ((v2 - (Prims.of_int (0x80))) * (Prims.of_int (64))))
      + (v3 - (Prims.of_int (0x80)))
  else Prims.int_zero
let is_valid_codepoint (cp : Prims.nat) : Prims.bool=
  (cp <= (Prims.parse_int "0x10FFFF")) &&
    (Prims.op_Negation
       ((cp >= (Prims.of_int (0xD800))) && (cp <= (Prims.of_int (0xDFFF)))))
let rec is_valid_utf8 (bytes : FStar_UInt8.t Prims.list) : Prims.bool=
  match bytes with
  | [] -> true
  | b0::rest ->
      let len = sequence_length b0 in
      if len = Prims.int_zero
      then false
      else
        if len = Prims.int_one
        then is_valid_utf8 rest
        else
          if len = (Prims.of_int (2))
          then
            (match rest with
             | b1::rest' -> (is_continuation b1) && (is_valid_utf8 rest')
             | uu___2 -> false)
          else
            if len = (Prims.of_int (3))
            then
              (match rest with
               | b1::b2::rest' ->
                   ((is_continuation b1) && (is_continuation b2)) &&
                     (is_valid_utf8 rest')
               | uu___3 -> false)
            else
              (match rest with
               | b1::b2::b3::rest' ->
                   (((is_continuation b1) && (is_continuation b2)) &&
                      (is_continuation b3))
                     && (is_valid_utf8 rest')
               | uu___4 -> false)
let rec is_all_ascii (bytes : FStar_UInt8.t Prims.list) : Prims.bool=
  match bytes with
  | [] -> true
  | b::rest -> (is_ascii b) && (is_all_ascii rest)
