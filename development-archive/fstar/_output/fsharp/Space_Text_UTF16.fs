#light "off"
module Space_Text_UTF16

type utf16_unit =
FStar_UInt16.t


let is_high_surrogate : utf16_unit  ->  Prims.bool = (fun ( u  :  utf16_unit ) -> (

let v = (FStar_UInt16.v u)
in ((v >= (Prims.parse_int "0xD800")) && (v <= (Prims.parse_int "0xDBFF")))))


let is_low_surrogate : utf16_unit  ->  Prims.bool = (fun ( u  :  utf16_unit ) -> (

let v = (FStar_UInt16.v u)
in ((v >= (Prims.parse_int "0xDC00")) && (v <= (Prims.parse_int "0xDFFF")))))


let is_surrogate : utf16_unit  ->  Prims.bool = (fun ( u  :  utf16_unit ) -> ((is_high_surrogate u) || (is_low_surrogate u)))


let is_bmp : utf16_unit  ->  Prims.bool = (fun ( u  :  utf16_unit ) -> (not ((is_surrogate u))))


let decode_surrogate_pair : utf16_unit  ->  utf16_unit  ->  Prims.nat = (fun ( high  :  utf16_unit ) ( low  :  utf16_unit ) -> (

let h = (FStar_UInt16.v high)
in (

let l = (FStar_UInt16.v low)
in (match (((h >= (Prims.parse_int "0xD800")) && (l >= (Prims.parse_int "0xDC00")))) with
| true -> begin
(

let high_offset = (h - (Prims.parse_int "0xD800"))
in (

let low_offset = (l - (Prims.parse_int "0xDC00"))
in (((Prims.parse_int "0x10000") + (high_offset * (Prims.parse_int "1024"))) + low_offset)))
end
| uu___ -> begin
(Prims.parse_int "0")
end))))


let encode_codepoint_utf16 : Prims.nat  ->  utf16_unit Prims.list = (fun ( cp  :  Prims.nat ) -> (match ((cp < (Prims.parse_int "0x10000"))) with
| true -> begin
((FStar_UInt16.uint_to_t cp))::[]
end
| uu___ -> begin
(

let cp' = (cp - (Prims.parse_int "0x10000"))
in (

let high = ((Prims.parse_int "0xD800") + (cp' / (Prims.parse_int "0x400")))
in (

let low = ((Prims.parse_int "0xDC00") + (Prims.mod_f cp' (Prims.parse_int "0x400")))
in ((FStar_UInt16.uint_to_t high))::((FStar_UInt16.uint_to_t low))::[])))
end))


let utf16_to_bytes_le : utf16_unit  ->  FStar_UInt8.t Prims.list = (fun ( u  :  utf16_unit ) -> (

let v = (FStar_UInt16.v u)
in (

let low_byte = (Prims.mod_f v (Prims.parse_int "256"))
in (

let high_byte = (v / (Prims.parse_int "256"))
in ((FStar_UInt8.uint_to_t low_byte))::((FStar_UInt8.uint_to_t high_byte))::[]))))


let utf16_to_bytes_be : utf16_unit  ->  FStar_UInt8.t Prims.list = (fun ( u  :  utf16_unit ) -> (

let v = (FStar_UInt16.v u)
in (

let low_byte = (Prims.mod_f v (Prims.parse_int "256"))
in (

let high_byte = (v / (Prims.parse_int "256"))
in ((FStar_UInt8.uint_to_t high_byte))::((FStar_UInt8.uint_to_t low_byte))::[]))))

type utf16_byte_order =
| LittleEndian
| BigEndian


let uu___is_LittleEndian : utf16_byte_order  ->  Prims.bool = (fun ( projectee  :  utf16_byte_order ) -> (match (projectee) with
| LittleEndian -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_BigEndian : utf16_byte_order  ->  Prims.bool = (fun ( projectee  :  utf16_byte_order ) -> (match (projectee) with
| BigEndian -> begin
true
end
| uu___ -> begin
false
end))


let rec codepoints_to_utf16_bytes : Prims.nat Prims.list  ->  utf16_byte_order  ->  FStar_UInt8.t Prims.list = (fun ( cps  :  Prims.nat Prims.list ) ( order  :  utf16_byte_order ) -> (match (cps) with
| [] -> begin
[]
end
| (cp)::rest -> begin
(match (((cp > (Prims.parse_int "0x10FFFF")) || ((cp >= (Prims.parse_int "0xD800")) && (cp <= (Prims.parse_int "0xDFFF"))))) with
| true -> begin
(codepoints_to_utf16_bytes rest order)
end
| uu___ -> begin
(

let units = (encode_codepoint_utf16 cp)
in (

let bytes = (match (order) with
| LittleEndian -> begin
(FStar_List_Tot_Base.concatMap utf16_to_bytes_le units)
end
| BigEndian -> begin
(FStar_List_Tot_Base.concatMap utf16_to_bytes_be units)
end)
in (FStar_List_Tot_Base.op_At bytes (codepoints_to_utf16_bytes rest order))))
end)
end))


let rec extract_codepoints : FStar_UInt8.t Prims.list  ->  Prims.nat Prims.list = (fun ( bytes  :  FStar_UInt8.t Prims.list ) -> (match (bytes) with
| [] -> begin
[]
end
| (b0)::rest -> begin
(

let len = (Space_Text_UTF8.sequence_length b0)
in (match ((Prims.op_Equality len (Prims.parse_int "0"))) with
| true -> begin
(extract_codepoints rest)
end
| uu___ -> begin
(match ((Prims.op_Equality len (Prims.parse_int "1"))) with
| true -> begin
((FStar_UInt8.v b0))::(extract_codepoints rest)
end
| uu___1 -> begin
(match ((Prims.op_Equality len (Prims.parse_int "2"))) with
| true -> begin
(match (rest) with
| (b1)::rest' -> begin
((Space_Text_UTF8.decode_codepoint_2 b0 b1))::(extract_codepoints rest')
end
| uu___2 -> begin
[]
end)
end
| uu___2 -> begin
(match ((Prims.op_Equality len (Prims.parse_int "3"))) with
| true -> begin
(match (rest) with
| (b1)::(b2)::rest' -> begin
((Space_Text_UTF8.decode_codepoint_3 b0 b1 b2))::(extract_codepoints rest')
end
| uu___3 -> begin
[]
end)
end
| uu___3 -> begin
(match (rest) with
| (b1)::(b2)::(b3)::rest' -> begin
((Space_Text_UTF8.decode_codepoint_4 b0 b1 b2 b3))::(extract_codepoints rest')
end
| uu___4 -> begin
[]
end)
end)
end)
end)
end))
end))


let text_to_utf16 : Space_Text_Types.text  ->  FStar_UInt8.t Prims.list = (fun ( t  :  Space_Text_Types.text ) -> (

let cps = (extract_codepoints t.data)
in (codepoints_to_utf16_bytes cps LittleEndian)))


let text_to_utf16_be : Space_Text_Types.text  ->  FStar_UInt8.t Prims.list = (fun ( t  :  Space_Text_Types.text ) -> (

let cps = (extract_codepoints t.data)
in (codepoints_to_utf16_bytes cps BigEndian)))


let text_to_utf16_with_bom : Space_Text_Types.text  ->  utf16_byte_order  ->  FStar_UInt8.t Prims.list = (fun ( t  :  Space_Text_Types.text ) ( order  :  utf16_byte_order ) -> (

let bom = (match (order) with
| LittleEndian -> begin
(0xFF)::(0xFE)::[]
end
| BigEndian -> begin
(0xFE)::(0xFF)::[]
end)
in (FStar_List_Tot_Base.op_At bom (

let cps = (extract_codepoints t.data)
in (codepoints_to_utf16_bytes cps order)))))


let bytes_to_utf16_le : FStar_UInt8.t  ->  FStar_UInt8.t  ->  utf16_unit = (fun ( b0  :  FStar_UInt8.t ) ( b1  :  FStar_UInt8.t ) -> (

let low = (FStar_UInt8.v b0)
in (

let high = (FStar_UInt8.v b1)
in (FStar_UInt16.uint_to_t ((high * (Prims.parse_int "256")) + low)))))


let bytes_to_utf16_be : FStar_UInt8.t  ->  FStar_UInt8.t  ->  utf16_unit = (fun ( b0  :  FStar_UInt8.t ) ( b1  :  FStar_UInt8.t ) -> (

let high = (FStar_UInt8.v b0)
in (

let low = (FStar_UInt8.v b1)
in (FStar_UInt16.uint_to_t ((high * (Prims.parse_int "256")) + low)))))


let encode_codepoint_utf8 : Prims.nat  ->  FStar_UInt8.t Prims.list = (fun ( cp  :  Prims.nat ) -> (match ((cp <= (Prims.parse_int "0x7F"))) with
| true -> begin
((FStar_UInt8.uint_to_t cp))::[]
end
| uu___ -> begin
(match ((cp <= (Prims.parse_int "0x7FF"))) with
| true -> begin
(

let b0 = ((Prims.parse_int "0xC0") + (cp / (Prims.parse_int "64")))
in (

let b1 = ((Prims.parse_int "0x80") + (Prims.mod_f cp (Prims.parse_int "64")))
in ((FStar_UInt8.uint_to_t b0))::((FStar_UInt8.uint_to_t b1))::[]))
end
| uu___1 -> begin
(match ((cp <= (Prims.parse_int "0xFFFF"))) with
| true -> begin
(

let b0 = ((Prims.parse_int "0xE0") + (cp / (Prims.parse_int "4096")))
in (

let b1 = ((Prims.parse_int "0x80") + (Prims.mod_f (cp / (Prims.parse_int "64")) (Prims.parse_int "64")))
in (

let b2 = ((Prims.parse_int "0x80") + (Prims.mod_f cp (Prims.parse_int "64")))
in ((FStar_UInt8.uint_to_t b0))::((FStar_UInt8.uint_to_t b1))::((FStar_UInt8.uint_to_t b2))::[])))
end
| uu___2 -> begin
(match ((cp <= (Prims.parse_int "0x10FFFF"))) with
| true -> begin
(

let b0 = ((Prims.parse_int "0xF0") + (cp / (Prims.parse_int "262144")))
in (

let b1 = ((Prims.parse_int "0x80") + (Prims.mod_f (cp / (Prims.parse_int "4096")) (Prims.parse_int "64")))
in (

let b2 = ((Prims.parse_int "0x80") + (Prims.mod_f (cp / (Prims.parse_int "64")) (Prims.parse_int "64")))
in (

let b3 = ((Prims.parse_int "0x80") + (Prims.mod_f cp (Prims.parse_int "64")))
in ((FStar_UInt8.uint_to_t b0))::((FStar_UInt8.uint_to_t b1))::((FStar_UInt8.uint_to_t b2))::((FStar_UInt8.uint_to_t b3))::[]))))
end
| uu___3 -> begin
[]
end)
end)
end)
end))


let rec decode_utf16_le : FStar_UInt8.t Prims.list  ->  Prims.nat Prims.list = (fun ( bytes  :  FStar_UInt8.t Prims.list ) -> (match (bytes) with
| [] -> begin
[]
end
| (uu___)::[] -> begin
[]
end
| (b0)::(b1)::rest -> begin
(

let unit = (bytes_to_utf16_le b0 b1)
in (match ((is_high_surrogate unit)) with
| true -> begin
(match (rest) with
| (b2)::(b3)::rest' -> begin
(

let low_unit = (bytes_to_utf16_le b2 b3)
in (match ((is_low_surrogate low_unit)) with
| true -> begin
((decode_surrogate_pair unit low_unit))::(decode_utf16_le rest')
end
| uu___ -> begin
(decode_utf16_le rest)
end))
end
| uu___ -> begin
[]
end)
end
| uu___ -> begin
(match ((is_low_surrogate unit)) with
| true -> begin
(decode_utf16_le rest)
end
| uu___1 -> begin
((FStar_UInt16.v unit))::(decode_utf16_le rest)
end)
end))
end))


let rec decode_utf16_be : FStar_UInt8.t Prims.list  ->  Prims.nat Prims.list = (fun ( bytes  :  FStar_UInt8.t Prims.list ) -> (match (bytes) with
| [] -> begin
[]
end
| (uu___)::[] -> begin
[]
end
| (b0)::(b1)::rest -> begin
(

let unit = (bytes_to_utf16_be b0 b1)
in (match ((is_high_surrogate unit)) with
| true -> begin
(match (rest) with
| (b2)::(b3)::rest' -> begin
(

let low_unit = (bytes_to_utf16_be b2 b3)
in (match ((is_low_surrogate low_unit)) with
| true -> begin
((decode_surrogate_pair unit low_unit))::(decode_utf16_be rest')
end
| uu___ -> begin
(decode_utf16_be rest)
end))
end
| uu___ -> begin
[]
end)
end
| uu___ -> begin
(match ((is_low_surrogate unit)) with
| true -> begin
(decode_utf16_be rest)
end
| uu___1 -> begin
((FStar_UInt16.v unit))::(decode_utf16_be rest)
end)
end))
end))


let rec codepoints_to_utf8 : Prims.nat Prims.list  ->  FStar_UInt8.t Prims.list = (fun ( cps  :  Prims.nat Prims.list ) -> (match (cps) with
| [] -> begin
[]
end
| (cp)::rest -> begin
(match (((cp > (Prims.parse_int "0x10FFFF")) || ((cp >= (Prims.parse_int "0xD800")) && (cp <= (Prims.parse_int "0xDFFF"))))) with
| true -> begin
(codepoints_to_utf8 rest)
end
| uu___ -> begin
(FStar_List_Tot_Base.op_At (encode_codepoint_utf8 cp) (codepoints_to_utf8 rest))
end)
end))


let utf16_to_text : FStar_UInt8.t Prims.list  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( bytes  :  FStar_UInt8.t Prims.list ) -> (

let cps = (decode_utf16_le bytes)
in (

let utf8_bytes = (codepoints_to_utf8 cps)
in (Space_Text_Create.text_from_bytes utf8_bytes))))


let utf16_to_text_be : FStar_UInt8.t Prims.list  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( bytes  :  FStar_UInt8.t Prims.list ) -> (

let cps = (decode_utf16_be bytes)
in (

let utf8_bytes = (codepoints_to_utf8 cps)
in (Space_Text_Create.text_from_bytes utf8_bytes))))


let utf16_to_text_auto : FStar_UInt8.t Prims.list  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( bytes  :  FStar_UInt8.t Prims.list ) -> (match (bytes) with
| (uu___)::(uu___1)::rest when ((uu___ = 0xFF) && (uu___1 = 0xFE)) -> begin
(utf16_to_text rest)
end
| (uu___)::(uu___1)::rest when ((uu___ = 0xFE) && (uu___1 = 0xFF)) -> begin
(utf16_to_text_be rest)
end
| uu___ -> begin
(utf16_to_text bytes)
end))


let text_utf16_length : Space_Text_Types.text  ->  Prims.nat = (fun ( t  :  Space_Text_Types.text ) -> (

let cps = (extract_codepoints t.data)
in (

let rec count_units : Prims.nat Prims.list  ->  Prims.nat = (fun ( cps1  :  Prims.nat Prims.list ) -> (match (cps1) with
| [] -> begin
(Prims.parse_int "0")
end
| (cp)::rest -> begin
(

let units = (match (((cp >= (Prims.parse_int "0x10000")) && (cp <= (Prims.parse_int "0x10FFFF")))) with
| true -> begin
(Prims.parse_int "2")
end
| uu___ -> begin
(Prims.parse_int "1")
end)
in (units + (count_units rest)))
end))
in (count_units cps))))




