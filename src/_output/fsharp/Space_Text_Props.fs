#light "off"
module Space_Text_Props

let grapheme_byte_length : Space_Text_Types.grapheme  ->  Prims.nat = (fun ( g  :  Space_Text_Types.grapheme ) -> g.len)


let grapheme_is_ascii : Space_Text_Types.grapheme  ->  Prims.bool = (fun ( g  :  Space_Text_Types.grapheme ) -> ((Prims.op_Equality g.len (Prims.parse_int "1")) && (match (g.bytes) with
| (b)::[] -> begin
(Space_Text_UTF8.is_ascii b)
end
| uu___ -> begin
false
end)))


let rec list_drop : Prims.nat  ->  FStar_UInt8.t Prims.list  ->  FStar_UInt8.t Prims.list = (fun ( n  :  Prims.nat ) ( xs  :  FStar_UInt8.t Prims.list ) -> (match ((Prims.op_Equality n (Prims.parse_int "0"))) with
| true -> begin
xs
end
| uu___ -> begin
(match (xs) with
| [] -> begin
[]
end
| (uu___1)::rest -> begin
(list_drop (n - (Prims.parse_int "1")) rest)
end)
end))


let rec count_codepoints_aux : FStar_UInt8.t Prims.list  ->  Prims.nat  ->  Prims.nat = (fun ( bytes  :  FStar_UInt8.t Prims.list ) ( fuel  :  Prims.nat ) -> (match ((Prims.op_Equality fuel (Prims.parse_int "0"))) with
| true -> begin
(Prims.parse_int "0")
end
| uu___ -> begin
(match (bytes) with
| [] -> begin
(Prims.parse_int "0")
end
| (b0)::rest -> begin
(

let len = (Space_Text_UTF8.sequence_length b0)
in (match ((Prims.op_Equality len (Prims.parse_int "0"))) with
| true -> begin
(Prims.parse_int "0")
end
| uu___1 -> begin
((Prims.parse_int "1") + (count_codepoints_aux (list_drop len ((b0)::rest)) (fuel - (Prims.parse_int "1"))))
end))
end)
end))


let count_codepoints : FStar_UInt8.t Prims.list  ->  Prims.nat = (fun ( bytes  :  FStar_UInt8.t Prims.list ) -> (count_codepoints_aux bytes (FStar_List_Tot_Base.length bytes)))


let grapheme_codepoint_count : Space_Text_Types.grapheme  ->  Prims.nat = (fun ( g  :  Space_Text_Types.grapheme ) -> (count_codepoints g.bytes))


let text_is_empty : Space_Text_Types.text  ->  Prims.bool = (fun ( t  :  Space_Text_Types.text ) -> (Prims.op_Equality t.header.grapheme_count (Prims.parse_int "0")))


let text_is_ascii : Space_Text_Types.text  ->  Prims.bool = (fun ( t  :  Space_Text_Types.text ) -> (Prims.op_Equality t.header.complexity Space_Text_Types.Simple))


let text_first_byte : Space_Text_Types.text  ->  FStar_UInt8.t FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) -> (match (t.data) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (b)::uu___ -> begin
FStar_Pervasives_Native.Some (b)
end))


let rec list_last : FStar_UInt8.t Prims.list  ->  FStar_UInt8.t FStar_Pervasives_Native.option = (fun ( xs  :  FStar_UInt8.t Prims.list ) -> (match (xs) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (x)::[] -> begin
FStar_Pervasives_Native.Some (x)
end
| (uu___)::rest -> begin
(list_last rest)
end))


let text_last_byte : Space_Text_Types.text  ->  FStar_UInt8.t FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) -> (list_last t.data))




