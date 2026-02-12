#light "off"
module Space_Text_Codepoint

type codepoint =
Prims.nat


let rec drop_n : Prims.nat  ->  FStar_UInt8.t Prims.list  ->  FStar_UInt8.t Prims.list = (fun ( n  :  Prims.nat ) ( xs  :  FStar_UInt8.t Prims.list ) -> (match ((Prims.op_Equality n (Prims.parse_int "0"))) with
| true -> begin
xs
end
| uu___ -> begin
(match (xs) with
| [] -> begin
[]
end
| (uu___1)::rest -> begin
(drop_n (n - (Prims.parse_int "1")) rest)
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
(

let remaining = (drop_n len ((b0)::rest))
in ((Prims.parse_int "1") + (count_codepoints_aux remaining (fuel - (Prims.parse_int "1")))))
end))
end)
end))


let text_codepoint_count : Space_Text_Types.text  ->  Prims.nat = (fun ( t  :  Space_Text_Types.text ) -> (count_codepoints_aux t.data (FStar_List_Tot_Base.length t.data)))


let decode_at : FStar_UInt8.t Prims.list  ->  (codepoint * Prims.nat) FStar_Pervasives_Native.option = (fun ( bytes  :  FStar_UInt8.t Prims.list ) -> (match (bytes) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (b0)::rest -> begin
(

let len = (Space_Text_UTF8.sequence_length b0)
in (match ((Prims.op_Equality len (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((Prims.op_Equality len (Prims.parse_int "1"))) with
| true -> begin
FStar_Pervasives_Native.Some ((((Space_Text_UTF8.decode_codepoint_1 b0)), ((Prims.parse_int "1"))))
end
| uu___1 -> begin
(match ((Prims.op_Equality len (Prims.parse_int "2"))) with
| true -> begin
(match (rest) with
| (b1)::uu___2 -> begin
FStar_Pervasives_Native.Some ((((Space_Text_UTF8.decode_codepoint_2 b0 b1)), ((Prims.parse_int "2"))))
end
| uu___2 -> begin
FStar_Pervasives_Native.None
end)
end
| uu___2 -> begin
(match ((Prims.op_Equality len (Prims.parse_int "3"))) with
| true -> begin
(match (rest) with
| (b1)::(b2)::uu___3 -> begin
FStar_Pervasives_Native.Some ((((Space_Text_UTF8.decode_codepoint_3 b0 b1 b2)), ((Prims.parse_int "3"))))
end
| uu___3 -> begin
FStar_Pervasives_Native.None
end)
end
| uu___3 -> begin
(match (rest) with
| (b1)::(b2)::(b3)::uu___4 -> begin
FStar_Pervasives_Native.Some ((((Space_Text_UTF8.decode_codepoint_4 b0 b1 b2 b3)), ((Prims.parse_int "4"))))
end
| uu___4 -> begin
FStar_Pervasives_Native.None
end)
end)
end)
end)
end))
end))


let rec get_codepoint_at : FStar_UInt8.t Prims.list  ->  Prims.nat  ->  Prims.nat  ->  codepoint FStar_Pervasives_Native.option = (fun ( bytes  :  FStar_UInt8.t Prims.list ) ( idx  :  Prims.nat ) ( fuel  :  Prims.nat ) -> (match ((Prims.op_Equality fuel (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((decode_at bytes)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (cp, len) -> begin
(match ((Prims.op_Equality idx (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.Some (cp)
end
| uu___1 -> begin
(

let remaining = (drop_n len bytes)
in (get_codepoint_at remaining (idx - (Prims.parse_int "1")) (fuel - (Prims.parse_int "1"))))
end)
end)
end))


let text_codepoint_at : Space_Text_Types.text  ->  Prims.nat  ->  codepoint FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) ( idx  :  Prims.nat ) -> (get_codepoint_at t.data idx (FStar_List_Tot_Base.length t.data)))

type codepoint_iter =
{bytes : FStar_UInt8.t Prims.list; position : Prims.nat}


let __proj__Mkcodepoint_iter__item__bytes : codepoint_iter  ->  FStar_UInt8.t Prims.list = (fun ( projectee  :  codepoint_iter ) -> (match (projectee) with
| {bytes = bytes; position = position} -> begin
bytes
end))


let __proj__Mkcodepoint_iter__item__position : codepoint_iter  ->  Prims.nat = (fun ( projectee  :  codepoint_iter ) -> (match (projectee) with
| {bytes = bytes; position = position} -> begin
position
end))


let codepoint_iter_begin : Space_Text_Types.text  ->  codepoint_iter = (fun ( t  :  Space_Text_Types.text ) -> {bytes = t.data; position = (Prims.parse_int "0")})


let codepoint_iter_at_end : codepoint_iter  ->  Prims.bool = (fun ( it  :  codepoint_iter ) -> (match (it.bytes) with
| [] -> begin
true
end
| uu___ -> begin
false
end))


let codepoint_iter_position : codepoint_iter  ->  Prims.nat = (fun ( it  :  codepoint_iter ) -> it.position)


let rec iter_drop : Prims.nat  ->  FStar_UInt8.t Prims.list  ->  FStar_UInt8.t Prims.list = (fun ( n  :  Prims.nat ) ( xs  :  FStar_UInt8.t Prims.list ) -> (match ((Prims.op_Equality n (Prims.parse_int "0"))) with
| true -> begin
xs
end
| uu___ -> begin
(match (xs) with
| [] -> begin
[]
end
| (uu___1)::rest -> begin
(iter_drop (n - (Prims.parse_int "1")) rest)
end)
end))


let codepoint_iter_next : codepoint_iter  ->  (codepoint * codepoint_iter) FStar_Pervasives_Native.option = (fun ( it  :  codepoint_iter ) -> (match ((decode_at it.bytes)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (cp, len) -> begin
(

let remaining = (iter_drop len it.bytes)
in FStar_Pervasives_Native.Some (((cp), ({bytes = remaining; position = (it.position + (Prims.parse_int "1"))}))))
end))


let rec collect_codepoints : codepoint_iter  ->  Prims.nat  ->  codepoint Prims.list = (fun ( it  :  codepoint_iter ) ( fuel  :  Prims.nat ) -> (match ((Prims.op_Equality fuel (Prims.parse_int "0"))) with
| true -> begin
[]
end
| uu___ -> begin
(match ((codepoint_iter_at_end it)) with
| true -> begin
[]
end
| uu___1 -> begin
(match ((codepoint_iter_next it)) with
| FStar_Pervasives_Native.None -> begin
[]
end
| FStar_Pervasives_Native.Some (cp, it') -> begin
(cp)::(collect_codepoints it' (fuel - (Prims.parse_int "1")))
end)
end)
end))


let text_codepoints : Space_Text_Types.text  ->  codepoint Prims.list = (fun ( t  :  Space_Text_Types.text ) -> (

let it = (codepoint_iter_begin t)
in (collect_codepoints it (FStar_List_Tot_Base.length t.data))))




