#light "off"
module Space_Text_Iter
type text_iter =
{text : Space_Text_Types.text; byte_pos : Prims.nat; grapheme_pos : Prims.nat; break_state : Space_Text_Grapheme.break_state}


let __proj__Mktext_iter__item__text : text_iter  ->  Space_Text_Types.text = (fun ( projectee  :  text_iter ) -> (match (projectee) with
| {text = text; byte_pos = byte_pos; grapheme_pos = grapheme_pos; break_state = break_state} -> begin
text
end))


let __proj__Mktext_iter__item__byte_pos : text_iter  ->  Prims.nat = (fun ( projectee  :  text_iter ) -> (match (projectee) with
| {text = text; byte_pos = byte_pos; grapheme_pos = grapheme_pos; break_state = break_state} -> begin
byte_pos
end))


let __proj__Mktext_iter__item__grapheme_pos : text_iter  ->  Prims.nat = (fun ( projectee  :  text_iter ) -> (match (projectee) with
| {text = text; byte_pos = byte_pos; grapheme_pos = grapheme_pos; break_state = break_state} -> begin
grapheme_pos
end))


let __proj__Mktext_iter__item__break_state : text_iter  ->  Space_Text_Grapheme.break_state = (fun ( projectee  :  text_iter ) -> (match (projectee) with
| {text = text; byte_pos = byte_pos; grapheme_pos = grapheme_pos; break_state = break_state} -> begin
break_state
end))


let iter_begin : Space_Text_Types.text  ->  text_iter = (fun ( t  :  Space_Text_Types.text ) -> {text = t; byte_pos = (Prims.parse_int "0"); grapheme_pos = (Prims.parse_int "0"); break_state = Space_Text_Grapheme.initial_state})


let iter_at_end : text_iter  ->  Prims.bool = (fun ( it  :  text_iter ) -> (it.grapheme_pos >= it.text.header.grapheme_count))


let iter_position : text_iter  ->  Prims.nat = (fun ( it  :  text_iter ) -> it.grapheme_pos)


let rec take_bytes : FStar_UInt8.t Prims.list  ->  Prims.nat  ->  FStar_UInt8.t Prims.list = (fun ( bytes  :  FStar_UInt8.t Prims.list ) ( n  :  Prims.nat ) -> (match ((Prims.op_Equality n (Prims.parse_int "0"))) with
| true -> begin
[]
end
| uu___ -> begin
(match (bytes) with
| [] -> begin
[]
end
| (b)::rest -> begin
(b)::(take_bytes rest (n - (Prims.parse_int "1")))
end)
end))


let rec drop_bytes : FStar_UInt8.t Prims.list  ->  Prims.nat  ->  FStar_UInt8.t Prims.list = (fun ( bytes  :  FStar_UInt8.t Prims.list ) ( n  :  Prims.nat ) -> (match ((Prims.op_Equality n (Prims.parse_int "0"))) with
| true -> begin
bytes
end
| uu___ -> begin
(match (bytes) with
| [] -> begin
[]
end
| (uu___1)::rest -> begin
(drop_bytes rest (n - (Prims.parse_int "1")))
end)
end))


let rec scan_grapheme : FStar_UInt8.t Prims.list  ->  Space_Text_Grapheme.break_state  ->  FStar_UInt8.t Prims.list  ->  (Space_Text_Types.grapheme * FStar_UInt8.t Prims.list * Space_Text_Grapheme.break_state) FStar_Pervasives_Native.option = (fun ( bytes  :  FStar_UInt8.t Prims.list ) ( state  :  Space_Text_Grapheme.break_state ) ( acc  :  FStar_UInt8.t Prims.list ) -> (match (bytes) with
| [] -> begin
(match (((FStar_List_Tot_Base.length acc) > (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.Some ((({Space_Text_Types.bytes = acc; Space_Text_Types.len = (FStar_List_Tot_Base.length acc)}), ([]), (state)))
end
| uu___ -> begin
FStar_Pervasives_Native.None
end)
end
| (b0)::rest -> begin
(

let len = (Space_Text_UTF8.sequence_length b0)
in (match ((Prims.op_Equality len (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(

let cp_bytes = (take_bytes bytes len)
in (

let remaining = (drop_bytes bytes len)
in (

let cp = (match ((Prims.op_Equality len (Prims.parse_int "1"))) with
| true -> begin
(Space_Text_UTF8.decode_codepoint_1 b0)
end
| uu___1 -> begin
(match ((Prims.op_Equality len (Prims.parse_int "2"))) with
| true -> begin
(match (rest) with
| (b1)::uu___2 -> begin
(Space_Text_UTF8.decode_codepoint_2 b0 b1)
end
| uu___2 -> begin
(Prims.parse_int "0")
end)
end
| uu___2 -> begin
(match ((Prims.op_Equality len (Prims.parse_int "3"))) with
| true -> begin
(match (rest) with
| (b1)::(b2)::uu___3 -> begin
(Space_Text_UTF8.decode_codepoint_3 b0 b1 b2)
end
| uu___3 -> begin
(Prims.parse_int "0")
end)
end
| uu___3 -> begin
(match (rest) with
| (b1)::(b2)::(b3)::uu___4 -> begin
(Space_Text_UTF8.decode_codepoint_4 b0 b1 b2 b3)
end
| uu___4 -> begin
(Prims.parse_int "0")
end)
end)
end)
end)
in (

let gbp = (Space_Text_Grapheme.gbp_from_codepoint cp)
in (

let uu___1 = (Space_Text_Grapheme.should_break state.prev_gbp gbp state)
in (match (uu___1) with
| (do_break, new_state) -> begin
(

let new_acc = (FStar_List_Tot_Base.append acc cp_bytes)
in (match ((do_break && ((FStar_List_Tot_Base.length acc) > (Prims.parse_int "0")))) with
| true -> begin
FStar_Pervasives_Native.Some ((({Space_Text_Types.bytes = acc; Space_Text_Types.len = (FStar_List_Tot_Base.length acc)}), (bytes), ({Space_Text_Grapheme.prev_gbp = gbp; Space_Text_Grapheme.in_emoji_seq = state.in_emoji_seq; Space_Text_Grapheme.ri_count = state.ri_count})))
end
| uu___2 -> begin
(scan_grapheme remaining new_state new_acc)
end))
end))))))
end))
end))


let iter_next : text_iter  ->  (Space_Text_Types.grapheme * text_iter) FStar_Pervasives_Native.option = (fun ( it  :  text_iter ) -> (match ((iter_at_end it)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(

let remaining = (drop_bytes it.text.data it.byte_pos)
in (match ((scan_grapheme remaining it.break_state [])) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (g, rest, new_state) -> begin
(

let new_pos = (it.byte_pos + g.len)
in FStar_Pervasives_Native.Some (((g), ({text = it.text; byte_pos = new_pos; grapheme_pos = (it.grapheme_pos + (Prims.parse_int "1")); break_state = new_state}))))
end))
end))


let rec collect_all : text_iter  ->  Space_Text_Types.grapheme Prims.list = (fun ( it  :  text_iter ) -> (match ((iter_at_end it)) with
| true -> begin
[]
end
| uu___ -> begin
(match ((iter_next it)) with
| FStar_Pervasives_Native.None -> begin
[]
end
| FStar_Pervasives_Native.Some (g, it') -> begin
(match ((it'.grapheme_pos > it.grapheme_pos)) with
| true -> begin
(g)::(collect_all it')
end
| uu___1 -> begin
[]
end)
end)
end))




