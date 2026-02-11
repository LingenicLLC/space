#light "off"
module Space_Text_Create

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


let rec take_n : Prims.nat  ->  FStar_UInt8.t Prims.list  ->  FStar_UInt8.t Prims.list = (fun ( n  :  Prims.nat ) ( xs  :  FStar_UInt8.t Prims.list ) -> (match ((Prims.op_Equality n (Prims.parse_int "0"))) with
| true -> begin
[]
end
| uu___ -> begin
(match (xs) with
| [] -> begin
[]
end
| (x)::rest -> begin
(x)::(take_n (n - (Prims.parse_int "1")) rest)
end)
end))

type analyze_result =
{gcount : Prims.nat; is_simple : Prims.bool; entries : Space_Text_Types.grapheme_entry Prims.list}


let __proj__Mkanalyze_result__item__gcount : analyze_result  ->  Prims.nat = (fun ( projectee  :  analyze_result ) -> (match (projectee) with
| {gcount = gcount; is_simple = is_simple; entries = entries} -> begin
gcount
end))


let __proj__Mkanalyze_result__item__is_simple : analyze_result  ->  Prims.bool = (fun ( projectee  :  analyze_result ) -> (match (projectee) with
| {gcount = gcount; is_simple = is_simple; entries = entries} -> begin
is_simple
end))


let __proj__Mkanalyze_result__item__entries : analyze_result  ->  Space_Text_Types.grapheme_entry Prims.list = (fun ( projectee  :  analyze_result ) -> (match (projectee) with
| {gcount = gcount; is_simple = is_simple; entries = entries} -> begin
entries
end))


let rec build_index_aux : FStar_UInt8.t Prims.list  ->  Space_Text_Grapheme.break_state  ->  Prims.nat  ->  Prims.nat  ->  Prims.nat  ->  Space_Text_Types.grapheme_entry Prims.list  ->  Prims.nat  ->  Prims.bool  ->  analyze_result = (fun ( bytes  :  FStar_UInt8.t Prims.list ) ( state  :  Space_Text_Grapheme.break_state ) ( byte_pos  :  Prims.nat ) ( current_start  :  Prims.nat ) ( current_len  :  Prims.nat ) ( acc_entries  :  Space_Text_Types.grapheme_entry Prims.list ) ( acc_count  :  Prims.nat ) ( acc_simple  :  Prims.bool ) -> (match (bytes) with
| [] -> begin
(match ((current_len > (Prims.parse_int "0"))) with
| true -> begin
(

let entry = {Space_Text_Types.byte_offset = current_start; Space_Text_Types.byte_len = current_len}
in {gcount = (acc_count + (Prims.parse_int "1")); is_simple = acc_simple; entries = (FStar_List_Tot_Base.append acc_entries ((entry)::[]))})
end
| uu___ -> begin
{gcount = acc_count; is_simple = acc_simple; entries = acc_entries}
end)
end
| (b0)::rest -> begin
(

let still_simple = (acc_simple && (Space_Text_UTF8.is_ascii b0))
in (

let len = (Space_Text_UTF8.sequence_length b0)
in (match ((Prims.op_Equality len (Prims.parse_int "0"))) with
| true -> begin
{gcount = acc_count; is_simple = false; entries = acc_entries}
end
| uu___ -> begin
(

let cp = (match ((Prims.op_Equality len (Prims.parse_int "1"))) with
| true -> begin
(FStar_UInt8.v b0)
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

let remaining = (drop_n len bytes)
in (match ((do_break && (current_len > (Prims.parse_int "0")))) with
| true -> begin
(

let entry = {Space_Text_Types.byte_offset = current_start; Space_Text_Types.byte_len = current_len}
in (

let new_entries = (FStar_List_Tot_Base.append acc_entries ((entry)::[]))
in (build_index_aux remaining new_state (byte_pos + len) byte_pos len new_entries (acc_count + (Prims.parse_int "1")) still_simple)))
end
| uu___2 -> begin
(build_index_aux remaining new_state (byte_pos + len) current_start (current_len + len) acc_entries acc_count still_simple)
end))
end))))
end)))
end))


let build_index : FStar_UInt8.t Prims.list  ->  analyze_result = (fun ( bytes  :  FStar_UInt8.t Prims.list ) -> (build_index_aux bytes Space_Text_Grapheme.initial_state (Prims.parse_int "0") (Prims.parse_int "0") (Prims.parse_int "0") [] (Prims.parse_int "0") true))


let text_from_bytes : FStar_UInt8.t Prims.list  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( bytes  :  FStar_UInt8.t Prims.list ) -> (match ((not ((Space_Text_UTF8.is_valid_utf8 bytes)))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(

let result = (build_index bytes)
in (

let header = {Space_Text_Types.grapheme_count = result.gcount; Space_Text_Types.byte_length = (FStar_List_Tot_Base.length bytes); Space_Text_Types.complexity = (match (result.is_simple) with
| true -> begin
Space_Text_Types.Simple
end
| uu___1 -> begin
Space_Text_Types.Complex
end)}
in FStar_Pervasives_Native.Some ({Space_Text_Types.header = header; Space_Text_Types.index = (match (result.is_simple) with
| true -> begin
[]
end
| uu___1 -> begin
result.entries
end); Space_Text_Types.data = bytes})))
end))


let byte_at : Space_Text_Types.text  ->  Prims.nat  ->  FStar_UInt8.t FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) ( pos  :  Prims.nat ) -> (FStar_List_Tot_Base.nth t.data pos))


let simple_grapheme_at : Space_Text_Types.text  ->  Prims.nat  ->  Space_Text_Types.grapheme FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) ( idx  :  Prims.nat ) -> (match ((idx >= t.header.grapheme_count)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((Prims.op_disEquality t.header.complexity Space_Text_Types.Simple)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
(match ((byte_at t idx)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (b) -> begin
FStar_Pervasives_Native.Some ({Space_Text_Types.bytes = (b)::[]; Space_Text_Types.len = (Prims.parse_int "1")})
end)
end)
end))


let rec get_entry : Space_Text_Types.grapheme_entry Prims.list  ->  Prims.nat  ->  Space_Text_Types.grapheme_entry FStar_Pervasives_Native.option = (fun ( entries  :  Space_Text_Types.grapheme_entry Prims.list ) ( idx  :  Prims.nat ) -> (match (entries) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (e)::rest -> begin
(match ((Prims.op_Equality idx (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.Some (e)
end
| uu___ -> begin
(get_entry rest (idx - (Prims.parse_int "1")))
end)
end))


let rec extract_bytes : FStar_UInt8.t Prims.list  ->  Prims.nat  ->  Prims.nat  ->  FStar_UInt8.t Prims.list = (fun ( data  :  FStar_UInt8.t Prims.list ) ( offset  :  Prims.nat ) ( len  :  Prims.nat ) -> (match ((offset > (Prims.parse_int "0"))) with
| true -> begin
(match (data) with
| [] -> begin
[]
end
| (uu___)::rest -> begin
(extract_bytes rest (offset - (Prims.parse_int "1")) len)
end)
end
| uu___ -> begin
(take_n len data)
end))


let complex_grapheme_at : Space_Text_Types.text  ->  Prims.nat  ->  Space_Text_Types.grapheme FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) ( idx  :  Prims.nat ) -> (match ((get_entry t.index idx)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (entry) -> begin
(

let bytes = (extract_bytes t.data entry.byte_offset entry.byte_len)
in FStar_Pervasives_Native.Some ({Space_Text_Types.bytes = bytes; Space_Text_Types.len = entry.byte_len}))
end))


let grapheme_at : Space_Text_Types.text  ->  Prims.nat  ->  Space_Text_Types.grapheme FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) ( idx  :  Prims.nat ) -> (match ((idx >= t.header.grapheme_count)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((Prims.op_Equality t.header.complexity Space_Text_Types.Simple)) with
| true -> begin
(simple_grapheme_at t idx)
end
| uu___1 -> begin
(complex_grapheme_at t idx)
end)
end))


let text_grapheme_count : Space_Text_Types.text  ->  Prims.nat = (fun ( t  :  Space_Text_Types.text ) -> t.header.grapheme_count)


let text_byte_length : Space_Text_Types.text  ->  Prims.nat = (fun ( t  :  Space_Text_Types.text ) -> t.header.byte_length)


let text_is_simple : Space_Text_Types.text  ->  Prims.bool = (fun ( t  :  Space_Text_Types.text ) -> (Prims.op_Equality t.header.complexity Space_Text_Types.Simple))


let text_grapheme_first : Space_Text_Types.text  ->  Space_Text_Types.grapheme FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) -> (match ((Prims.op_Equality t.header.grapheme_count (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(grapheme_at t (Prims.parse_int "0"))
end))


let text_grapheme_last : Space_Text_Types.text  ->  Space_Text_Types.grapheme FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) -> (match ((Prims.op_Equality t.header.grapheme_count (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(grapheme_at t (t.header.grapheme_count - (Prims.parse_int "1")))
end))




