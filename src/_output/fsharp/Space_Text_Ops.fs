#light "off"
module Space_Text_Ops

let rec bytes_equal : FStar_UInt8.t Prims.list  ->  FStar_UInt8.t Prims.list  ->  Prims.bool = (fun ( a  :  FStar_UInt8.t Prims.list ) ( b  :  FStar_UInt8.t Prims.list ) -> (match (((a), (b))) with
| ([], []) -> begin
true
end
| ((x)::xs, (y)::ys) -> begin
((Prims.op_Equality x y) && (bytes_equal xs ys))
end
| (uu___, uu___1) -> begin
false
end))


let text_equal : Space_Text_Types.text  ->  Space_Text_Types.text  ->  Prims.bool = (fun ( t1  :  Space_Text_Types.text ) ( t2  :  Space_Text_Types.text ) -> ((Prims.op_Equality t1.header.byte_length t2.header.byte_length) && (bytes_equal t1.data t2.data)))

type ordering =
| Less
| Equal
| Greater


let uu___is_Less : ordering  ->  Prims.bool = (fun ( projectee  :  ordering ) -> (match (projectee) with
| Less -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_Equal : ordering  ->  Prims.bool = (fun ( projectee  :  ordering ) -> (match (projectee) with
| Equal -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_Greater : ordering  ->  Prims.bool = (fun ( projectee  :  ordering ) -> (match (projectee) with
| Greater -> begin
true
end
| uu___ -> begin
false
end))


let rec bytes_compare : FStar_UInt8.t Prims.list  ->  FStar_UInt8.t Prims.list  ->  ordering = (fun ( a  :  FStar_UInt8.t Prims.list ) ( b  :  FStar_UInt8.t Prims.list ) -> (match (((a), (b))) with
| ([], []) -> begin
Equal
end
| ([], uu___) -> begin
Less
end
| (uu___, []) -> begin
Greater
end
| ((x)::xs, (y)::ys) -> begin
(match (((FStar_UInt8.v x) < (FStar_UInt8.v y))) with
| true -> begin
Less
end
| uu___ -> begin
(match (((FStar_UInt8.v x) > (FStar_UInt8.v y))) with
| true -> begin
Greater
end
| uu___1 -> begin
(bytes_compare xs ys)
end)
end)
end))


let text_compare : Space_Text_Types.text  ->  Space_Text_Types.text  ->  ordering = (fun ( t1  :  Space_Text_Types.text ) ( t2  :  Space_Text_Types.text ) -> (bytes_compare t1.data t2.data))


let bytes_concat : FStar_UInt8.t Prims.list  ->  FStar_UInt8.t Prims.list  ->  FStar_UInt8.t Prims.list = (fun ( a  :  FStar_UInt8.t Prims.list ) ( b  :  FStar_UInt8.t Prims.list ) -> (FStar_List_Tot_Base.append a b))


let rec generate_simple_index : Prims.nat  ->  Prims.nat  ->  Space_Text_Types.grapheme_entry Prims.list = (fun ( count  :  Prims.nat ) ( offset  :  Prims.nat ) -> (match ((Prims.op_Equality count (Prims.parse_int "0"))) with
| true -> begin
[]
end
| uu___ -> begin
({Space_Text_Types.byte_offset = offset; Space_Text_Types.byte_len = (Prims.parse_int "1")})::(generate_simple_index (count - (Prims.parse_int "1")) (offset + (Prims.parse_int "1")))
end))


let get_text_index : Space_Text_Types.text  ->  Space_Text_Types.grapheme_entry Prims.list = (fun ( t  :  Space_Text_Types.text ) -> (match ((Prims.op_Equality t.header.complexity Space_Text_Types.Simple)) with
| true -> begin
(generate_simple_index t.header.grapheme_count (Prims.parse_int "0"))
end
| uu___ -> begin
t.index
end))


let rec offset_index : Space_Text_Types.grapheme_entry Prims.list  ->  Prims.nat  ->  Space_Text_Types.grapheme_entry Prims.list = (fun ( idx  :  Space_Text_Types.grapheme_entry Prims.list ) ( offset  :  Prims.nat ) -> (match (idx) with
| [] -> begin
[]
end
| (e)::rest -> begin
({Space_Text_Types.byte_offset = (e.byte_offset + offset); Space_Text_Types.byte_len = e.byte_len})::(offset_index rest offset)
end))


let text_concat : Space_Text_Types.text  ->  Space_Text_Types.text  ->  Space_Text_Types.text = (fun ( t1  :  Space_Text_Types.text ) ( t2  :  Space_Text_Types.text ) -> (

let new_data = (bytes_concat t1.data t2.data)
in (

let new_complexity = (match (((Prims.op_Equality t1.header.complexity Space_Text_Types.Complex) || (Prims.op_Equality t2.header.complexity Space_Text_Types.Complex))) with
| true -> begin
Space_Text_Types.Complex
end
| uu___ -> begin
Space_Text_Types.Simple
end)
in (

let new_index = (match ((Prims.op_Equality new_complexity Space_Text_Types.Simple)) with
| true -> begin
[]
end
| uu___ -> begin
(

let idx1 = (get_text_index t1)
in (

let idx2 = (get_text_index t2)
in (

let idx2_offset = (offset_index idx2 t1.header.byte_length)
in (FStar_List_Tot_Base.append idx1 idx2_offset))))
end)
in {Space_Text_Types.header = {Space_Text_Types.grapheme_count = (t1.header.grapheme_count + t2.header.grapheme_count); Space_Text_Types.byte_length = (t1.header.byte_length + t2.header.byte_length); Space_Text_Types.complexity = new_complexity}; Space_Text_Types.index = new_index; Space_Text_Types.data = new_data}))))


let rec list_take : Prims.nat  ->  FStar_UInt8.t Prims.list  ->  FStar_UInt8.t Prims.list = (fun ( n  :  Prims.nat ) ( xs  :  FStar_UInt8.t Prims.list ) -> (match ((Prims.op_Equality n (Prims.parse_int "0"))) with
| true -> begin
[]
end
| uu___ -> begin
(match (xs) with
| [] -> begin
[]
end
| (x)::rest -> begin
(x)::(list_take (n - (Prims.parse_int "1")) rest)
end)
end))


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


let bytes_slice_nat : FStar_UInt8.t Prims.list  ->  Prims.nat  ->  Prims.nat  ->  FStar_UInt8.t Prims.list = (fun ( bytes  :  FStar_UInt8.t Prims.list ) ( start  :  Prims.nat ) ( finish  :  Prims.nat ) -> (match ((finish >= start)) with
| true -> begin
(list_take (finish - start) (list_drop start bytes))
end
| uu___ -> begin
[]
end))


let bytes_slice : FStar_UInt8.t Prims.list  ->  Prims.nat  ->  Prims.nat  ->  FStar_UInt8.t Prims.list = (fun ( bytes  :  FStar_UInt8.t Prims.list ) ( start  :  Prims.nat ) ( finish  :  Prims.nat ) -> (list_take (finish - start) (list_drop start bytes)))


let simple_grapheme_offset : Space_Text_Types.text  ->  Prims.nat  ->  Prims.nat = (fun ( t  :  Space_Text_Types.text ) ( idx  :  Prims.nat ) -> (match ((Prims.op_Equality t.header.complexity Space_Text_Types.Simple)) with
| true -> begin
idx
end
| uu___ -> begin
(Prims.parse_int "0")
end))


let rec get_index_entry : Space_Text_Types.grapheme_entry Prims.list  ->  Prims.nat  ->  Space_Text_Types.grapheme_entry FStar_Pervasives_Native.option = (fun ( idx  :  Space_Text_Types.grapheme_entry Prims.list ) ( n  :  Prims.nat ) -> (match (idx) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (e)::rest -> begin
(match ((Prims.op_Equality n (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.Some (e)
end
| uu___ -> begin
(get_index_entry rest (n - (Prims.parse_int "1")))
end)
end))


let rec sum_index_bytes : Space_Text_Types.grapheme_entry Prims.list  ->  Prims.nat  ->  Prims.nat  ->  Prims.nat  ->  Prims.nat = (fun ( idx  :  Space_Text_Types.grapheme_entry Prims.list ) ( start  :  Prims.nat ) ( finish  :  Prims.nat ) ( current  :  Prims.nat ) -> (match ((current >= finish)) with
| true -> begin
(Prims.parse_int "0")
end
| uu___ -> begin
(match (idx) with
| [] -> begin
(Prims.parse_int "0")
end
| (e)::rest -> begin
(match ((current < start)) with
| true -> begin
(sum_index_bytes rest start finish (current + (Prims.parse_int "1")))
end
| uu___1 -> begin
(e.byte_len + (sum_index_bytes rest start finish (current + (Prims.parse_int "1"))))
end)
end)
end))


let rec slice_index : Space_Text_Types.grapheme_entry Prims.list  ->  Prims.nat  ->  Prims.nat  ->  Prims.nat  ->  Prims.nat  ->  Space_Text_Types.grapheme_entry Prims.list = (fun ( idx  :  Space_Text_Types.grapheme_entry Prims.list ) ( start  :  Prims.nat ) ( finish  :  Prims.nat ) ( current  :  Prims.nat ) ( base_offset  :  Prims.nat ) -> (match ((current >= finish)) with
| true -> begin
[]
end
| uu___ -> begin
(match (idx) with
| [] -> begin
[]
end
| (e)::rest -> begin
(match ((current < start)) with
| true -> begin
(slice_index rest start finish (current + (Prims.parse_int "1")) base_offset)
end
| uu___1 -> begin
(

let new_offset = (match ((e.byte_offset >= base_offset)) with
| true -> begin
(e.byte_offset - base_offset)
end
| uu___2 -> begin
(Prims.parse_int "0")
end)
in (

let new_entry = {Space_Text_Types.byte_offset = new_offset; Space_Text_Types.byte_len = e.byte_len}
in (new_entry)::(slice_index rest start finish (current + (Prims.parse_int "1")) base_offset)))
end)
end)
end))


let text_slice : Space_Text_Types.text  ->  Prims.nat  ->  Prims.nat  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) ( start  :  Prims.nat ) ( finish  :  Prims.nat ) -> (match ((finish < start)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((finish > t.header.grapheme_count)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
(match ((Prims.op_Equality start finish)) with
| true -> begin
FStar_Pervasives_Native.Some (Space_Text_Types.empty_text)
end
| uu___2 -> begin
(match ((Prims.op_Equality t.header.complexity Space_Text_Types.Simple)) with
| true -> begin
(

let new_data = (bytes_slice t.data start finish)
in (

let new_count = (finish - start)
in FStar_Pervasives_Native.Some ({Space_Text_Types.header = {Space_Text_Types.grapheme_count = new_count; Space_Text_Types.byte_length = new_count; Space_Text_Types.complexity = Space_Text_Types.Simple}; Space_Text_Types.index = []; Space_Text_Types.data = new_data})))
end
| uu___3 -> begin
(match ((get_index_entry t.index start)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (start_entry) -> begin
(

let start_byte = start_entry.byte_offset
in (

let end_byte = (match ((Prims.op_Equality finish t.header.grapheme_count)) with
| true -> begin
t.header.byte_length
end
| uu___4 -> begin
(match ((get_index_entry t.index finish)) with
| FStar_Pervasives_Native.None -> begin
t.header.byte_length
end
| FStar_Pervasives_Native.Some (end_entry) -> begin
end_entry.byte_offset
end)
end)
in (match ((end_byte < start_byte)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___4 -> begin
(

let new_byte_len = (end_byte - start_byte)
in (

let new_data = (bytes_slice_nat t.data start_byte end_byte)
in (

let new_index = (slice_index t.index start finish (Prims.parse_int "0") start_byte)
in (

let new_count = (finish - start)
in FStar_Pervasives_Native.Some ({Space_Text_Types.header = {Space_Text_Types.grapheme_count = new_count; Space_Text_Types.byte_length = new_byte_len; Space_Text_Types.complexity = Space_Text_Types.Complex}; Space_Text_Types.index = new_index; Space_Text_Types.data = new_data})))))
end)))
end)
end)
end)
end)
end))


let text_slice_simple : Space_Text_Types.text  ->  Prims.nat  ->  Prims.nat  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) ( start  :  Prims.nat ) ( finish  :  Prims.nat ) -> (match ((Prims.op_disEquality t.header.complexity Space_Text_Types.Simple)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(text_slice t start finish)
end))




