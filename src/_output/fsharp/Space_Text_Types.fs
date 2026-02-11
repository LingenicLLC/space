#light "off"
module Space_Text_Types
type text_complexity =
| Simple
| Complex


let uu___is_Simple : text_complexity  ->  Prims.bool = (fun ( projectee  :  text_complexity ) -> (match (projectee) with
| Simple -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_Complex : text_complexity  ->  Prims.bool = (fun ( projectee  :  text_complexity ) -> (match (projectee) with
| Complex -> begin
true
end
| uu___ -> begin
false
end))

type text_header =
{grapheme_count : Prims.nat; byte_length : Prims.nat; complexity : text_complexity}


let __proj__Mktext_header__item__grapheme_count : text_header  ->  Prims.nat = (fun ( projectee  :  text_header ) -> (match (projectee) with
| {grapheme_count = grapheme_count; byte_length = byte_length; complexity = complexity} -> begin
grapheme_count
end))


let __proj__Mktext_header__item__byte_length : text_header  ->  Prims.nat = (fun ( projectee  :  text_header ) -> (match (projectee) with
| {grapheme_count = grapheme_count; byte_length = byte_length; complexity = complexity} -> begin
byte_length
end))


let __proj__Mktext_header__item__complexity : text_header  ->  text_complexity = (fun ( projectee  :  text_header ) -> (match (projectee) with
| {grapheme_count = grapheme_count; byte_length = byte_length; complexity = complexity} -> begin
complexity
end))

type grapheme_entry =
{byte_offset : Prims.nat; byte_len : Prims.nat}


let __proj__Mkgrapheme_entry__item__byte_offset : grapheme_entry  ->  Prims.nat = (fun ( projectee  :  grapheme_entry ) -> (match (projectee) with
| {byte_offset = byte_offset; byte_len = byte_len} -> begin
byte_offset
end))


let __proj__Mkgrapheme_entry__item__byte_len : grapheme_entry  ->  Prims.nat = (fun ( projectee  :  grapheme_entry ) -> (match (projectee) with
| {byte_offset = byte_offset; byte_len = byte_len} -> begin
byte_len
end))

type grapheme =
{bytes : FStar_UInt8.t Prims.list; len : Prims.nat}


let __proj__Mkgrapheme__item__bytes : grapheme  ->  FStar_UInt8.t Prims.list = (fun ( projectee  :  grapheme ) -> (match (projectee) with
| {bytes = bytes; len = len} -> begin
bytes
end))


let __proj__Mkgrapheme__item__len : grapheme  ->  Prims.nat = (fun ( projectee  :  grapheme ) -> (match (projectee) with
| {bytes = bytes; len = len} -> begin
len
end))


let empty_grapheme : grapheme = {bytes = []; len = (Prims.parse_int "0")}

type text =
{header : text_header; index : grapheme_entry Prims.list; data : FStar_UInt8.t Prims.list}


let __proj__Mktext__item__header : text  ->  text_header = (fun ( projectee  :  text ) -> (match (projectee) with
| {header = header; index = index; data = data} -> begin
header
end))


let __proj__Mktext__item__index : text  ->  grapheme_entry Prims.list = (fun ( projectee  :  text ) -> (match (projectee) with
| {header = header; index = index; data = data} -> begin
index
end))


let __proj__Mktext__item__data : text  ->  FStar_UInt8.t Prims.list = (fun ( projectee  :  text ) -> (match (projectee) with
| {header = header; index = index; data = data} -> begin
data
end))


let empty_text : text = {header = {grapheme_count = (Prims.parse_int "0"); byte_length = (Prims.parse_int "0"); complexity = Simple}; index = []; data = []}


let is_simple : text  ->  Prims.bool = (fun ( t  :  text ) -> (Prims.op_Equality t.header.complexity Simple))


let is_complex : text  ->  Prims.bool = (fun ( t  :  text ) -> (Prims.op_Equality t.header.complexity Complex))


let grapheme_count : text  ->  Prims.nat = (fun ( t  :  text ) -> t.header.grapheme_count)


let byte_length : text  ->  Prims.nat = (fun ( t  :  text ) -> t.header.byte_length)




