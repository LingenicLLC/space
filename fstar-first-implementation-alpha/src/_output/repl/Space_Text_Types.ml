open Prims
type text_complexity =
  | Simple 
  | Complex 
let uu___is_Simple (projectee : text_complexity) : Prims.bool=
  match projectee with | Simple -> true | uu___ -> false
let uu___is_Complex (projectee : text_complexity) : Prims.bool=
  match projectee with | Complex -> true | uu___ -> false
type text_header =
  {
  grapheme_count: Prims.nat ;
  byte_length: Prims.nat ;
  complexity: text_complexity }
let __proj__Mktext_header__item__grapheme_count (projectee : text_header) :
  Prims.nat=
  match projectee with
  | { grapheme_count; byte_length; complexity;_} -> grapheme_count
let __proj__Mktext_header__item__byte_length (projectee : text_header) :
  Prims.nat=
  match projectee with
  | { grapheme_count; byte_length; complexity;_} -> byte_length
let __proj__Mktext_header__item__complexity (projectee : text_header) :
  text_complexity=
  match projectee with
  | { grapheme_count; byte_length; complexity;_} -> complexity
type grapheme_entry = {
  byte_offset: Prims.nat ;
  byte_len: Prims.nat }
let __proj__Mkgrapheme_entry__item__byte_offset (projectee : grapheme_entry)
  : Prims.nat=
  match projectee with | { byte_offset; byte_len;_} -> byte_offset
let __proj__Mkgrapheme_entry__item__byte_len (projectee : grapheme_entry) :
  Prims.nat= match projectee with | { byte_offset; byte_len;_} -> byte_len
type grapheme = {
  bytes: FStar_UInt8.t Prims.list ;
  len: Prims.nat }
let __proj__Mkgrapheme__item__bytes (projectee : grapheme) :
  FStar_UInt8.t Prims.list= match projectee with | { bytes; len;_} -> bytes
let __proj__Mkgrapheme__item__len (projectee : grapheme) : Prims.nat=
  match projectee with | { bytes; len;_} -> len
let empty_grapheme : grapheme= { bytes = []; len = Prims.int_zero }
type text =
  {
  header: text_header ;
  index: grapheme_entry Prims.list ;
  data: FStar_UInt8.t Prims.list }
let __proj__Mktext__item__header (projectee : text) : text_header=
  match projectee with | { header; index; data;_} -> header
let __proj__Mktext__item__index (projectee : text) :
  grapheme_entry Prims.list=
  match projectee with | { header; index; data;_} -> index
let __proj__Mktext__item__data (projectee : text) : FStar_UInt8.t Prims.list=
  match projectee with | { header; index; data;_} -> data
let empty_text : text=
  {
    header =
      {
        grapheme_count = Prims.int_zero;
        byte_length = Prims.int_zero;
        complexity = Simple
      };
    index = [];
    data = []
  }
let is_simple (t : text) : Prims.bool= (t.header).complexity = Simple
let is_complex (t : text) : Prims.bool= (t.header).complexity = Complex
let grapheme_count (t : text) : Prims.nat= (t.header).grapheme_count
let byte_length (t : text) : Prims.nat= (t.header).byte_length
