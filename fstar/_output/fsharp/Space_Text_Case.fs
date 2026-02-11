#light "off"
module Space_Text_Case
type case_type =
| Uppercase
| Lowercase
| Titlecase


let uu___is_Uppercase : case_type  ->  Prims.bool = (fun ( projectee  :  case_type ) -> (match (projectee) with
| Uppercase -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_Lowercase : case_type  ->  Prims.bool = (fun ( projectee  :  case_type ) -> (match (projectee) with
| Lowercase -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_Titlecase : case_type  ->  Prims.bool = (fun ( projectee  :  case_type ) -> (match (projectee) with
| Titlecase -> begin
true
end
| uu___ -> begin
false
end))

type simple_case_mapping =
{codepoint : Prims.nat; uppercase : Prims.nat; lowercase : Prims.nat; titlecase : Prims.nat}


let __proj__Mksimple_case_mapping__item__codepoint : simple_case_mapping  ->  Prims.nat = (fun ( projectee  :  simple_case_mapping ) -> (match (projectee) with
| {codepoint = codepoint; uppercase = uppercase; lowercase = lowercase; titlecase = titlecase} -> begin
codepoint
end))


let __proj__Mksimple_case_mapping__item__uppercase : simple_case_mapping  ->  Prims.nat = (fun ( projectee  :  simple_case_mapping ) -> (match (projectee) with
| {codepoint = codepoint; uppercase = uppercase; lowercase = lowercase; titlecase = titlecase} -> begin
uppercase
end))


let __proj__Mksimple_case_mapping__item__lowercase : simple_case_mapping  ->  Prims.nat = (fun ( projectee  :  simple_case_mapping ) -> (match (projectee) with
| {codepoint = codepoint; uppercase = uppercase; lowercase = lowercase; titlecase = titlecase} -> begin
lowercase
end))


let __proj__Mksimple_case_mapping__item__titlecase : simple_case_mapping  ->  Prims.nat = (fun ( projectee  :  simple_case_mapping ) -> (match (projectee) with
| {codepoint = codepoint; uppercase = uppercase; lowercase = lowercase; titlecase = titlecase} -> begin
titlecase
end))

type special_case_mapping =
{codepoint1 : Prims.nat; condition : Prims.string; uppercase1 : Prims.nat Prims.list; lowercase1 : Prims.nat Prims.list; titlecase1 : Prims.nat Prims.list}


let __proj__Mkspecial_case_mapping__item__codepoint : special_case_mapping  ->  Prims.nat = (fun ( projectee  :  special_case_mapping ) -> (match (projectee) with
| {codepoint1 = codepoint; condition = condition; uppercase1 = uppercase; lowercase1 = lowercase; titlecase1 = titlecase} -> begin
codepoint
end))


let __proj__Mkspecial_case_mapping__item__condition : special_case_mapping  ->  Prims.string = (fun ( projectee  :  special_case_mapping ) -> (match (projectee) with
| {codepoint1 = codepoint; condition = condition; uppercase1 = uppercase; lowercase1 = lowercase; titlecase1 = titlecase} -> begin
condition
end))


let __proj__Mkspecial_case_mapping__item__uppercase : special_case_mapping  ->  Prims.nat Prims.list = (fun ( projectee  :  special_case_mapping ) -> (match (projectee) with
| {codepoint1 = codepoint; condition = condition; uppercase1 = uppercase; lowercase1 = lowercase; titlecase1 = titlecase} -> begin
uppercase
end))


let __proj__Mkspecial_case_mapping__item__lowercase : special_case_mapping  ->  Prims.nat Prims.list = (fun ( projectee  :  special_case_mapping ) -> (match (projectee) with
| {codepoint1 = codepoint; condition = condition; uppercase1 = uppercase; lowercase1 = lowercase; titlecase1 = titlecase} -> begin
lowercase
end))


let __proj__Mkspecial_case_mapping__item__titlecase : special_case_mapping  ->  Prims.nat Prims.list = (fun ( projectee  :  special_case_mapping ) -> (match (projectee) with
| {codepoint1 = codepoint; condition = condition; uppercase1 = uppercase; lowercase1 = lowercase; titlecase1 = titlecase} -> begin
titlecase
end))


let simple_uppercase : Prims.nat  ->  Prims.nat = (fun ( cp  :  Prims.nat ) -> (match ((Space_Text_UCD_Types.lookup_mapping cp Space_Text_UCD_Case.uppercase_mappings)) with
| FStar_Pervasives_Native.Some (mapped) -> begin
mapped
end
| FStar_Pervasives_Native.None -> begin
cp
end))


let simple_lowercase : Prims.nat  ->  Prims.nat = (fun ( cp  :  Prims.nat ) -> (match ((Space_Text_UCD_Types.lookup_mapping cp Space_Text_UCD_Case.lowercase_mappings)) with
| FStar_Pervasives_Native.Some (mapped) -> begin
mapped
end
| FStar_Pervasives_Native.None -> begin
cp
end))


let simple_titlecase : Prims.nat  ->  Prims.nat = (fun ( cp  :  Prims.nat ) -> (match ((Prims.op_Equality cp (Prims.parse_int "0x01C6"))) with
| true -> begin
(Prims.parse_int "0x01C5")
end
| uu___ -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x01C9"))) with
| true -> begin
(Prims.parse_int "0x01C8")
end
| uu___1 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x01CC"))) with
| true -> begin
(Prims.parse_int "0x01CB")
end
| uu___2 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x01F3"))) with
| true -> begin
(Prims.parse_int "0x01F2")
end
| uu___3 -> begin
(simple_uppercase cp)
end)
end)
end)
end))


let has_case : Prims.nat  ->  Prims.bool = (fun ( cp  :  Prims.nat ) -> ((Prims.op_disEquality (simple_uppercase cp) cp) || (Prims.op_disEquality (simple_lowercase cp) cp)))


let is_uppercase : Prims.nat  ->  Prims.bool = (fun ( cp  :  Prims.nat ) -> ((has_case cp) && (Prims.op_Equality (simple_uppercase cp) cp)))


let is_lowercase : Prims.nat  ->  Prims.bool = (fun ( cp  :  Prims.nat ) -> ((has_case cp) && (Prims.op_Equality (simple_lowercase cp) cp)))


let rec lookup_special_casing : Prims.nat  ->  Space_Text_UCD_Types.special_case_entry Prims.list  ->  Space_Text_UCD_Types.special_case_entry FStar_Pervasives_Native.option = (fun ( cp  :  Prims.nat ) ( table  :  Space_Text_UCD_Types.special_case_entry Prims.list ) -> (match (table) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (entry)::rest -> begin
(match ((Prims.op_Equality entry.codepoint4 cp)) with
| true -> begin
FStar_Pervasives_Native.Some (entry)
end
| uu___ -> begin
(lookup_special_casing cp rest)
end)
end))


let full_uppercase : Prims.nat  ->  Prims.nat Prims.list = (fun ( cp  :  Prims.nat ) -> (match ((lookup_special_casing cp Space_Text_UCD_Case.special_casing_table)) with
| FStar_Pervasives_Native.Some (entry) -> begin
(match ((Prims.uu___is_Cons entry.upper)) with
| true -> begin
entry.upper
end
| uu___ -> begin
((simple_uppercase cp))::[]
end)
end
| FStar_Pervasives_Native.None -> begin
((simple_uppercase cp))::[]
end))


let full_lowercase : Prims.nat  ->  Prims.nat Prims.list = (fun ( cp  :  Prims.nat ) -> (match ((lookup_special_casing cp Space_Text_UCD_Case.special_casing_table)) with
| FStar_Pervasives_Native.Some (entry) -> begin
(match ((Prims.uu___is_Cons entry.lower)) with
| true -> begin
entry.lower
end
| uu___ -> begin
((simple_lowercase cp))::[]
end)
end
| FStar_Pervasives_Native.None -> begin
((simple_lowercase cp))::[]
end))


let full_titlecase : Prims.nat  ->  Prims.nat Prims.list = (fun ( cp  :  Prims.nat ) -> (match ((lookup_special_casing cp Space_Text_UCD_Case.special_casing_table)) with
| FStar_Pervasives_Native.Some (entry) -> begin
(match ((Prims.uu___is_Cons entry.title)) with
| true -> begin
entry.title
end
| uu___ -> begin
((simple_titlecase cp))::[]
end)
end
| FStar_Pervasives_Native.None -> begin
((simple_titlecase cp))::[]
end))


let rec bytes_to_codepoints : FStar_UInt8.t Prims.list  ->  Prims.nat Prims.list = (fun ( bytes  :  FStar_UInt8.t Prims.list ) -> (match (bytes) with
| [] -> begin
[]
end
| (b0)::rest -> begin
(

let len = (Space_Text_UTF8.sequence_length b0)
in (match ((Prims.op_Equality len (Prims.parse_int "0"))) with
| true -> begin
(bytes_to_codepoints rest)
end
| uu___ -> begin
(match ((Prims.op_Equality len (Prims.parse_int "1"))) with
| true -> begin
((FStar_UInt8.v b0))::(bytes_to_codepoints rest)
end
| uu___1 -> begin
(match ((Prims.op_Equality len (Prims.parse_int "2"))) with
| true -> begin
(match (rest) with
| (b1)::rest' -> begin
((Space_Text_UTF8.decode_codepoint_2 b0 b1))::(bytes_to_codepoints rest')
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
((Space_Text_UTF8.decode_codepoint_3 b0 b1 b2))::(bytes_to_codepoints rest')
end
| uu___3 -> begin
[]
end)
end
| uu___3 -> begin
(match (rest) with
| (b1)::(b2)::(b3)::rest' -> begin
((Space_Text_UTF8.decode_codepoint_4 b0 b1 b2 b3))::(bytes_to_codepoints rest')
end
| uu___4 -> begin
[]
end)
end)
end)
end)
end))
end))


let encode_codepoint : Prims.nat  ->  FStar_UInt8.t Prims.list = (fun ( cp  :  Prims.nat ) -> (match ((cp <= (Prims.parse_int "0x7F"))) with
| true -> begin
((FStar_UInt8.uint_to_t cp))::[]
end
| uu___ -> begin
(match ((cp <= (Prims.parse_int "0x7FF"))) with
| true -> begin
((FStar_UInt8.uint_to_t ((Prims.parse_int "0xC0") + (cp / (Prims.parse_int "64")))))::((FStar_UInt8.uint_to_t ((Prims.parse_int "0x80") + (Prims.mod_f cp (Prims.parse_int "64")))))::[]
end
| uu___1 -> begin
(match ((cp <= (Prims.parse_int "0xFFFF"))) with
| true -> begin
((FStar_UInt8.uint_to_t ((Prims.parse_int "0xE0") + (cp / (Prims.parse_int "4096")))))::((FStar_UInt8.uint_to_t ((Prims.parse_int "0x80") + (Prims.mod_f (cp / (Prims.parse_int "64")) (Prims.parse_int "64")))))::((FStar_UInt8.uint_to_t ((Prims.parse_int "0x80") + (Prims.mod_f cp (Prims.parse_int "64")))))::[]
end
| uu___2 -> begin
(match ((cp <= (Prims.parse_int "0x10FFFF"))) with
| true -> begin
((FStar_UInt8.uint_to_t ((Prims.parse_int "0xF0") + (cp / (Prims.parse_int "262144")))))::((FStar_UInt8.uint_to_t ((Prims.parse_int "0x80") + (Prims.mod_f (cp / (Prims.parse_int "4096")) (Prims.parse_int "64")))))::((FStar_UInt8.uint_to_t ((Prims.parse_int "0x80") + (Prims.mod_f (cp / (Prims.parse_int "64")) (Prims.parse_int "64")))))::((FStar_UInt8.uint_to_t ((Prims.parse_int "0x80") + (Prims.mod_f cp (Prims.parse_int "64")))))::[]
end
| uu___3 -> begin
[]
end)
end)
end)
end))


let codepoints_to_bytes : Prims.nat Prims.list  ->  FStar_UInt8.t Prims.list = (fun ( cps  :  Prims.nat Prims.list ) -> (FStar_List_Tot_Base.concatMap encode_codepoint cps))


let map_case : Prims.nat Prims.list  ->  (Prims.nat  ->  Prims.nat Prims.list)  ->  Prims.nat Prims.list = (fun ( cps  :  Prims.nat Prims.list ) ( f  :  Prims.nat  ->  Prims.nat Prims.list ) -> (FStar_List_Tot_Base.concatMap f cps))


let text_to_upper : Space_Text_Types.text  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) -> (

let cps = (bytes_to_codepoints t.data)
in (

let upper_cps = (map_case cps full_uppercase)
in (

let bytes = (codepoints_to_bytes upper_cps)
in (Space_Text_Create.text_from_bytes bytes)))))


let text_to_lower : Space_Text_Types.text  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) -> (

let cps = (bytes_to_codepoints t.data)
in (

let lower_cps = (map_case cps full_lowercase)
in (

let bytes = (codepoints_to_bytes lower_cps)
in (Space_Text_Create.text_from_bytes bytes)))))


let is_word_boundary : Prims.nat  ->  Prims.bool = (fun ( cp  :  Prims.nat ) -> ((((((((((((((((((((((((((((((((((Prims.op_Equality cp (Prims.parse_int "0x20")) || (Prims.op_Equality cp (Prims.parse_int "0x09"))) || (Prims.op_Equality cp (Prims.parse_int "0x0A"))) || (Prims.op_Equality cp (Prims.parse_int "0x0D"))) || (Prims.op_Equality cp (Prims.parse_int "0x0B"))) || (Prims.op_Equality cp (Prims.parse_int "0x0C"))) || (Prims.op_Equality cp (Prims.parse_int "0xA0"))) || (Prims.op_Equality cp (Prims.parse_int "0x2007"))) || (Prims.op_Equality cp (Prims.parse_int "0x202F"))) || ((cp >= (Prims.parse_int "0x2000")) && (cp <= (Prims.parse_int "0x200A")))) || (Prims.op_Equality cp (Prims.parse_int "0x2028"))) || (Prims.op_Equality cp (Prims.parse_int "0x2029"))) || (Prims.op_Equality cp (Prims.parse_int "0x2D"))) || (Prims.op_Equality cp (Prims.parse_int "0x2014"))) || (Prims.op_Equality cp (Prims.parse_int "0x2013"))) || (Prims.op_Equality cp (Prims.parse_int "0x27"))) || (Prims.op_Equality cp (Prims.parse_int "0x2019"))) || (Prims.op_Equality cp (Prims.parse_int "0x2E"))) || (Prims.op_Equality cp (Prims.parse_int "0x2C"))) || (Prims.op_Equality cp (Prims.parse_int "0x3B"))) || (Prims.op_Equality cp (Prims.parse_int "0x3A"))) || (Prims.op_Equality cp (Prims.parse_int "0x21"))) || (Prims.op_Equality cp (Prims.parse_int "0x3F"))) || (Prims.op_Equality cp (Prims.parse_int "0x28"))) || (Prims.op_Equality cp (Prims.parse_int "0x29"))) || (Prims.op_Equality cp (Prims.parse_int "0x5B"))) || (Prims.op_Equality cp (Prims.parse_int "0x5D"))) || (Prims.op_Equality cp (Prims.parse_int "0x7B"))) || (Prims.op_Equality cp (Prims.parse_int "0x7D"))) || (Prims.op_Equality cp (Prims.parse_int "0x22"))) || (Prims.op_Equality cp (Prims.parse_int "0x201C"))) || (Prims.op_Equality cp (Prims.parse_int "0x201D"))) || (Prims.op_Equality cp (Prims.parse_int "0x2F"))) || (Prims.op_Equality cp (Prims.parse_int "0x5C"))))


let is_word_continue : Prims.nat  ->  Prims.bool = (fun ( cp  :  Prims.nat ) -> ((((has_case cp) || (Prims.op_Equality cp (Prims.parse_int "0x27"))) || (Prims.op_Equality cp (Prims.parse_int "0x2019"))) || ((cp >= (Prims.parse_int "0x30")) && (cp <= (Prims.parse_int "0x39")))))


let text_to_title : Space_Text_Types.text  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) -> (

let cps = (bytes_to_codepoints t.data)
in (

let rec titlecase_aux : Prims.nat Prims.list  ->  Prims.bool  ->  Prims.nat Prims.list = (fun ( cps1  :  Prims.nat Prims.list ) ( at_word_start  :  Prims.bool ) -> (match (cps1) with
| [] -> begin
[]
end
| (cp)::rest -> begin
(

let is_cased = (has_case cp)
in (

let is_boundary = (is_word_boundary cp)
in (

let continues_word = (is_word_continue cp)
in (match ((at_word_start && is_cased)) with
| true -> begin
(FStar_List_Tot_Base.op_At (full_titlecase cp) (titlecase_aux rest false))
end
| uu___ -> begin
(match (is_boundary) with
| true -> begin
(FStar_List_Tot_Base.op_At ((cp)::[]) (titlecase_aux rest true))
end
| uu___1 -> begin
(match (is_cased) with
| true -> begin
(FStar_List_Tot_Base.op_At (full_lowercase cp) (titlecase_aux rest false))
end
| uu___2 -> begin
(match (continues_word) with
| true -> begin
(FStar_List_Tot_Base.op_At ((cp)::[]) (titlecase_aux rest false))
end
| uu___3 -> begin
(FStar_List_Tot_Base.op_At ((cp)::[]) (titlecase_aux rest at_word_start))
end)
end)
end)
end))))
end))
in (

let title_cps = (titlecase_aux cps true)
in (

let bytes = (codepoints_to_bytes title_cps)
in (Space_Text_Create.text_from_bytes bytes))))))


let casefold_codepoint : Prims.nat  ->  Prims.nat Prims.list = (fun ( cp  :  Prims.nat ) -> (match ((Prims.op_Equality cp (Prims.parse_int "0x00DF"))) with
| true -> begin
((Prims.parse_int "0x0073"))::((Prims.parse_int "0x0073"))::[]
end
| uu___ -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x0130"))) with
| true -> begin
((Prims.parse_int "0x0069"))::((Prims.parse_int "0x0307"))::[]
end
| uu___1 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x0149"))) with
| true -> begin
((Prims.parse_int "0x02BC"))::((Prims.parse_int "0x006E"))::[]
end
| uu___2 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x01F0"))) with
| true -> begin
((Prims.parse_int "0x006A"))::((Prims.parse_int "0x030C"))::[]
end
| uu___3 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x0390"))) with
| true -> begin
((Prims.parse_int "0x03B9"))::((Prims.parse_int "0x0308"))::((Prims.parse_int "0x0301"))::[]
end
| uu___4 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x03B0"))) with
| true -> begin
((Prims.parse_int "0x03C5"))::((Prims.parse_int "0x0308"))::((Prims.parse_int "0x0301"))::[]
end
| uu___5 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1E96"))) with
| true -> begin
((Prims.parse_int "0x0068"))::((Prims.parse_int "0x0331"))::[]
end
| uu___6 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1E97"))) with
| true -> begin
((Prims.parse_int "0x0074"))::((Prims.parse_int "0x0308"))::[]
end
| uu___7 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1E98"))) with
| true -> begin
((Prims.parse_int "0x0077"))::((Prims.parse_int "0x030A"))::[]
end
| uu___8 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1E99"))) with
| true -> begin
((Prims.parse_int "0x0079"))::((Prims.parse_int "0x030A"))::[]
end
| uu___9 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1E9A"))) with
| true -> begin
((Prims.parse_int "0x0061"))::((Prims.parse_int "0x02BE"))::[]
end
| uu___10 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1E9E"))) with
| true -> begin
((Prims.parse_int "0x0073"))::((Prims.parse_int "0x0073"))::[]
end
| uu___11 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F50"))) with
| true -> begin
((Prims.parse_int "0x03C5"))::((Prims.parse_int "0x0313"))::[]
end
| uu___12 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F52"))) with
| true -> begin
((Prims.parse_int "0x03C5"))::((Prims.parse_int "0x0313"))::((Prims.parse_int "0x0300"))::[]
end
| uu___13 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F54"))) with
| true -> begin
((Prims.parse_int "0x03C5"))::((Prims.parse_int "0x0313"))::((Prims.parse_int "0x0301"))::[]
end
| uu___14 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F56"))) with
| true -> begin
((Prims.parse_int "0x03C5"))::((Prims.parse_int "0x0313"))::((Prims.parse_int "0x0342"))::[]
end
| uu___15 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F80"))) with
| true -> begin
((Prims.parse_int "0x1F00"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___16 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F81"))) with
| true -> begin
((Prims.parse_int "0x1F01"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___17 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F82"))) with
| true -> begin
((Prims.parse_int "0x1F02"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___18 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F83"))) with
| true -> begin
((Prims.parse_int "0x1F03"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___19 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F84"))) with
| true -> begin
((Prims.parse_int "0x1F04"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___20 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F85"))) with
| true -> begin
((Prims.parse_int "0x1F05"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___21 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F86"))) with
| true -> begin
((Prims.parse_int "0x1F06"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___22 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F87"))) with
| true -> begin
((Prims.parse_int "0x1F07"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___23 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F88"))) with
| true -> begin
((Prims.parse_int "0x1F00"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___24 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F89"))) with
| true -> begin
((Prims.parse_int "0x1F01"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___25 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F8A"))) with
| true -> begin
((Prims.parse_int "0x1F02"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___26 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F8B"))) with
| true -> begin
((Prims.parse_int "0x1F03"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___27 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F8C"))) with
| true -> begin
((Prims.parse_int "0x1F04"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___28 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F8D"))) with
| true -> begin
((Prims.parse_int "0x1F05"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___29 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F8E"))) with
| true -> begin
((Prims.parse_int "0x1F06"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___30 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1F8F"))) with
| true -> begin
((Prims.parse_int "0x1F07"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___31 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FB2"))) with
| true -> begin
((Prims.parse_int "0x1F70"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___32 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FB3"))) with
| true -> begin
((Prims.parse_int "0x03B1"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___33 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FB4"))) with
| true -> begin
((Prims.parse_int "0x03AC"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___34 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FB6"))) with
| true -> begin
((Prims.parse_int "0x03B1"))::((Prims.parse_int "0x0342"))::[]
end
| uu___35 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FB7"))) with
| true -> begin
((Prims.parse_int "0x03B1"))::((Prims.parse_int "0x0342"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___36 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FBC"))) with
| true -> begin
((Prims.parse_int "0x03B1"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___37 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FC2"))) with
| true -> begin
((Prims.parse_int "0x1F74"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___38 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FC3"))) with
| true -> begin
((Prims.parse_int "0x03B7"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___39 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FC4"))) with
| true -> begin
((Prims.parse_int "0x03AE"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___40 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FC6"))) with
| true -> begin
((Prims.parse_int "0x03B7"))::((Prims.parse_int "0x0342"))::[]
end
| uu___41 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FC7"))) with
| true -> begin
((Prims.parse_int "0x03B7"))::((Prims.parse_int "0x0342"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___42 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FCC"))) with
| true -> begin
((Prims.parse_int "0x03B7"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___43 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FD2"))) with
| true -> begin
((Prims.parse_int "0x03B9"))::((Prims.parse_int "0x0308"))::((Prims.parse_int "0x0300"))::[]
end
| uu___44 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FD3"))) with
| true -> begin
((Prims.parse_int "0x03B9"))::((Prims.parse_int "0x0308"))::((Prims.parse_int "0x0301"))::[]
end
| uu___45 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FD6"))) with
| true -> begin
((Prims.parse_int "0x03B9"))::((Prims.parse_int "0x0342"))::[]
end
| uu___46 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FD7"))) with
| true -> begin
((Prims.parse_int "0x03B9"))::((Prims.parse_int "0x0308"))::((Prims.parse_int "0x0342"))::[]
end
| uu___47 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FE2"))) with
| true -> begin
((Prims.parse_int "0x03C5"))::((Prims.parse_int "0x0308"))::((Prims.parse_int "0x0300"))::[]
end
| uu___48 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FE3"))) with
| true -> begin
((Prims.parse_int "0x03C5"))::((Prims.parse_int "0x0308"))::((Prims.parse_int "0x0301"))::[]
end
| uu___49 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FE4"))) with
| true -> begin
((Prims.parse_int "0x03C1"))::((Prims.parse_int "0x0313"))::[]
end
| uu___50 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FE6"))) with
| true -> begin
((Prims.parse_int "0x03C5"))::((Prims.parse_int "0x0342"))::[]
end
| uu___51 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FE7"))) with
| true -> begin
((Prims.parse_int "0x03C5"))::((Prims.parse_int "0x0308"))::((Prims.parse_int "0x0342"))::[]
end
| uu___52 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FF2"))) with
| true -> begin
((Prims.parse_int "0x1F7C"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___53 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FF3"))) with
| true -> begin
((Prims.parse_int "0x03C9"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___54 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FF4"))) with
| true -> begin
((Prims.parse_int "0x03CE"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___55 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FF6"))) with
| true -> begin
((Prims.parse_int "0x03C9"))::((Prims.parse_int "0x0342"))::[]
end
| uu___56 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FF7"))) with
| true -> begin
((Prims.parse_int "0x03C9"))::((Prims.parse_int "0x0342"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___57 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x1FFC"))) with
| true -> begin
((Prims.parse_int "0x03C9"))::((Prims.parse_int "0x03B9"))::[]
end
| uu___58 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0xFB00"))) with
| true -> begin
((Prims.parse_int "0x0066"))::((Prims.parse_int "0x0066"))::[]
end
| uu___59 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0xFB01"))) with
| true -> begin
((Prims.parse_int "0x0066"))::((Prims.parse_int "0x0069"))::[]
end
| uu___60 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0xFB02"))) with
| true -> begin
((Prims.parse_int "0x0066"))::((Prims.parse_int "0x006C"))::[]
end
| uu___61 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0xFB03"))) with
| true -> begin
((Prims.parse_int "0x0066"))::((Prims.parse_int "0x0066"))::((Prims.parse_int "0x0069"))::[]
end
| uu___62 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0xFB04"))) with
| true -> begin
((Prims.parse_int "0x0066"))::((Prims.parse_int "0x0066"))::((Prims.parse_int "0x006C"))::[]
end
| uu___63 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0xFB05"))) with
| true -> begin
((Prims.parse_int "0x0073"))::((Prims.parse_int "0x0074"))::[]
end
| uu___64 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0xFB06"))) with
| true -> begin
((Prims.parse_int "0x0073"))::((Prims.parse_int "0x0074"))::[]
end
| uu___65 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x017F"))) with
| true -> begin
((Prims.parse_int "0x0073"))::[]
end
| uu___66 -> begin
((simple_lowercase cp))::[]
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end))


let text_casefold : Space_Text_Types.text  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) -> (

let cps = (bytes_to_codepoints t.data)
in (

let folded = (FStar_List_Tot_Base.concatMap casefold_codepoint cps)
in (

let bytes = (codepoints_to_bytes folded)
in (Space_Text_Create.text_from_bytes bytes)))))


let text_equal_ignore_case : Space_Text_Types.text  ->  Space_Text_Types.text  ->  Prims.bool = (fun ( t1  :  Space_Text_Types.text ) ( t2  :  Space_Text_Types.text ) -> (match ((((text_casefold t1)), ((text_casefold t2)))) with
| (FStar_Pervasives_Native.Some (f1), FStar_Pervasives_Native.Some (f2)) -> begin
((Prims.op_Equality f1.header.byte_length f2.header.byte_length) && (Prims.op_Equality f1.data f2.data))
end
| uu___ -> begin
false
end))


let text_is_upper : Space_Text_Types.text  ->  Prims.bool = (fun ( t  :  Space_Text_Types.text ) -> (

let cps = (bytes_to_codepoints t.data)
in (FStar_List_Tot_Base.for_all (fun ( cp  :  Prims.nat ) -> ((not ((has_case cp))) || (is_uppercase cp))) cps)))


let text_is_lower : Space_Text_Types.text  ->  Prims.bool = (fun ( t  :  Space_Text_Types.text ) -> (

let cps = (bytes_to_codepoints t.data)
in (FStar_List_Tot_Base.for_all (fun ( cp  :  Prims.nat ) -> ((not ((has_case cp))) || (is_lowercase cp))) cps)))


let text_capitalize : Space_Text_Types.text  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) -> (

let cps = (bytes_to_codepoints t.data)
in (match (cps) with
| [] -> begin
FStar_Pervasives_Native.Some (t)
end
| (first)::rest -> begin
(

let cap_cps = (FStar_List_Tot_Base.op_At (full_titlecase first) (map_case rest full_lowercase))
in (

let bytes = (codepoints_to_bytes cap_cps)
in (Space_Text_Create.text_from_bytes bytes)))
end)))


let text_swapcase : Space_Text_Types.text  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) -> (

let cps = (bytes_to_codepoints t.data)
in (

let swap = (fun ( cp  :  Prims.nat ) -> (match ((is_uppercase cp)) with
| true -> begin
(full_lowercase cp)
end
| uu___ -> begin
(match ((is_lowercase cp)) with
| true -> begin
(full_uppercase cp)
end
| uu___1 -> begin
(cp)::[]
end)
end))
in (

let swapped = (map_case cps swap)
in (

let bytes = (codepoints_to_bytes swapped)
in (Space_Text_Create.text_from_bytes bytes))))))




