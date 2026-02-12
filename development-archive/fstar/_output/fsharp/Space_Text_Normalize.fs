#light "off"
module Space_Text_Normalize
type normalization_form =
| NFC
| NFD
| NFKC
| NFKD


let uu___is_NFC : normalization_form  ->  Prims.bool = (fun ( projectee  :  normalization_form ) -> (match (projectee) with
| NFC -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_NFD : normalization_form  ->  Prims.bool = (fun ( projectee  :  normalization_form ) -> (match (projectee) with
| NFD -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_NFKC : normalization_form  ->  Prims.bool = (fun ( projectee  :  normalization_form ) -> (match (projectee) with
| NFKC -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_NFKD : normalization_form  ->  Prims.bool = (fun ( projectee  :  normalization_form ) -> (match (projectee) with
| NFKD -> begin
true
end
| uu___ -> begin
false
end))


type combining_class =
Prims.nat

type decomposition_type =
| Canonical
| Compatibility


let uu___is_Canonical : decomposition_type  ->  Prims.bool = (fun ( projectee  :  decomposition_type ) -> (match (projectee) with
| Canonical -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_Compatibility : decomposition_type  ->  Prims.bool = (fun ( projectee  :  decomposition_type ) -> (match (projectee) with
| Compatibility -> begin
true
end
| uu___ -> begin
false
end))

type decomposition_entry =
{codepoint : Prims.nat; decomp_type : decomposition_type; decomposition : Prims.nat Prims.list}


let __proj__Mkdecomposition_entry__item__codepoint : decomposition_entry  ->  Prims.nat = (fun ( projectee  :  decomposition_entry ) -> (match (projectee) with
| {codepoint = codepoint; decomp_type = decomp_type; decomposition = decomposition} -> begin
codepoint
end))


let __proj__Mkdecomposition_entry__item__decomp_type : decomposition_entry  ->  decomposition_type = (fun ( projectee  :  decomposition_entry ) -> (match (projectee) with
| {codepoint = codepoint; decomp_type = decomp_type; decomposition = decomposition} -> begin
decomp_type
end))


let __proj__Mkdecomposition_entry__item__decomposition : decomposition_entry  ->  Prims.nat Prims.list = (fun ( projectee  :  decomposition_entry ) -> (match (projectee) with
| {codepoint = codepoint; decomp_type = decomp_type; decomposition = decomposition} -> begin
decomposition
end))

type composition_entry =
{first : Prims.nat; second : Prims.nat; composed : Prims.nat}


let __proj__Mkcomposition_entry__item__first : composition_entry  ->  Prims.nat = (fun ( projectee  :  composition_entry ) -> (match (projectee) with
| {first = first; second = second; composed = composed} -> begin
first
end))


let __proj__Mkcomposition_entry__item__second : composition_entry  ->  Prims.nat = (fun ( projectee  :  composition_entry ) -> (match (projectee) with
| {first = first; second = second; composed = composed} -> begin
second
end))


let __proj__Mkcomposition_entry__item__composed : composition_entry  ->  Prims.nat = (fun ( projectee  :  composition_entry ) -> (match (projectee) with
| {first = first; second = second; composed = composed} -> begin
composed
end))

type quick_check =
| QC_Yes
| QC_No
| QC_Maybe


let uu___is_QC_Yes : quick_check  ->  Prims.bool = (fun ( projectee  :  quick_check ) -> (match (projectee) with
| QC_Yes -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_QC_No : quick_check  ->  Prims.bool = (fun ( projectee  :  quick_check ) -> (match (projectee) with
| QC_No -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_QC_Maybe : quick_check  ->  Prims.bool = (fun ( projectee  :  quick_check ) -> (match (projectee) with
| QC_Maybe -> begin
true
end
| uu___ -> begin
false
end))


let get_combining_class : Prims.nat  ->  combining_class = (fun ( cp  :  Prims.nat ) -> (

let ccc = (Space_Text_UCD_Types.lookup_ccc cp Space_Text_UCD_CCC.combining_class_table)
in (match ((ccc <= (Prims.parse_int "254"))) with
| true -> begin
ccc
end
| uu___ -> begin
(Prims.parse_int "0")
end)))


let is_starter : Prims.nat  ->  Prims.bool = (fun ( cp  :  Prims.nat ) -> (Prims.op_Equality (get_combining_class cp) (Prims.parse_int "0")))


let get_canonical_decomposition : Prims.nat  ->  Prims.nat Prims.list FStar_Pervasives_Native.option = (fun ( cp  :  Prims.nat ) -> (Space_Text_UCD_Types.lookup_decomp cp Space_Text_UCD_Decomp.canonical_decomposition_table))


let get_compatibility_decomposition : Prims.nat  ->  Prims.nat Prims.list FStar_Pervasives_Native.option = (fun ( cp  :  Prims.nat ) -> (match ((get_canonical_decomposition cp)) with
| FStar_Pervasives_Native.Some (d) -> begin
FStar_Pervasives_Native.Some (d)
end
| FStar_Pervasives_Native.None -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x00BC"))) with
| true -> begin
FStar_Pervasives_Native.Some (((Prims.parse_int "0x0031"))::((Prims.parse_int "0x2044"))::((Prims.parse_int "0x0034"))::[])
end
| uu___ -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x00BD"))) with
| true -> begin
FStar_Pervasives_Native.Some (((Prims.parse_int "0x0031"))::((Prims.parse_int "0x2044"))::((Prims.parse_int "0x0032"))::[])
end
| uu___1 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x00BE"))) with
| true -> begin
FStar_Pervasives_Native.Some (((Prims.parse_int "0x0033"))::((Prims.parse_int "0x2044"))::((Prims.parse_int "0x0034"))::[])
end
| uu___2 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x2126"))) with
| true -> begin
FStar_Pervasives_Native.Some (((Prims.parse_int "0x03A9"))::[])
end
| uu___3 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x212A"))) with
| true -> begin
FStar_Pervasives_Native.Some (((Prims.parse_int "0x004B"))::[])
end
| uu___4 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x212B"))) with
| true -> begin
FStar_Pervasives_Native.Some (((Prims.parse_int "0x00C5"))::[])
end
| uu___5 -> begin
(match (((cp >= (Prims.parse_int "0xFF21")) && (cp <= (Prims.parse_int "0xFF3A")))) with
| true -> begin
FStar_Pervasives_Native.Some ((((cp - (Prims.parse_int "0xFF21")) + (Prims.parse_int "0x0041")))::[])
end
| uu___6 -> begin
(match (((cp >= (Prims.parse_int "0xFF41")) && (cp <= (Prims.parse_int "0xFF5A")))) with
| true -> begin
FStar_Pervasives_Native.Some ((((cp - (Prims.parse_int "0xFF41")) + (Prims.parse_int "0x0061")))::[])
end
| uu___7 -> begin
FStar_Pervasives_Native.None
end)
end)
end)
end)
end)
end)
end)
end)
end))


let get_composition : Prims.nat  ->  Prims.nat  ->  Prims.nat FStar_Pervasives_Native.option = (fun ( first  :  Prims.nat ) ( second  :  Prims.nat ) -> (Space_Text_UCD_Comp.try_compose first second))


let decompose_one : Prims.nat  ->  Prims.bool  ->  Prims.nat Prims.list = (fun ( cp  :  Prims.nat ) ( compat  :  Prims.bool ) -> (

let decomp = (match (compat) with
| true -> begin
(get_compatibility_decomposition cp)
end
| uu___ -> begin
(get_canonical_decomposition cp)
end)
in (match (decomp) with
| FStar_Pervasives_Native.None -> begin
(cp)::[]
end
| FStar_Pervasives_Native.Some (cps) -> begin
cps
end)))


let rec decompose_pass : Prims.nat Prims.list  ->  Prims.bool  ->  Prims.nat  ->  Prims.nat Prims.list = (fun ( cps  :  Prims.nat Prims.list ) ( compat  :  Prims.bool ) ( fuel  :  Prims.nat ) -> (match ((Prims.op_Equality fuel (Prims.parse_int "0"))) with
| true -> begin
cps
end
| uu___ -> begin
(

let fuel' = (fuel - (Prims.parse_int "1"))
in (

let expanded = (FStar_List_Tot_Base.concatMap (fun ( cp  :  Prims.nat ) -> (decompose_one cp compat)) cps)
in (match ((Prims.op_Equality (FStar_List_Tot_Base.length expanded) (FStar_List_Tot_Base.length cps))) with
| true -> begin
cps
end
| uu___1 -> begin
(decompose_pass expanded compat fuel')
end)))
end))


let full_decomposition : Prims.nat  ->  Prims.bool  ->  Prims.nat  ->  Prims.nat Prims.list = (fun ( cp  :  Prims.nat ) ( compat  :  Prims.bool ) ( fuel  :  Prims.nat ) -> (decompose_pass ((cp)::[]) compat fuel))


let decompose_codepoints : Prims.nat Prims.list  ->  Prims.bool  ->  Prims.nat Prims.list = (fun ( cps  :  Prims.nat Prims.list ) ( compat  :  Prims.bool ) -> (FStar_List_Tot_Base.concatMap (fun ( cp  :  Prims.nat ) -> (full_decomposition cp compat (Prims.parse_int "10"))) cps))


let rec insert_by_ccc : Prims.nat  ->  Prims.nat Prims.list  ->  Prims.nat Prims.list = (fun ( cp  :  Prims.nat ) ( sorted  :  Prims.nat Prims.list ) -> (match (sorted) with
| [] -> begin
(cp)::[]
end
| (x)::rest -> begin
(

let cp_ccc = (get_combining_class cp)
in (

let x_ccc = (get_combining_class x)
in (match ((cp_ccc < x_ccc)) with
| true -> begin
(cp)::(x)::rest
end
| uu___ -> begin
(x)::(insert_by_ccc cp rest)
end)))
end))


let rec sort_by_ccc : Prims.nat Prims.list  ->  Prims.nat Prims.list = (fun ( cps  :  Prims.nat Prims.list ) -> (match (cps) with
| [] -> begin
[]
end
| (cp)::rest -> begin
(insert_by_ccc cp (sort_by_ccc rest))
end))


let rec canonical_order_aux : Prims.nat Prims.list  ->  Prims.nat Prims.list  ->  Prims.nat Prims.list = (fun ( cps  :  Prims.nat Prims.list ) ( acc  :  Prims.nat Prims.list ) -> (match (cps) with
| [] -> begin
(sort_by_ccc acc)
end
| (cp)::rest -> begin
(match (((is_starter cp) && ((FStar_List_Tot_Base.length acc) > (Prims.parse_int "0")))) with
| true -> begin
(FStar_List_Tot_Base.op_At (sort_by_ccc acc) (canonical_order_aux rest ((cp)::[])))
end
| uu___ -> begin
(canonical_order_aux rest (FStar_List_Tot_Base.op_At acc ((cp)::[])))
end)
end))


let canonical_order : Prims.nat Prims.list  ->  Prims.nat Prims.list = (fun ( cps  :  Prims.nat Prims.list ) -> (canonical_order_aux cps []))


let rec compose_pass_aux : Prims.nat Prims.list  ->  Prims.nat  ->  Prims.nat Prims.list = (fun ( cps  :  Prims.nat Prims.list ) ( fuel  :  Prims.nat ) -> (match ((Prims.op_Equality fuel (Prims.parse_int "0"))) with
| true -> begin
cps
end
| uu___ -> begin
(

let fuel' = (fuel - (Prims.parse_int "1"))
in (match (cps) with
| [] -> begin
[]
end
| (cp)::[] -> begin
(cp)::[]
end
| (first)::(second)::rest -> begin
(match ((get_composition first second)) with
| FStar_Pervasives_Native.Some (composed) -> begin
(compose_pass_aux ((composed)::rest) fuel')
end
| FStar_Pervasives_Native.None -> begin
(first)::(compose_pass_aux ((second)::rest) fuel')
end)
end))
end))


let compose_pass : Prims.nat Prims.list  ->  Prims.nat Prims.list = (fun ( cps  :  Prims.nat Prims.list ) -> (compose_pass_aux cps ((FStar_List_Tot_Base.length cps) + (Prims.parse_int "1"))))


let rec compose_full : Prims.nat Prims.list  ->  Prims.nat  ->  Prims.nat Prims.list = (fun ( cps  :  Prims.nat Prims.list ) ( fuel  :  Prims.nat ) -> (match ((Prims.op_Equality fuel (Prims.parse_int "0"))) with
| true -> begin
cps
end
| uu___ -> begin
(

let fuel' = (fuel - (Prims.parse_int "1"))
in (

let composed = (compose_pass cps)
in (match ((Prims.op_Equality (FStar_List_Tot_Base.length composed) (FStar_List_Tot_Base.length cps))) with
| true -> begin
cps
end
| uu___1 -> begin
(compose_full composed fuel')
end)))
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


let normalize_nfd : Space_Text_Types.text  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) -> (

let cps = (bytes_to_codepoints t.data)
in (

let decomposed = (decompose_codepoints cps false)
in (

let ordered = (canonical_order decomposed)
in (

let bytes = (codepoints_to_bytes ordered)
in (Space_Text_Create.text_from_bytes bytes))))))


let normalize_nfc : Space_Text_Types.text  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) -> (

let cps = (bytes_to_codepoints t.data)
in (

let decomposed = (decompose_codepoints cps false)
in (

let ordered = (canonical_order decomposed)
in (

let composed = (compose_full ordered (Prims.parse_int "10"))
in (

let bytes = (codepoints_to_bytes composed)
in (Space_Text_Create.text_from_bytes bytes)))))))


let normalize_nfkd : Space_Text_Types.text  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) -> (

let cps = (bytes_to_codepoints t.data)
in (

let decomposed = (decompose_codepoints cps true)
in (

let ordered = (canonical_order decomposed)
in (

let bytes = (codepoints_to_bytes ordered)
in (Space_Text_Create.text_from_bytes bytes))))))


let normalize_nfkc : Space_Text_Types.text  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) -> (

let cps = (bytes_to_codepoints t.data)
in (

let decomposed = (decompose_codepoints cps true)
in (

let ordered = (canonical_order decomposed)
in (

let composed = (compose_full ordered (Prims.parse_int "10"))
in (

let bytes = (codepoints_to_bytes composed)
in (Space_Text_Create.text_from_bytes bytes)))))))


let text_normalize : Space_Text_Types.text  ->  normalization_form  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) ( form  :  normalization_form ) -> (match (form) with
| NFC -> begin
(normalize_nfc t)
end
| NFD -> begin
(normalize_nfd t)
end
| NFKC -> begin
(normalize_nfkc t)
end
| NFKD -> begin
(normalize_nfkd t)
end))


let is_normalized : Space_Text_Types.text  ->  normalization_form  ->  Prims.bool = (fun ( t  :  Space_Text_Types.text ) ( form  :  normalization_form ) -> (match ((text_normalize t form)) with
| FStar_Pervasives_Native.None -> begin
false
end
| FStar_Pervasives_Native.Some (normalized) -> begin
((Prims.op_Equality t.header.byte_length normalized.header.byte_length) && (Prims.op_Equality t.data normalized.data))
end))




