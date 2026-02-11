#light "off"
module Space_Text_UCD_Types
type mapping_entry =
{codepoint : Prims.nat; mapped : Prims.nat}


let __proj__Mkmapping_entry__item__codepoint : mapping_entry  ->  Prims.nat = (fun ( projectee  :  mapping_entry ) -> (match (projectee) with
| {codepoint = codepoint; mapped = mapped} -> begin
codepoint
end))


let __proj__Mkmapping_entry__item__mapped : mapping_entry  ->  Prims.nat = (fun ( projectee  :  mapping_entry ) -> (match (projectee) with
| {codepoint = codepoint; mapped = mapped} -> begin
mapped
end))

type multi_mapping_entry =
{codepoint1 : Prims.nat; mapped_to : Prims.nat Prims.list}


let __proj__Mkmulti_mapping_entry__item__codepoint : multi_mapping_entry  ->  Prims.nat = (fun ( projectee  :  multi_mapping_entry ) -> (match (projectee) with
| {codepoint1 = codepoint; mapped_to = mapped_to} -> begin
codepoint
end))


let __proj__Mkmulti_mapping_entry__item__mapped_to : multi_mapping_entry  ->  Prims.nat Prims.list = (fun ( projectee  :  multi_mapping_entry ) -> (match (projectee) with
| {codepoint1 = codepoint; mapped_to = mapped_to} -> begin
mapped_to
end))

type ccc_entry =
{codepoint2 : Prims.nat; ccc : Prims.nat}


let __proj__Mkccc_entry__item__codepoint : ccc_entry  ->  Prims.nat = (fun ( projectee  :  ccc_entry ) -> (match (projectee) with
| {codepoint2 = codepoint; ccc = ccc} -> begin
codepoint
end))


let __proj__Mkccc_entry__item__ccc : ccc_entry  ->  Prims.nat = (fun ( projectee  :  ccc_entry ) -> (match (projectee) with
| {codepoint2 = codepoint; ccc = ccc} -> begin
ccc
end))

type decomp_entry =
{codepoint3 : Prims.nat; decomposition : Prims.nat Prims.list; is_canonical : Prims.bool}


let __proj__Mkdecomp_entry__item__codepoint : decomp_entry  ->  Prims.nat = (fun ( projectee  :  decomp_entry ) -> (match (projectee) with
| {codepoint3 = codepoint; decomposition = decomposition; is_canonical = is_canonical} -> begin
codepoint
end))


let __proj__Mkdecomp_entry__item__decomposition : decomp_entry  ->  Prims.nat Prims.list = (fun ( projectee  :  decomp_entry ) -> (match (projectee) with
| {codepoint3 = codepoint; decomposition = decomposition; is_canonical = is_canonical} -> begin
decomposition
end))


let __proj__Mkdecomp_entry__item__is_canonical : decomp_entry  ->  Prims.bool = (fun ( projectee  :  decomp_entry ) -> (match (projectee) with
| {codepoint3 = codepoint; decomposition = decomposition; is_canonical = is_canonical} -> begin
is_canonical
end))

type comp_entry =
{first : Prims.nat; second : Prims.nat; composed : Prims.nat}


let __proj__Mkcomp_entry__item__first : comp_entry  ->  Prims.nat = (fun ( projectee  :  comp_entry ) -> (match (projectee) with
| {first = first; second = second; composed = composed} -> begin
first
end))


let __proj__Mkcomp_entry__item__second : comp_entry  ->  Prims.nat = (fun ( projectee  :  comp_entry ) -> (match (projectee) with
| {first = first; second = second; composed = composed} -> begin
second
end))


let __proj__Mkcomp_entry__item__composed : comp_entry  ->  Prims.nat = (fun ( projectee  :  comp_entry ) -> (match (projectee) with
| {first = first; second = second; composed = composed} -> begin
composed
end))

type special_case_entry =
{codepoint4 : Prims.nat; lower : Prims.nat Prims.list; title : Prims.nat Prims.list; upper : Prims.nat Prims.list}


let __proj__Mkspecial_case_entry__item__codepoint : special_case_entry  ->  Prims.nat = (fun ( projectee  :  special_case_entry ) -> (match (projectee) with
| {codepoint4 = codepoint; lower = lower; title = title; upper = upper} -> begin
codepoint
end))


let __proj__Mkspecial_case_entry__item__lower : special_case_entry  ->  Prims.nat Prims.list = (fun ( projectee  :  special_case_entry ) -> (match (projectee) with
| {codepoint4 = codepoint; lower = lower; title = title; upper = upper} -> begin
lower
end))


let __proj__Mkspecial_case_entry__item__title : special_case_entry  ->  Prims.nat Prims.list = (fun ( projectee  :  special_case_entry ) -> (match (projectee) with
| {codepoint4 = codepoint; lower = lower; title = title; upper = upper} -> begin
title
end))


let __proj__Mkspecial_case_entry__item__upper : special_case_entry  ->  Prims.nat Prims.list = (fun ( projectee  :  special_case_entry ) -> (match (projectee) with
| {codepoint4 = codepoint; lower = lower; title = title; upper = upper} -> begin
upper
end))


let rec lookup_mapping : Prims.nat  ->  mapping_entry Prims.list  ->  Prims.nat FStar_Pervasives_Native.option = (fun ( cp  :  Prims.nat ) ( table  :  mapping_entry Prims.list ) -> (match (table) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (entry)::rest -> begin
(match ((Prims.op_Equality entry.codepoint cp)) with
| true -> begin
FStar_Pervasives_Native.Some (entry.mapped)
end
| uu___ -> begin
(lookup_mapping cp rest)
end)
end))


let rec lookup_ccc : Prims.nat  ->  ccc_entry Prims.list  ->  Prims.nat = (fun ( cp  :  Prims.nat ) ( table  :  ccc_entry Prims.list ) -> (match (table) with
| [] -> begin
(Prims.parse_int "0")
end
| (entry)::rest -> begin
(match ((Prims.op_Equality entry.codepoint2 cp)) with
| true -> begin
entry.ccc
end
| uu___ -> begin
(lookup_ccc cp rest)
end)
end))


let rec lookup_decomp : Prims.nat  ->  decomp_entry Prims.list  ->  Prims.nat Prims.list FStar_Pervasives_Native.option = (fun ( cp  :  Prims.nat ) ( table  :  decomp_entry Prims.list ) -> (match (table) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (entry)::rest -> begin
(match ((Prims.op_Equality entry.codepoint3 cp)) with
| true -> begin
FStar_Pervasives_Native.Some (entry.decomposition)
end
| uu___ -> begin
(lookup_decomp cp rest)
end)
end))


let rec lookup_composition : Prims.nat  ->  Prims.nat  ->  comp_entry Prims.list  ->  Prims.nat FStar_Pervasives_Native.option = (fun ( first  :  Prims.nat ) ( second  :  Prims.nat ) ( table  :  comp_entry Prims.list ) -> (match (table) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (entry)::rest -> begin
(match (((Prims.op_Equality entry.first first) && (Prims.op_Equality entry.second second))) with
| true -> begin
FStar_Pervasives_Native.Some (entry.composed)
end
| uu___ -> begin
(lookup_composition first second rest)
end)
end))


let rec is_excluded : Prims.nat  ->  Prims.nat Prims.list  ->  Prims.bool = (fun ( cp  :  Prims.nat ) ( exclusions  :  Prims.nat Prims.list ) -> (match (exclusions) with
| [] -> begin
false
end
| (x)::rest -> begin
((Prims.op_Equality x cp) || (is_excluded cp rest))
end))




