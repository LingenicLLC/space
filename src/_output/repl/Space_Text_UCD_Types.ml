open Prims
type mapping_entry = {
  codepoint: Prims.nat ;
  mapped: Prims.nat }
let __proj__Mkmapping_entry__item__codepoint (projectee : mapping_entry) :
  Prims.nat= match projectee with | { codepoint; mapped;_} -> codepoint
let __proj__Mkmapping_entry__item__mapped (projectee : mapping_entry) :
  Prims.nat= match projectee with | { codepoint; mapped;_} -> mapped
type multi_mapping_entry =
  {
  codepoint1: Prims.nat ;
  mapped_to: Prims.nat Prims.list }
let __proj__Mkmulti_mapping_entry__item__codepoint
  (projectee : multi_mapping_entry) : Prims.nat=
  match projectee with | { codepoint1 = codepoint; mapped_to;_} -> codepoint
let __proj__Mkmulti_mapping_entry__item__mapped_to
  (projectee : multi_mapping_entry) : Prims.nat Prims.list=
  match projectee with | { codepoint1 = codepoint; mapped_to;_} -> mapped_to
type ccc_entry = {
  codepoint2: Prims.nat ;
  ccc: Prims.nat }
let __proj__Mkccc_entry__item__codepoint (projectee : ccc_entry) : Prims.nat=
  match projectee with | { codepoint2 = codepoint; ccc;_} -> codepoint
let __proj__Mkccc_entry__item__ccc (projectee : ccc_entry) : Prims.nat=
  match projectee with | { codepoint2 = codepoint; ccc;_} -> ccc
type decomp_entry =
  {
  codepoint3: Prims.nat ;
  decomposition: Prims.nat Prims.list ;
  is_canonical: Prims.bool }
let __proj__Mkdecomp_entry__item__codepoint (projectee : decomp_entry) :
  Prims.nat=
  match projectee with
  | { codepoint3 = codepoint; decomposition; is_canonical;_} -> codepoint
let __proj__Mkdecomp_entry__item__decomposition (projectee : decomp_entry) :
  Prims.nat Prims.list=
  match projectee with
  | { codepoint3 = codepoint; decomposition; is_canonical;_} -> decomposition
let __proj__Mkdecomp_entry__item__is_canonical (projectee : decomp_entry) :
  Prims.bool=
  match projectee with
  | { codepoint3 = codepoint; decomposition; is_canonical;_} -> is_canonical
type comp_entry = {
  first: Prims.nat ;
  second: Prims.nat ;
  composed: Prims.nat }
let __proj__Mkcomp_entry__item__first (projectee : comp_entry) : Prims.nat=
  match projectee with | { first; second; composed;_} -> first
let __proj__Mkcomp_entry__item__second (projectee : comp_entry) : Prims.nat=
  match projectee with | { first; second; composed;_} -> second
let __proj__Mkcomp_entry__item__composed (projectee : comp_entry) :
  Prims.nat= match projectee with | { first; second; composed;_} -> composed
type special_case_entry =
  {
  codepoint4: Prims.nat ;
  lower: Prims.nat Prims.list ;
  title: Prims.nat Prims.list ;
  upper: Prims.nat Prims.list }
let __proj__Mkspecial_case_entry__item__codepoint
  (projectee : special_case_entry) : Prims.nat=
  match projectee with
  | { codepoint4 = codepoint; lower; title; upper;_} -> codepoint
let __proj__Mkspecial_case_entry__item__lower
  (projectee : special_case_entry) : Prims.nat Prims.list=
  match projectee with
  | { codepoint4 = codepoint; lower; title; upper;_} -> lower
let __proj__Mkspecial_case_entry__item__title
  (projectee : special_case_entry) : Prims.nat Prims.list=
  match projectee with
  | { codepoint4 = codepoint; lower; title; upper;_} -> title
let __proj__Mkspecial_case_entry__item__upper
  (projectee : special_case_entry) : Prims.nat Prims.list=
  match projectee with
  | { codepoint4 = codepoint; lower; title; upper;_} -> upper
let rec lookup_mapping (cp : Prims.nat) (table : mapping_entry Prims.list) :
  Prims.nat FStar_Pervasives_Native.option=
  match table with
  | [] -> FStar_Pervasives_Native.None
  | entry::rest ->
      if entry.codepoint = cp
      then FStar_Pervasives_Native.Some (entry.mapped)
      else lookup_mapping cp rest
let rec lookup_ccc (cp : Prims.nat) (table : ccc_entry Prims.list) :
  Prims.nat=
  match table with
  | [] -> Prims.int_zero
  | entry::rest ->
      if entry.codepoint2 = cp then entry.ccc else lookup_ccc cp rest
let rec lookup_decomp (cp : Prims.nat) (table : decomp_entry Prims.list) :
  Prims.nat Prims.list FStar_Pervasives_Native.option=
  match table with
  | [] -> FStar_Pervasives_Native.None
  | entry::rest ->
      if entry.codepoint3 = cp
      then FStar_Pervasives_Native.Some (entry.decomposition)
      else lookup_decomp cp rest
let rec lookup_composition (first : Prims.nat) (second : Prims.nat)
  (table : comp_entry Prims.list) : Prims.nat FStar_Pervasives_Native.option=
  match table with
  | [] -> FStar_Pervasives_Native.None
  | entry::rest ->
      if (entry.first = first) && (entry.second = second)
      then FStar_Pervasives_Native.Some (entry.composed)
      else lookup_composition first second rest
let rec is_excluded (cp : Prims.nat) (exclusions : Prims.nat Prims.list) :
  Prims.bool=
  match exclusions with
  | [] -> false
  | x::rest -> (x = cp) || (is_excluded cp rest)
