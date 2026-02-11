#light "off"
module Space_Text_Grapheme
type grapheme_break_property =
| GBP_Other
| GBP_CR
| GBP_LF
| GBP_Control
| GBP_Extend
| GBP_ZWJ
| GBP_Regional_Indicator
| GBP_Prepend
| GBP_SpacingMark
| GBP_L
| GBP_V
| GBP_T
| GBP_LV
| GBP_LVT
| GBP_Extended_Pictographic


let uu___is_GBP_Other : grapheme_break_property  ->  Prims.bool = (fun ( projectee  :  grapheme_break_property ) -> (match (projectee) with
| GBP_Other -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_GBP_CR : grapheme_break_property  ->  Prims.bool = (fun ( projectee  :  grapheme_break_property ) -> (match (projectee) with
| GBP_CR -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_GBP_LF : grapheme_break_property  ->  Prims.bool = (fun ( projectee  :  grapheme_break_property ) -> (match (projectee) with
| GBP_LF -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_GBP_Control : grapheme_break_property  ->  Prims.bool = (fun ( projectee  :  grapheme_break_property ) -> (match (projectee) with
| GBP_Control -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_GBP_Extend : grapheme_break_property  ->  Prims.bool = (fun ( projectee  :  grapheme_break_property ) -> (match (projectee) with
| GBP_Extend -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_GBP_ZWJ : grapheme_break_property  ->  Prims.bool = (fun ( projectee  :  grapheme_break_property ) -> (match (projectee) with
| GBP_ZWJ -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_GBP_Regional_Indicator : grapheme_break_property  ->  Prims.bool = (fun ( projectee  :  grapheme_break_property ) -> (match (projectee) with
| GBP_Regional_Indicator -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_GBP_Prepend : grapheme_break_property  ->  Prims.bool = (fun ( projectee  :  grapheme_break_property ) -> (match (projectee) with
| GBP_Prepend -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_GBP_SpacingMark : grapheme_break_property  ->  Prims.bool = (fun ( projectee  :  grapheme_break_property ) -> (match (projectee) with
| GBP_SpacingMark -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_GBP_L : grapheme_break_property  ->  Prims.bool = (fun ( projectee  :  grapheme_break_property ) -> (match (projectee) with
| GBP_L -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_GBP_V : grapheme_break_property  ->  Prims.bool = (fun ( projectee  :  grapheme_break_property ) -> (match (projectee) with
| GBP_V -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_GBP_T : grapheme_break_property  ->  Prims.bool = (fun ( projectee  :  grapheme_break_property ) -> (match (projectee) with
| GBP_T -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_GBP_LV : grapheme_break_property  ->  Prims.bool = (fun ( projectee  :  grapheme_break_property ) -> (match (projectee) with
| GBP_LV -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_GBP_LVT : grapheme_break_property  ->  Prims.bool = (fun ( projectee  :  grapheme_break_property ) -> (match (projectee) with
| GBP_LVT -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_GBP_Extended_Pictographic : grapheme_break_property  ->  Prims.bool = (fun ( projectee  :  grapheme_break_property ) -> (match (projectee) with
| GBP_Extended_Pictographic -> begin
true
end
| uu___ -> begin
false
end))

type break_state =
{prev_gbp : grapheme_break_property; in_emoji_seq : Prims.bool; ri_count : Prims.nat}


let __proj__Mkbreak_state__item__prev_gbp : break_state  ->  grapheme_break_property = (fun ( projectee  :  break_state ) -> (match (projectee) with
| {prev_gbp = prev_gbp; in_emoji_seq = in_emoji_seq; ri_count = ri_count} -> begin
prev_gbp
end))


let __proj__Mkbreak_state__item__in_emoji_seq : break_state  ->  Prims.bool = (fun ( projectee  :  break_state ) -> (match (projectee) with
| {prev_gbp = prev_gbp; in_emoji_seq = in_emoji_seq; ri_count = ri_count} -> begin
in_emoji_seq
end))


let __proj__Mkbreak_state__item__ri_count : break_state  ->  Prims.nat = (fun ( projectee  :  break_state ) -> (match (projectee) with
| {prev_gbp = prev_gbp; in_emoji_seq = in_emoji_seq; ri_count = ri_count} -> begin
ri_count
end))


let initial_state : break_state = {prev_gbp = GBP_Other; in_emoji_seq = false; ri_count = (Prims.parse_int "0")}


let rec has_ccc_aux : Space_Text_UCD_Types.ccc_entry Prims.list  ->  Prims.nat  ->  Prims.bool = (fun ( table  :  Space_Text_UCD_Types.ccc_entry Prims.list ) ( cp  :  Prims.nat ) -> (match (table) with
| [] -> begin
false
end
| (e)::rest -> begin
(match ((Prims.op_Equality e.codepoint2 cp)) with
| true -> begin
(e.ccc > (Prims.parse_int "0"))
end
| uu___ -> begin
(has_ccc_aux rest cp)
end)
end))


let has_combining_class : Prims.nat  ->  Prims.bool = (fun ( cp  :  Prims.nat ) -> (has_ccc_aux Space_Text_UCD_CCC.combining_class_table cp))


let is_extend : Prims.nat  ->  Prims.bool = (fun ( cp  :  Prims.nat ) -> ((((((((((((((((((((((((((cp >= (Prims.parse_int "0x0300")) && (cp <= (Prims.parse_int "0x036F"))) || ((cp >= (Prims.parse_int "0x1AB0")) && (cp <= (Prims.parse_int "0x1AFF")))) || ((cp >= (Prims.parse_int "0x1DC0")) && (cp <= (Prims.parse_int "0x1DFF")))) || ((cp >= (Prims.parse_int "0x20D0")) && (cp <= (Prims.parse_int "0x20FF")))) || ((cp >= (Prims.parse_int "0xFE20")) && (cp <= (Prims.parse_int "0xFE2F")))) || ((cp >= (Prims.parse_int "0x064B")) && (cp <= (Prims.parse_int "0x065F")))) || ((cp >= (Prims.parse_int "0x0591")) && (cp <= (Prims.parse_int "0x05BD")))) || (Prims.op_Equality cp (Prims.parse_int "0x05BF"))) || ((cp >= (Prims.parse_int "0x05C1")) && (cp <= (Prims.parse_int "0x05C2")))) || (Prims.op_Equality cp (Prims.parse_int "0x05C4"))) || (Prims.op_Equality cp (Prims.parse_int "0x05C5"))) || (Prims.op_Equality cp (Prims.parse_int "0x05C7"))) || ((cp >= (Prims.parse_int "0x0901")) && (cp <= (Prims.parse_int "0x0903")))) || ((cp >= (Prims.parse_int "0x093A")) && (cp <= (Prims.parse_int "0x094F")))) || ((cp >= (Prims.parse_int "0x0951")) && (cp <= (Prims.parse_int "0x0957")))) || ((cp >= (Prims.parse_int "0x0962")) && (cp <= (Prims.parse_int "0x0963")))) || ((cp >= (Prims.parse_int "0x0E31")) && (cp <= (Prims.parse_int "0x0E3A")))) || ((cp >= (Prims.parse_int "0x0E47")) && (cp <= (Prims.parse_int "0x0E4E")))) || ((cp >= (Prims.parse_int "0xFE00")) && (cp <= (Prims.parse_int "0xFE0F")))) || ((cp >= (Prims.parse_int "0xE0100")) && (cp <= (Prims.parse_int "0xE01EF")))) || ((cp >= (Prims.parse_int "0x1F3FB")) && (cp <= (Prims.parse_int "0x1F3FF")))) || (Prims.op_Equality cp (Prims.parse_int "0x034F"))) || (Prims.op_Equality cp (Prims.parse_int "0x200C"))) || (Prims.op_Equality cp (Prims.parse_int "0x200D"))) || (has_combining_class cp)))


let is_extended_pictographic : Prims.nat  ->  Prims.bool = (fun ( cp  :  Prims.nat ) -> ((((((((((((((((((((((((cp >= (Prims.parse_int "0x1F300")) && (cp <= (Prims.parse_int "0x1F5FF"))) || ((cp >= (Prims.parse_int "0x1F600")) && (cp <= (Prims.parse_int "0x1F64F")))) || ((cp >= (Prims.parse_int "0x1F680")) && (cp <= (Prims.parse_int "0x1F6FF")))) || ((cp >= (Prims.parse_int "0x1F900")) && (cp <= (Prims.parse_int "0x1F9FF")))) || ((cp >= (Prims.parse_int "0x1FA00")) && (cp <= (Prims.parse_int "0x1FA6F")))) || ((cp >= (Prims.parse_int "0x1FA70")) && (cp <= (Prims.parse_int "0x1FAFF")))) || ((cp >= (Prims.parse_int "0x2700")) && (cp <= (Prims.parse_int "0x27BF")))) || ((cp >= (Prims.parse_int "0x2600")) && (cp <= (Prims.parse_int "0x26FF")))) || (Prims.op_Equality cp (Prims.parse_int "0x2764"))) || (Prims.op_Equality cp (Prims.parse_int "0x2763"))) || (Prims.op_Equality cp (Prims.parse_int "0x2665"))) || (Prims.op_Equality cp (Prims.parse_int "0x2666"))) || (Prims.op_Equality cp (Prims.parse_int "0x2660"))) || (Prims.op_Equality cp (Prims.parse_int "0x2663"))) || (Prims.op_Equality cp (Prims.parse_int "0x2615"))) || (Prims.op_Equality cp (Prims.parse_int "0x231A"))) || (Prims.op_Equality cp (Prims.parse_int "0x231B"))) || (Prims.op_Equality cp (Prims.parse_int "0x23E9"))) || (Prims.op_Equality cp (Prims.parse_int "0x23EA"))) || (Prims.op_Equality cp (Prims.parse_int "0x23F0"))) || (Prims.op_Equality cp (Prims.parse_int "0x23F3"))) || (Prims.op_Equality cp (Prims.parse_int "0x2328"))) || (Prims.op_Equality cp (Prims.parse_int "0x260E"))))


let is_spacing_mark : Prims.nat  ->  Prims.bool = (fun ( cp  :  Prims.nat ) -> (((((((cp >= (Prims.parse_int "0x0E40")) && (cp <= (Prims.parse_int "0x0E44"))) || ((cp >= (Prims.parse_int "0x0EC0")) && (cp <= (Prims.parse_int "0x0EC4")))) || ((cp >= (Prims.parse_int "0x0F3E")) && (cp <= (Prims.parse_int "0x0F3F")))) || ((cp >= (Prims.parse_int "0x1031")) && (cp <= (Prims.parse_int "0x1031")))) || ((cp >= (Prims.parse_int "0x09BE")) && (cp <= (Prims.parse_int "0x09C4")))) || ((cp >= (Prims.parse_int "0x093E")) && (cp <= (Prims.parse_int "0x0940")))))


let gbp_from_codepoint : Prims.nat  ->  grapheme_break_property = (fun ( cp  :  Prims.nat ) -> (match ((Prims.op_Equality cp (Prims.parse_int "0x0D"))) with
| true -> begin
GBP_CR
end
| uu___ -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x0A"))) with
| true -> begin
GBP_LF
end
| uu___1 -> begin
(match ((cp < (Prims.parse_int "0x20"))) with
| true -> begin
GBP_Control
end
| uu___2 -> begin
(match (((cp >= (Prims.parse_int "0x7F")) && (cp <= (Prims.parse_int "0x9F")))) with
| true -> begin
GBP_Control
end
| uu___3 -> begin
(match (((Prims.op_Equality cp (Prims.parse_int "0x2028")) || (Prims.op_Equality cp (Prims.parse_int "0x2029")))) with
| true -> begin
GBP_Control
end
| uu___4 -> begin
(match ((Prims.op_Equality cp (Prims.parse_int "0x200D"))) with
| true -> begin
GBP_ZWJ
end
| uu___5 -> begin
(match (((cp >= (Prims.parse_int "0x1F1E6")) && (cp <= (Prims.parse_int "0x1F1FF")))) with
| true -> begin
GBP_Regional_Indicator
end
| uu___6 -> begin
(match (((cp >= (Prims.parse_int "0x1100")) && (cp <= (Prims.parse_int "0x115F")))) with
| true -> begin
GBP_L
end
| uu___7 -> begin
(match (((cp >= (Prims.parse_int "0xA960")) && (cp <= (Prims.parse_int "0xA97C")))) with
| true -> begin
GBP_L
end
| uu___8 -> begin
(match (((cp >= (Prims.parse_int "0x1160")) && (cp <= (Prims.parse_int "0x11A7")))) with
| true -> begin
GBP_V
end
| uu___9 -> begin
(match (((cp >= (Prims.parse_int "0xD7B0")) && (cp <= (Prims.parse_int "0xD7C6")))) with
| true -> begin
GBP_V
end
| uu___10 -> begin
(match (((cp >= (Prims.parse_int "0x11A8")) && (cp <= (Prims.parse_int "0x11FF")))) with
| true -> begin
GBP_T
end
| uu___11 -> begin
(match (((cp >= (Prims.parse_int "0xD7CB")) && (cp <= (Prims.parse_int "0xD7FB")))) with
| true -> begin
GBP_T
end
| uu___12 -> begin
(match (((cp >= (Prims.parse_int "0xAC00")) && (cp <= (Prims.parse_int "0xD7A3")))) with
| true -> begin
(

let idx = (cp - (Prims.parse_int "0xAC00"))
in (match ((Prims.op_Equality (Prims.mod_f idx (Prims.parse_int "28")) (Prims.parse_int "0"))) with
| true -> begin
GBP_LV
end
| uu___13 -> begin
GBP_LVT
end))
end
| uu___13 -> begin
(match ((is_extended_pictographic cp)) with
| true -> begin
GBP_Extended_Pictographic
end
| uu___14 -> begin
(match ((is_spacing_mark cp)) with
| true -> begin
GBP_SpacingMark
end
| uu___15 -> begin
(match ((is_extend cp)) with
| true -> begin
GBP_Extend
end
| uu___16 -> begin
(match (((((cp >= (Prims.parse_int "0x0600")) && (cp <= (Prims.parse_int "0x0605"))) || (Prims.op_Equality cp (Prims.parse_int "0x06DD"))) || (Prims.op_Equality cp (Prims.parse_int "0x070F")))) with
| true -> begin
GBP_Prepend
end
| uu___17 -> begin
(match ((((Prims.op_Equality cp (Prims.parse_int "0x0890")) || (Prims.op_Equality cp (Prims.parse_int "0x0891"))) || (Prims.op_Equality cp (Prims.parse_int "0x08E2")))) with
| true -> begin
GBP_Prepend
end
| uu___18 -> begin
(match (((Prims.op_Equality cp (Prims.parse_int "0x110BD")) || (Prims.op_Equality cp (Prims.parse_int "0x110CD")))) with
| true -> begin
GBP_Prepend
end
| uu___19 -> begin
GBP_Other
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


let should_break : grapheme_break_property  ->  grapheme_break_property  ->  break_state  ->  (Prims.bool * break_state) = (fun ( left  :  grapheme_break_property ) ( right  :  grapheme_break_property ) ( state  :  break_state ) -> (match (((Prims.op_Equality left GBP_CR) && (Prims.op_Equality right GBP_LF))) with
| true -> begin
((false), ({prev_gbp = right; in_emoji_seq = state.in_emoji_seq; ri_count = state.ri_count}))
end
| uu___ -> begin
(match ((((Prims.op_Equality left GBP_Control) || (Prims.op_Equality left GBP_CR)) || (Prims.op_Equality left GBP_LF))) with
| true -> begin
((true), ({prev_gbp = right; in_emoji_seq = false; ri_count = (Prims.parse_int "0")}))
end
| uu___1 -> begin
(match ((((Prims.op_Equality right GBP_Control) || (Prims.op_Equality right GBP_CR)) || (Prims.op_Equality right GBP_LF))) with
| true -> begin
((true), ({prev_gbp = right; in_emoji_seq = false; ri_count = (Prims.parse_int "0")}))
end
| uu___2 -> begin
(match (((Prims.op_Equality left GBP_L) && ((((Prims.op_Equality right GBP_L) || (Prims.op_Equality right GBP_V)) || (Prims.op_Equality right GBP_LV)) || (Prims.op_Equality right GBP_LVT)))) with
| true -> begin
((false), ({prev_gbp = right; in_emoji_seq = state.in_emoji_seq; ri_count = state.ri_count}))
end
| uu___3 -> begin
(match ((((Prims.op_Equality left GBP_LV) || (Prims.op_Equality left GBP_V)) && ((Prims.op_Equality right GBP_V) || (Prims.op_Equality right GBP_T)))) with
| true -> begin
((false), ({prev_gbp = right; in_emoji_seq = state.in_emoji_seq; ri_count = state.ri_count}))
end
| uu___4 -> begin
(match ((((Prims.op_Equality left GBP_LVT) || (Prims.op_Equality left GBP_T)) && (Prims.op_Equality right GBP_T))) with
| true -> begin
((false), ({prev_gbp = right; in_emoji_seq = state.in_emoji_seq; ri_count = state.ri_count}))
end
| uu___5 -> begin
(match (((Prims.op_Equality right GBP_Extend) || (Prims.op_Equality right GBP_ZWJ))) with
| true -> begin
((false), ({prev_gbp = right; in_emoji_seq = (state.in_emoji_seq || (Prims.op_Equality left GBP_Extended_Pictographic)); ri_count = state.ri_count}))
end
| uu___6 -> begin
(match ((Prims.op_Equality right GBP_SpacingMark)) with
| true -> begin
((false), ({prev_gbp = right; in_emoji_seq = state.in_emoji_seq; ri_count = state.ri_count}))
end
| uu___7 -> begin
(match ((Prims.op_Equality left GBP_Prepend)) with
| true -> begin
((false), ({prev_gbp = right; in_emoji_seq = state.in_emoji_seq; ri_count = state.ri_count}))
end
| uu___8 -> begin
(match (((state.in_emoji_seq && (Prims.op_Equality left GBP_ZWJ)) && (Prims.op_Equality right GBP_Extended_Pictographic))) with
| true -> begin
((false), ({prev_gbp = right; in_emoji_seq = true; ri_count = state.ri_count}))
end
| uu___9 -> begin
(match (((Prims.op_Equality left GBP_Regional_Indicator) && (Prims.op_Equality right GBP_Regional_Indicator))) with
| true -> begin
(

let new_count = (state.ri_count + (Prims.parse_int "1"))
in (match ((Prims.op_Equality (Prims.mod_f new_count (Prims.parse_int "2")) (Prims.parse_int "0"))) with
| true -> begin
((true), ({prev_gbp = right; in_emoji_seq = state.in_emoji_seq; ri_count = (Prims.parse_int "0")}))
end
| uu___10 -> begin
((false), ({prev_gbp = right; in_emoji_seq = state.in_emoji_seq; ri_count = new_count}))
end))
end
| uu___10 -> begin
((true), ({prev_gbp = right; in_emoji_seq = (Prims.op_Equality right GBP_Extended_Pictographic); ri_count = (Prims.parse_int "0")}))
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




