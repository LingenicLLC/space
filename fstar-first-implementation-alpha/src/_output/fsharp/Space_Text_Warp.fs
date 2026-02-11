#light "off"
module Space_Text_Warp
type text_direction =
| Forward
| Backward


let uu___is_Forward : text_direction  ->  Prims.bool = (fun ( projectee  :  text_direction ) -> (match (projectee) with
| Forward -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_Backward : text_direction  ->  Prims.bool = (fun ( projectee  :  text_direction ) -> (match (projectee) with
| Backward -> begin
true
end
| uu___ -> begin
false
end))

type text_warp =
{iter : Space_Text_Iter.text_iter; direction : text_direction; start_pos : Prims.nat; end_pos : Prims.nat}


let __proj__Mktext_warp__item__iter : text_warp  ->  Space_Text_Iter.text_iter = (fun ( projectee  :  text_warp ) -> (match (projectee) with
| {iter = iter; direction = direction; start_pos = start_pos; end_pos = end_pos} -> begin
iter
end))


let __proj__Mktext_warp__item__direction : text_warp  ->  text_direction = (fun ( projectee  :  text_warp ) -> (match (projectee) with
| {iter = iter; direction = direction; start_pos = start_pos; end_pos = end_pos} -> begin
direction
end))


let __proj__Mktext_warp__item__start_pos : text_warp  ->  Prims.nat = (fun ( projectee  :  text_warp ) -> (match (projectee) with
| {iter = iter; direction = direction; start_pos = start_pos; end_pos = end_pos} -> begin
start_pos
end))


let __proj__Mktext_warp__item__end_pos : text_warp  ->  Prims.nat = (fun ( projectee  :  text_warp ) -> (match (projectee) with
| {iter = iter; direction = direction; start_pos = start_pos; end_pos = end_pos} -> begin
end_pos
end))


let rec advance_iter_to : Space_Text_Iter.text_iter  ->  Prims.nat  ->  Prims.nat  ->  Space_Text_Iter.text_iter FStar_Pervasives_Native.option = (fun ( it  :  Space_Text_Iter.text_iter ) ( target  :  Prims.nat ) ( fuel  :  Prims.nat ) -> (match ((Prims.op_Equality fuel (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match (((Space_Text_Iter.iter_position it) >= target)) with
| true -> begin
FStar_Pervasives_Native.Some (it)
end
| uu___1 -> begin
(match ((Space_Text_Iter.iter_next it)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (uu___2, it') -> begin
(advance_iter_to it' target (fuel - (Prims.parse_int "1")))
end)
end)
end))


let text_warp_begin : Space_Text_Types.text  ->  text_warp = (fun ( t  :  Space_Text_Types.text ) -> {iter = (Space_Text_Iter.iter_begin t); direction = Forward; start_pos = (Prims.parse_int "0"); end_pos = t.header.grapheme_count})


let text_warp_begin_backward : Space_Text_Types.text  ->  text_warp FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) -> (match ((Prims.op_Equality t.header.grapheme_count (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.Some ({iter = (Space_Text_Iter.iter_begin t); direction = Backward; start_pos = (Prims.parse_int "0"); end_pos = (Prims.parse_int "0")})
end
| uu___ -> begin
(

let it = (Space_Text_Iter.iter_begin t)
in (

let target = (t.header.grapheme_count - (Prims.parse_int "1"))
in (match ((advance_iter_to it target (target + (Prims.parse_int "1")))) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (it') -> begin
FStar_Pervasives_Native.Some ({iter = it'; direction = Backward; start_pos = (Prims.parse_int "0"); end_pos = t.header.grapheme_count})
end)))
end))


let text_warp_range : Space_Text_Types.text  ->  Prims.nat  ->  Prims.nat  ->  text_warp FStar_Pervasives_Native.option = (fun ( t  :  Space_Text_Types.text ) ( start  :  Prims.nat ) ( finish  :  Prims.nat ) -> (match ((finish < start)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((finish > t.header.grapheme_count)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
(

let it = (Space_Text_Iter.iter_begin t)
in (match ((advance_iter_to it start (start + (Prims.parse_int "1")))) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (it') -> begin
FStar_Pervasives_Native.Some ({iter = it'; direction = Forward; start_pos = start; end_pos = finish})
end))
end)
end))


let text_warp_done : text_warp  ->  Prims.bool = (fun ( w  :  text_warp ) -> (match (w.direction) with
| Forward -> begin
((Space_Text_Iter.iter_position w.iter) >= w.end_pos)
end
| Backward -> begin
((Space_Text_Iter.iter_position w.iter) <= w.start_pos)
end))


let text_warp_position : text_warp  ->  Prims.nat = (fun ( w  :  text_warp ) -> (Space_Text_Iter.iter_position w.iter))


let iter_prev : Space_Text_Iter.text_iter  ->  Space_Text_Iter.text_iter FStar_Pervasives_Native.option = (fun ( it  :  Space_Text_Iter.text_iter ) -> (

let pos = (Space_Text_Iter.iter_position it)
in (match ((Prims.op_Equality pos (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(

let t = it.text
in (

let target = (pos - (Prims.parse_int "1"))
in (

let fresh = (Space_Text_Iter.iter_begin t)
in (advance_iter_to fresh target (target + (Prims.parse_int "1"))))))
end)))


let text_warp_next : text_warp  ->  (Space_Text_Types.grapheme * text_warp) FStar_Pervasives_Native.option = (fun ( w  :  text_warp ) -> (match ((text_warp_done w)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match (w.direction) with
| Forward -> begin
(match ((Space_Text_Iter.iter_next w.iter)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (g, it') -> begin
FStar_Pervasives_Native.Some (((g), ({iter = it'; direction = w.direction; start_pos = w.start_pos; end_pos = w.end_pos})))
end)
end
| Backward -> begin
(match ((Space_Text_Iter.iter_next w.iter)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (g, uu___1) -> begin
(match ((iter_prev w.iter)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.Some (((g), ({iter = w.iter; direction = w.direction; start_pos = w.start_pos; end_pos = w.end_pos})))
end
| FStar_Pervasives_Native.Some (it') -> begin
FStar_Pervasives_Native.Some (((g), ({iter = it'; direction = w.direction; start_pos = w.start_pos; end_pos = w.end_pos})))
end)
end)
end)
end))


let text_warp_remaining : text_warp  ->  Prims.nat = (fun ( w  :  text_warp ) -> (match ((text_warp_done w)) with
| true -> begin
(Prims.parse_int "0")
end
| uu___ -> begin
(match (w.direction) with
| Forward -> begin
(w.end_pos - (Space_Text_Iter.iter_position w.iter))
end
| Backward -> begin
((Space_Text_Iter.iter_position w.iter) - w.start_pos)
end)
end))


let rec text_warp_collect : text_warp  ->  Prims.nat  ->  Space_Text_Types.grapheme Prims.list = (fun ( w  :  text_warp ) ( fuel  :  Prims.nat ) -> (match ((Prims.op_Equality fuel (Prims.parse_int "0"))) with
| true -> begin
[]
end
| uu___ -> begin
(match ((text_warp_done w)) with
| true -> begin
[]
end
| uu___1 -> begin
(match ((text_warp_next w)) with
| FStar_Pervasives_Native.None -> begin
[]
end
| FStar_Pervasives_Native.Some (g, w') -> begin
(g)::(text_warp_collect w' (fuel - (Prims.parse_int "1")))
end)
end)
end))


let grapheme_to_cell : Space_Text_Types.grapheme  ->  Space_Types.cell = (fun ( g  :  Space_Text_Types.grapheme ) -> (match (g.bytes) with
| [] -> begin
(FStar_UInt64.uint_to_t (Prims.parse_int "0"))
end
| (b)::uu___ -> begin
(FStar_UInt64.uint_to_t (FStar_UInt8.v b))
end))


let text_warp_current : text_warp  ->  Space_Text_Types.grapheme FStar_Pervasives_Native.option = (fun ( w  :  text_warp ) -> (match ((text_warp_done w)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((Space_Text_Iter.iter_next w.iter)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (g, uu___1) -> begin
FStar_Pervasives_Native.Some (g)
end)
end))


let rec advance_to_position : Space_Text_Iter.text_iter  ->  Prims.nat  ->  Prims.nat  ->  Space_Text_Iter.text_iter FStar_Pervasives_Native.option = (fun ( it  :  Space_Text_Iter.text_iter ) ( target  :  Prims.nat ) ( fuel  :  Prims.nat ) -> (match ((Prims.op_Equality fuel (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match (((Space_Text_Iter.iter_position it) >= target)) with
| true -> begin
FStar_Pervasives_Native.Some (it)
end
| uu___1 -> begin
(match ((Space_Text_Iter.iter_next it)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (uu___2, it') -> begin
(advance_to_position it' target (fuel - (Prims.parse_int "1")))
end)
end)
end))


let text_warp_goto : text_warp  ->  Prims.nat  ->  text_warp FStar_Pervasives_Native.option = (fun ( w  :  text_warp ) ( pos  :  Prims.nat ) -> (match ((pos < w.start_pos)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((pos > w.end_pos)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
(

let t = w.iter.text
in (

let fresh_iter = (Space_Text_Iter.iter_begin t)
in (match ((advance_to_position fresh_iter pos (t.header.grapheme_count + (Prims.parse_int "1")))) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (it') -> begin
FStar_Pervasives_Native.Some ({iter = it'; direction = w.direction; start_pos = w.start_pos; end_pos = w.end_pos})
end)))
end)
end))


let text_warp_has_grapheme : text_warp  ->  Prims.bool = (fun ( w  :  text_warp ) -> (not ((text_warp_done w))))




