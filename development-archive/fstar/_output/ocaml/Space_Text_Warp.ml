open Prims
type text_direction =
  | Forward 
  | Backward 
let uu___is_Forward (projectee : text_direction) : Prims.bool=
  match projectee with | Forward -> true | uu___ -> false
let uu___is_Backward (projectee : text_direction) : Prims.bool=
  match projectee with | Backward -> true | uu___ -> false
type text_warp =
  {
  iter: Space_Text_Iter.text_iter ;
  direction: text_direction ;
  start_pos: Prims.nat ;
  end_pos: Prims.nat }
let __proj__Mktext_warp__item__iter (projectee : text_warp) :
  Space_Text_Iter.text_iter=
  match projectee with | { iter; direction; start_pos; end_pos;_} -> iter
let __proj__Mktext_warp__item__direction (projectee : text_warp) :
  text_direction=
  match projectee with
  | { iter; direction; start_pos; end_pos;_} -> direction
let __proj__Mktext_warp__item__start_pos (projectee : text_warp) : Prims.nat=
  match projectee with
  | { iter; direction; start_pos; end_pos;_} -> start_pos
let __proj__Mktext_warp__item__end_pos (projectee : text_warp) : Prims.nat=
  match projectee with | { iter; direction; start_pos; end_pos;_} -> end_pos
let rec advance_iter_to (it : Space_Text_Iter.text_iter) (target : Prims.nat)
  (fuel : Prims.nat) :
  Space_Text_Iter.text_iter FStar_Pervasives_Native.option=
  if fuel = Prims.int_zero
  then FStar_Pervasives_Native.None
  else
    if (Space_Text_Iter.iter_position it) >= target
    then FStar_Pervasives_Native.Some it
    else
      (match Space_Text_Iter.iter_next it with
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
       | FStar_Pervasives_Native.Some (uu___2, it') ->
           advance_iter_to it' target (fuel - Prims.int_one))
let text_warp_begin (t : Space_Text_Types.text) : text_warp=
  {
    iter = (Space_Text_Iter.iter_begin t);
    direction = Forward;
    start_pos = Prims.int_zero;
    end_pos = ((t.Space_Text_Types.header).Space_Text_Types.grapheme_count)
  }
let text_warp_begin_backward (t : Space_Text_Types.text) :
  text_warp FStar_Pervasives_Native.option=
  if
    (t.Space_Text_Types.header).Space_Text_Types.grapheme_count =
      Prims.int_zero
  then
    FStar_Pervasives_Native.Some
      {
        iter = (Space_Text_Iter.iter_begin t);
        direction = Backward;
        start_pos = Prims.int_zero;
        end_pos = Prims.int_zero
      }
  else
    (let it = Space_Text_Iter.iter_begin t in
     let target =
       (t.Space_Text_Types.header).Space_Text_Types.grapheme_count -
         Prims.int_one in
     match advance_iter_to it target (target + Prims.int_one) with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some it' ->
         FStar_Pervasives_Native.Some
           {
             iter = it';
             direction = Backward;
             start_pos = Prims.int_zero;
             end_pos =
               ((t.Space_Text_Types.header).Space_Text_Types.grapheme_count)
           })
let text_warp_range (t : Space_Text_Types.text) (start : Prims.nat)
  (finish : Prims.nat) : text_warp FStar_Pervasives_Native.option=
  if finish < start
  then FStar_Pervasives_Native.None
  else
    if finish > (t.Space_Text_Types.header).Space_Text_Types.grapheme_count
    then FStar_Pervasives_Native.None
    else
      (let it = Space_Text_Iter.iter_begin t in
       match advance_iter_to it start (start + Prims.int_one) with
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
       | FStar_Pervasives_Native.Some it' ->
           FStar_Pervasives_Native.Some
             {
               iter = it';
               direction = Forward;
               start_pos = start;
               end_pos = finish
             })
let text_warp_done (w : text_warp) : Prims.bool=
  match w.direction with
  | Forward -> (Space_Text_Iter.iter_position w.iter) >= w.end_pos
  | Backward -> (Space_Text_Iter.iter_position w.iter) <= w.start_pos
let text_warp_position (w : text_warp) : Prims.nat=
  Space_Text_Iter.iter_position w.iter
let iter_prev (it : Space_Text_Iter.text_iter) :
  Space_Text_Iter.text_iter FStar_Pervasives_Native.option=
  let pos = Space_Text_Iter.iter_position it in
  if pos = Prims.int_zero
  then FStar_Pervasives_Native.None
  else
    (let t = it.Space_Text_Iter.text in
     let target = pos - Prims.int_one in
     let fresh = Space_Text_Iter.iter_begin t in
     advance_iter_to fresh target (target + Prims.int_one))
let text_warp_next (w : text_warp) :
  (Space_Text_Types.grapheme * text_warp) FStar_Pervasives_Native.option=
  if text_warp_done w
  then FStar_Pervasives_Native.None
  else
    (match w.direction with
     | Forward ->
         (match Space_Text_Iter.iter_next w.iter with
          | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
          | FStar_Pervasives_Native.Some (g, it') ->
              FStar_Pervasives_Native.Some
                (g,
                  {
                    iter = it';
                    direction = (w.direction);
                    start_pos = (w.start_pos);
                    end_pos = (w.end_pos)
                  }))
     | Backward ->
         (match Space_Text_Iter.iter_next w.iter with
          | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
          | FStar_Pervasives_Native.Some (g, uu___1) ->
              (match iter_prev w.iter with
               | FStar_Pervasives_Native.None ->
                   FStar_Pervasives_Native.Some
                     (g,
                       {
                         iter = (w.iter);
                         direction = (w.direction);
                         start_pos = (w.start_pos);
                         end_pos = (w.end_pos)
                       })
               | FStar_Pervasives_Native.Some it' ->
                   FStar_Pervasives_Native.Some
                     (g,
                       {
                         iter = it';
                         direction = (w.direction);
                         start_pos = (w.start_pos);
                         end_pos = (w.end_pos)
                       }))))
let text_warp_remaining (w : text_warp) : Prims.nat=
  if text_warp_done w
  then Prims.int_zero
  else
    (match w.direction with
     | Forward -> w.end_pos - (Space_Text_Iter.iter_position w.iter)
     | Backward -> (Space_Text_Iter.iter_position w.iter) - w.start_pos)
let rec text_warp_collect (w : text_warp) (fuel : Prims.nat) :
  Space_Text_Types.grapheme Prims.list=
  if fuel = Prims.int_zero
  then []
  else
    if text_warp_done w
    then []
    else
      (match text_warp_next w with
       | FStar_Pervasives_Native.None -> []
       | FStar_Pervasives_Native.Some (g, w') -> g ::
           (text_warp_collect w' (fuel - Prims.int_one)))
let grapheme_to_cell (g : Space_Text_Types.grapheme) : Space_Types.cell=
  match g.Space_Text_Types.bytes with
  | [] -> FStar_UInt64.uint_to_t Prims.int_zero
  | b::uu___ -> FStar_UInt64.uint_to_t (FStar_UInt8.v b)
let text_warp_current (w : text_warp) :
  Space_Text_Types.grapheme FStar_Pervasives_Native.option=
  if text_warp_done w
  then FStar_Pervasives_Native.None
  else
    (match Space_Text_Iter.iter_next w.iter with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some (g, uu___1) ->
         FStar_Pervasives_Native.Some g)
let rec advance_to_position (it : Space_Text_Iter.text_iter)
  (target : Prims.nat) (fuel : Prims.nat) :
  Space_Text_Iter.text_iter FStar_Pervasives_Native.option=
  if fuel = Prims.int_zero
  then FStar_Pervasives_Native.None
  else
    if (Space_Text_Iter.iter_position it) >= target
    then FStar_Pervasives_Native.Some it
    else
      (match Space_Text_Iter.iter_next it with
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
       | FStar_Pervasives_Native.Some (uu___2, it') ->
           advance_to_position it' target (fuel - Prims.int_one))
let text_warp_goto (w : text_warp) (pos : Prims.nat) :
  text_warp FStar_Pervasives_Native.option=
  if pos < w.start_pos
  then FStar_Pervasives_Native.None
  else
    if pos > w.end_pos
    then FStar_Pervasives_Native.None
    else
      (let t = (w.iter).Space_Text_Iter.text in
       let fresh_iter = Space_Text_Iter.iter_begin t in
       match advance_to_position fresh_iter pos
               ((t.Space_Text_Types.header).Space_Text_Types.grapheme_count +
                  Prims.int_one)
       with
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
       | FStar_Pervasives_Native.Some it' ->
           FStar_Pervasives_Native.Some
             {
               iter = it';
               direction = (w.direction);
               start_pos = (w.start_pos);
               end_pos = (w.end_pos)
             })
let text_warp_has_grapheme (w : text_warp) : Prims.bool=
  Prims.op_Negation (text_warp_done w)
