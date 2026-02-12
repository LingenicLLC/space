open Prims
type stack = Space_Types.cell Prims.list
let empty : stack= []
let is_empty (s : stack) : Prims.bool=
  match s with | [] -> true | uu___ -> false
let size (s : stack) : Prims.nat= FStar_List_Tot_Base.length s
let push (s : stack) (v : Space_Types.cell) : stack= v :: s
let pop (s : stack) :
  (Space_Types.cell * stack) FStar_Pervasives_Native.option=
  match s with
  | [] -> FStar_Pervasives_Native.None
  | x::xs -> FStar_Pervasives_Native.Some (x, xs)
let peek (s : stack) : Space_Types.cell FStar_Pervasives_Native.option=
  match s with
  | [] -> FStar_Pervasives_Native.None
  | x::uu___ -> FStar_Pervasives_Native.Some x
let dup (s : stack) : stack FStar_Pervasives_Native.option=
  match s with
  | [] -> FStar_Pervasives_Native.None
  | x::xs -> FStar_Pervasives_Native.Some (x :: x :: xs)
let drop (s : stack) : stack FStar_Pervasives_Native.option=
  match s with
  | [] -> FStar_Pervasives_Native.None
  | uu___::xs -> FStar_Pervasives_Native.Some xs
let swap (s : stack) : stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some (b :: a :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let over (s : stack) : stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some (b :: a :: b :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let rot (s : stack) : stack FStar_Pervasives_Native.option=
  match s with
  | a::b::c::xs -> FStar_Pervasives_Native.Some (c :: a :: b :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let nip (s : stack) : stack FStar_Pervasives_Native.option=
  match s with
  | a::uu___::xs -> FStar_Pervasives_Native.Some (a :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let tuck (s : stack) : stack FStar_Pervasives_Native.option=
  match s with
  | a::b::xs -> FStar_Pervasives_Native.Some (a :: b :: a :: xs)
  | uu___ -> FStar_Pervasives_Native.None
let rec nth_opt (s : stack) (n : Prims.nat) :
  Space_Types.cell FStar_Pervasives_Native.option=
  match (s, n) with
  | ([], uu___) -> FStar_Pervasives_Native.None
  | (x::uu___, uu___1) when uu___1 = Prims.int_zero ->
      FStar_Pervasives_Native.Some x
  | (uu___::xs, n1) -> nth_opt xs (n1 - Prims.int_one)
let pick (s : stack) : stack FStar_Pervasives_Native.option=
  match s with
  | n_cell::rest ->
      let n = FStar_UInt64.v n_cell in
      (match nth_opt rest n with
       | FStar_Pervasives_Native.Some x ->
           FStar_Pervasives_Native.Some (x :: rest)
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
  | uu___ -> FStar_Pervasives_Native.None
