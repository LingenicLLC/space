module Space.Comparison

(** Comparison operations on cells *)

open FStar.UInt64
open Space.Types
open Space.Stack

(** Boolean to cell (0 = false, 1 = true) *)
let bool_to_cell (b: bool) : cell =
  if b then 1uL else 0uL

(** Equal *)
let cell_eq (a b: cell) : cell =
  bool_to_cell (a = b)

(** Not equal *)
let cell_neq (a b: cell) : cell =
  bool_to_cell (a <> b)

(** Less than (unsigned) *)
let cell_lt (a b: cell) : cell =
  bool_to_cell (a <^ b)

(** Less than or equal (unsigned) *)
let cell_lte (a b: cell) : cell =
  bool_to_cell (a <=^ b)

(** Greater than (unsigned) *)
let cell_gt (a b: cell) : cell =
  bool_to_cell (a >^ b)

(** Greater than or equal (unsigned) *)
let cell_gte (a b: cell) : cell =
  bool_to_cell (a >=^ b)

(** Stack: equal *)
let stack_eq (s: stack) : option stack =
  match s with
  | a :: b :: xs -> Some (cell_eq b a :: xs)
  | _ -> None

(** Stack: not equal *)
let stack_neq (s: stack) : option stack =
  match s with
  | a :: b :: xs -> Some (cell_neq b a :: xs)
  | _ -> None

(** Stack: less than *)
let stack_lt (s: stack) : option stack =
  match s with
  | a :: b :: xs -> Some (cell_lt b a :: xs)
  | _ -> None

(** Stack: greater than *)
let stack_gt (s: stack) : option stack =
  match s with
  | a :: b :: xs -> Some (cell_gt b a :: xs)
  | _ -> None

(** Is cell truthy (non-zero)? *)
let is_truthy (c: cell) : bool =
  c <> 0uL

(** Check if high bit is set (negative in signed interpretation) *)
let is_negative (a: cell) : bool =
  v a >= 0x8000000000000000

(** Signed less than using two's complement interpretation *)
let cell_lt_signed (a b: cell) : cell =
  let a_neg = is_negative a in
  let b_neg = is_negative b in
  if a_neg && not b_neg then bool_to_cell true       (* negative < positive *)
  else if not a_neg && b_neg then bool_to_cell false (* positive >= negative *)
  else bool_to_cell (a <^ b)                         (* same sign: compare normally *)

(** Signed greater than using two's complement interpretation *)
let cell_gt_signed (a b: cell) : cell =
  let a_neg = is_negative a in
  let b_neg = is_negative b in
  if a_neg && not b_neg then bool_to_cell false      (* negative < positive *)
  else if not a_neg && b_neg then bool_to_cell true  (* positive > negative *)
  else bool_to_cell (a >^ b)                         (* same sign: compare normally *)

(** Stack: signed less than *)
let stack_lt_signed (s: stack) : option stack =
  match s with
  | a :: b :: xs -> Some (cell_lt_signed b a :: xs)
  | _ -> None

(** Stack: signed greater than *)
let stack_gt_signed (s: stack) : option stack =
  match s with
  | a :: b :: xs -> Some (cell_gt_signed b a :: xs)
  | _ -> None
