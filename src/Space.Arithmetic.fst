module Space.Arithmetic

(** Arithmetic operations on cells - wrapping semantics *)

open FStar.UInt64
open Space.Types
open Space.Stack

(** Add two cells (wrapping) *)
let add_cells (a b: cell) : cell =
  add_mod a b

(** Subtract two cells (wrapping) *)
let sub_cells (a b: cell) : cell =
  sub_mod a b

(** Multiply two cells (wrapping) *)
let mul_cells (a b: cell) : cell =
  mul_mod a b

(** Divide unsigned *)
let div_cells (a b: cell) : option cell =
  if v b = 0
  then None
  else Some (a /^ b)

(** Modulo *)
let mod_cells (a b: cell) : option cell =
  if v b = 0
  then None
  else Some (a %^ b)

(** Stack operation: add top two elements *)
let stack_add (s: stack) : option stack =
  match s with
  | a :: b :: xs -> Some (add_cells b a :: xs)
  | _ -> None

(** Stack operation: subtract (second - top) *)
let stack_sub (s: stack) : option stack =
  match s with
  | a :: b :: xs -> Some (sub_cells b a :: xs)
  | _ -> None

(** Stack operation: multiply *)
let stack_mul (s: stack) : option stack =
  match s with
  | a :: b :: xs -> Some (mul_cells b a :: xs)
  | _ -> None

(** Stack operation: divide unsigned *)
let stack_div (s: stack) : option stack =
  match s with
  | a :: b :: xs ->
    (match div_cells b a with
     | Some r -> Some (r :: xs)
     | None -> None)
  | _ -> None

(** Stack operation: modulo *)
let stack_mod (s: stack) : option stack =
  match s with
  | a :: b :: xs ->
    (match mod_cells b a with
     | Some r -> Some (r :: xs)
     | None -> None)
  | _ -> None

(** Negate (two's complement) *)
let negate_cell (a: cell) : cell =
  sub_mod 0uL a

(** Stack operation: negate *)
let stack_negate (s: stack) : option stack =
  match s with
  | a :: xs -> Some (negate_cell a :: xs)
  | _ -> None

(** Check if cell represents negative signed value *)
let is_negative (a: cell) : bool =
  v a >= 0x8000000000000000

(** Absolute value for signed interpretation *)
let abs_signed (a: cell) : cell =
  if is_negative a then negate_cell a else a

(** Signed division using two's complement interpretation *)
let div_signed (a b: cell) : option cell =
  if v b = 0 then None
  else
    let a_neg = is_negative a in
    let b_neg = is_negative b in
    let a_abs = abs_signed a in
    let b_abs = abs_signed b in
    let quot = a_abs /^ b_abs in
    (* Result is negative if exactly one operand was negative *)
    if a_neg <> b_neg then Some (negate_cell quot)
    else Some quot

(** Stack operation: divide signed *)
let stack_div_signed (s: stack) : option stack =
  match s with
  | a :: b :: xs ->
    (match div_signed b a with
     | Some r -> Some (r :: xs)
     | None -> None)
  | _ -> None
