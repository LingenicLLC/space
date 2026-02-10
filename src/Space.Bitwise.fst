module Space.Bitwise

(** Bitwise operations on cells *)

open FStar.UInt64
open Space.Types
open Space.Stack

(** Bitwise AND *)
let bit_and (a b: cell) : cell =
  a &^ b

(** Bitwise OR *)
let bit_or (a b: cell) : cell =
  a |^ b

(** Bitwise XOR *)
let bit_xor (a b: cell) : cell =
  a ^^ b

(** Bitwise NOT *)
let bit_not (a: cell) : cell =
  lognot a

(** Shift left - requires shift < 64 *)
let shift_left (a: cell) (n: UInt32.t{UInt32.v n < 64}) : cell =
  a <<^ n

(** Shift right (logical) - requires shift < 64 *)
let shift_right (a: cell) (n: UInt32.t{UInt32.v n < 64}) : cell =
  a >>^ n

(** Stack: bitwise AND *)
let stack_and (s: stack) : option stack =
  match s with
  | a :: b :: xs -> Some (bit_and b a :: xs)
  | _ -> None

(** Stack: bitwise OR *)
let stack_or (s: stack) : option stack =
  match s with
  | a :: b :: xs -> Some (bit_or b a :: xs)
  | _ -> None

(** Stack: bitwise XOR *)
let stack_xor (s: stack) : option stack =
  match s with
  | a :: b :: xs -> Some (bit_xor b a :: xs)
  | _ -> None

(** Stack: bitwise NOT *)
let stack_not (s: stack) : option stack =
  match s with
  | a :: xs -> Some (bit_not a :: xs)
  | _ -> None
