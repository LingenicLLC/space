module Space.Arithmetic.Properties

(** Verified properties of arithmetic operations *)

open FStar.UInt64
open Space.Types
open Space.Stack
open Space.Arithmetic

(** Addition is commutative *)
let add_commutative (a b: cell) :
  Lemma (add_cells a b = add_cells b a) =
  ()

(** Subtraction of same value yields zero *)
let sub_self_zero (a: cell) :
  Lemma (sub_cells a a = 0uL) =
  ()

(** Addition with zero is identity *)
let add_zero_identity (a: cell) :
  Lemma (add_cells a 0uL = a) =
  ()

(** Subtraction of zero is identity *)
let sub_zero_identity (a: cell) :
  Lemma (sub_cells a 0uL = a) =
  ()

(** Multiplication by zero is zero *)
let mul_zero (a: cell) :
  Lemma (mul_cells a 0uL = 0uL) =
  ()

(** Multiplication by one is identity *)
let mul_one_identity (a: cell) :
  Lemma (mul_cells a 1uL = a) =
  ()

(** Multiplication is commutative *)
let mul_commutative (a b: cell) :
  Lemma (mul_cells a b = mul_cells b a) =
  ()

(** Division by zero returns None *)
let div_by_zero_none (a: cell) :
  Lemma (None? (div_cells a 0uL)) =
  ()

(** Modulo by zero returns None *)
let mod_by_zero_none (a: cell) :
  Lemma (None? (mod_cells a 0uL)) =
  ()

(** Division by one is identity *)
let div_one_identity (a: cell) :
  Lemma (div_cells a 1uL = Some a) =
  ()

(** Zero divided by non-zero is zero *)
let div_zero_is_zero (b: cell) :
  Lemma (requires v b > 0)
        (ensures div_cells 0uL b = Some 0uL) =
  ()

(** Modulo of zero is zero *)
let mod_zero_is_zero (b: cell) :
  Lemma (requires v b > 0)
        (ensures mod_cells 0uL b = Some 0uL) =
  ()

(** Double negation is identity *)
let negate_negate_identity (a: cell) :
  Lemma (negate_cell (negate_cell a) = a) =
  ()

(** Negation of zero is zero *)
let negate_zero (a: cell) :
  Lemma (requires a = 0uL)
        (ensures negate_cell a = 0uL) =
  ()

(** Stack add requires two elements *)
let stack_add_requires_two (s: stack) :
  Lemma (requires size s < 2)
        (ensures None? (stack_add s)) =
  ()

(** Stack add reduces size by one *)
let stack_add_reduces_size (s: stack) :
  Lemma (requires size s >= 2)
        (ensures (match stack_add s with
                  | Some s' -> size s' = size s - 1
                  | None -> False)) =
  ()

(** Stack sub requires two elements *)
let stack_sub_requires_two (s: stack) :
  Lemma (requires size s < 2)
        (ensures None? (stack_sub s)) =
  ()

(** Stack sub reduces size by one *)
let stack_sub_reduces_size (s: stack) :
  Lemma (requires size s >= 2)
        (ensures (match stack_sub s with
                  | Some s' -> size s' = size s - 1
                  | None -> False)) =
  ()

(** Stack mul requires two elements *)
let stack_mul_requires_two (s: stack) :
  Lemma (requires size s < 2)
        (ensures None? (stack_mul s)) =
  ()

(** Stack mul reduces size by one *)
let stack_mul_reduces_size (s: stack) :
  Lemma (requires size s >= 2)
        (ensures (match stack_mul s with
                  | Some s' -> size s' = size s - 1
                  | None -> False)) =
  ()

(** Stack negate requires one element *)
let stack_negate_requires_one (s: stack) :
  Lemma (requires is_empty s)
        (ensures None? (stack_negate s)) =
  ()

(** Stack negate preserves size *)
let stack_negate_preserves_size (s: stack) :
  Lemma (requires not (is_empty s))
        (ensures (match stack_negate s with
                  | Some s' -> size s' = size s
                  | None -> False)) =
  ()

(** Signed division by zero returns None *)
let signed_div_by_zero_none (a: cell) :
  Lemma (None? (div_signed a 0uL)) =
  ()

(** Absolute value of positive is same *)
let abs_positive_identity (a: cell) :
  Lemma (requires not (is_negative a))
        (ensures abs_signed a = a) =
  ()

(** is_negative is correct for small values *)
let small_not_negative (a: cell) :
  Lemma (requires v a < 0x8000000000000000)
        (ensures not (is_negative a)) =
  ()

(** is_negative is correct for large values *)
let large_is_negative (a: cell) :
  Lemma (requires v a >= 0x8000000000000000)
        (ensures is_negative a) =
  ()

(** Zero is not negative *)
let zero_not_negative () :
  Lemma (not (is_negative 0uL)) =
  ()

(** Add then sub returns original *)
let add_sub_identity (a b: cell) :
  Lemma (sub_cells (add_cells a b) b = a) =
  ()

(** Sub then add returns original *)
let sub_add_identity (a b: cell) :
  Lemma (add_cells (sub_cells a b) b = a) =
  ()
