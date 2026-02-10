module Space.Stack

(** Stack operations for universes *)

open Space.Types

(** Abstract stack type - list of cells *)
type stack = list cell

(** Empty stack *)
let empty : stack = []

(** Check if stack is empty *)
let is_empty (s: stack) : bool =
  match s with
  | [] -> true
  | _ -> false

(** Stack size *)
let size (s: stack) : nat =
  List.Tot.length s

(** Push a value onto the stack *)
let push (s: stack) (v: cell) : stack =
  v :: s

(** Pop a value from the stack *)
let pop (s: stack) : option (cell * stack) =
  match s with
  | [] -> None
  | x :: xs -> Some (x, xs)

(** Peek at top without removing *)
let peek (s: stack) : option cell =
  match s with
  | [] -> None
  | x :: _ -> Some x

(** Duplicate top of stack *)
let dup (s: stack) : option stack =
  match s with
  | [] -> None
  | x :: xs -> Some (x :: x :: xs)

(** Drop top of stack *)
let drop (s: stack) : option stack =
  match s with
  | [] -> None
  | _ :: xs -> Some xs

(** Swap top two elements *)
let swap (s: stack) : option stack =
  match s with
  | a :: b :: xs -> Some (b :: a :: xs)
  | _ -> None

(** Over: copy second element to top *)
let over (s: stack) : option stack =
  match s with
  | a :: b :: xs -> Some (b :: a :: b :: xs)
  | _ -> None

(** Rot: rotate top three elements *)
let rot (s: stack) : option stack =
  match s with
  | a :: b :: c :: xs -> Some (c :: a :: b :: xs)
  | _ -> None
