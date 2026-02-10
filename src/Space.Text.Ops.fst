module Space.Text.Ops

(** Text operations: slice, concat, equal, compare *)

open FStar.UInt8
open Space.Text.Types
open Space.Text.UTF8
open Space.Text.Create
open Space.Text.Iter

(** Compare two byte lists *)
let rec bytes_equal (a b: list UInt8.t) : bool =
  match a, b with
  | [], [] -> true
  | x :: xs, y :: ys -> x = y && bytes_equal xs ys
  | _, _ -> false

(** Text equality (byte-wise) *)
let text_equal (t1 t2: text) : bool =
  t1.header.byte_length = t2.header.byte_length &&
  bytes_equal t1.data t2.data

(** Lexicographic comparison result *)
type ordering =
  | Less
  | Equal
  | Greater

(** Compare two byte lists lexicographically *)
let rec bytes_compare (a b: list UInt8.t) : ordering =
  match a, b with
  | [], [] -> Equal
  | [], _ -> Less
  | _, [] -> Greater
  | x :: xs, y :: ys ->
    if UInt8.v x < UInt8.v y then Less
    else if UInt8.v x > UInt8.v y then Greater
    else bytes_compare xs ys

(** Text comparison (lexicographic by bytes) *)
let text_compare (t1 t2: text) : ordering =
  bytes_compare t1.data t2.data

(** Concatenate two byte lists *)
let bytes_concat (a b: list UInt8.t) : list UInt8.t =
  List.Tot.append a b

(** Concatenate two texts *)
let text_concat (t1 t2: text) : text =
  let new_data = bytes_concat t1.data t2.data in
  let new_complexity =
    if t1.header.complexity = Complex || t2.header.complexity = Complex
    then Complex
    else Simple in
  {
    header = {
      grapheme_count = t1.header.grapheme_count + t2.header.grapheme_count;
      byte_length = t1.header.byte_length + t2.header.byte_length;
      complexity = new_complexity;
    };
    index = [];  (* Would need to rebuild *)
    data = new_data;
  }

(** Take first n elements from list *)
let rec list_take (n: nat) (xs: list UInt8.t) : list UInt8.t =
  if n = 0 then []
  else match xs with
    | [] -> []
    | x :: rest -> x :: list_take (n - 1) rest

(** Drop first n elements from list *)
let rec list_drop (n: nat) (xs: list UInt8.t) : list UInt8.t =
  if n = 0 then xs
  else match xs with
    | [] -> []
    | _ :: rest -> list_drop (n - 1) rest

(** Slice bytes from start to end position *)
let bytes_slice (bytes: list UInt8.t) (start: nat) (finish: nat{finish >= start}) : list UInt8.t =
  list_take (finish - start) (list_drop start bytes)

(** Helper: get byte offset for grapheme index in Simple text *)
let simple_grapheme_offset (t: text) (idx: nat) : nat =
  if t.header.complexity = Simple then idx else 0

(** Slice text by grapheme range [start, end) - Simple text only for now *)
let text_slice_simple (t: text) (start finish: nat) : option text =
  if t.header.complexity <> Simple then None
  else if finish < start then None
  else if finish > t.header.grapheme_count then None
  else
    let new_data = bytes_slice t.data start finish in
    let new_count = finish - start in
    Some {
      header = {
        grapheme_count = new_count;
        byte_length = new_count;
        complexity = Simple;
      };
      index = [];
      data = new_data;
    }
