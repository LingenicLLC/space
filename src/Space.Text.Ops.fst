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

(** Generate simple index: each grapheme is 1 byte, starting at given offset *)
let rec generate_simple_index (count offset: nat) : Tot (list grapheme_entry) (decreases count) =
  if count = 0 then []
  else { byte_offset = offset; byte_len = 1 } :: generate_simple_index (count - 1) (offset + 1)

(** Get index for text (generate for Simple text) *)
let get_text_index (t: text) : list grapheme_entry =
  if t.header.complexity = Simple then
    generate_simple_index t.header.grapheme_count 0
  else
    t.index

(** Offset all entries in an index by a byte offset *)
let rec offset_index (idx: list grapheme_entry) (offset: nat) : list grapheme_entry =
  match idx with
  | [] -> []
  | e :: rest ->
    { byte_offset = e.byte_offset + offset; byte_len = e.byte_len } :: offset_index rest offset

(** Concatenate two texts *)
let text_concat (t1 t2: text) : text =
  let new_data = bytes_concat t1.data t2.data in
  let new_complexity =
    if t1.header.complexity = Complex || t2.header.complexity = Complex
    then Complex
    else Simple in
  let new_index =
    if new_complexity = Simple then []
    else
      (* Rebuild index for Complex result *)
      let idx1 = get_text_index t1 in
      let idx2 = get_text_index t2 in
      let idx2_offset = offset_index idx2 t1.header.byte_length in
      List.Tot.append idx1 idx2_offset
  in
  {
    header = {
      grapheme_count = t1.header.grapheme_count + t2.header.grapheme_count;
      byte_length = t1.header.byte_length + t2.header.byte_length;
      complexity = new_complexity;
    };
    index = new_index;
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

(** Slice bytes from start to end position - unchecked version *)
let bytes_slice_nat (bytes: list UInt8.t) (start finish: nat) : list UInt8.t =
  if finish >= start then list_take (finish - start) (list_drop start bytes)
  else []

(** Slice bytes from start to end position *)
let bytes_slice (bytes: list UInt8.t) (start: nat) (finish: nat{finish >= start}) : list UInt8.t =
  list_take (finish - start) (list_drop start bytes)

(** Helper: get byte offset for grapheme index in Simple text *)
let simple_grapheme_offset (t: text) (idx: nat) : nat =
  if t.header.complexity = Simple then idx else 0

(** Helper: get nth element from index *)
let rec get_index_entry (idx: list grapheme_entry) (n: nat) : option grapheme_entry =
  match idx with
  | [] -> None
  | e :: rest -> if n = 0 then Some e else get_index_entry rest (n - 1)

(** Helper: sum byte lengths from index entries [start, finish) *)
let rec sum_index_bytes (idx: list grapheme_entry) (start finish: nat) (current: nat) : nat =
  if current >= finish then 0
  else match idx with
    | [] -> 0
    | e :: rest ->
      if current < start then sum_index_bytes rest start finish (current + 1)
      else e.byte_len + sum_index_bytes rest start finish (current + 1)

(** Helper: take index entries [start, finish) and adjust offsets *)
let rec slice_index (idx: list grapheme_entry) (start finish: nat) (current base_offset: nat)
  : list grapheme_entry =
  if current >= finish then []
  else match idx with
    | [] -> []
    | e :: rest ->
      if current < start then slice_index rest start finish (current + 1) base_offset
      else
        let new_offset = if e.byte_offset >= base_offset then e.byte_offset - base_offset else 0 in
        let new_entry = { byte_offset = new_offset; byte_len = e.byte_len } in
        new_entry :: slice_index rest start finish (current + 1) base_offset

(** Slice text by grapheme range [start, finish) - works for both Simple and Complex *)
let text_slice (t: text) (start finish: nat) : option text =
  if finish < start then None
  else if finish > t.header.grapheme_count then None
  else if start = finish then Some empty_text
  else if t.header.complexity = Simple then
    (* Simple: 1 byte per grapheme *)
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
  else
    (* Complex: use index to find byte boundaries *)
    match get_index_entry t.index start with
    | None -> None
    | Some start_entry ->
      let start_byte = start_entry.byte_offset in
      (* For end, we need byte after last grapheme *)
      let end_byte =
        if finish = t.header.grapheme_count then t.header.byte_length
        else match get_index_entry t.index finish with
          | None -> t.header.byte_length
          | Some end_entry -> end_entry.byte_offset
      in
      (* Ensure end_byte >= start_byte for valid slice *)
      if end_byte < start_byte then None
      else
        let new_byte_len = end_byte - start_byte in
        let new_data = bytes_slice_nat t.data start_byte end_byte in
        let new_index = slice_index t.index start finish 0 start_byte in
        let new_count = finish - start in
        Some {
          header = {
            grapheme_count = new_count;
            byte_length = new_byte_len;
            complexity = Complex;
          };
          index = new_index;
          data = new_data;
        }

(** Slice text by grapheme range [start, end) - Simple text only for now *)
let text_slice_simple (t: text) (start finish: nat) : option text =
  if t.header.complexity <> Simple then None
  else text_slice t start finish
