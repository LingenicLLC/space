module Space.Text.Normalize

(** Unicode normalization forms: NFC, NFD, NFKC, NFKD *)

open FStar.UInt8
open FStar.List.Tot
open Space.Text.Types
open Space.Text.UTF8
open Space.Text.Create
open Space.Text.UCD.Types
open Space.Text.UCD.CCC
open Space.Text.UCD.Decomp
open Space.Text.UCD.Comp

(** Normalization form *)
type normalization_form =
  | NFC   (* Canonical Decomposition, followed by Canonical Composition *)
  | NFD   (* Canonical Decomposition *)
  | NFKC  (* Compatibility Decomposition, followed by Canonical Composition *)
  | NFKD  (* Compatibility Decomposition *)

(** Canonical Combining Class (CCC) - 0-254 *)
type combining_class = n:nat{n <= 254}

(** Decomposition type *)
type decomposition_type =
  | Canonical
  | Compatibility

(** Decomposition mapping entry *)
noeq type decomposition_entry = {
  codepoint: nat;
  decomp_type: decomposition_type;
  decomposition: list nat;  (* Decomposed codepoints *)
}

(** Composition pair entry *)
noeq type composition_entry = {
  first: nat;
  second: nat;
  composed: nat;
}

(** Quick check values *)
type quick_check =
  | QC_Yes
  | QC_No
  | QC_Maybe

(** Get combining class for codepoint using UCD table *)
let get_combining_class (cp: nat) : combining_class =
  let ccc = lookup_ccc cp combining_class_table in
  if ccc <= 254 then ccc else 0

(** Check if codepoint is a starter (CCC = 0) *)
let is_starter (cp: nat) : bool =
  get_combining_class cp = 0

(** Get canonical decomposition using UCD table *)
let get_canonical_decomposition (cp: nat) : option (list nat) =
  lookup_decomp cp canonical_decomposition_table

(** Get compatibility decomposition - uses canonical table with compatibility extensions *)
let get_compatibility_decomposition (cp: nat) : option (list nat) =
  (* First check canonical decompositions *)
  match get_canonical_decomposition cp with
  | Some d -> Some d
  | None ->
    (* Compatibility-only decompositions (hardcoded common cases) *)
    (* Fractions *)
    if cp = 0x00BC then Some [0x0031; 0x2044; 0x0034]  (* ¼ = 1/4 *)
    else if cp = 0x00BD then Some [0x0031; 0x2044; 0x0032]  (* ½ = 1/2 *)
    else if cp = 0x00BE then Some [0x0033; 0x2044; 0x0034]  (* ¾ = 3/4 *)
    (* Symbol equivalents *)
    else if cp = 0x2126 then Some [0x03A9]  (* Ω (ohm) = Ω (omega) *)
    else if cp = 0x212A then Some [0x004B]  (* K (kelvin) = K *)
    else if cp = 0x212B then Some [0x00C5]  (* Å (angstrom) = Å *)
    (* Fullwidth Latin (common subset) *)
    else if cp >= 0xFF21 && cp <= 0xFF3A then Some [cp - 0xFF21 + 0x0041]
    else if cp >= 0xFF41 && cp <= 0xFF5A then Some [cp - 0xFF41 + 0x0061]
    else None

(** Get composition using UCD table with exclusion checking *)
let get_composition (first second: nat) : option nat =
  Space.Text.UCD.Comp.try_compose first second

(** Decompose a single codepoint one level *)
let decompose_one (cp: nat) (compat: bool) : list nat =
  let decomp = if compat then get_compatibility_decomposition cp
               else get_canonical_decomposition cp in
  match decomp with
  | None -> [cp]
  | Some cps -> cps

(** Recursively decompose until fixed point, with fuel *)
let rec decompose_pass (cps: list nat) (compat: bool) (fuel: nat)
  : Tot (list nat) (decreases fuel) =
  if fuel = 0 then cps
  else
    let fuel' : nat = fuel - 1 in
    let expanded = List.Tot.concatMap (fun cp -> decompose_one cp compat) cps in
    if List.Tot.length expanded = List.Tot.length cps then cps
    else decompose_pass expanded compat fuel'

(** Recursively decompose a codepoint *)
let full_decomposition (cp: nat) (compat: bool) (fuel: nat) : list nat =
  decompose_pass [cp] compat fuel

(** Decompose all codepoints in list *)
let decompose_codepoints (cps: list nat) (compat: bool) : list nat =
  List.Tot.concatMap (fun cp -> full_decomposition cp compat 10) cps

(** Sort combining marks by canonical combining class (stable sort) *)
let rec insert_by_ccc (cp: nat) (sorted: list nat) : list nat =
  match sorted with
  | [] -> [cp]
  | x :: rest ->
    let cp_ccc = get_combining_class cp in
    let x_ccc = get_combining_class x in
    if cp_ccc < x_ccc then cp :: x :: rest
    else x :: insert_by_ccc cp rest

let rec sort_by_ccc (cps: list nat) : Tot (list nat) (decreases (length cps)) =
  match cps with
  | [] -> []
  | cp :: rest -> insert_by_ccc cp (sort_by_ccc rest)

(** Canonical ordering: sort combining marks after each starter *)
let rec canonical_order_aux (cps: list nat) (acc: list nat) : list nat =
  match cps with
  | [] -> sort_by_ccc acc
  | cp :: rest ->
    if is_starter cp && length acc > 0 then
      sort_by_ccc acc @ canonical_order_aux rest [cp]
    else
      canonical_order_aux rest (acc @ [cp])

let canonical_order (cps: list nat) : list nat =
  canonical_order_aux cps []

(** Compose adjacent characters where possible (with fuel for termination) *)
let rec compose_pass_aux (cps: list nat) (fuel: nat) : Tot (list nat) (decreases fuel) =
  if fuel = 0 then cps
  else
    let fuel' : nat = fuel - 1 in
    match cps with
    | [] -> []
    | [cp] -> [cp]
    | first :: second :: rest ->
      match get_composition first second with
      | Some composed ->
        (* Composed successfully, try to compose more *)
        compose_pass_aux (composed :: rest) fuel'
      | None ->
        (* No composition - move on *)
        first :: compose_pass_aux (second :: rest) fuel'

(** Compose pass wrapper *)
let compose_pass (cps: list nat) : list nat =
  compose_pass_aux cps (length cps + 1)

(** Full composition pass (may need multiple iterations) *)
let rec compose_full (cps: list nat) (fuel: nat) : Tot (list nat) (decreases fuel) =
  if fuel = 0 then cps
  else
    let fuel' : nat = fuel - 1 in
    let composed = compose_pass cps in
    if length composed = length cps then cps
    else compose_full composed fuel'

(** Extract codepoints from UTF-8 bytes *)
let rec bytes_to_codepoints (bytes: list UInt8.t) : Tot (list nat) (decreases (length bytes)) =
  match bytes with
  | [] -> []
  | b0 :: rest ->
    let len = sequence_length b0 in
    if len = 0 then bytes_to_codepoints rest
    else if len = 1 then
      UInt8.v b0 :: bytes_to_codepoints rest
    else if len = 2 then
      match rest with
      | b1 :: rest' -> decode_codepoint_2 b0 b1 :: bytes_to_codepoints rest'
      | _ -> []
    else if len = 3 then
      match rest with
      | b1 :: b2 :: rest' -> decode_codepoint_3 b0 b1 b2 :: bytes_to_codepoints rest'
      | _ -> []
    else
      match rest with
      | b1 :: b2 :: b3 :: rest' -> decode_codepoint_4 b0 b1 b2 b3 :: bytes_to_codepoints rest'
      | _ -> []

(** Encode codepoint to UTF-8 bytes *)
let encode_codepoint (cp: nat) : list UInt8.t =
  if cp <= 0x7F then
    [UInt8.uint_to_t cp]
  else if cp <= 0x7FF then
    [UInt8.uint_to_t (0xC0 + cp / 64);
     UInt8.uint_to_t (0x80 + cp % 64)]
  else if cp <= 0xFFFF then
    [UInt8.uint_to_t (0xE0 + cp / 4096);
     UInt8.uint_to_t (0x80 + (cp / 64) % 64);
     UInt8.uint_to_t (0x80 + cp % 64)]
  else if cp <= 0x10FFFF then
    [UInt8.uint_to_t (0xF0 + cp / 262144);
     UInt8.uint_to_t (0x80 + (cp / 4096) % 64);
     UInt8.uint_to_t (0x80 + (cp / 64) % 64);
     UInt8.uint_to_t (0x80 + cp % 64)]
  else []

(** Encode codepoints to UTF-8 bytes *)
let codepoints_to_bytes (cps: list nat) : list UInt8.t =
  List.Tot.concatMap encode_codepoint cps

(** NFD: Canonical Decomposition *)
let normalize_nfd (t: text) : option text =
  let cps = bytes_to_codepoints t.data in
  let decomposed = decompose_codepoints cps false in
  let ordered = canonical_order decomposed in
  let bytes = codepoints_to_bytes ordered in
  text_from_bytes bytes

(** NFC: Canonical Decomposition + Canonical Composition *)
let normalize_nfc (t: text) : option text =
  let cps = bytes_to_codepoints t.data in
  let decomposed = decompose_codepoints cps false in
  let ordered = canonical_order decomposed in
  let composed = compose_full ordered 10 in
  let bytes = codepoints_to_bytes composed in
  text_from_bytes bytes

(** NFKD: Compatibility Decomposition *)
let normalize_nfkd (t: text) : option text =
  let cps = bytes_to_codepoints t.data in
  let decomposed = decompose_codepoints cps true in
  let ordered = canonical_order decomposed in
  let bytes = codepoints_to_bytes ordered in
  text_from_bytes bytes

(** NFKC: Compatibility Decomposition + Canonical Composition *)
let normalize_nfkc (t: text) : option text =
  let cps = bytes_to_codepoints t.data in
  let decomposed = decompose_codepoints cps true in
  let ordered = canonical_order decomposed in
  let composed = compose_full ordered 10 in
  let bytes = codepoints_to_bytes composed in
  text_from_bytes bytes

(** Normalize text with specified form *)
let text_normalize (t: text) (form: normalization_form) : option text =
  match form with
  | NFC -> normalize_nfc t
  | NFD -> normalize_nfd t
  | NFKC -> normalize_nfkc t
  | NFKD -> normalize_nfkd t

(** Check if text is in specified normal form (quick check) *)
let is_normalized (t: text) (form: normalization_form) : bool =
  match text_normalize t form with
  | None -> false
  | Some normalized ->
    t.header.byte_length = normalized.header.byte_length &&
    t.data = normalized.data
