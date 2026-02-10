module Space.Text.UCD.Types

(** Type definitions for Unicode Character Database tables *)

(** Single mapping entry (codepoint -> codepoint) *)
noeq type mapping_entry = {
  codepoint: nat;
  mapped: nat;
}

(** Multi-character mapping entry (codepoint -> list of codepoints) *)
noeq type multi_mapping_entry = {
  codepoint: nat;
  mapped_to: list nat;
}

(** Combining class entry *)
noeq type ccc_entry = {
  codepoint: nat;
  ccc: nat;  (* 0-254 *)
}

(** Decomposition entry *)
noeq type decomp_entry = {
  codepoint: nat;
  decomposition: list nat;
  is_canonical: bool;
}

(** Composition pair entry *)
noeq type comp_entry = {
  first: nat;
  second: nat;
  composed: nat;
}

(** Special casing entry (for multi-char case mappings) *)
noeq type special_case_entry = {
  codepoint: nat;
  lower: list nat;
  title: list nat;
  upper: list nat;
}

(** Lookup a mapping in a table *)
let rec lookup_mapping (cp: nat) (table: list mapping_entry) : option nat =
  match table with
  | [] -> None
  | entry :: rest ->
    if entry.codepoint = cp then Some entry.mapped
    else lookup_mapping cp rest

(** Lookup combining class *)
let rec lookup_ccc (cp: nat) (table: list ccc_entry) : nat =
  match table with
  | [] -> 0  (* Default CCC is 0 *)
  | entry :: rest ->
    if entry.codepoint = cp then entry.ccc
    else lookup_ccc cp rest

(** Lookup decomposition *)
let rec lookup_decomp (cp: nat) (table: list decomp_entry) : option (list nat) =
  match table with
  | [] -> None
  | entry :: rest ->
    if entry.codepoint = cp then Some entry.decomposition
    else lookup_decomp cp rest

(** Lookup composition *)
let rec lookup_composition (first second: nat) (table: list comp_entry) : option nat =
  match table with
  | [] -> None
  | entry :: rest ->
    if entry.first = first && entry.second = second then Some entry.composed
    else lookup_composition first second rest

(** Check if codepoint is in exclusion list *)
let rec is_excluded (cp: nat) (exclusions: list nat) : bool =
  match exclusions with
  | [] -> false
  | x :: rest -> x = cp || is_excluded cp rest

