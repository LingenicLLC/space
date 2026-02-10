module Space.Text.UCD

(** Unicode Character Database - Complete tables for Unicode 15.0.0

    This module aggregates all UCD data tables:
    - Space.Text.UCD.Types: Type definitions
    - Space.Text.UCD.Case: Case mappings (1450 upper, 1433 lower, 103 special)
    - Space.Text.UCD.CCC: Combining classes (922 entries)
    - Space.Text.UCD.Decomp: Canonical decompositions (2061 entries)
    - Space.Text.UCD.Comp: Composition pairs (1026) and exclusions (81)

    Data extracted from official Unicode Character Database files:
    - UnicodeData.txt
    - SpecialCasing.txt
    - CompositionExclusions.txt
*)

open Space.Text.UCD.Types
open Space.Text.UCD.Case
open Space.Text.UCD.CCC
open Space.Text.UCD.Decomp
open Space.Text.UCD.Comp

(** Re-export type definitions *)
type mapping_entry = Space.Text.UCD.Types.mapping_entry
type ccc_entry = Space.Text.UCD.Types.ccc_entry
type decomp_entry = Space.Text.UCD.Types.decomp_entry
type comp_entry = Space.Text.UCD.Types.comp_entry

(** Unified API *)

(** Get simple uppercase mapping *)
let simple_uppercase (cp: nat) : nat =
  get_uppercase cp

(** Get simple lowercase mapping *)
let simple_lowercase (cp: nat) : nat =
  get_lowercase cp

(** Get combining class *)
let combining_class (cp: nat) : nat =
  get_ccc cp

(** Check if starter *)
let is_ccc_starter (cp: nat) : bool =
  is_starter cp

(** Get canonical decomposition *)
let canonical_decomposition (cp: nat) : option (list nat) =
  get_decomposition cp

(** Compose two codepoints *)
let compose (first second: nat) : option nat =
  try_compose first second

(** Check if has case *)
let codepoint_has_case (cp: nat) : bool =
  has_case cp

