module Space.Compiler.AST

(** Abstract Syntax Tree for the Space language *)

open FStar.List.Tot
open Space.Compiler.Token

(** Discipline specifier *)
type discipline =
  | Discipline_Linear
  | Discipline_Affine
  | Discipline_Unrestricted

(** Literal values *)
noeq type literal =
  | Lit_Int of int
  | Lit_String of string

(** Forward declaration for block *)
val block : Type0

(** Expression (single operation) *)
noeq type expr =
  (* Literals *)
  | E_Literal of literal
  (* Primitive operations *)
  | E_Primitive of primitive
  (* Call user-defined word *)
  | E_Call of string
  (* Transfer to named universe *)
  | E_TransferTo of string
  (* Control flow - use lists for blocks *)
  | E_IfTrue of list expr & option (list expr)  (* if-true ... [if-else ...] end-if *)
  | E_IfZero of list expr & option (list expr)  (* if-zero ... [if-else ...] end-if *)
  | E_Loop of list expr                          (* begin-loop ... end-loop *)
  | E_While of list expr & list expr             (* begin-while ... do-while ... end-while *)
  | E_Times of list expr                         (* do-times ... end-times *)
  | E_Exit                                       (* exit-word *)
  (* Universe management *)
  | E_CreateUniverse of nat & discipline & string & list expr  (* size discipline name body *)
  | E_ReleaseUniverse of string
  (* Warp management *)
  | E_WarpInto of string & list expr             (* ptr warp-into as name body end-warp *)
  | E_WarpIntoReadonly of string & list expr
  (* Text warp *)
  | E_TextWarpInto of list expr                  (* text text-warp-into body end-text-warp *)

(** Block is a sequence of expressions *)
let block = list expr

(** Word definition record *)
noeq type word_def = {
  wd_name: string;
  wd_stack_effect: option string;
  wd_body: block;
  wd_loc: source_loc;
}

(** Constant definition record *)
noeq type const_def = {
  cd_name: string;
  cd_value: int;
  cd_loc: source_loc;
}

(** Text definition record *)
noeq type text_def = {
  td_name: string;
  td_value: string;
  td_loc: source_loc;
}

(** Top-level definition *)
noeq type definition =
  | Def_Word of word_def
  | Def_Constant of const_def
  | Def_Text of text_def

(** Program is a sequence of definitions *)
noeq type program = {
  definitions: list definition;
}

(** Helper: create an empty program *)
let empty_program : program = { definitions = [] }

(** Helper: add definition to program *)
let add_definition (p: program) (d: definition) : program =
  { definitions = p.definitions @ [d] }

(** Helper: find word definition by name *)
let rec find_word (defs: list definition) (name: string) : option definition =
  match defs with
  | [] -> None
  | d :: rest ->
    match d with
    | Def_Word w -> if w.wd_name = name then Some d else find_word rest name
    | _ -> find_word rest name

(** Helper: find constant by name *)
let rec find_constant (defs: list definition) (name: string) : option int =
  match defs with
  | [] -> None
  | d :: rest ->
    match d with
    | Def_Constant c -> if c.cd_name = name then Some c.cd_value else find_constant rest name
    | _ -> find_constant rest name

(** Helper: find text by name *)
let rec find_text (defs: list definition) (name: string) : option string =
  match defs with
  | [] -> None
  | d :: rest ->
    match d with
    | Def_Text t -> if t.td_name = name then Some t.td_value else find_text rest name
    | _ -> find_text rest name

(** Helper: get all word names defined in program *)
let rec get_word_names (defs: list definition) : list string =
  match defs with
  | [] -> []
  | Def_Word w :: rest -> w.wd_name :: get_word_names rest
  | _ :: rest -> get_word_names rest

(** Helper: get all constant names defined in program *)
let rec get_constant_names (defs: list definition) : list string =
  match defs with
  | [] -> []
  | Def_Constant c :: rest -> c.cd_name :: get_constant_names rest
  | _ :: rest -> get_constant_names rest

(** Helper: get all text names defined in program *)
let rec get_text_names (defs: list definition) : list string =
  match defs with
  | [] -> []
  | Def_Text t :: rest -> t.td_name :: get_text_names rest
  | _ :: rest -> get_text_names rest
