module Space.System.Universes

(** System universes: data (default) and return (call stack) *)

open FStar.UInt64
open Space.Types
open Space.Stack
open Space.Discipline

(** System universe identifiers - reserved IDs *)
let data_universe_id : universe_id = 0
let return_universe_id : universe_id = 1

(** First user-allocatable universe ID *)
let first_user_universe_id : universe_id = 2

(** System universe configuration *)
type system_universe_config = {
  data_size: nat;     (* Size of data universe stack *)
  return_size: nat;   (* Size of return universe stack *)
}

(** Default configuration *)
let default_config : system_universe_config = {
  data_size = 1024;
  return_size = 256;
}

(** Data universe state - unrestricted discipline *)
noeq type data_universe = {
  stack: stack;
  max_depth: nat;
}

(** Return universe state - linear discipline for return addresses *)
noeq type return_universe = {
  stack: stack;
  max_depth: nat;
  obligation_count: nat;  (* Linear tracking *)
}

(** Create empty data universe *)
let create_data_universe (size: nat) : data_universe = {
  stack = [];
  max_depth = size;
}

(** Create empty return universe *)
let create_return_universe (size: nat) : return_universe = {
  stack = [];
  max_depth = size;
  obligation_count = 0;
}

(** Data universe operations - unrestricted allows dup/drop *)
let data_push (u: data_universe) (v: cell) : option data_universe =
  if List.Tot.length u.stack >= u.max_depth
  then None
  else Some { u with stack = v :: u.stack }

let data_pop (u: data_universe) : option (cell * data_universe) =
  match u.stack with
  | [] -> None
  | x :: rest -> Some (x, { u with stack = rest })

let data_dup (u: data_universe) : option data_universe =
  match u.stack with
  | [] -> None
  | x :: _ ->
    if List.Tot.length u.stack >= u.max_depth
    then None
    else Some { u with stack = x :: u.stack }

let data_drop (u: data_universe) : option data_universe =
  match u.stack with
  | [] -> None
  | _ :: rest -> Some { u with stack = rest }

(** Return universe operations - linear, no dup/drop *)
let return_push (u: return_universe) (v: cell) : option return_universe =
  if List.Tot.length u.stack >= u.max_depth
  then None
  else Some {
    stack = v :: u.stack;
    max_depth = u.max_depth;
    obligation_count = u.obligation_count + 1;
  }

let return_pop (u: return_universe) : option (cell * return_universe) =
  match u.stack with
  | [] -> None
  | x :: rest -> Some (x, {
      stack = rest;
      max_depth = u.max_depth;
      obligation_count = if u.obligation_count > 0 then u.obligation_count - 1 else 0;
    })

(** Check if return universe is balanced (all returns consumed) *)
let return_is_balanced (u: return_universe) : bool =
  u.obligation_count = 0

(** Combined system state *)
noeq type system_universes = {
  data: data_universe;
  return: return_universe;
}

(** Initialize system universes with config *)
let init_system_universes (cfg: system_universe_config) : system_universes = {
  data = create_data_universe cfg.data_size;
  return = create_return_universe cfg.return_size;
}

(** Initialize with default config *)
let init_default : system_universes =
  init_system_universes default_config

(** Call operation: push return address to return stack *)
let call_push_return (sys: system_universes) (addr: cell) : option system_universes =
  match return_push sys.return addr with
  | Some ret' -> Some { sys with return = ret' }
  | None -> None

(** Return operation: pop return address from return stack *)
let call_pop_return (sys: system_universes) : option (cell * system_universes) =
  match return_pop sys.return with
  | Some (addr, ret') -> Some (addr, { sys with return = ret' })
  | None -> None

