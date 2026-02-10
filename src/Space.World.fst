module Space.World

(** World: runtime context managing multiple universes *)

open Space.Types
open Space.Stack
open Space.Universe

(** World state containing all universes *)
noeq type world = {
  universes: list universe;
  next_id: universe_id;
  current: option universe_id;  (* Active universe *)
}

(** Empty world *)
let empty_world : world = {
  universes = [];
  next_id = 0;
  current = None;
}

(** Find universe by id in a list *)
let rec find_by_id_in_list (us: list universe) (id: universe_id) : option universe =
  match us with
  | [] -> None
  | u :: rest -> if u.id = id then Some u else find_by_id_in_list rest id

(** Find universe by id *)
let find_universe (w: world) (id: universe_id) : option universe =
  find_by_id_in_list w.universes id

(** Find live universe by name in a list *)
let rec find_live_by_name_in_list (us: list universe) (name: universe_name) : option universe =
  match us with
  | [] -> None
  | u :: rest -> if u.name = name && is_live u then Some u else find_live_by_name_in_list rest name

(** Find universe by name *)
let find_by_name (w: world) (name: universe_name) : option universe =
  find_live_by_name_in_list w.universes name

(** Update universe in world *)
let update_universe (w: world) (u: universe) : world =
  let universes' = List.Tot.map
    (fun u' -> if u'.id = u.id then u else u')
    w.universes in
  { w with universes = universes' }

(** Create a new universe in the world *)
let create_universe (w: world) (name: universe_name) (cap: nat) (disc: discipline) : world * universe_id =
  let id = w.next_id in
  let u = create id name cap disc in
  let w' = {
    universes = u :: w.universes;
    next_id = id + 1;
    current = Some id;
  } in
  (w', id)

(** Get current universe *)
let get_current (w: world) : option universe =
  match w.current with
  | None -> None
  | Some id -> find_universe w id

(** Set current universe by name *)
let set_current (w: world) (name: universe_name) : option world =
  match find_by_name w name with
  | None -> None
  | Some u -> Some { w with current = Some u.id }

(** End current universe (for linear: triggers self-destruct check) *)
let end_current (w: world) : option world =
  match get_current w with
  | None -> None
  | Some u ->
    if u.discipline = Linear && not (stack_empty u)
    then None  (* Linear universe must be empty *)
    else
      let u' = destroy u in
      Some { (update_universe w u') with current = None }
