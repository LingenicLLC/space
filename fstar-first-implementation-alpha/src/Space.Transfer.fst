module Space.Transfer

(** Transfer values between universes *)

open Space.Types
open Space.Stack
open Space.Universe

(** Transfer a value from one universe to another *)
let transfer (src: universe) (dst: universe) : option (universe * universe) =
  if not (is_live src) then None
  else if not (is_live dst) then None
  else if not (has_space dst) then None
  else
    match universe_pop src with
    | None -> None
    | Some (v, src') ->
      match universe_push dst v with
      | None -> None
      | Some dst' -> Some (src', dst')

(** Transfer result with possible self-destruction *)
noeq type transfer_result =
  | TransferOk of universe * universe
  | SourceDestroyed of universe  (* src self-destructed, dst updated *)
  | TransferFailed

(** Transfer with self-destruction check for linear source *)
let transfer_checked (src: universe) (dst: universe) : transfer_result =
  match transfer src dst with
  | None -> TransferFailed
  | Some (src', dst') ->
    if should_self_destruct src'
    then SourceDestroyed dst'
    else TransferOk (src', dst')
