module Space.Step

(** Single-step execution of the interpreter *)

open FStar.UInt64
open Space.Types
open Space.Stack
open Space.Universe
open Space.World
open Space.Instruction
open Space.Interpreter
open Space.Execute
open Space.Control

(** Step result *)
noeq type step_result =
  | StepOk of machine
  | StepHalt of machine
  | StepError of machine * string

(** Execute push instruction *)
let step_push (m: machine) (v: cell) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match universe_push u v with
    | None -> StepError (m, "push failed")
    | Some u' -> StepOk (advance_ip (update_current m u'))

(** Execute primitive instruction *)
let step_prim (m: machine) (op: prim_op) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match exec_prim op u with
    | ExecOk u' -> StepOk (advance_ip (update_current m u'))
    | ExecHalt -> StepHalt (halt m)
    | ExecError msg -> StepError (m, msg)

(** Execute branch if zero *)
let step_branch_zero (m: machine) (target: ip) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match if_true u.stack with
    | None -> StepError (m, "branch: stack underflow")
    | Some (TakeBranch, s') ->
      let u' = { u with stack = s' } in
      StepOk (advance_ip (update_current m u'))
    | Some (SkipBranch, s') ->
      let u' = { u with stack = s' } in
      StepOk (jump (update_current m u') target)

(** Execute branch if non-zero *)
let step_branch_nonzero (m: machine) (target: ip) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some u ->
    match if_false u.stack with
    | None -> StepError (m, "branch: stack underflow")
    | Some (TakeBranch, s') ->
      let u' = { u with stack = s' } in
      StepOk (advance_ip (update_current m u'))
    | Some (SkipBranch, s') ->
      let u' = { u with stack = s' } in
      StepOk (jump (update_current m u') target)

(** Execute call instruction *)
let step_call (m: machine) (target: ip) : step_result =
  StepOk (call m target)

(** Execute return instruction *)
let step_return (m: machine) : step_result =
  let m' = do_return m in
  match m'.error with
  | Some msg -> StepError (m', msg)
  | None -> StepOk m'

(** Execute unconditional branch *)
let step_branch (m: machine) (target: ip) : step_result =
  StepOk (jump m target)

(** Create universe instruction *)
let step_create_universe (m: machine) (name: universe_name) (cap: nat) (disc: discipline) : step_result =
  let (w', _uid) = create_universe m.mworld name cap disc in
  StepOk (advance_ip { m with mworld = w' })

(** End universe instruction *)
let step_end_universe (m: machine) (name: universe_name) : step_result =
  match find_by_name m.mworld name with
  | None -> StepError (m, "universe not found")
  | Some u ->
    if u.discipline = Linear && not (stack_empty u)
    then StepError (m, "linear universe not empty")
    else
      let u' = destroy u in
      let w' = update_universe m.mworld u' in
      StepOk (advance_ip { m with mworld = w' })
