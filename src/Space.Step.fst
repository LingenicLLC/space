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

(** Release universe instruction *)
let step_release_universe (m: machine) (name: universe_name) : step_result =
  match find_by_name m.mworld name with
  | None -> StepError (m, "universe not found")
  | Some u ->
    match release u with
    | None -> StepError (m, "release failed: not affine or not live")
    | Some u' ->
      let w' = update_universe m.mworld u' in
      StepOk (advance_ip { m with mworld = w' })

(** Transfer to universe instruction *)
let step_transfer (m: machine) (name: universe_name) : step_result =
  match current_universe m with
  | None -> StepError (m, "no current universe")
  | Some src ->
    match find_by_name m.mworld name with
    | None -> StepError (m, "target universe not found")
    | Some dst ->
      match src.stack with
      | [] -> StepError (m, "transfer: stack underflow")
      | v :: rest ->
        let src' = { src with stack = rest } in
        let dst' = { dst with stack = v :: dst.stack } in
        let w' = update_universe (update_universe m.mworld src') dst' in
        StepOk (advance_ip { m with mworld = w' })

(** Execute one instruction *)
let step_one (m: machine) (instr: instruction) : step_result =
  match instr with
  | IPush v -> step_push m v
  | ICall target -> step_call m target
  | IReturn -> step_return m
  | IPrimitive op -> step_prim m op
  | IBranch target -> step_branch m target
  | IBranchZero target -> step_branch_zero m target
  | IBranchNonZero target -> step_branch_nonzero m target
  | ICreateUniverse (name, disc) -> step_create_universe m name 1024 disc
  | IEndUniverse name -> step_end_universe m name
  | IReleaseUniverse name -> step_release_universe m name
  | ITransferTo name -> step_transfer m name

(** Fetch instruction at current IP from program *)
let fetch_instr (prog: list instruction) (ip: ip) : option instruction =
  let idx = FStar.UInt64.v ip in
  if idx < List.Tot.length prog then List.Tot.nth prog idx
  else None

(** Run execution loop with fuel *)
let rec run_loop (m: machine) (prog: list instruction) (fuel: nat)
  : Tot step_result (decreases fuel) =
  if fuel = 0 then StepError (m, "fuel exhausted")
  else if not m.running then StepOk m
  else
    match fetch_instr prog m.inst_ptr with
    | None -> StepError (m, "invalid instruction pointer")
    | Some instr ->
      match step_one m instr with
      | StepOk m' -> run_loop m' prog (fuel - 1)
      | StepHalt m' -> StepHalt m'
      | StepError (m', msg) -> StepError (m', msg)

(** Run a program from initial state *)
let run_program (prog: list instruction) (fuel: nat) : step_result =
  let m = { initial_machine with running = true } in
  (* Create default data universe *)
  let (w', _) = create_universe m.mworld "data" 65536 Unrestricted in
  match set_current w' "data" with
  | None -> StepError (m, "failed to set current universe")
  | Some w'' -> run_loop { m with mworld = w'' } prog fuel
