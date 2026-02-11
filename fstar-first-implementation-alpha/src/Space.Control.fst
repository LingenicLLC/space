module Space.Control

(** Control flow primitives *)

open Space.Types
open Space.Stack
open Space.Comparison

(** Conditional execution result *)
type branch_result =
  | TakeBranch
  | SkipBranch

(** Check if top of stack is truthy (non-zero) *)
let check_condition (s: stack) : option (bool * stack) =
  match s with
  | c :: rest -> Some (is_truthy c, rest)
  | [] -> None

(** If-true: consume flag, return branch decision *)
let if_true (s: stack) : option (branch_result * stack) =
  match check_condition s with
  | Some (true, rest) -> Some (TakeBranch, rest)
  | Some (false, rest) -> Some (SkipBranch, rest)
  | None -> None

(** If-false: consume flag, return branch decision (inverse) *)
let if_false (s: stack) : option (branch_result * stack) =
  match check_condition s with
  | Some (true, rest) -> Some (SkipBranch, rest)
  | Some (false, rest) -> Some (TakeBranch, rest)
  | None -> None

(** Loop state *)
type loop_state =
  | Continue
  | Exit

(** Check loop exit condition *)
let check_loop_exit (s: stack) : option (loop_state * stack) =
  match s with
  | c :: rest ->
    if is_truthy c
    then Some (Exit, rest)
    else Some (Continue, rest)
  | [] -> None

(** If-zero: consume flag, branch if zero *)
let if_zero (s: stack) : option (branch_result * stack) =
  match s with
  | c :: rest ->
    if c = 0uL
    then Some (TakeBranch, rest)
    else Some (SkipBranch, rest)
  | [] -> None

(** While loop state - condition at top *)
type while_state =
  | WhileContinue   (* Condition true, execute body *)
  | WhileExit       (* Condition false, exit loop *)

(** Check while condition (begin-while ... do-while) *)
let check_while_condition (s: stack) : option (while_state * stack) =
  match s with
  | c :: rest ->
    if is_truthy c
    then Some (WhileContinue, rest)
    else Some (WhileExit, rest)
  | [] -> None

(** Times loop state - counted iteration *)
type times_state =
  | TimesIterate of FStar.UInt64.t  (* Remaining count *)
  | TimesDone                        (* Count exhausted *)

(** Initialize times loop (do-times) *)
let times_init (s: stack) : option (times_state * stack) =
  match s with
  | count :: rest ->
    if FStar.UInt64.v count = 0
    then Some (TimesDone, rest)
    else Some (TimesIterate count, rest)
  | [] -> None

(** Decrement times counter *)
let times_decrement (state: times_state) : times_state =
  match state with
  | TimesIterate n ->
    if FStar.UInt64.v n <= 1
    then TimesDone
    else TimesIterate (FStar.UInt64.sub n 1uL)
  | TimesDone -> TimesDone

(** Check if times loop should continue *)
let times_should_continue (state: times_state) : bool =
  match state with
  | TimesIterate _ -> true
  | TimesDone -> false

(** Exit word marker - signals early return *)
type exit_state =
  | NormalExecution
  | EarlyExit

(** Set early exit flag *)
let trigger_exit : exit_state = EarlyExit

(** Check if execution should continue *)
let should_continue (state: exit_state) : bool =
  match state with
  | NormalExecution -> true
  | EarlyExit -> false
