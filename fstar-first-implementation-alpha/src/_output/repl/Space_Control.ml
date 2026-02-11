open Prims
type branch_result =
  | TakeBranch 
  | SkipBranch 
let uu___is_TakeBranch (projectee : branch_result) : Prims.bool=
  match projectee with | TakeBranch -> true | uu___ -> false
let uu___is_SkipBranch (projectee : branch_result) : Prims.bool=
  match projectee with | SkipBranch -> true | uu___ -> false
let check_condition (s : Space_Stack.stack) :
  (Prims.bool * Space_Stack.stack) FStar_Pervasives_Native.option=
  match s with
  | c::rest ->
      FStar_Pervasives_Native.Some ((Space_Comparison.is_truthy c), rest)
  | [] -> FStar_Pervasives_Native.None
let if_true (s : Space_Stack.stack) :
  (branch_result * Space_Stack.stack) FStar_Pervasives_Native.option=
  match check_condition s with
  | FStar_Pervasives_Native.Some (true, rest) ->
      FStar_Pervasives_Native.Some (TakeBranch, rest)
  | FStar_Pervasives_Native.Some (false, rest) ->
      FStar_Pervasives_Native.Some (SkipBranch, rest)
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
let if_false (s : Space_Stack.stack) :
  (branch_result * Space_Stack.stack) FStar_Pervasives_Native.option=
  match check_condition s with
  | FStar_Pervasives_Native.Some (true, rest) ->
      FStar_Pervasives_Native.Some (SkipBranch, rest)
  | FStar_Pervasives_Native.Some (false, rest) ->
      FStar_Pervasives_Native.Some (TakeBranch, rest)
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
type loop_state =
  | Continue 
  | Exit 
let uu___is_Continue (projectee : loop_state) : Prims.bool=
  match projectee with | Continue -> true | uu___ -> false
let uu___is_Exit (projectee : loop_state) : Prims.bool=
  match projectee with | Exit -> true | uu___ -> false
let check_loop_exit (s : Space_Stack.stack) :
  (loop_state * Space_Stack.stack) FStar_Pervasives_Native.option=
  match s with
  | c::rest ->
      if Space_Comparison.is_truthy c
      then FStar_Pervasives_Native.Some (Exit, rest)
      else FStar_Pervasives_Native.Some (Continue, rest)
  | [] -> FStar_Pervasives_Native.None
let if_zero (s : Space_Stack.stack) :
  (branch_result * Space_Stack.stack) FStar_Pervasives_Native.option=
  match s with
  | c::rest ->
      if c = Stdint.Uint64.zero
      then FStar_Pervasives_Native.Some (TakeBranch, rest)
      else FStar_Pervasives_Native.Some (SkipBranch, rest)
  | [] -> FStar_Pervasives_Native.None
type while_state =
  | WhileContinue 
  | WhileExit 
let uu___is_WhileContinue (projectee : while_state) : Prims.bool=
  match projectee with | WhileContinue -> true | uu___ -> false
let uu___is_WhileExit (projectee : while_state) : Prims.bool=
  match projectee with | WhileExit -> true | uu___ -> false
let check_while_condition (s : Space_Stack.stack) :
  (while_state * Space_Stack.stack) FStar_Pervasives_Native.option=
  match s with
  | c::rest ->
      if Space_Comparison.is_truthy c
      then FStar_Pervasives_Native.Some (WhileContinue, rest)
      else FStar_Pervasives_Native.Some (WhileExit, rest)
  | [] -> FStar_Pervasives_Native.None
type times_state =
  | TimesIterate of FStar_UInt64.t 
  | TimesDone 
let uu___is_TimesIterate (projectee : times_state) : Prims.bool=
  match projectee with | TimesIterate _0 -> true | uu___ -> false
let __proj__TimesIterate__item___0 (projectee : times_state) :
  FStar_UInt64.t= match projectee with | TimesIterate _0 -> _0
let uu___is_TimesDone (projectee : times_state) : Prims.bool=
  match projectee with | TimesDone -> true | uu___ -> false
let times_init (s : Space_Stack.stack) :
  (times_state * Space_Stack.stack) FStar_Pervasives_Native.option=
  match s with
  | count::rest ->
      if (FStar_UInt64.v count) = Prims.int_zero
      then FStar_Pervasives_Native.Some (TimesDone, rest)
      else FStar_Pervasives_Native.Some ((TimesIterate count), rest)
  | [] -> FStar_Pervasives_Native.None
let times_decrement (state : times_state) : times_state=
  match state with
  | TimesIterate n ->
      if (FStar_UInt64.v n) <= Prims.int_one
      then TimesDone
      else TimesIterate (FStar_UInt64.sub n Stdint.Uint64.one)
  | TimesDone -> TimesDone
let times_should_continue (state : times_state) : Prims.bool=
  match state with | TimesIterate uu___ -> true | TimesDone -> false
type exit_state =
  | NormalExecution 
  | EarlyExit 
let uu___is_NormalExecution (projectee : exit_state) : Prims.bool=
  match projectee with | NormalExecution -> true | uu___ -> false
let uu___is_EarlyExit (projectee : exit_state) : Prims.bool=
  match projectee with | EarlyExit -> true | uu___ -> false
let trigger_exit : exit_state= EarlyExit
let should_continue (state : exit_state) : Prims.bool=
  match state with | NormalExecution -> true | EarlyExit -> false
