#light "off"
module Space_Control
type branch_result =
| TakeBranch
| SkipBranch


let uu___is_TakeBranch : branch_result  ->  Prims.bool = (fun ( projectee  :  branch_result ) -> (match (projectee) with
| TakeBranch -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_SkipBranch : branch_result  ->  Prims.bool = (fun ( projectee  :  branch_result ) -> (match (projectee) with
| SkipBranch -> begin
true
end
| uu___ -> begin
false
end))


let check_condition : Space_Stack.stack  ->  (Prims.bool * Space_Stack.stack) FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (c)::rest -> begin
FStar_Pervasives_Native.Some ((((Space_Comparison.is_truthy c)), (rest)))
end
| [] -> begin
FStar_Pervasives_Native.None
end))


let if_true : Space_Stack.stack  ->  (branch_result * Space_Stack.stack) FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match ((check_condition s)) with
| FStar_Pervasives_Native.Some (true, rest) -> begin
FStar_Pervasives_Native.Some (((TakeBranch), (rest)))
end
| FStar_Pervasives_Native.Some (false, rest) -> begin
FStar_Pervasives_Native.Some (((SkipBranch), (rest)))
end
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end))


let if_false : Space_Stack.stack  ->  (branch_result * Space_Stack.stack) FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match ((check_condition s)) with
| FStar_Pervasives_Native.Some (true, rest) -> begin
FStar_Pervasives_Native.Some (((SkipBranch), (rest)))
end
| FStar_Pervasives_Native.Some (false, rest) -> begin
FStar_Pervasives_Native.Some (((TakeBranch), (rest)))
end
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end))

type loop_state =
| Continue
| Exit


let uu___is_Continue : loop_state  ->  Prims.bool = (fun ( projectee  :  loop_state ) -> (match (projectee) with
| Continue -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_Exit : loop_state  ->  Prims.bool = (fun ( projectee  :  loop_state ) -> (match (projectee) with
| Exit -> begin
true
end
| uu___ -> begin
false
end))


let check_loop_exit : Space_Stack.stack  ->  (loop_state * Space_Stack.stack) FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (c)::rest -> begin
(match ((Space_Comparison.is_truthy c)) with
| true -> begin
FStar_Pervasives_Native.Some (((Exit), (rest)))
end
| uu___ -> begin
FStar_Pervasives_Native.Some (((Continue), (rest)))
end)
end
| [] -> begin
FStar_Pervasives_Native.None
end))


let if_zero : Space_Stack.stack  ->  (branch_result * Space_Stack.stack) FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (c)::rest -> begin
(match ((Prims.op_Equality c (FStar_UInt64.uint_to_t ((Prims.parse_int "0"))))) with
| true -> begin
FStar_Pervasives_Native.Some (((TakeBranch), (rest)))
end
| uu___ -> begin
FStar_Pervasives_Native.Some (((SkipBranch), (rest)))
end)
end
| [] -> begin
FStar_Pervasives_Native.None
end))

type while_state =
| WhileContinue
| WhileExit


let uu___is_WhileContinue : while_state  ->  Prims.bool = (fun ( projectee  :  while_state ) -> (match (projectee) with
| WhileContinue -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_WhileExit : while_state  ->  Prims.bool = (fun ( projectee  :  while_state ) -> (match (projectee) with
| WhileExit -> begin
true
end
| uu___ -> begin
false
end))


let check_while_condition : Space_Stack.stack  ->  (while_state * Space_Stack.stack) FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (c)::rest -> begin
(match ((Space_Comparison.is_truthy c)) with
| true -> begin
FStar_Pervasives_Native.Some (((WhileContinue), (rest)))
end
| uu___ -> begin
FStar_Pervasives_Native.Some (((WhileExit), (rest)))
end)
end
| [] -> begin
FStar_Pervasives_Native.None
end))

type times_state =
| TimesIterate of FStar_UInt64.t
| TimesDone


let uu___is_TimesIterate : times_state  ->  Prims.bool = (fun ( projectee  :  times_state ) -> (match (projectee) with
| TimesIterate (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__TimesIterate__item___0 : times_state  ->  FStar_UInt64.t = (fun ( projectee  :  times_state ) -> (match (projectee) with
| TimesIterate (_0) -> begin
_0
end))


let uu___is_TimesDone : times_state  ->  Prims.bool = (fun ( projectee  :  times_state ) -> (match (projectee) with
| TimesDone -> begin
true
end
| uu___ -> begin
false
end))


let times_init : Space_Stack.stack  ->  (times_state * Space_Stack.stack) FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (count)::rest -> begin
(match ((Prims.op_Equality (FStar_UInt64.v count) (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.Some (((TimesDone), (rest)))
end
| uu___ -> begin
FStar_Pervasives_Native.Some (((TimesIterate (count)), (rest)))
end)
end
| [] -> begin
FStar_Pervasives_Native.None
end))


let times_decrement : times_state  ->  times_state = (fun ( state  :  times_state ) -> (match (state) with
| TimesIterate (n) -> begin
(match (((FStar_UInt64.v n) <= (Prims.parse_int "1"))) with
| true -> begin
TimesDone
end
| uu___ -> begin
TimesIterate ((FStar_UInt64.sub n (FStar_UInt64.uint_to_t ((Prims.parse_int "1")))))
end)
end
| TimesDone -> begin
TimesDone
end))


let times_should_continue : times_state  ->  Prims.bool = (fun ( state  :  times_state ) -> (match (state) with
| TimesIterate (uu___) -> begin
true
end
| TimesDone -> begin
false
end))

type exit_state =
| NormalExecution
| EarlyExit


let uu___is_NormalExecution : exit_state  ->  Prims.bool = (fun ( projectee  :  exit_state ) -> (match (projectee) with
| NormalExecution -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_EarlyExit : exit_state  ->  Prims.bool = (fun ( projectee  :  exit_state ) -> (match (projectee) with
| EarlyExit -> begin
true
end
| uu___ -> begin
false
end))


let trigger_exit : exit_state = EarlyExit


let should_continue : exit_state  ->  Prims.bool = (fun ( state  :  exit_state ) -> (match (state) with
| NormalExecution -> begin
true
end
| EarlyExit -> begin
false
end))




