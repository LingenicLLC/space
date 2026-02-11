open Prims
let run (prog : Space_Instruction.instruction Prims.list) (fuel : Prims.nat)
  : Prims.nat Prims.list=
  match Space_Step.run_program prog fuel with
  | Space_Step.StepOk m -> m.Space_Interpreter.output
  | Space_Step.StepHalt m -> m.Space_Interpreter.output
  | Space_Step.StepError (m, uu___) -> m.Space_Interpreter.output
let run_machine (prog : Space_Instruction.instruction Prims.list)
  (fuel : Prims.nat) : Space_Step.step_result=
  Space_Step.run_program prog fuel
let run_ok (prog : Space_Instruction.instruction Prims.list)
  (fuel : Prims.nat) : Prims.bool=
  match Space_Step.run_program prog fuel with
  | Space_Step.StepOk uu___ -> true
  | Space_Step.StepHalt uu___ -> true
  | Space_Step.StepError uu___ -> false
let run_error (prog : Space_Instruction.instruction Prims.list)
  (fuel : Prims.nat) : Prims.string FStar_Pervasives_Native.option=
  match Space_Step.run_program prog fuel with
  | Space_Step.StepOk uu___ -> FStar_Pervasives_Native.None
  | Space_Step.StepHalt uu___ -> FStar_Pervasives_Native.None
  | Space_Step.StepError (uu___, msg) -> FStar_Pervasives_Native.Some msg
let run_stack (prog : Space_Instruction.instruction Prims.list)
  (fuel : Prims.nat) :
  Space_Types.cell Prims.list FStar_Pervasives_Native.option=
  match Space_Step.run_program prog fuel with
  | Space_Step.StepOk m ->
      (match Space_Interpreter.current_universe m with
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
       | FStar_Pervasives_Native.Some u ->
           FStar_Pervasives_Native.Some (u.Space_Universe.stack))
  | Space_Step.StepHalt m ->
      (match Space_Interpreter.current_universe m with
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
       | FStar_Pervasives_Native.Some u ->
           FStar_Pervasives_Native.Some (u.Space_Universe.stack))
  | Space_Step.StepError uu___ -> FStar_Pervasives_Native.None
let ex_emit_42 : Space_Instruction.instruction Prims.list=
  [Space_Instruction.IPush (Stdint.Uint64.of_int (42));
  Space_Instruction.IPrimitive Space_Instruction.PrimEmit;
  Space_Instruction.IPrimitive Space_Instruction.PrimHalt]
let ex_emit_abc : Space_Instruction.instruction Prims.list=
  [Space_Instruction.IPush (Stdint.Uint64.of_int (65));
  Space_Instruction.IPrimitive Space_Instruction.PrimEmit;
  Space_Instruction.IPush (Stdint.Uint64.of_int (66));
  Space_Instruction.IPrimitive Space_Instruction.PrimEmit;
  Space_Instruction.IPush (Stdint.Uint64.of_int (67));
  Space_Instruction.IPrimitive Space_Instruction.PrimEmit;
  Space_Instruction.IPrimitive Space_Instruction.PrimHalt]
let ex_add : Space_Instruction.instruction Prims.list=
  [Space_Instruction.IPush (Stdint.Uint64.of_int (3));
  Space_Instruction.IPush (Stdint.Uint64.of_int (5));
  Space_Instruction.IPrimitive Space_Instruction.PrimAdd;
  Space_Instruction.IPrimitive Space_Instruction.PrimHalt]
let ex_factorial : Space_Instruction.instruction Prims.list=
  [Space_Instruction.IPush Stdint.Uint64.one;
  Space_Instruction.IPush (Stdint.Uint64.of_int (5));
  Space_Instruction.IPrimitive Space_Instruction.PrimDup;
  Space_Instruction.IBranchZero (Stdint.Uint64.of_int (9));
  Space_Instruction.IPrimitive Space_Instruction.PrimSwap;
  Space_Instruction.IPrimitive Space_Instruction.PrimOver;
  Space_Instruction.IPrimitive Space_Instruction.PrimMul;
  Space_Instruction.IPrimitive Space_Instruction.PrimSwap;
  Space_Instruction.IPush Stdint.Uint64.one;
  Space_Instruction.IPrimitive Space_Instruction.PrimSub;
  Space_Instruction.IBranch (Stdint.Uint64.of_int (2));
  Space_Instruction.IPrimitive Space_Instruction.PrimDrop;
  Space_Instruction.IPrimitive Space_Instruction.PrimHalt]
let ex_stack_ops : Space_Instruction.instruction Prims.list=
  [Space_Instruction.IPush (Stdint.Uint64.of_int (10));
  Space_Instruction.IPush (Stdint.Uint64.of_int (20));
  Space_Instruction.IPrimitive Space_Instruction.PrimDup;
  Space_Instruction.IPrimitive Space_Instruction.PrimRot;
  Space_Instruction.IPrimitive Space_Instruction.PrimSwap;
  Space_Instruction.IPrimitive Space_Instruction.PrimHalt]
let ex_memory : Space_Instruction.instruction Prims.list=
  [Space_Instruction.IPush Stdint.Uint64.one;
  Space_Instruction.IPrimitive Space_Instruction.PrimAlloc;
  Space_Instruction.IPrimitive Space_Instruction.PrimDup;
  Space_Instruction.IPush (Stdint.Uint64.of_int (999));
  Space_Instruction.IPrimitive Space_Instruction.PrimStore;
  Space_Instruction.IPrimitive Space_Instruction.PrimFetch;
  Space_Instruction.IPrimitive Space_Instruction.PrimHalt]
let test_emit_42 : Prims.nat Prims.list= run ex_emit_42 (Prims.of_int (100))
let test_emit_abc : Prims.nat Prims.list=
  run ex_emit_abc (Prims.of_int (100))
let test_add_stack :
  Space_Types.cell Prims.list FStar_Pervasives_Native.option=
  run_stack ex_add (Prims.of_int (100))
let test_factorial_stack :
  Space_Types.cell Prims.list FStar_Pervasives_Native.option=
  run_stack ex_factorial (Prims.of_int (100))
let test_memory_stack :
  Space_Types.cell Prims.list FStar_Pervasives_Native.option=
  run_stack ex_memory (Prims.of_int (100))
