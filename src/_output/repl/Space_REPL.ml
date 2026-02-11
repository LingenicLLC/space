open Prims
let print_nat_char (n : Prims.nat) : unit=
  if n < (Prims.of_int (128))
  then
    FStar_IO.print_string
      (FStar_String.make Prims.int_one (FStar_Char.char_of_int n))
  else FStar_IO.print_string "?"
let rec print_output (out : Prims.nat Prims.list) : unit=
  match out with
  | [] -> ()
  | c::rest -> (print_nat_char c; print_output rest)
let rec print_stack_inner (s : Space_Types.cell Prims.list) : unit=
  match s with
  | [] -> ()
  | v::rest ->
      (print_stack_inner rest;
       FStar_IO.print_string " ";
       FStar_IO.print_string (Prims.string_of_int (FStar_UInt64.v v)))
let print_stack (s : Space_Types.cell Prims.list) : unit=
  FStar_IO.print_string "["; print_stack_inner s; FStar_IO.print_string " ]"
let run_and_show (name : Prims.string)
  (prog : Space_Instruction.instruction Prims.list) : unit=
  FStar_IO.print_string "--- ";
  FStar_IO.print_string name;
  FStar_IO.print_string " ---\n";
  (let result = Space_Step.run_program prog (Prims.of_int (10000)) in
   (match result with
    | Space_Step.StepOk m ->
        (FStar_IO.print_string "OK\n";
         FStar_IO.print_string "Output: ";
         print_output m.Space_Interpreter.output;
         FStar_IO.print_string "\n";
         (match Space_Interpreter.current_universe m with
          | FStar_Pervasives_Native.None ->
              FStar_IO.print_string "Stack: (no universe)\n"
          | FStar_Pervasives_Native.Some u ->
              (FStar_IO.print_string "Stack: ";
               print_stack u.Space_Universe.stack;
               FStar_IO.print_string "\n")))
    | Space_Step.StepHalt m ->
        (FStar_IO.print_string "HALT\n";
         FStar_IO.print_string "Output: ";
         print_output m.Space_Interpreter.output;
         FStar_IO.print_string "\n";
         (match Space_Interpreter.current_universe m with
          | FStar_Pervasives_Native.None ->
              FStar_IO.print_string "Stack: (no universe)\n"
          | FStar_Pervasives_Native.Some u ->
              (FStar_IO.print_string "Stack: ";
               print_stack u.Space_Universe.stack;
               FStar_IO.print_string "\n")))
    | Space_Step.StepError (uu___4, msg) ->
        (FStar_IO.print_string "ERROR: ";
         FStar_IO.print_string msg;
         FStar_IO.print_string "\n"));
   FStar_IO.print_string "\n")
let main (uu___ : unit) : unit=
  FStar_IO.print_string "=== Space VM REPL ===\n\n";
  run_and_show "emit_42" Space_Run.ex_emit_42;
  run_and_show "emit_abc" Space_Run.ex_emit_abc;
  run_and_show "add (3+5)" Space_Run.ex_add;
  run_and_show "factorial(5)" Space_Run.ex_factorial;
  run_and_show "stack_ops" Space_Run.ex_stack_ops;
  run_and_show "memory" Space_Run.ex_memory;
  FStar_IO.print_string "=== Done ===\n"
