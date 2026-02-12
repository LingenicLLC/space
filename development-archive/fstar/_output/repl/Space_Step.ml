open Prims
type step_result =
  | StepOk of Space_Interpreter.machine 
  | StepHalt of Space_Interpreter.machine 
  | StepError of (Space_Interpreter.machine * Prims.string) 
let uu___is_StepOk (projectee : step_result) : Prims.bool=
  match projectee with | StepOk _0 -> true | uu___ -> false
let __proj__StepOk__item___0 (projectee : step_result) :
  Space_Interpreter.machine= match projectee with | StepOk _0 -> _0
let uu___is_StepHalt (projectee : step_result) : Prims.bool=
  match projectee with | StepHalt _0 -> true | uu___ -> false
let __proj__StepHalt__item___0 (projectee : step_result) :
  Space_Interpreter.machine= match projectee with | StepHalt _0 -> _0
let uu___is_StepError (projectee : step_result) : Prims.bool=
  match projectee with | StepError _0 -> true | uu___ -> false
let __proj__StepError__item___0 (projectee : step_result) :
  (Space_Interpreter.machine * Prims.string)=
  match projectee with | StepError _0 -> _0
let step_push (m : Space_Interpreter.machine) (v : Space_Types.cell) :
  step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match Space_Universe.universe_push u v with
       | FStar_Pervasives_Native.None -> StepError (m, "push failed")
       | FStar_Pervasives_Native.Some u' ->
           StepOk
             (Space_Interpreter.advance_ip
                (Space_Interpreter.update_current m u')))
let step_branch_zero (m : Space_Interpreter.machine)
  (target : Space_Instruction.ip) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match Space_Control.if_true u.Space_Universe.stack with
       | FStar_Pervasives_Native.None ->
           StepError (m, "branch: stack underflow")
       | FStar_Pervasives_Native.Some (Space_Control.TakeBranch, s') ->
           let u' =
             {
               Space_Universe.id = (u.Space_Universe.id);
               Space_Universe.name = (u.Space_Universe.name);
               Space_Universe.discipline = (u.Space_Universe.discipline);
               Space_Universe.stack = s';
               Space_Universe.memory = (u.Space_Universe.memory);
               Space_Universe.capacity = (u.Space_Universe.capacity);
               Space_Universe.state = (u.Space_Universe.state)
             } in
           StepOk
             (Space_Interpreter.advance_ip
                (Space_Interpreter.update_current m u'))
       | FStar_Pervasives_Native.Some (Space_Control.SkipBranch, s') ->
           let u' =
             {
               Space_Universe.id = (u.Space_Universe.id);
               Space_Universe.name = (u.Space_Universe.name);
               Space_Universe.discipline = (u.Space_Universe.discipline);
               Space_Universe.stack = s';
               Space_Universe.memory = (u.Space_Universe.memory);
               Space_Universe.capacity = (u.Space_Universe.capacity);
               Space_Universe.state = (u.Space_Universe.state)
             } in
           StepOk
             (Space_Interpreter.jump (Space_Interpreter.update_current m u')
                target))
let step_branch_nonzero (m : Space_Interpreter.machine)
  (target : Space_Instruction.ip) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match Space_Control.if_false u.Space_Universe.stack with
       | FStar_Pervasives_Native.None ->
           StepError (m, "branch: stack underflow")
       | FStar_Pervasives_Native.Some (Space_Control.TakeBranch, s') ->
           let u' =
             {
               Space_Universe.id = (u.Space_Universe.id);
               Space_Universe.name = (u.Space_Universe.name);
               Space_Universe.discipline = (u.Space_Universe.discipline);
               Space_Universe.stack = s';
               Space_Universe.memory = (u.Space_Universe.memory);
               Space_Universe.capacity = (u.Space_Universe.capacity);
               Space_Universe.state = (u.Space_Universe.state)
             } in
           StepOk
             (Space_Interpreter.advance_ip
                (Space_Interpreter.update_current m u'))
       | FStar_Pervasives_Native.Some (Space_Control.SkipBranch, s') ->
           let u' =
             {
               Space_Universe.id = (u.Space_Universe.id);
               Space_Universe.name = (u.Space_Universe.name);
               Space_Universe.discipline = (u.Space_Universe.discipline);
               Space_Universe.stack = s';
               Space_Universe.memory = (u.Space_Universe.memory);
               Space_Universe.capacity = (u.Space_Universe.capacity);
               Space_Universe.state = (u.Space_Universe.state)
             } in
           StepOk
             (Space_Interpreter.jump (Space_Interpreter.update_current m u')
                target))
let step_call (m : Space_Interpreter.machine) (target : Space_Instruction.ip)
  : step_result= StepOk (Space_Interpreter.call m target)
let step_return (m : Space_Interpreter.machine) : step_result=
  let m' = Space_Interpreter.do_return m in
  match m'.Space_Interpreter.error with
  | FStar_Pervasives_Native.Some msg -> StepError (m', msg)
  | FStar_Pervasives_Native.None -> StepOk m'
let step_branch (m : Space_Interpreter.machine)
  (target : Space_Instruction.ip) : step_result=
  StepOk (Space_Interpreter.jump m target)
let step_create_universe (m : Space_Interpreter.machine)
  (name : Space_Types.universe_name) (cap : Prims.nat)
  (disc : Space_Types.discipline) : step_result=
  let uu___ =
    Space_World.create_universe m.Space_Interpreter.mworld name cap disc in
  match uu___ with
  | (w', _uid) ->
      StepOk
        (Space_Interpreter.advance_ip
           {
             Space_Interpreter.mworld = w';
             Space_Interpreter.borrows = (m.Space_Interpreter.borrows);
             Space_Interpreter.warps1 = (m.Space_Interpreter.warps1);
             Space_Interpreter.texts1 = (m.Space_Interpreter.texts1);
             Space_Interpreter.text_warps = (m.Space_Interpreter.text_warps);
             Space_Interpreter.graphemes1 = (m.Space_Interpreter.graphemes1);
             Space_Interpreter.bytes_meta = (m.Space_Interpreter.bytes_meta);
             Space_Interpreter.output = (m.Space_Interpreter.output);
             Space_Interpreter.input = (m.Space_Interpreter.input);
             Space_Interpreter.inst_ptr = (m.Space_Interpreter.inst_ptr);
             Space_Interpreter.return_stack =
               (m.Space_Interpreter.return_stack);
             Space_Interpreter.running = (m.Space_Interpreter.running);
             Space_Interpreter.error = (m.Space_Interpreter.error)
           })
let step_end_universe (m : Space_Interpreter.machine)
  (name : Space_Types.universe_name) : step_result=
  match Space_World.find_by_name m.Space_Interpreter.mworld name with
  | FStar_Pervasives_Native.None -> StepError (m, "universe not found")
  | FStar_Pervasives_Native.Some u ->
      if
        (u.Space_Universe.discipline = Space_Types.Linear) &&
          (Prims.op_Negation (Space_Universe.stack_empty u))
      then StepError (m, "linear universe not empty")
      else
        (let u' = Space_Universe.destroy u in
         let w' = Space_World.update_universe m.Space_Interpreter.mworld u' in
         StepOk
           (Space_Interpreter.advance_ip
              {
                Space_Interpreter.mworld = w';
                Space_Interpreter.borrows = (m.Space_Interpreter.borrows);
                Space_Interpreter.warps1 = (m.Space_Interpreter.warps1);
                Space_Interpreter.texts1 = (m.Space_Interpreter.texts1);
                Space_Interpreter.text_warps =
                  (m.Space_Interpreter.text_warps);
                Space_Interpreter.graphemes1 =
                  (m.Space_Interpreter.graphemes1);
                Space_Interpreter.bytes_meta =
                  (m.Space_Interpreter.bytes_meta);
                Space_Interpreter.output = (m.Space_Interpreter.output);
                Space_Interpreter.input = (m.Space_Interpreter.input);
                Space_Interpreter.inst_ptr = (m.Space_Interpreter.inst_ptr);
                Space_Interpreter.return_stack =
                  (m.Space_Interpreter.return_stack);
                Space_Interpreter.running = (m.Space_Interpreter.running);
                Space_Interpreter.error = (m.Space_Interpreter.error)
              }))
let step_release_universe (m : Space_Interpreter.machine)
  (name : Space_Types.universe_name) : step_result=
  match Space_World.find_by_name m.Space_Interpreter.mworld name with
  | FStar_Pervasives_Native.None -> StepError (m, "universe not found")
  | FStar_Pervasives_Native.Some u ->
      (match Space_Universe.release u with
       | FStar_Pervasives_Native.None ->
           StepError (m, "release failed: not affine or not live")
       | FStar_Pervasives_Native.Some u' ->
           let w' = Space_World.update_universe m.Space_Interpreter.mworld u' in
           StepOk
             (Space_Interpreter.advance_ip
                {
                  Space_Interpreter.mworld = w';
                  Space_Interpreter.borrows = (m.Space_Interpreter.borrows);
                  Space_Interpreter.warps1 = (m.Space_Interpreter.warps1);
                  Space_Interpreter.texts1 = (m.Space_Interpreter.texts1);
                  Space_Interpreter.text_warps =
                    (m.Space_Interpreter.text_warps);
                  Space_Interpreter.graphemes1 =
                    (m.Space_Interpreter.graphemes1);
                  Space_Interpreter.bytes_meta =
                    (m.Space_Interpreter.bytes_meta);
                  Space_Interpreter.output = (m.Space_Interpreter.output);
                  Space_Interpreter.input = (m.Space_Interpreter.input);
                  Space_Interpreter.inst_ptr = (m.Space_Interpreter.inst_ptr);
                  Space_Interpreter.return_stack =
                    (m.Space_Interpreter.return_stack);
                  Space_Interpreter.running = (m.Space_Interpreter.running);
                  Space_Interpreter.error = (m.Space_Interpreter.error)
                }))
let step_transfer (m : Space_Interpreter.machine)
  (name : Space_Types.universe_name) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some src ->
      (match Space_World.find_by_name m.Space_Interpreter.mworld name with
       | FStar_Pervasives_Native.None ->
           StepError (m, "target universe not found")
       | FStar_Pervasives_Native.Some dst ->
           (match src.Space_Universe.stack with
            | [] -> StepError (m, "transfer: stack underflow")
            | v::rest ->
                let src' =
                  {
                    Space_Universe.id = (src.Space_Universe.id);
                    Space_Universe.name = (src.Space_Universe.name);
                    Space_Universe.discipline =
                      (src.Space_Universe.discipline);
                    Space_Universe.stack = rest;
                    Space_Universe.memory = (src.Space_Universe.memory);
                    Space_Universe.capacity = (src.Space_Universe.capacity);
                    Space_Universe.state = (src.Space_Universe.state)
                  } in
                let dst' =
                  {
                    Space_Universe.id = (dst.Space_Universe.id);
                    Space_Universe.name = (dst.Space_Universe.name);
                    Space_Universe.discipline =
                      (dst.Space_Universe.discipline);
                    Space_Universe.stack = (v :: (dst.Space_Universe.stack));
                    Space_Universe.memory = (dst.Space_Universe.memory);
                    Space_Universe.capacity = (dst.Space_Universe.capacity);
                    Space_Universe.state = (dst.Space_Universe.state)
                  } in
                let w' =
                  Space_World.update_universe
                    (Space_World.update_universe m.Space_Interpreter.mworld
                       src') dst' in
                StepOk
                  (Space_Interpreter.advance_ip
                     {
                       Space_Interpreter.mworld = w';
                       Space_Interpreter.borrows =
                         (m.Space_Interpreter.borrows);
                       Space_Interpreter.warps1 =
                         (m.Space_Interpreter.warps1);
                       Space_Interpreter.texts1 =
                         (m.Space_Interpreter.texts1);
                       Space_Interpreter.text_warps =
                         (m.Space_Interpreter.text_warps);
                       Space_Interpreter.graphemes1 =
                         (m.Space_Interpreter.graphemes1);
                       Space_Interpreter.bytes_meta =
                         (m.Space_Interpreter.bytes_meta);
                       Space_Interpreter.output =
                         (m.Space_Interpreter.output);
                       Space_Interpreter.input = (m.Space_Interpreter.input);
                       Space_Interpreter.inst_ptr =
                         (m.Space_Interpreter.inst_ptr);
                       Space_Interpreter.return_stack =
                         (m.Space_Interpreter.return_stack);
                       Space_Interpreter.running =
                         (m.Space_Interpreter.running);
                       Space_Interpreter.error = (m.Space_Interpreter.error)
                     })))
let read_byte_from_mem (mem : Space_Memory.memory) (base_addr : Prims.nat)
  (offset : Prims.nat) : FStar_UInt8.t FStar_Pervasives_Native.option=
  if offset >= (Prims.parse_int "18446744073709551616")
  then FStar_Pervasives_Native.None
  else
    (let offset64 = FStar_UInt64.uint_to_t offset in
     let cell_offset = FStar_UInt64.div offset64 (Stdint.Uint64.of_int (8)) in
     let cell_idx = base_addr + (FStar_UInt64.v cell_offset) in
     let byte_pos = FStar_UInt64.rem offset64 (Stdint.Uint64.of_int (8)) in
     let shift_amt = FStar_UInt64.mul byte_pos (Stdint.Uint64.of_int (8)) in
     match Space_Memory.mem_fetch mem cell_idx with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some cell_val ->
         if (FStar_UInt64.v shift_amt) >= (Prims.of_int (64))
         then FStar_Pervasives_Native.None
         else
           (let n32 = FStar_UInt32.uint_to_t (FStar_UInt64.v shift_amt) in
            let shifted = FStar_UInt64.shift_right cell_val n32 in
            let byte_val =
              FStar_UInt64.v
                (FStar_UInt64.logand shifted (Stdint.Uint64.of_int (0xFF))) in
            if byte_val < (Prims.of_int (256))
            then
              FStar_Pervasives_Native.Some (FStar_UInt8.uint_to_t byte_val)
            else FStar_Pervasives_Native.None))
let rec read_bytes_from_mem (mem : Space_Memory.memory) (addr : Prims.nat)
  (remaining : Prims.nat) :
  FStar_UInt8.t Prims.list FStar_Pervasives_Native.option=
  if remaining = Prims.int_zero
  then FStar_Pervasives_Native.Some []
  else
    (match read_byte_from_mem mem addr Prims.int_zero with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some b ->
         (match read_bytes_from_mem mem addr (remaining - Prims.int_one) with
          | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
          | FStar_Pervasives_Native.Some rest ->
              FStar_Pervasives_Native.Some (b :: rest)))
let rec read_bytes_seq (mem : Space_Memory.memory) (base_addr : Prims.nat)
  (offset : Prims.nat) (remaining : Prims.nat) :
  FStar_UInt8.t Prims.list FStar_Pervasives_Native.option=
  if remaining = Prims.int_zero
  then FStar_Pervasives_Native.Some []
  else
    (match read_byte_from_mem mem base_addr offset with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some b ->
         (match read_bytes_seq mem base_addr (offset + Prims.int_one)
                  (remaining - Prims.int_one)
          with
          | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
          | FStar_Pervasives_Native.Some rest ->
              FStar_Pervasives_Native.Some (b :: rest)))
let step_create_text (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | len_cell::addr_cell::rest ->
           let len = FStar_UInt64.v len_cell in
           let addr = FStar_UInt64.v addr_cell in
           if len = Prims.int_zero
           then
             let uu___ =
               Space_Interpreter.add_text m.Space_Interpreter.texts1
                 Space_Text_Types.empty_text in
             (match uu___ with
              | (tt', handle) ->
                  let handle_cell =
                    if handle < (Prims.parse_int "18446744073709551616")
                    then FStar_UInt64.uint_to_t handle
                    else Stdint.Uint64.zero in
                  let u' =
                    {
                      Space_Universe.id = (u.Space_Universe.id);
                      Space_Universe.name = (u.Space_Universe.name);
                      Space_Universe.discipline =
                        (u.Space_Universe.discipline);
                      Space_Universe.stack = (handle_cell :: rest);
                      Space_Universe.memory = (u.Space_Universe.memory);
                      Space_Universe.capacity = (u.Space_Universe.capacity);
                      Space_Universe.state = (u.Space_Universe.state)
                    } in
                  StepOk
                    (Space_Interpreter.advance_ip
                       (let uu___1 = Space_Interpreter.update_current m u' in
                        {
                          Space_Interpreter.mworld =
                            (uu___1.Space_Interpreter.mworld);
                          Space_Interpreter.borrows =
                            (uu___1.Space_Interpreter.borrows);
                          Space_Interpreter.warps1 =
                            (uu___1.Space_Interpreter.warps1);
                          Space_Interpreter.texts1 = tt';
                          Space_Interpreter.text_warps =
                            (uu___1.Space_Interpreter.text_warps);
                          Space_Interpreter.graphemes1 =
                            (uu___1.Space_Interpreter.graphemes1);
                          Space_Interpreter.bytes_meta =
                            (uu___1.Space_Interpreter.bytes_meta);
                          Space_Interpreter.output =
                            (uu___1.Space_Interpreter.output);
                          Space_Interpreter.input =
                            (uu___1.Space_Interpreter.input);
                          Space_Interpreter.inst_ptr =
                            (uu___1.Space_Interpreter.inst_ptr);
                          Space_Interpreter.return_stack =
                            (uu___1.Space_Interpreter.return_stack);
                          Space_Interpreter.running =
                            (uu___1.Space_Interpreter.running);
                          Space_Interpreter.error =
                            (uu___1.Space_Interpreter.error)
                        })))
           else
             (match read_bytes_seq u.Space_Universe.memory addr
                      Prims.int_zero len
              with
              | FStar_Pervasives_Native.None ->
                  StepError (m, "create-text: failed to read memory")
              | FStar_Pervasives_Native.Some bytes ->
                  (match Space_Text_Create.text_from_bytes bytes with
                   | FStar_Pervasives_Native.None ->
                       StepError (m, "create-text: invalid UTF-8")
                   | FStar_Pervasives_Native.Some t ->
                       let uu___1 =
                         Space_Interpreter.add_text
                           m.Space_Interpreter.texts1 t in
                       (match uu___1 with
                        | (tt', handle) ->
                            let handle_cell =
                              if
                                handle <
                                  (Prims.parse_int "18446744073709551616")
                              then FStar_UInt64.uint_to_t handle
                              else Stdint.Uint64.zero in
                            let u' =
                              {
                                Space_Universe.id = (u.Space_Universe.id);
                                Space_Universe.name = (u.Space_Universe.name);
                                Space_Universe.discipline =
                                  (u.Space_Universe.discipline);
                                Space_Universe.stack = (handle_cell :: rest);
                                Space_Universe.memory =
                                  (u.Space_Universe.memory);
                                Space_Universe.capacity =
                                  (u.Space_Universe.capacity);
                                Space_Universe.state =
                                  (u.Space_Universe.state)
                              } in
                            StepOk
                              (Space_Interpreter.advance_ip
                                 (let uu___2 =
                                    Space_Interpreter.update_current m u' in
                                  {
                                    Space_Interpreter.mworld =
                                      (uu___2.Space_Interpreter.mworld);
                                    Space_Interpreter.borrows =
                                      (uu___2.Space_Interpreter.borrows);
                                    Space_Interpreter.warps1 =
                                      (uu___2.Space_Interpreter.warps1);
                                    Space_Interpreter.texts1 = tt';
                                    Space_Interpreter.text_warps =
                                      (uu___2.Space_Interpreter.text_warps);
                                    Space_Interpreter.graphemes1 =
                                      (uu___2.Space_Interpreter.graphemes1);
                                    Space_Interpreter.bytes_meta =
                                      (uu___2.Space_Interpreter.bytes_meta);
                                    Space_Interpreter.output =
                                      (uu___2.Space_Interpreter.output);
                                    Space_Interpreter.input =
                                      (uu___2.Space_Interpreter.input);
                                    Space_Interpreter.inst_ptr =
                                      (uu___2.Space_Interpreter.inst_ptr);
                                    Space_Interpreter.return_stack =
                                      (uu___2.Space_Interpreter.return_stack);
                                    Space_Interpreter.running =
                                      (uu___2.Space_Interpreter.running);
                                    Space_Interpreter.error =
                                      (uu___2.Space_Interpreter.error)
                                  })))))
       | uu___ -> StepError (m, "create-text: stack underflow"))
let step_text_byte_length (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | handle_cell::rest ->
           let handle = FStar_UInt64.v handle_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1
                    handle
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-byte-length: invalid handle")
            | FStar_Pervasives_Native.Some t ->
                let len = Space_Text_Create.text_byte_length t in
                let len_cell =
                  if len < (Prims.parse_int "18446744073709551616")
                  then FStar_UInt64.uint_to_t len
                  else Stdint.Uint64.zero in
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = (len_cell :: rest);
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (Space_Interpreter.update_current m u')))
       | uu___ -> StepError (m, "text-byte-length: stack underflow"))
let step_text_grapheme_count (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | handle_cell::rest ->
           let handle = FStar_UInt64.v handle_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1
                    handle
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-grapheme-count: invalid handle")
            | FStar_Pervasives_Native.Some t ->
                let count = Space_Text_Create.text_grapheme_count t in
                let count_cell =
                  if count < (Prims.parse_int "18446744073709551616")
                  then FStar_UInt64.uint_to_t count
                  else Stdint.Uint64.zero in
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = (count_cell :: rest);
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (Space_Interpreter.update_current m u')))
       | uu___ -> StepError (m, "text-grapheme-count: stack underflow"))
let step_text_is_simple (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | handle_cell::rest ->
           let handle = FStar_UInt64.v handle_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1
                    handle
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-is-simple: invalid handle")
            | FStar_Pervasives_Native.Some t ->
                let flag =
                  if Space_Text_Create.text_is_simple t
                  then Stdint.Uint64.one
                  else Stdint.Uint64.zero in
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = (flag :: rest);
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (Space_Interpreter.update_current m u')))
       | uu___ -> StepError (m, "text-is-simple: stack underflow"))
let step_text_equal (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | h2_cell::h1_cell::rest ->
           let h1 = FStar_UInt64.v h1_cell in
           let h2 = FStar_UInt64.v h2_cell in
           (match ((Space_Interpreter.get_text m.Space_Interpreter.texts1 h1),
                    (Space_Interpreter.get_text m.Space_Interpreter.texts1 h2))
            with
            | (FStar_Pervasives_Native.Some t1, FStar_Pervasives_Native.Some
               t2) ->
                let eq =
                  if Space_Text_Ops.text_equal t1 t2
                  then Stdint.Uint64.one
                  else Stdint.Uint64.zero in
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = (eq :: rest);
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (Space_Interpreter.update_current m u'))
            | (uu___, uu___1) -> StepError (m, "text-equal: invalid handle"))
       | uu___ -> StepError (m, "text-equal: stack underflow"))
let step_text_concat (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | h2_cell::h1_cell::rest ->
           let h1 = FStar_UInt64.v h1_cell in
           let h2 = FStar_UInt64.v h2_cell in
           (match ((Space_Interpreter.get_text m.Space_Interpreter.texts1 h1),
                    (Space_Interpreter.get_text m.Space_Interpreter.texts1 h2))
            with
            | (FStar_Pervasives_Native.Some t1, FStar_Pervasives_Native.Some
               t2) ->
                let t3 = Space_Text_Ops.text_concat t1 t2 in
                let uu___ =
                  Space_Interpreter.add_text m.Space_Interpreter.texts1 t3 in
                (match uu___ with
                 | (tt', h3) ->
                     let h3_cell =
                       if h3 < (Prims.parse_int "18446744073709551616")
                       then FStar_UInt64.uint_to_t h3
                       else Stdint.Uint64.zero in
                     let u' =
                       {
                         Space_Universe.id = (u.Space_Universe.id);
                         Space_Universe.name = (u.Space_Universe.name);
                         Space_Universe.discipline =
                           (u.Space_Universe.discipline);
                         Space_Universe.stack = (h3_cell :: rest);
                         Space_Universe.memory = (u.Space_Universe.memory);
                         Space_Universe.capacity =
                           (u.Space_Universe.capacity);
                         Space_Universe.state = (u.Space_Universe.state)
                       } in
                     StepOk
                       (Space_Interpreter.advance_ip
                          (let uu___1 = Space_Interpreter.update_current m u' in
                           {
                             Space_Interpreter.mworld =
                               (uu___1.Space_Interpreter.mworld);
                             Space_Interpreter.borrows =
                               (uu___1.Space_Interpreter.borrows);
                             Space_Interpreter.warps1 =
                               (uu___1.Space_Interpreter.warps1);
                             Space_Interpreter.texts1 = tt';
                             Space_Interpreter.text_warps =
                               (uu___1.Space_Interpreter.text_warps);
                             Space_Interpreter.graphemes1 =
                               (uu___1.Space_Interpreter.graphemes1);
                             Space_Interpreter.bytes_meta =
                               (uu___1.Space_Interpreter.bytes_meta);
                             Space_Interpreter.output =
                               (uu___1.Space_Interpreter.output);
                             Space_Interpreter.input =
                               (uu___1.Space_Interpreter.input);
                             Space_Interpreter.inst_ptr =
                               (uu___1.Space_Interpreter.inst_ptr);
                             Space_Interpreter.return_stack =
                               (uu___1.Space_Interpreter.return_stack);
                             Space_Interpreter.running =
                               (uu___1.Space_Interpreter.running);
                             Space_Interpreter.error =
                               (uu___1.Space_Interpreter.error)
                           })))
            | (uu___, uu___1) -> StepError (m, "text-concat: invalid handle"))
       | uu___ -> StepError (m, "text-concat: stack underflow"))
let step_text_slice (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | end_cell::start_cell::handle_cell::rest ->
           let handle = FStar_UInt64.v handle_cell in
           let start = FStar_UInt64.v start_cell in
           let finish = FStar_UInt64.v end_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1
                    handle
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-slice: invalid handle")
            | FStar_Pervasives_Native.Some t ->
                (match Space_Text_Ops.text_slice t start finish with
                 | FStar_Pervasives_Native.None ->
                     StepError (m, "text-slice: invalid range")
                 | FStar_Pervasives_Native.Some t' ->
                     let uu___ =
                       Space_Interpreter.add_text m.Space_Interpreter.texts1
                         t' in
                     (match uu___ with
                      | (tt', h') ->
                          let h_cell =
                            if h' < (Prims.parse_int "18446744073709551616")
                            then FStar_UInt64.uint_to_t h'
                            else Stdint.Uint64.zero in
                          let u' =
                            {
                              Space_Universe.id = (u.Space_Universe.id);
                              Space_Universe.name = (u.Space_Universe.name);
                              Space_Universe.discipline =
                                (u.Space_Universe.discipline);
                              Space_Universe.stack = (h_cell :: rest);
                              Space_Universe.memory =
                                (u.Space_Universe.memory);
                              Space_Universe.capacity =
                                (u.Space_Universe.capacity);
                              Space_Universe.state = (u.Space_Universe.state)
                            } in
                          StepOk
                            (Space_Interpreter.advance_ip
                               (let uu___1 =
                                  Space_Interpreter.update_current m u' in
                                {
                                  Space_Interpreter.mworld =
                                    (uu___1.Space_Interpreter.mworld);
                                  Space_Interpreter.borrows =
                                    (uu___1.Space_Interpreter.borrows);
                                  Space_Interpreter.warps1 =
                                    (uu___1.Space_Interpreter.warps1);
                                  Space_Interpreter.texts1 = tt';
                                  Space_Interpreter.text_warps =
                                    (uu___1.Space_Interpreter.text_warps);
                                  Space_Interpreter.graphemes1 =
                                    (uu___1.Space_Interpreter.graphemes1);
                                  Space_Interpreter.bytes_meta =
                                    (uu___1.Space_Interpreter.bytes_meta);
                                  Space_Interpreter.output =
                                    (uu___1.Space_Interpreter.output);
                                  Space_Interpreter.input =
                                    (uu___1.Space_Interpreter.input);
                                  Space_Interpreter.inst_ptr =
                                    (uu___1.Space_Interpreter.inst_ptr);
                                  Space_Interpreter.return_stack =
                                    (uu___1.Space_Interpreter.return_stack);
                                  Space_Interpreter.running =
                                    (uu___1.Space_Interpreter.running);
                                  Space_Interpreter.error =
                                    (uu___1.Space_Interpreter.error)
                                })))))
       | uu___ -> StepError (m, "text-slice: stack underflow"))
let step_bytes_alloc (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | n_cell::rest ->
           let n = FStar_UInt64.v n_cell in
           if n = Prims.int_zero
           then StepError (m, "bytes-alloc: cannot allocate zero bytes")
           else
             (let cells_needed =
                (n + (Prims.of_int (7))) / (Prims.of_int (8)) in
              let uu___1 =
                Space_Memory.mem_alloc u.Space_Universe.memory cells_needed in
              match uu___1 with
              | (mem', base_addr) ->
                  let addr_cell =
                    if base_addr < (Prims.parse_int "18446744073709551616")
                    then FStar_UInt64.uint_to_t base_addr
                    else Stdint.Uint64.zero in
                  let u' =
                    {
                      Space_Universe.id = (u.Space_Universe.id);
                      Space_Universe.name = (u.Space_Universe.name);
                      Space_Universe.discipline =
                        (u.Space_Universe.discipline);
                      Space_Universe.stack = (addr_cell :: rest);
                      Space_Universe.memory = mem';
                      Space_Universe.capacity = (u.Space_Universe.capacity);
                      Space_Universe.state = (u.Space_Universe.state)
                    } in
                  let bm' =
                    Space_Interpreter.record_bytes_alloc
                      m.Space_Interpreter.bytes_meta base_addr n in
                  StepOk
                    (Space_Interpreter.advance_ip
                       (let uu___2 = Space_Interpreter.update_current m u' in
                        {
                          Space_Interpreter.mworld =
                            (uu___2.Space_Interpreter.mworld);
                          Space_Interpreter.borrows =
                            (uu___2.Space_Interpreter.borrows);
                          Space_Interpreter.warps1 =
                            (uu___2.Space_Interpreter.warps1);
                          Space_Interpreter.texts1 =
                            (uu___2.Space_Interpreter.texts1);
                          Space_Interpreter.text_warps =
                            (uu___2.Space_Interpreter.text_warps);
                          Space_Interpreter.graphemes1 =
                            (uu___2.Space_Interpreter.graphemes1);
                          Space_Interpreter.bytes_meta = bm';
                          Space_Interpreter.output =
                            (uu___2.Space_Interpreter.output);
                          Space_Interpreter.input =
                            (uu___2.Space_Interpreter.input);
                          Space_Interpreter.inst_ptr =
                            (uu___2.Space_Interpreter.inst_ptr);
                          Space_Interpreter.return_stack =
                            (uu___2.Space_Interpreter.return_stack);
                          Space_Interpreter.running =
                            (uu___2.Space_Interpreter.running);
                          Space_Interpreter.error =
                            (uu___2.Space_Interpreter.error)
                        })))
       | uu___ -> StepError (m, "bytes-alloc: stack underflow"))
let step_bytes_len (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | addr_cell::rest ->
           let addr = FStar_UInt64.v addr_cell in
           (match Space_Interpreter.get_bytes_len
                    m.Space_Interpreter.bytes_meta addr
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "bytes-len: unknown allocation")
            | FStar_Pervasives_Native.Some len ->
                let len_cell =
                  if len < (Prims.parse_int "18446744073709551616")
                  then FStar_UInt64.uint_to_t len
                  else Stdint.Uint64.zero in
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = (len_cell :: rest);
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (Space_Interpreter.update_current m u')))
       | uu___ -> StepError (m, "bytes-len: stack underflow"))
let is_bytes_meta_prim (op : Space_Instruction.prim_op) : Prims.bool=
  match op with
  | Space_Instruction.PrimBytesAlloc -> true
  | Space_Instruction.PrimBytesLen -> true
  | uu___ -> false
let step_bytes_meta_prim (m : Space_Interpreter.machine)
  (op : Space_Instruction.prim_op) : step_result=
  match op with
  | Space_Instruction.PrimBytesAlloc -> step_bytes_alloc m
  | Space_Instruction.PrimBytesLen -> step_bytes_len m
  | uu___ -> StepError (m, "unknown bytes primitive")
let step_emit (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | char_cell::rest ->
           let char_val = FStar_UInt64.v char_cell in
           let u' =
             {
               Space_Universe.id = (u.Space_Universe.id);
               Space_Universe.name = (u.Space_Universe.name);
               Space_Universe.discipline = (u.Space_Universe.discipline);
               Space_Universe.stack = rest;
               Space_Universe.memory = (u.Space_Universe.memory);
               Space_Universe.capacity = (u.Space_Universe.capacity);
               Space_Universe.state = (u.Space_Universe.state)
             } in
           let m' =
             let uu___ = Space_Interpreter.update_current m u' in
             {
               Space_Interpreter.mworld = (uu___.Space_Interpreter.mworld);
               Space_Interpreter.borrows = (uu___.Space_Interpreter.borrows);
               Space_Interpreter.warps1 = (uu___.Space_Interpreter.warps1);
               Space_Interpreter.texts1 = (uu___.Space_Interpreter.texts1);
               Space_Interpreter.text_warps =
                 (uu___.Space_Interpreter.text_warps);
               Space_Interpreter.graphemes1 =
                 (uu___.Space_Interpreter.graphemes1);
               Space_Interpreter.bytes_meta =
                 (uu___.Space_Interpreter.bytes_meta);
               Space_Interpreter.output =
                 (FStar_List_Tot_Base.append m.Space_Interpreter.output
                    [char_val]);
               Space_Interpreter.input = (uu___.Space_Interpreter.input);
               Space_Interpreter.inst_ptr =
                 (uu___.Space_Interpreter.inst_ptr);
               Space_Interpreter.return_stack =
                 (uu___.Space_Interpreter.return_stack);
               Space_Interpreter.running = (uu___.Space_Interpreter.running);
               Space_Interpreter.error = (uu___.Space_Interpreter.error)
             } in
           StepOk (Space_Interpreter.advance_ip m')
       | uu___ -> StepError (m, "emit: stack underflow"))
let step_key (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match m.Space_Interpreter.input with
       | char_val::rest_input ->
           let char_cell =
             if char_val < (Prims.parse_int "18446744073709551616")
             then FStar_UInt64.uint_to_t char_val
             else Stdint.Uint64.zero in
           let u' =
             {
               Space_Universe.id = (u.Space_Universe.id);
               Space_Universe.name = (u.Space_Universe.name);
               Space_Universe.discipline = (u.Space_Universe.discipline);
               Space_Universe.stack = (char_cell :: (u.Space_Universe.stack));
               Space_Universe.memory = (u.Space_Universe.memory);
               Space_Universe.capacity = (u.Space_Universe.capacity);
               Space_Universe.state = (u.Space_Universe.state)
             } in
           let m' =
             let uu___ = Space_Interpreter.update_current m u' in
             {
               Space_Interpreter.mworld = (uu___.Space_Interpreter.mworld);
               Space_Interpreter.borrows = (uu___.Space_Interpreter.borrows);
               Space_Interpreter.warps1 = (uu___.Space_Interpreter.warps1);
               Space_Interpreter.texts1 = (uu___.Space_Interpreter.texts1);
               Space_Interpreter.text_warps =
                 (uu___.Space_Interpreter.text_warps);
               Space_Interpreter.graphemes1 =
                 (uu___.Space_Interpreter.graphemes1);
               Space_Interpreter.bytes_meta =
                 (uu___.Space_Interpreter.bytes_meta);
               Space_Interpreter.output = (uu___.Space_Interpreter.output);
               Space_Interpreter.input = rest_input;
               Space_Interpreter.inst_ptr =
                 (uu___.Space_Interpreter.inst_ptr);
               Space_Interpreter.return_stack =
                 (uu___.Space_Interpreter.return_stack);
               Space_Interpreter.running = (uu___.Space_Interpreter.running);
               Space_Interpreter.error = (uu___.Space_Interpreter.error)
             } in
           StepOk (Space_Interpreter.advance_ip m')
       | [] ->
           let u' =
             {
               Space_Universe.id = (u.Space_Universe.id);
               Space_Universe.name = (u.Space_Universe.name);
               Space_Universe.discipline = (u.Space_Universe.discipline);
               Space_Universe.stack =
                 ((Stdint.Uint64.of_string "0xFFFFFFFFFFFFFFFF") ::
                 (u.Space_Universe.stack));
               Space_Universe.memory = (u.Space_Universe.memory);
               Space_Universe.capacity = (u.Space_Universe.capacity);
               Space_Universe.state = (u.Space_Universe.state)
             } in
           StepOk
             (Space_Interpreter.advance_ip
                (Space_Interpreter.update_current m u')))
let is_io_prim (op : Space_Instruction.prim_op) : Prims.bool=
  match op with
  | Space_Instruction.PrimEmit -> true
  | Space_Instruction.PrimKey -> true
  | uu___ -> false
let step_io_prim (m : Space_Interpreter.machine)
  (op : Space_Instruction.prim_op) : step_result=
  match op with
  | Space_Instruction.PrimEmit -> step_emit m
  | Space_Instruction.PrimKey -> step_key m
  | uu___ -> StepError (m, "unknown I/O primitive")
let step_borrow_pointer (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match Space_Borrow.borrow_pointer u with
       | FStar_Pervasives_Native.None ->
           StepError (m, "borrow-pointer: failed")
       | FStar_Pervasives_Native.Some (b, u') ->
           let bs' = Space_Borrow.add_borrow m.Space_Interpreter.borrows b in
           StepOk
             (Space_Interpreter.advance_ip
                (let uu___ = Space_Interpreter.update_current m u' in
                 {
                   Space_Interpreter.mworld =
                     (uu___.Space_Interpreter.mworld);
                   Space_Interpreter.borrows = bs';
                   Space_Interpreter.warps1 =
                     (uu___.Space_Interpreter.warps1);
                   Space_Interpreter.texts1 =
                     (uu___.Space_Interpreter.texts1);
                   Space_Interpreter.text_warps =
                     (uu___.Space_Interpreter.text_warps);
                   Space_Interpreter.graphemes1 =
                     (uu___.Space_Interpreter.graphemes1);
                   Space_Interpreter.bytes_meta =
                     (uu___.Space_Interpreter.bytes_meta);
                   Space_Interpreter.output =
                     (uu___.Space_Interpreter.output);
                   Space_Interpreter.input = (uu___.Space_Interpreter.input);
                   Space_Interpreter.inst_ptr =
                     (uu___.Space_Interpreter.inst_ptr);
                   Space_Interpreter.return_stack =
                     (uu___.Space_Interpreter.return_stack);
                   Space_Interpreter.running =
                     (uu___.Space_Interpreter.running);
                   Space_Interpreter.error = (uu___.Space_Interpreter.error)
                 })))
let step_return_pointer (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match (m.Space_Interpreter.borrows).Space_Borrow.borrows with
       | [] -> StepError (m, "return-pointer: no active borrow")
       | b::rest ->
           (match Space_Borrow.return_borrowed b u with
            | FStar_Pervasives_Native.None ->
                StepError (m, "return-pointer: failed")
            | FStar_Pervasives_Native.Some u' ->
                let bs' = { Space_Borrow.borrows = rest } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (let uu___ = Space_Interpreter.update_current m u' in
                      {
                        Space_Interpreter.mworld =
                          (uu___.Space_Interpreter.mworld);
                        Space_Interpreter.borrows = bs';
                        Space_Interpreter.warps1 =
                          (uu___.Space_Interpreter.warps1);
                        Space_Interpreter.texts1 =
                          (uu___.Space_Interpreter.texts1);
                        Space_Interpreter.text_warps =
                          (uu___.Space_Interpreter.text_warps);
                        Space_Interpreter.graphemes1 =
                          (uu___.Space_Interpreter.graphemes1);
                        Space_Interpreter.bytes_meta =
                          (uu___.Space_Interpreter.bytes_meta);
                        Space_Interpreter.output =
                          (uu___.Space_Interpreter.output);
                        Space_Interpreter.input =
                          (uu___.Space_Interpreter.input);
                        Space_Interpreter.inst_ptr =
                          (uu___.Space_Interpreter.inst_ptr);
                        Space_Interpreter.return_stack =
                          (uu___.Space_Interpreter.return_stack);
                        Space_Interpreter.running =
                          (uu___.Space_Interpreter.running);
                        Space_Interpreter.error =
                          (uu___.Space_Interpreter.error)
                      }))))
let step_drop_pointer (m : Space_Interpreter.machine) : step_result=
  match (m.Space_Interpreter.borrows).Space_Borrow.borrows with
  | [] -> StepError (m, "drop-pointer: no active borrow")
  | b::rest ->
      let bs' = { Space_Borrow.borrows = rest } in
      StepOk
        (Space_Interpreter.advance_ip
           {
             Space_Interpreter.mworld = (m.Space_Interpreter.mworld);
             Space_Interpreter.borrows = bs';
             Space_Interpreter.warps1 = (m.Space_Interpreter.warps1);
             Space_Interpreter.texts1 = (m.Space_Interpreter.texts1);
             Space_Interpreter.text_warps = (m.Space_Interpreter.text_warps);
             Space_Interpreter.graphemes1 = (m.Space_Interpreter.graphemes1);
             Space_Interpreter.bytes_meta = (m.Space_Interpreter.bytes_meta);
             Space_Interpreter.output = (m.Space_Interpreter.output);
             Space_Interpreter.input = (m.Space_Interpreter.input);
             Space_Interpreter.inst_ptr = (m.Space_Interpreter.inst_ptr);
             Space_Interpreter.return_stack =
               (m.Space_Interpreter.return_stack);
             Space_Interpreter.running = (m.Space_Interpreter.running);
             Space_Interpreter.error = (m.Space_Interpreter.error)
           })
let step_fetch_borrowed (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match (m.Space_Interpreter.borrows).Space_Borrow.borrows with
       | [] -> StepError (m, "fetch-borrowed: no active borrow")
       | b::uu___ ->
           let um =
             {
               Space_BorrowOps.universe = u;
               Space_BorrowOps.memory = (u.Space_Universe.memory)
             } in
           (match Space_BorrowOps.fetch_borrowed b um with
            | FStar_Pervasives_Native.None ->
                StepError (m, "fetch-borrowed: failed")
            | FStar_Pervasives_Native.Some v ->
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = (v :: (u.Space_Universe.stack));
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (Space_Interpreter.update_current m u'))))
let step_store_borrowed (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | [] -> StepError (m, "store-borrowed: stack underflow")
       | v::rest ->
           (match (m.Space_Interpreter.borrows).Space_Borrow.borrows with
            | [] -> StepError (m, "store-borrowed: no active borrow")
            | b::uu___ ->
                let um =
                  {
                    Space_BorrowOps.universe = u;
                    Space_BorrowOps.memory = (u.Space_Universe.memory)
                  } in
                (match Space_BorrowOps.store_borrowed b v um with
                 | FStar_Pervasives_Native.None ->
                     StepError (m, "store-borrowed: failed")
                 | FStar_Pervasives_Native.Some um' ->
                     let u' =
                       let uu___1 = um'.Space_BorrowOps.universe in
                       {
                         Space_Universe.id = (uu___1.Space_Universe.id);
                         Space_Universe.name = (uu___1.Space_Universe.name);
                         Space_Universe.discipline =
                           (uu___1.Space_Universe.discipline);
                         Space_Universe.stack = rest;
                         Space_Universe.memory = (um'.Space_BorrowOps.memory);
                         Space_Universe.capacity =
                           (uu___1.Space_Universe.capacity);
                         Space_Universe.state = (uu___1.Space_Universe.state)
                       } in
                     StepOk
                       (Space_Interpreter.advance_ip
                          (Space_Interpreter.update_current m u')))))
let step_fetch_and_end (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match (m.Space_Interpreter.borrows).Space_Borrow.borrows with
       | [] -> StepError (m, "fetch-and-end: no active borrow")
       | b::rest ->
           let um =
             {
               Space_BorrowOps.universe = u;
               Space_BorrowOps.memory = (u.Space_Universe.memory)
             } in
           (match Space_BorrowOps.fetch_and_end b um with
            | FStar_Pervasives_Native.None ->
                StepError (m, "fetch-and-end: failed")
            | FStar_Pervasives_Native.Some (v, uu___) ->
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = (v :: (u.Space_Universe.stack));
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                let bs' = { Space_Borrow.borrows = rest } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (let uu___1 = Space_Interpreter.update_current m u' in
                      {
                        Space_Interpreter.mworld =
                          (uu___1.Space_Interpreter.mworld);
                        Space_Interpreter.borrows = bs';
                        Space_Interpreter.warps1 =
                          (uu___1.Space_Interpreter.warps1);
                        Space_Interpreter.texts1 =
                          (uu___1.Space_Interpreter.texts1);
                        Space_Interpreter.text_warps =
                          (uu___1.Space_Interpreter.text_warps);
                        Space_Interpreter.graphemes1 =
                          (uu___1.Space_Interpreter.graphemes1);
                        Space_Interpreter.bytes_meta =
                          (uu___1.Space_Interpreter.bytes_meta);
                        Space_Interpreter.output =
                          (uu___1.Space_Interpreter.output);
                        Space_Interpreter.input =
                          (uu___1.Space_Interpreter.input);
                        Space_Interpreter.inst_ptr =
                          (uu___1.Space_Interpreter.inst_ptr);
                        Space_Interpreter.return_stack =
                          (uu___1.Space_Interpreter.return_stack);
                        Space_Interpreter.running =
                          (uu___1.Space_Interpreter.running);
                        Space_Interpreter.error =
                          (uu___1.Space_Interpreter.error)
                      }))))
let step_store_and_end (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | [] -> StepError (m, "store-and-end: stack underflow")
       | v::rest ->
           (match (m.Space_Interpreter.borrows).Space_Borrow.borrows with
            | [] -> StepError (m, "store-and-end: no active borrow")
            | b::brest ->
                let um =
                  {
                    Space_BorrowOps.universe = u;
                    Space_BorrowOps.memory = (u.Space_Universe.memory)
                  } in
                (match Space_BorrowOps.store_and_end b v um with
                 | FStar_Pervasives_Native.None ->
                     StepError (m, "store-and-end: failed")
                 | FStar_Pervasives_Native.Some (um', uu___) ->
                     let u' =
                       let uu___1 = um'.Space_BorrowOps.universe in
                       {
                         Space_Universe.id = (uu___1.Space_Universe.id);
                         Space_Universe.name = (uu___1.Space_Universe.name);
                         Space_Universe.discipline =
                           (uu___1.Space_Universe.discipline);
                         Space_Universe.stack = rest;
                         Space_Universe.memory = (um'.Space_BorrowOps.memory);
                         Space_Universe.capacity =
                           (uu___1.Space_Universe.capacity);
                         Space_Universe.state = (uu___1.Space_Universe.state)
                       } in
                     let bs' = { Space_Borrow.borrows = brest } in
                     StepOk
                       (Space_Interpreter.advance_ip
                          (let uu___1 = Space_Interpreter.update_current m u' in
                           {
                             Space_Interpreter.mworld =
                               (uu___1.Space_Interpreter.mworld);
                             Space_Interpreter.borrows = bs';
                             Space_Interpreter.warps1 =
                               (uu___1.Space_Interpreter.warps1);
                             Space_Interpreter.texts1 =
                               (uu___1.Space_Interpreter.texts1);
                             Space_Interpreter.text_warps =
                               (uu___1.Space_Interpreter.text_warps);
                             Space_Interpreter.graphemes1 =
                               (uu___1.Space_Interpreter.graphemes1);
                             Space_Interpreter.bytes_meta =
                               (uu___1.Space_Interpreter.bytes_meta);
                             Space_Interpreter.output =
                               (uu___1.Space_Interpreter.output);
                             Space_Interpreter.input =
                               (uu___1.Space_Interpreter.input);
                             Space_Interpreter.inst_ptr =
                               (uu___1.Space_Interpreter.inst_ptr);
                             Space_Interpreter.return_stack =
                               (uu___1.Space_Interpreter.return_stack);
                             Space_Interpreter.running =
                               (uu___1.Space_Interpreter.running);
                             Space_Interpreter.error =
                               (uu___1.Space_Interpreter.error)
                           })))))
let step_offset_borrowed (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | [] -> StepError (m, "offset-borrowed: stack underflow")
       | offset::rest ->
           (match (m.Space_Interpreter.borrows).Space_Borrow.borrows with
            | [] -> StepError (m, "offset-borrowed: no active borrow")
            | b::brest ->
                let b' = Space_BorrowOps.offset_borrowed b offset in
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = rest;
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                let bs' = { Space_Borrow.borrows = (b' :: brest) } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (let uu___ = Space_Interpreter.update_current m u' in
                      {
                        Space_Interpreter.mworld =
                          (uu___.Space_Interpreter.mworld);
                        Space_Interpreter.borrows = bs';
                        Space_Interpreter.warps1 =
                          (uu___.Space_Interpreter.warps1);
                        Space_Interpreter.texts1 =
                          (uu___.Space_Interpreter.texts1);
                        Space_Interpreter.text_warps =
                          (uu___.Space_Interpreter.text_warps);
                        Space_Interpreter.graphemes1 =
                          (uu___.Space_Interpreter.graphemes1);
                        Space_Interpreter.bytes_meta =
                          (uu___.Space_Interpreter.bytes_meta);
                        Space_Interpreter.output =
                          (uu___.Space_Interpreter.output);
                        Space_Interpreter.input =
                          (uu___.Space_Interpreter.input);
                        Space_Interpreter.inst_ptr =
                          (uu___.Space_Interpreter.inst_ptr);
                        Space_Interpreter.return_stack =
                          (uu___.Space_Interpreter.return_stack);
                        Space_Interpreter.running =
                          (uu___.Space_Interpreter.running);
                        Space_Interpreter.error =
                          (uu___.Space_Interpreter.error)
                      }))))
let is_borrow_prim (op : Space_Instruction.prim_op) : Prims.bool=
  match op with
  | Space_Instruction.PrimBorrowPointer -> true
  | Space_Instruction.PrimReturnPointer -> true
  | Space_Instruction.PrimDropPointer -> true
  | Space_Instruction.PrimFetchBorrowed -> true
  | Space_Instruction.PrimStoreBorrowed -> true
  | Space_Instruction.PrimFetchAndEnd -> true
  | Space_Instruction.PrimStoreAndEnd -> true
  | Space_Instruction.PrimOffsetBorrowed -> true
  | uu___ -> false
let step_borrow_prim (m : Space_Interpreter.machine)
  (op : Space_Instruction.prim_op) : step_result=
  match op with
  | Space_Instruction.PrimBorrowPointer -> step_borrow_pointer m
  | Space_Instruction.PrimReturnPointer -> step_return_pointer m
  | Space_Instruction.PrimDropPointer -> step_drop_pointer m
  | Space_Instruction.PrimFetchBorrowed -> step_fetch_borrowed m
  | Space_Instruction.PrimStoreBorrowed -> step_store_borrowed m
  | Space_Instruction.PrimFetchAndEnd -> step_fetch_and_end m
  | Space_Instruction.PrimStoreAndEnd -> step_store_and_end m
  | Space_Instruction.PrimOffsetBorrowed -> step_offset_borrowed m
  | uu___ -> StepError (m, "unknown borrow primitive")
let step_warp_fetch (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match Space_Warp.get_implicit_warp m.Space_Interpreter.warps1 with
       | FStar_Pervasives_Native.None ->
           StepError (m, "warp-fetch: no active warp")
       | FStar_Pervasives_Native.Some w ->
           (match Space_World.find_universe m.Space_Interpreter.mworld
                    w.Space_Warp.target_id
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "warp-fetch: target universe not found")
            | FStar_Pervasives_Native.Some target ->
                let tu =
                  {
                    Space_WarpOps.universe = target;
                    Space_WarpOps.memory = (target.Space_Universe.memory)
                  } in
                (match Space_WarpOps.warp_fetch w tu with
                 | FStar_Pervasives_Native.None ->
                     StepError (m, "warp-fetch: failed")
                 | FStar_Pervasives_Native.Some v ->
                     let u' =
                       {
                         Space_Universe.id = (u.Space_Universe.id);
                         Space_Universe.name = (u.Space_Universe.name);
                         Space_Universe.discipline =
                           (u.Space_Universe.discipline);
                         Space_Universe.stack = (v ::
                           (u.Space_Universe.stack));
                         Space_Universe.memory = (u.Space_Universe.memory);
                         Space_Universe.capacity =
                           (u.Space_Universe.capacity);
                         Space_Universe.state = (u.Space_Universe.state)
                       } in
                     StepOk
                       (Space_Interpreter.advance_ip
                          (Space_Interpreter.update_current m u')))))
let step_warp_store (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | [] -> StepError (m, "warp-store: stack underflow")
       | v::rest ->
           (match Space_Warp.get_implicit_warp m.Space_Interpreter.warps1
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "warp-store: no active warp")
            | FStar_Pervasives_Native.Some w ->
                (match Space_World.find_universe m.Space_Interpreter.mworld
                         w.Space_Warp.target_id
                 with
                 | FStar_Pervasives_Native.None ->
                     StepError (m, "warp-store: target universe not found")
                 | FStar_Pervasives_Native.Some target ->
                     let tu =
                       {
                         Space_WarpOps.universe = target;
                         Space_WarpOps.memory =
                           (target.Space_Universe.memory)
                       } in
                     (match Space_WarpOps.warp_store w v tu with
                      | FStar_Pervasives_Native.None ->
                          StepError (m, "warp-store: failed")
                      | FStar_Pervasives_Native.Some tu' ->
                          let target' =
                            let uu___ = tu'.Space_WarpOps.universe in
                            {
                              Space_Universe.id = (uu___.Space_Universe.id);
                              Space_Universe.name =
                                (uu___.Space_Universe.name);
                              Space_Universe.discipline =
                                (uu___.Space_Universe.discipline);
                              Space_Universe.stack =
                                (uu___.Space_Universe.stack);
                              Space_Universe.memory =
                                (tu'.Space_WarpOps.memory);
                              Space_Universe.capacity =
                                (uu___.Space_Universe.capacity);
                              Space_Universe.state =
                                (uu___.Space_Universe.state)
                            } in
                          let w' =
                            Space_World.update_universe
                              m.Space_Interpreter.mworld target' in
                          let u' =
                            {
                              Space_Universe.id = (u.Space_Universe.id);
                              Space_Universe.name = (u.Space_Universe.name);
                              Space_Universe.discipline =
                                (u.Space_Universe.discipline);
                              Space_Universe.stack = rest;
                              Space_Universe.memory =
                                (u.Space_Universe.memory);
                              Space_Universe.capacity =
                                (u.Space_Universe.capacity);
                              Space_Universe.state = (u.Space_Universe.state)
                            } in
                          StepOk
                            (Space_Interpreter.advance_ip
                               (let uu___ =
                                  Space_Interpreter.update_current
                                    {
                                      Space_Interpreter.mworld = w';
                                      Space_Interpreter.borrows =
                                        (m.Space_Interpreter.borrows);
                                      Space_Interpreter.warps1 =
                                        (m.Space_Interpreter.warps1);
                                      Space_Interpreter.texts1 =
                                        (m.Space_Interpreter.texts1);
                                      Space_Interpreter.text_warps =
                                        (m.Space_Interpreter.text_warps);
                                      Space_Interpreter.graphemes1 =
                                        (m.Space_Interpreter.graphemes1);
                                      Space_Interpreter.bytes_meta =
                                        (m.Space_Interpreter.bytes_meta);
                                      Space_Interpreter.output =
                                        (m.Space_Interpreter.output);
                                      Space_Interpreter.input =
                                        (m.Space_Interpreter.input);
                                      Space_Interpreter.inst_ptr =
                                        (m.Space_Interpreter.inst_ptr);
                                      Space_Interpreter.return_stack =
                                        (m.Space_Interpreter.return_stack);
                                      Space_Interpreter.running =
                                        (m.Space_Interpreter.running);
                                      Space_Interpreter.error =
                                        (m.Space_Interpreter.error)
                                    } u' in
                                {
                                  Space_Interpreter.mworld = w';
                                  Space_Interpreter.borrows =
                                    (uu___.Space_Interpreter.borrows);
                                  Space_Interpreter.warps1 =
                                    (uu___.Space_Interpreter.warps1);
                                  Space_Interpreter.texts1 =
                                    (uu___.Space_Interpreter.texts1);
                                  Space_Interpreter.text_warps =
                                    (uu___.Space_Interpreter.text_warps);
                                  Space_Interpreter.graphemes1 =
                                    (uu___.Space_Interpreter.graphemes1);
                                  Space_Interpreter.bytes_meta =
                                    (uu___.Space_Interpreter.bytes_meta);
                                  Space_Interpreter.output =
                                    (uu___.Space_Interpreter.output);
                                  Space_Interpreter.input =
                                    (uu___.Space_Interpreter.input);
                                  Space_Interpreter.inst_ptr =
                                    (uu___.Space_Interpreter.inst_ptr);
                                  Space_Interpreter.return_stack =
                                    (uu___.Space_Interpreter.return_stack);
                                  Space_Interpreter.running =
                                    (uu___.Space_Interpreter.running);
                                  Space_Interpreter.error =
                                    (uu___.Space_Interpreter.error)
                                }))))))
let step_warp_advance (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | [] -> StepError (m, "warp-advance: stack underflow")
       | offset::rest ->
           (match Space_Warp.get_implicit_warp m.Space_Interpreter.warps1
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "warp-advance: no active warp")
            | FStar_Pervasives_Native.Some w ->
                let w' = Space_WarpOps.warp_advance w offset in
                let wt' =
                  Space_Warp.update_warp m.Space_Interpreter.warps1 w' in
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = rest;
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (let uu___ = Space_Interpreter.update_current m u' in
                      {
                        Space_Interpreter.mworld =
                          (uu___.Space_Interpreter.mworld);
                        Space_Interpreter.borrows =
                          (uu___.Space_Interpreter.borrows);
                        Space_Interpreter.warps1 = wt';
                        Space_Interpreter.texts1 =
                          (uu___.Space_Interpreter.texts1);
                        Space_Interpreter.text_warps =
                          (uu___.Space_Interpreter.text_warps);
                        Space_Interpreter.graphemes1 =
                          (uu___.Space_Interpreter.graphemes1);
                        Space_Interpreter.bytes_meta =
                          (uu___.Space_Interpreter.bytes_meta);
                        Space_Interpreter.output =
                          (uu___.Space_Interpreter.output);
                        Space_Interpreter.input =
                          (uu___.Space_Interpreter.input);
                        Space_Interpreter.inst_ptr =
                          (uu___.Space_Interpreter.inst_ptr);
                        Space_Interpreter.return_stack =
                          (uu___.Space_Interpreter.return_stack);
                        Space_Interpreter.running =
                          (uu___.Space_Interpreter.running);
                        Space_Interpreter.error =
                          (uu___.Space_Interpreter.error)
                      }))))
let step_warp_follow (m : Space_Interpreter.machine) : step_result=
  match Space_Warp.get_implicit_warp m.Space_Interpreter.warps1 with
  | FStar_Pervasives_Native.None ->
      StepError (m, "warp-follow: no active warp")
  | FStar_Pervasives_Native.Some w ->
      (match Space_World.find_universe m.Space_Interpreter.mworld
               w.Space_Warp.target_id
       with
       | FStar_Pervasives_Native.None ->
           StepError (m, "warp-follow: target universe not found")
       | FStar_Pervasives_Native.Some target ->
           let tu =
             {
               Space_WarpOps.universe = target;
               Space_WarpOps.memory = (target.Space_Universe.memory)
             } in
           (match Space_WarpOps.warp_follow w tu with
            | FStar_Pervasives_Native.None ->
                StepError (m, "warp-follow: failed")
            | FStar_Pervasives_Native.Some w' ->
                let wt' =
                  Space_Warp.update_warp m.Space_Interpreter.warps1 w' in
                StepOk
                  (Space_Interpreter.advance_ip
                     {
                       Space_Interpreter.mworld =
                         (m.Space_Interpreter.mworld);
                       Space_Interpreter.borrows =
                         (m.Space_Interpreter.borrows);
                       Space_Interpreter.warps1 = wt';
                       Space_Interpreter.texts1 =
                         (m.Space_Interpreter.texts1);
                       Space_Interpreter.text_warps =
                         (m.Space_Interpreter.text_warps);
                       Space_Interpreter.graphemes1 =
                         (m.Space_Interpreter.graphemes1);
                       Space_Interpreter.bytes_meta =
                         (m.Space_Interpreter.bytes_meta);
                       Space_Interpreter.output =
                         (m.Space_Interpreter.output);
                       Space_Interpreter.input = (m.Space_Interpreter.input);
                       Space_Interpreter.inst_ptr =
                         (m.Space_Interpreter.inst_ptr);
                       Space_Interpreter.return_stack =
                         (m.Space_Interpreter.return_stack);
                       Space_Interpreter.running =
                         (m.Space_Interpreter.running);
                       Space_Interpreter.error = (m.Space_Interpreter.error)
                     })))
let step_warp_position (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match Space_Warp.get_implicit_warp m.Space_Interpreter.warps1 with
       | FStar_Pervasives_Native.None ->
           StepError (m, "warp-position: no active warp")
       | FStar_Pervasives_Native.Some w ->
           let pos = Space_Warp.warp_position w in
           let u' =
             {
               Space_Universe.id = (u.Space_Universe.id);
               Space_Universe.name = (u.Space_Universe.name);
               Space_Universe.discipline = (u.Space_Universe.discipline);
               Space_Universe.stack = (pos :: (u.Space_Universe.stack));
               Space_Universe.memory = (u.Space_Universe.memory);
               Space_Universe.capacity = (u.Space_Universe.capacity);
               Space_Universe.state = (u.Space_Universe.state)
             } in
           StepOk
             (Space_Interpreter.advance_ip
                (Space_Interpreter.update_current m u')))
let step_warp_restore (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | [] -> StepError (m, "warp-restore: stack underflow")
       | pos::rest ->
           (match Space_Warp.get_implicit_warp m.Space_Interpreter.warps1
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "warp-restore: no active warp")
            | FStar_Pervasives_Native.Some w ->
                (match Space_WarpOps.warp_restore w pos with
                 | FStar_Pervasives_Native.None ->
                     StepError (m, "warp-restore: position not saved")
                 | FStar_Pervasives_Native.Some w' ->
                     let wt' =
                       Space_Warp.update_warp m.Space_Interpreter.warps1 w' in
                     let u' =
                       {
                         Space_Universe.id = (u.Space_Universe.id);
                         Space_Universe.name = (u.Space_Universe.name);
                         Space_Universe.discipline =
                           (u.Space_Universe.discipline);
                         Space_Universe.stack = rest;
                         Space_Universe.memory = (u.Space_Universe.memory);
                         Space_Universe.capacity =
                           (u.Space_Universe.capacity);
                         Space_Universe.state = (u.Space_Universe.state)
                       } in
                     StepOk
                       (Space_Interpreter.advance_ip
                          (let uu___ = Space_Interpreter.update_current m u' in
                           {
                             Space_Interpreter.mworld =
                               (uu___.Space_Interpreter.mworld);
                             Space_Interpreter.borrows =
                               (uu___.Space_Interpreter.borrows);
                             Space_Interpreter.warps1 = wt';
                             Space_Interpreter.texts1 =
                               (uu___.Space_Interpreter.texts1);
                             Space_Interpreter.text_warps =
                               (uu___.Space_Interpreter.text_warps);
                             Space_Interpreter.graphemes1 =
                               (uu___.Space_Interpreter.graphemes1);
                             Space_Interpreter.bytes_meta =
                               (uu___.Space_Interpreter.bytes_meta);
                             Space_Interpreter.output =
                               (uu___.Space_Interpreter.output);
                             Space_Interpreter.input =
                               (uu___.Space_Interpreter.input);
                             Space_Interpreter.inst_ptr =
                               (uu___.Space_Interpreter.inst_ptr);
                             Space_Interpreter.return_stack =
                               (uu___.Space_Interpreter.return_stack);
                             Space_Interpreter.running =
                               (uu___.Space_Interpreter.running);
                             Space_Interpreter.error =
                               (uu___.Space_Interpreter.error)
                           })))))
let step_warp_null (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match Space_Warp.get_implicit_warp m.Space_Interpreter.warps1 with
       | FStar_Pervasives_Native.None ->
           StepError (m, "warp-null: no active warp")
       | FStar_Pervasives_Native.Some w ->
           let flag =
             if Space_WarpOps.warp_at_null w
             then Stdint.Uint64.one
             else Stdint.Uint64.zero in
           let u' =
             {
               Space_Universe.id = (u.Space_Universe.id);
               Space_Universe.name = (u.Space_Universe.name);
               Space_Universe.discipline = (u.Space_Universe.discipline);
               Space_Universe.stack = (flag :: (u.Space_Universe.stack));
               Space_Universe.memory = (u.Space_Universe.memory);
               Space_Universe.capacity = (u.Space_Universe.capacity);
               Space_Universe.state = (u.Space_Universe.state)
             } in
           StepOk
             (Space_Interpreter.advance_ip
                (Space_Interpreter.update_current m u')))
let is_warp_prim (op : Space_Instruction.prim_op) : Prims.bool=
  match op with
  | Space_Instruction.PrimWarpFetch -> true
  | Space_Instruction.PrimWarpStore -> true
  | Space_Instruction.PrimWarpAdvance -> true
  | Space_Instruction.PrimWarpFollow -> true
  | Space_Instruction.PrimWarpPosition -> true
  | Space_Instruction.PrimWarpRestore -> true
  | Space_Instruction.PrimWarpNull -> true
  | uu___ -> false
let step_warp_prim (m : Space_Interpreter.machine)
  (op : Space_Instruction.prim_op) : step_result=
  match op with
  | Space_Instruction.PrimWarpFetch -> step_warp_fetch m
  | Space_Instruction.PrimWarpStore -> step_warp_store m
  | Space_Instruction.PrimWarpAdvance -> step_warp_advance m
  | Space_Instruction.PrimWarpFollow -> step_warp_follow m
  | Space_Instruction.PrimWarpPosition -> step_warp_position m
  | Space_Instruction.PrimWarpRestore -> step_warp_restore m
  | Space_Instruction.PrimWarpNull -> step_warp_null m
  | uu___ -> StepError (m, "unknown warp primitive")
let is_text_prim (op : Space_Instruction.prim_op) : Prims.bool=
  match op with
  | Space_Instruction.PrimCreateText -> true
  | Space_Instruction.PrimTextByteLength -> true
  | Space_Instruction.PrimTextGraphemeCount -> true
  | Space_Instruction.PrimTextIsSimple -> true
  | Space_Instruction.PrimTextGraphemeAt -> true
  | Space_Instruction.PrimTextGraphemeFirst -> true
  | Space_Instruction.PrimTextGraphemeLast -> true
  | Space_Instruction.PrimTextSlice -> true
  | Space_Instruction.PrimTextConcat -> true
  | Space_Instruction.PrimTextEqual -> true
  | Space_Instruction.PrimTextCompare -> true
  | Space_Instruction.PrimTextWarpHasGrapheme -> true
  | Space_Instruction.PrimTextWarpCurrentGrapheme -> true
  | Space_Instruction.PrimTextWarpNextGrapheme -> true
  | Space_Instruction.PrimTextWarpGraphemeIndex -> true
  | Space_Instruction.PrimTextWarpGotoGrapheme -> true
  | Space_Instruction.PrimGraphemeByteLength -> true
  | Space_Instruction.PrimGraphemeIsAscii -> true
  | Space_Instruction.PrimGraphemeCodePoints -> true
  | Space_Instruction.PrimTextCodePointCount -> true
  | Space_Instruction.PrimTextCodePointAt -> true
  | Space_Instruction.PrimTextNormalizeNfc -> true
  | Space_Instruction.PrimTextNormalizeNfd -> true
  | Space_Instruction.PrimTextNormalizeNfkc -> true
  | Space_Instruction.PrimTextNormalizeNfkd -> true
  | Space_Instruction.PrimTextToUpper -> true
  | Space_Instruction.PrimTextToLower -> true
  | Space_Instruction.PrimTextToTitle -> true
  | Space_Instruction.PrimEmitGrapheme -> true
  | uu___ -> false
let step_text_normalize_nfc (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | handle_cell::rest ->
           let handle = FStar_UInt64.v handle_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1
                    handle
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-normalize-nfc: invalid handle")
            | FStar_Pervasives_Native.Some t ->
                (match Space_Text_Normalize.normalize_nfc t with
                 | FStar_Pervasives_Native.None ->
                     StepError
                       (m, "text-normalize-nfc: normalization failed")
                 | FStar_Pervasives_Native.Some t' ->
                     let uu___ =
                       Space_Interpreter.add_text m.Space_Interpreter.texts1
                         t' in
                     (match uu___ with
                      | (tt', h') ->
                          let h_cell =
                            if h' < (Prims.parse_int "18446744073709551616")
                            then FStar_UInt64.uint_to_t h'
                            else Stdint.Uint64.zero in
                          let u' =
                            {
                              Space_Universe.id = (u.Space_Universe.id);
                              Space_Universe.name = (u.Space_Universe.name);
                              Space_Universe.discipline =
                                (u.Space_Universe.discipline);
                              Space_Universe.stack = (h_cell :: rest);
                              Space_Universe.memory =
                                (u.Space_Universe.memory);
                              Space_Universe.capacity =
                                (u.Space_Universe.capacity);
                              Space_Universe.state = (u.Space_Universe.state)
                            } in
                          StepOk
                            (Space_Interpreter.advance_ip
                               (let uu___1 =
                                  Space_Interpreter.update_current m u' in
                                {
                                  Space_Interpreter.mworld =
                                    (uu___1.Space_Interpreter.mworld);
                                  Space_Interpreter.borrows =
                                    (uu___1.Space_Interpreter.borrows);
                                  Space_Interpreter.warps1 =
                                    (uu___1.Space_Interpreter.warps1);
                                  Space_Interpreter.texts1 = tt';
                                  Space_Interpreter.text_warps =
                                    (uu___1.Space_Interpreter.text_warps);
                                  Space_Interpreter.graphemes1 =
                                    (uu___1.Space_Interpreter.graphemes1);
                                  Space_Interpreter.bytes_meta =
                                    (uu___1.Space_Interpreter.bytes_meta);
                                  Space_Interpreter.output =
                                    (uu___1.Space_Interpreter.output);
                                  Space_Interpreter.input =
                                    (uu___1.Space_Interpreter.input);
                                  Space_Interpreter.inst_ptr =
                                    (uu___1.Space_Interpreter.inst_ptr);
                                  Space_Interpreter.return_stack =
                                    (uu___1.Space_Interpreter.return_stack);
                                  Space_Interpreter.running =
                                    (uu___1.Space_Interpreter.running);
                                  Space_Interpreter.error =
                                    (uu___1.Space_Interpreter.error)
                                })))))
       | uu___ -> StepError (m, "text-normalize-nfc: stack underflow"))
let step_text_normalize_nfd (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | handle_cell::rest ->
           let handle = FStar_UInt64.v handle_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1
                    handle
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-normalize-nfd: invalid handle")
            | FStar_Pervasives_Native.Some t ->
                (match Space_Text_Normalize.normalize_nfd t with
                 | FStar_Pervasives_Native.None ->
                     StepError
                       (m, "text-normalize-nfd: normalization failed")
                 | FStar_Pervasives_Native.Some t' ->
                     let uu___ =
                       Space_Interpreter.add_text m.Space_Interpreter.texts1
                         t' in
                     (match uu___ with
                      | (tt', h') ->
                          let h_cell =
                            if h' < (Prims.parse_int "18446744073709551616")
                            then FStar_UInt64.uint_to_t h'
                            else Stdint.Uint64.zero in
                          let u' =
                            {
                              Space_Universe.id = (u.Space_Universe.id);
                              Space_Universe.name = (u.Space_Universe.name);
                              Space_Universe.discipline =
                                (u.Space_Universe.discipline);
                              Space_Universe.stack = (h_cell :: rest);
                              Space_Universe.memory =
                                (u.Space_Universe.memory);
                              Space_Universe.capacity =
                                (u.Space_Universe.capacity);
                              Space_Universe.state = (u.Space_Universe.state)
                            } in
                          StepOk
                            (Space_Interpreter.advance_ip
                               (let uu___1 =
                                  Space_Interpreter.update_current m u' in
                                {
                                  Space_Interpreter.mworld =
                                    (uu___1.Space_Interpreter.mworld);
                                  Space_Interpreter.borrows =
                                    (uu___1.Space_Interpreter.borrows);
                                  Space_Interpreter.warps1 =
                                    (uu___1.Space_Interpreter.warps1);
                                  Space_Interpreter.texts1 = tt';
                                  Space_Interpreter.text_warps =
                                    (uu___1.Space_Interpreter.text_warps);
                                  Space_Interpreter.graphemes1 =
                                    (uu___1.Space_Interpreter.graphemes1);
                                  Space_Interpreter.bytes_meta =
                                    (uu___1.Space_Interpreter.bytes_meta);
                                  Space_Interpreter.output =
                                    (uu___1.Space_Interpreter.output);
                                  Space_Interpreter.input =
                                    (uu___1.Space_Interpreter.input);
                                  Space_Interpreter.inst_ptr =
                                    (uu___1.Space_Interpreter.inst_ptr);
                                  Space_Interpreter.return_stack =
                                    (uu___1.Space_Interpreter.return_stack);
                                  Space_Interpreter.running =
                                    (uu___1.Space_Interpreter.running);
                                  Space_Interpreter.error =
                                    (uu___1.Space_Interpreter.error)
                                })))))
       | uu___ -> StepError (m, "text-normalize-nfd: stack underflow"))
let step_text_normalize_nfkc (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | handle_cell::rest ->
           let handle = FStar_UInt64.v handle_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1
                    handle
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-normalize-nfkc: invalid handle")
            | FStar_Pervasives_Native.Some t ->
                (match Space_Text_Normalize.normalize_nfkc t with
                 | FStar_Pervasives_Native.None ->
                     StepError
                       (m, "text-normalize-nfkc: normalization failed")
                 | FStar_Pervasives_Native.Some t' ->
                     let uu___ =
                       Space_Interpreter.add_text m.Space_Interpreter.texts1
                         t' in
                     (match uu___ with
                      | (tt', h') ->
                          let h_cell =
                            if h' < (Prims.parse_int "18446744073709551616")
                            then FStar_UInt64.uint_to_t h'
                            else Stdint.Uint64.zero in
                          let u' =
                            {
                              Space_Universe.id = (u.Space_Universe.id);
                              Space_Universe.name = (u.Space_Universe.name);
                              Space_Universe.discipline =
                                (u.Space_Universe.discipline);
                              Space_Universe.stack = (h_cell :: rest);
                              Space_Universe.memory =
                                (u.Space_Universe.memory);
                              Space_Universe.capacity =
                                (u.Space_Universe.capacity);
                              Space_Universe.state = (u.Space_Universe.state)
                            } in
                          StepOk
                            (Space_Interpreter.advance_ip
                               (let uu___1 =
                                  Space_Interpreter.update_current m u' in
                                {
                                  Space_Interpreter.mworld =
                                    (uu___1.Space_Interpreter.mworld);
                                  Space_Interpreter.borrows =
                                    (uu___1.Space_Interpreter.borrows);
                                  Space_Interpreter.warps1 =
                                    (uu___1.Space_Interpreter.warps1);
                                  Space_Interpreter.texts1 = tt';
                                  Space_Interpreter.text_warps =
                                    (uu___1.Space_Interpreter.text_warps);
                                  Space_Interpreter.graphemes1 =
                                    (uu___1.Space_Interpreter.graphemes1);
                                  Space_Interpreter.bytes_meta =
                                    (uu___1.Space_Interpreter.bytes_meta);
                                  Space_Interpreter.output =
                                    (uu___1.Space_Interpreter.output);
                                  Space_Interpreter.input =
                                    (uu___1.Space_Interpreter.input);
                                  Space_Interpreter.inst_ptr =
                                    (uu___1.Space_Interpreter.inst_ptr);
                                  Space_Interpreter.return_stack =
                                    (uu___1.Space_Interpreter.return_stack);
                                  Space_Interpreter.running =
                                    (uu___1.Space_Interpreter.running);
                                  Space_Interpreter.error =
                                    (uu___1.Space_Interpreter.error)
                                })))))
       | uu___ -> StepError (m, "text-normalize-nfkc: stack underflow"))
let step_text_normalize_nfkd (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | handle_cell::rest ->
           let handle = FStar_UInt64.v handle_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1
                    handle
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-normalize-nfkd: invalid handle")
            | FStar_Pervasives_Native.Some t ->
                (match Space_Text_Normalize.normalize_nfkd t with
                 | FStar_Pervasives_Native.None ->
                     StepError
                       (m, "text-normalize-nfkd: normalization failed")
                 | FStar_Pervasives_Native.Some t' ->
                     let uu___ =
                       Space_Interpreter.add_text m.Space_Interpreter.texts1
                         t' in
                     (match uu___ with
                      | (tt', h') ->
                          let h_cell =
                            if h' < (Prims.parse_int "18446744073709551616")
                            then FStar_UInt64.uint_to_t h'
                            else Stdint.Uint64.zero in
                          let u' =
                            {
                              Space_Universe.id = (u.Space_Universe.id);
                              Space_Universe.name = (u.Space_Universe.name);
                              Space_Universe.discipline =
                                (u.Space_Universe.discipline);
                              Space_Universe.stack = (h_cell :: rest);
                              Space_Universe.memory =
                                (u.Space_Universe.memory);
                              Space_Universe.capacity =
                                (u.Space_Universe.capacity);
                              Space_Universe.state = (u.Space_Universe.state)
                            } in
                          StepOk
                            (Space_Interpreter.advance_ip
                               (let uu___1 =
                                  Space_Interpreter.update_current m u' in
                                {
                                  Space_Interpreter.mworld =
                                    (uu___1.Space_Interpreter.mworld);
                                  Space_Interpreter.borrows =
                                    (uu___1.Space_Interpreter.borrows);
                                  Space_Interpreter.warps1 =
                                    (uu___1.Space_Interpreter.warps1);
                                  Space_Interpreter.texts1 = tt';
                                  Space_Interpreter.text_warps =
                                    (uu___1.Space_Interpreter.text_warps);
                                  Space_Interpreter.graphemes1 =
                                    (uu___1.Space_Interpreter.graphemes1);
                                  Space_Interpreter.bytes_meta =
                                    (uu___1.Space_Interpreter.bytes_meta);
                                  Space_Interpreter.output =
                                    (uu___1.Space_Interpreter.output);
                                  Space_Interpreter.input =
                                    (uu___1.Space_Interpreter.input);
                                  Space_Interpreter.inst_ptr =
                                    (uu___1.Space_Interpreter.inst_ptr);
                                  Space_Interpreter.return_stack =
                                    (uu___1.Space_Interpreter.return_stack);
                                  Space_Interpreter.running =
                                    (uu___1.Space_Interpreter.running);
                                  Space_Interpreter.error =
                                    (uu___1.Space_Interpreter.error)
                                })))))
       | uu___ -> StepError (m, "text-normalize-nfkd: stack underflow"))
let step_text_to_upper (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | handle_cell::rest ->
           let handle = FStar_UInt64.v handle_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1
                    handle
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-to-upper: invalid handle")
            | FStar_Pervasives_Native.Some t ->
                (match Space_Text_Case.text_to_upper t with
                 | FStar_Pervasives_Native.None ->
                     StepError (m, "text-to-upper: conversion failed")
                 | FStar_Pervasives_Native.Some t' ->
                     let uu___ =
                       Space_Interpreter.add_text m.Space_Interpreter.texts1
                         t' in
                     (match uu___ with
                      | (tt', h') ->
                          let h_cell =
                            if h' < (Prims.parse_int "18446744073709551616")
                            then FStar_UInt64.uint_to_t h'
                            else Stdint.Uint64.zero in
                          let u' =
                            {
                              Space_Universe.id = (u.Space_Universe.id);
                              Space_Universe.name = (u.Space_Universe.name);
                              Space_Universe.discipline =
                                (u.Space_Universe.discipline);
                              Space_Universe.stack = (h_cell :: rest);
                              Space_Universe.memory =
                                (u.Space_Universe.memory);
                              Space_Universe.capacity =
                                (u.Space_Universe.capacity);
                              Space_Universe.state = (u.Space_Universe.state)
                            } in
                          StepOk
                            (Space_Interpreter.advance_ip
                               (let uu___1 =
                                  Space_Interpreter.update_current m u' in
                                {
                                  Space_Interpreter.mworld =
                                    (uu___1.Space_Interpreter.mworld);
                                  Space_Interpreter.borrows =
                                    (uu___1.Space_Interpreter.borrows);
                                  Space_Interpreter.warps1 =
                                    (uu___1.Space_Interpreter.warps1);
                                  Space_Interpreter.texts1 = tt';
                                  Space_Interpreter.text_warps =
                                    (uu___1.Space_Interpreter.text_warps);
                                  Space_Interpreter.graphemes1 =
                                    (uu___1.Space_Interpreter.graphemes1);
                                  Space_Interpreter.bytes_meta =
                                    (uu___1.Space_Interpreter.bytes_meta);
                                  Space_Interpreter.output =
                                    (uu___1.Space_Interpreter.output);
                                  Space_Interpreter.input =
                                    (uu___1.Space_Interpreter.input);
                                  Space_Interpreter.inst_ptr =
                                    (uu___1.Space_Interpreter.inst_ptr);
                                  Space_Interpreter.return_stack =
                                    (uu___1.Space_Interpreter.return_stack);
                                  Space_Interpreter.running =
                                    (uu___1.Space_Interpreter.running);
                                  Space_Interpreter.error =
                                    (uu___1.Space_Interpreter.error)
                                })))))
       | uu___ -> StepError (m, "text-to-upper: stack underflow"))
let step_text_to_lower (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | handle_cell::rest ->
           let handle = FStar_UInt64.v handle_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1
                    handle
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-to-lower: invalid handle")
            | FStar_Pervasives_Native.Some t ->
                (match Space_Text_Case.text_to_lower t with
                 | FStar_Pervasives_Native.None ->
                     StepError (m, "text-to-lower: conversion failed")
                 | FStar_Pervasives_Native.Some t' ->
                     let uu___ =
                       Space_Interpreter.add_text m.Space_Interpreter.texts1
                         t' in
                     (match uu___ with
                      | (tt', h') ->
                          let h_cell =
                            if h' < (Prims.parse_int "18446744073709551616")
                            then FStar_UInt64.uint_to_t h'
                            else Stdint.Uint64.zero in
                          let u' =
                            {
                              Space_Universe.id = (u.Space_Universe.id);
                              Space_Universe.name = (u.Space_Universe.name);
                              Space_Universe.discipline =
                                (u.Space_Universe.discipline);
                              Space_Universe.stack = (h_cell :: rest);
                              Space_Universe.memory =
                                (u.Space_Universe.memory);
                              Space_Universe.capacity =
                                (u.Space_Universe.capacity);
                              Space_Universe.state = (u.Space_Universe.state)
                            } in
                          StepOk
                            (Space_Interpreter.advance_ip
                               (let uu___1 =
                                  Space_Interpreter.update_current m u' in
                                {
                                  Space_Interpreter.mworld =
                                    (uu___1.Space_Interpreter.mworld);
                                  Space_Interpreter.borrows =
                                    (uu___1.Space_Interpreter.borrows);
                                  Space_Interpreter.warps1 =
                                    (uu___1.Space_Interpreter.warps1);
                                  Space_Interpreter.texts1 = tt';
                                  Space_Interpreter.text_warps =
                                    (uu___1.Space_Interpreter.text_warps);
                                  Space_Interpreter.graphemes1 =
                                    (uu___1.Space_Interpreter.graphemes1);
                                  Space_Interpreter.bytes_meta =
                                    (uu___1.Space_Interpreter.bytes_meta);
                                  Space_Interpreter.output =
                                    (uu___1.Space_Interpreter.output);
                                  Space_Interpreter.input =
                                    (uu___1.Space_Interpreter.input);
                                  Space_Interpreter.inst_ptr =
                                    (uu___1.Space_Interpreter.inst_ptr);
                                  Space_Interpreter.return_stack =
                                    (uu___1.Space_Interpreter.return_stack);
                                  Space_Interpreter.running =
                                    (uu___1.Space_Interpreter.running);
                                  Space_Interpreter.error =
                                    (uu___1.Space_Interpreter.error)
                                })))))
       | uu___ -> StepError (m, "text-to-lower: stack underflow"))
let step_text_to_title (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | handle_cell::rest ->
           let handle = FStar_UInt64.v handle_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1
                    handle
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-to-title: invalid handle")
            | FStar_Pervasives_Native.Some t ->
                (match Space_Text_Case.text_to_title t with
                 | FStar_Pervasives_Native.None ->
                     StepError (m, "text-to-title: conversion failed")
                 | FStar_Pervasives_Native.Some t' ->
                     let uu___ =
                       Space_Interpreter.add_text m.Space_Interpreter.texts1
                         t' in
                     (match uu___ with
                      | (tt', h') ->
                          let h_cell =
                            if h' < (Prims.parse_int "18446744073709551616")
                            then FStar_UInt64.uint_to_t h'
                            else Stdint.Uint64.zero in
                          let u' =
                            {
                              Space_Universe.id = (u.Space_Universe.id);
                              Space_Universe.name = (u.Space_Universe.name);
                              Space_Universe.discipline =
                                (u.Space_Universe.discipline);
                              Space_Universe.stack = (h_cell :: rest);
                              Space_Universe.memory =
                                (u.Space_Universe.memory);
                              Space_Universe.capacity =
                                (u.Space_Universe.capacity);
                              Space_Universe.state = (u.Space_Universe.state)
                            } in
                          StepOk
                            (Space_Interpreter.advance_ip
                               (let uu___1 =
                                  Space_Interpreter.update_current m u' in
                                {
                                  Space_Interpreter.mworld =
                                    (uu___1.Space_Interpreter.mworld);
                                  Space_Interpreter.borrows =
                                    (uu___1.Space_Interpreter.borrows);
                                  Space_Interpreter.warps1 =
                                    (uu___1.Space_Interpreter.warps1);
                                  Space_Interpreter.texts1 = tt';
                                  Space_Interpreter.text_warps =
                                    (uu___1.Space_Interpreter.text_warps);
                                  Space_Interpreter.graphemes1 =
                                    (uu___1.Space_Interpreter.graphemes1);
                                  Space_Interpreter.bytes_meta =
                                    (uu___1.Space_Interpreter.bytes_meta);
                                  Space_Interpreter.output =
                                    (uu___1.Space_Interpreter.output);
                                  Space_Interpreter.input =
                                    (uu___1.Space_Interpreter.input);
                                  Space_Interpreter.inst_ptr =
                                    (uu___1.Space_Interpreter.inst_ptr);
                                  Space_Interpreter.return_stack =
                                    (uu___1.Space_Interpreter.return_stack);
                                  Space_Interpreter.running =
                                    (uu___1.Space_Interpreter.running);
                                  Space_Interpreter.error =
                                    (uu___1.Space_Interpreter.error)
                                })))))
       | uu___ -> StepError (m, "text-to-title: stack underflow"))
let step_text_warp_has_grapheme (m : Space_Interpreter.machine) :
  step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | wh_cell::rest ->
           let wh = FStar_UInt64.v wh_cell in
           (match Space_Interpreter.get_text_warp
                    m.Space_Interpreter.text_warps wh
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-warp-has-grapheme: invalid warp handle")
            | FStar_Pervasives_Native.Some tw ->
                let flag =
                  if Space_Text_Warp.text_warp_has_grapheme tw
                  then Stdint.Uint64.one
                  else Stdint.Uint64.zero in
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = (flag :: rest);
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (Space_Interpreter.update_current m u')))
       | uu___ -> StepError (m, "text-warp-has-grapheme: stack underflow"))
let step_text_warp_current_grapheme (m : Space_Interpreter.machine) :
  step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | wh_cell::rest ->
           let wh = FStar_UInt64.v wh_cell in
           (match Space_Interpreter.get_text_warp
                    m.Space_Interpreter.text_warps wh
            with
            | FStar_Pervasives_Native.None ->
                StepError
                  (m, "text-warp-current-grapheme: invalid warp handle")
            | FStar_Pervasives_Native.Some tw ->
                (match Space_Text_Warp.text_warp_current tw with
                 | FStar_Pervasives_Native.None ->
                     StepError
                       (m, "text-warp-current-grapheme: no current grapheme")
                 | FStar_Pervasives_Native.Some g ->
                     let uu___ =
                       Space_Interpreter.add_grapheme
                         m.Space_Interpreter.graphemes1 g in
                     (match uu___ with
                      | (gt', gh) ->
                          let gh_cell =
                            if gh < (Prims.parse_int "18446744073709551616")
                            then FStar_UInt64.uint_to_t gh
                            else Stdint.Uint64.zero in
                          let u' =
                            {
                              Space_Universe.id = (u.Space_Universe.id);
                              Space_Universe.name = (u.Space_Universe.name);
                              Space_Universe.discipline =
                                (u.Space_Universe.discipline);
                              Space_Universe.stack = (gh_cell :: rest);
                              Space_Universe.memory =
                                (u.Space_Universe.memory);
                              Space_Universe.capacity =
                                (u.Space_Universe.capacity);
                              Space_Universe.state = (u.Space_Universe.state)
                            } in
                          StepOk
                            (Space_Interpreter.advance_ip
                               (let uu___1 =
                                  Space_Interpreter.update_current m u' in
                                {
                                  Space_Interpreter.mworld =
                                    (uu___1.Space_Interpreter.mworld);
                                  Space_Interpreter.borrows =
                                    (uu___1.Space_Interpreter.borrows);
                                  Space_Interpreter.warps1 =
                                    (uu___1.Space_Interpreter.warps1);
                                  Space_Interpreter.texts1 =
                                    (uu___1.Space_Interpreter.texts1);
                                  Space_Interpreter.text_warps =
                                    (uu___1.Space_Interpreter.text_warps);
                                  Space_Interpreter.graphemes1 = gt';
                                  Space_Interpreter.bytes_meta =
                                    (uu___1.Space_Interpreter.bytes_meta);
                                  Space_Interpreter.output =
                                    (uu___1.Space_Interpreter.output);
                                  Space_Interpreter.input =
                                    (uu___1.Space_Interpreter.input);
                                  Space_Interpreter.inst_ptr =
                                    (uu___1.Space_Interpreter.inst_ptr);
                                  Space_Interpreter.return_stack =
                                    (uu___1.Space_Interpreter.return_stack);
                                  Space_Interpreter.running =
                                    (uu___1.Space_Interpreter.running);
                                  Space_Interpreter.error =
                                    (uu___1.Space_Interpreter.error)
                                })))))
       | uu___ ->
           StepError (m, "text-warp-current-grapheme: stack underflow"))
let step_text_warp_next_grapheme (m : Space_Interpreter.machine) :
  step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | wh_cell::rest ->
           let wh = FStar_UInt64.v wh_cell in
           (match Space_Interpreter.get_text_warp
                    m.Space_Interpreter.text_warps wh
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-warp-next-grapheme: invalid warp handle")
            | FStar_Pervasives_Native.Some tw ->
                (match Space_Text_Warp.text_warp_next tw with
                 | FStar_Pervasives_Native.None ->
                     StepError
                       (m, "text-warp-next-grapheme: no next grapheme")
                 | FStar_Pervasives_Native.Some (g, tw') ->
                     let uu___ =
                       Space_Interpreter.add_grapheme
                         m.Space_Interpreter.graphemes1 g in
                     (match uu___ with
                      | (gt', gh) ->
                          let twt' =
                            Space_Interpreter.update_text_warp
                              m.Space_Interpreter.text_warps wh tw' in
                          let gh_cell =
                            if gh < (Prims.parse_int "18446744073709551616")
                            then FStar_UInt64.uint_to_t gh
                            else Stdint.Uint64.zero in
                          let u' =
                            {
                              Space_Universe.id = (u.Space_Universe.id);
                              Space_Universe.name = (u.Space_Universe.name);
                              Space_Universe.discipline =
                                (u.Space_Universe.discipline);
                              Space_Universe.stack = (wh_cell :: gh_cell ::
                                rest);
                              Space_Universe.memory =
                                (u.Space_Universe.memory);
                              Space_Universe.capacity =
                                (u.Space_Universe.capacity);
                              Space_Universe.state = (u.Space_Universe.state)
                            } in
                          StepOk
                            (Space_Interpreter.advance_ip
                               (let uu___1 =
                                  Space_Interpreter.update_current
                                    {
                                      Space_Interpreter.mworld =
                                        (m.Space_Interpreter.mworld);
                                      Space_Interpreter.borrows =
                                        (m.Space_Interpreter.borrows);
                                      Space_Interpreter.warps1 =
                                        (m.Space_Interpreter.warps1);
                                      Space_Interpreter.texts1 =
                                        (m.Space_Interpreter.texts1);
                                      Space_Interpreter.text_warps = twt';
                                      Space_Interpreter.graphemes1 =
                                        (m.Space_Interpreter.graphemes1);
                                      Space_Interpreter.bytes_meta =
                                        (m.Space_Interpreter.bytes_meta);
                                      Space_Interpreter.output =
                                        (m.Space_Interpreter.output);
                                      Space_Interpreter.input =
                                        (m.Space_Interpreter.input);
                                      Space_Interpreter.inst_ptr =
                                        (m.Space_Interpreter.inst_ptr);
                                      Space_Interpreter.return_stack =
                                        (m.Space_Interpreter.return_stack);
                                      Space_Interpreter.running =
                                        (m.Space_Interpreter.running);
                                      Space_Interpreter.error =
                                        (m.Space_Interpreter.error)
                                    } u' in
                                {
                                  Space_Interpreter.mworld =
                                    (uu___1.Space_Interpreter.mworld);
                                  Space_Interpreter.borrows =
                                    (uu___1.Space_Interpreter.borrows);
                                  Space_Interpreter.warps1 =
                                    (uu___1.Space_Interpreter.warps1);
                                  Space_Interpreter.texts1 =
                                    (uu___1.Space_Interpreter.texts1);
                                  Space_Interpreter.text_warps =
                                    (uu___1.Space_Interpreter.text_warps);
                                  Space_Interpreter.graphemes1 = gt';
                                  Space_Interpreter.bytes_meta =
                                    (uu___1.Space_Interpreter.bytes_meta);
                                  Space_Interpreter.output =
                                    (uu___1.Space_Interpreter.output);
                                  Space_Interpreter.input =
                                    (uu___1.Space_Interpreter.input);
                                  Space_Interpreter.inst_ptr =
                                    (uu___1.Space_Interpreter.inst_ptr);
                                  Space_Interpreter.return_stack =
                                    (uu___1.Space_Interpreter.return_stack);
                                  Space_Interpreter.running =
                                    (uu___1.Space_Interpreter.running);
                                  Space_Interpreter.error =
                                    (uu___1.Space_Interpreter.error)
                                })))))
       | uu___ -> StepError (m, "text-warp-next-grapheme: stack underflow"))
let step_text_warp_grapheme_index (m : Space_Interpreter.machine) :
  step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | wh_cell::rest ->
           let wh = FStar_UInt64.v wh_cell in
           (match Space_Interpreter.get_text_warp
                    m.Space_Interpreter.text_warps wh
            with
            | FStar_Pervasives_Native.None ->
                StepError
                  (m, "text-warp-grapheme-index: invalid warp handle")
            | FStar_Pervasives_Native.Some tw ->
                let pos = Space_Text_Warp.text_warp_position tw in
                let pos_cell =
                  if pos < (Prims.parse_int "18446744073709551616")
                  then FStar_UInt64.uint_to_t pos
                  else Stdint.Uint64.zero in
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = (pos_cell :: rest);
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (Space_Interpreter.update_current m u')))
       | uu___ -> StepError (m, "text-warp-grapheme-index: stack underflow"))
let step_text_warp_goto_grapheme (m : Space_Interpreter.machine) :
  step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | idx_cell::wh_cell::rest ->
           let wh = FStar_UInt64.v wh_cell in
           let idx = FStar_UInt64.v idx_cell in
           (match Space_Interpreter.get_text_warp
                    m.Space_Interpreter.text_warps wh
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-warp-goto-grapheme: invalid warp handle")
            | FStar_Pervasives_Native.Some tw ->
                (match Space_Text_Warp.text_warp_goto tw idx with
                 | FStar_Pervasives_Native.None ->
                     StepError (m, "text-warp-goto-grapheme: invalid index")
                 | FStar_Pervasives_Native.Some tw' ->
                     let twt' =
                       Space_Interpreter.update_text_warp
                         m.Space_Interpreter.text_warps wh tw' in
                     let u' =
                       {
                         Space_Universe.id = (u.Space_Universe.id);
                         Space_Universe.name = (u.Space_Universe.name);
                         Space_Universe.discipline =
                           (u.Space_Universe.discipline);
                         Space_Universe.stack = (wh_cell :: rest);
                         Space_Universe.memory = (u.Space_Universe.memory);
                         Space_Universe.capacity =
                           (u.Space_Universe.capacity);
                         Space_Universe.state = (u.Space_Universe.state)
                       } in
                     StepOk
                       (Space_Interpreter.advance_ip
                          (let uu___ = Space_Interpreter.update_current m u' in
                           {
                             Space_Interpreter.mworld =
                               (uu___.Space_Interpreter.mworld);
                             Space_Interpreter.borrows =
                               (uu___.Space_Interpreter.borrows);
                             Space_Interpreter.warps1 =
                               (uu___.Space_Interpreter.warps1);
                             Space_Interpreter.texts1 =
                               (uu___.Space_Interpreter.texts1);
                             Space_Interpreter.text_warps = twt';
                             Space_Interpreter.graphemes1 =
                               (uu___.Space_Interpreter.graphemes1);
                             Space_Interpreter.bytes_meta =
                               (uu___.Space_Interpreter.bytes_meta);
                             Space_Interpreter.output =
                               (uu___.Space_Interpreter.output);
                             Space_Interpreter.input =
                               (uu___.Space_Interpreter.input);
                             Space_Interpreter.inst_ptr =
                               (uu___.Space_Interpreter.inst_ptr);
                             Space_Interpreter.return_stack =
                               (uu___.Space_Interpreter.return_stack);
                             Space_Interpreter.running =
                               (uu___.Space_Interpreter.running);
                             Space_Interpreter.error =
                               (uu___.Space_Interpreter.error)
                           }))))
       | uu___ -> StepError (m, "text-warp-goto-grapheme: stack underflow"))
let step_grapheme_byte_length (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | gh_cell::rest ->
           let gh = FStar_UInt64.v gh_cell in
           (match Space_Interpreter.get_grapheme
                    m.Space_Interpreter.graphemes1 gh
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "grapheme-byte-length: invalid handle")
            | FStar_Pervasives_Native.Some g ->
                let len = g.Space_Text_Types.len in
                let len_cell =
                  if len < (Prims.parse_int "18446744073709551616")
                  then FStar_UInt64.uint_to_t len
                  else Stdint.Uint64.zero in
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = (len_cell :: rest);
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (Space_Interpreter.update_current m u')))
       | uu___ -> StepError (m, "grapheme-byte-length: stack underflow"))
let step_grapheme_is_ascii (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | gh_cell::rest ->
           let gh = FStar_UInt64.v gh_cell in
           (match Space_Interpreter.get_grapheme
                    m.Space_Interpreter.graphemes1 gh
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "grapheme-is-ascii: invalid handle")
            | FStar_Pervasives_Native.Some g ->
                let is_ascii =
                  (g.Space_Text_Types.len = Prims.int_one) &&
                    (match g.Space_Text_Types.bytes with
                     | b::[] -> (FStar_UInt8.v b) < (Prims.of_int (128))
                     | uu___ -> false) in
                let flag =
                  if is_ascii then Stdint.Uint64.one else Stdint.Uint64.zero in
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = (flag :: rest);
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (Space_Interpreter.update_current m u')))
       | uu___ -> StepError (m, "grapheme-is-ascii: stack underflow"))
let step_grapheme_code_points (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | gh_cell::rest ->
           let gh = FStar_UInt64.v gh_cell in
           (match Space_Interpreter.get_grapheme
                    m.Space_Interpreter.graphemes1 gh
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "grapheme-code-points: invalid handle")
            | FStar_Pervasives_Native.Some g ->
                let rec count_codepoints bs =
                  match bs with
                  | [] -> Prims.int_zero
                  | b::rest' ->
                      let bv = FStar_UInt8.v b in
                      if
                        (bv < (Prims.of_int (128))) ||
                          (bv >= (Prims.of_int (192)))
                      then Prims.int_one + (count_codepoints rest')
                      else count_codepoints rest' in
                let count = count_codepoints g.Space_Text_Types.bytes in
                let count_cell =
                  if count < (Prims.parse_int "18446744073709551616")
                  then FStar_UInt64.uint_to_t count
                  else Stdint.Uint64.zero in
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = (count_cell :: rest);
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (Space_Interpreter.update_current m u')))
       | uu___ -> StepError (m, "grapheme-code-points: stack underflow"))
let step_text_grapheme_at (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | idx_cell::th_cell::rest ->
           let th = FStar_UInt64.v th_cell in
           let idx = FStar_UInt64.v idx_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1 th
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-grapheme-at: invalid text handle")
            | FStar_Pervasives_Native.Some t ->
                if
                  idx >=
                    (t.Space_Text_Types.header).Space_Text_Types.grapheme_count
                then StepError (m, "text-grapheme-at: index out of bounds")
                else
                  (let tw = Space_Text_Warp.text_warp_begin t in
                   match Space_Text_Warp.text_warp_goto tw idx with
                   | FStar_Pervasives_Native.None ->
                       StepError (m, "text-grapheme-at: seek failed")
                   | FStar_Pervasives_Native.Some tw' ->
                       (match Space_Text_Warp.text_warp_current tw' with
                        | FStar_Pervasives_Native.None ->
                            StepError
                              (m, "text-grapheme-at: no grapheme at index")
                        | FStar_Pervasives_Native.Some g ->
                            let uu___1 =
                              Space_Interpreter.add_grapheme
                                m.Space_Interpreter.graphemes1 g in
                            (match uu___1 with
                             | (gt', gh) ->
                                 let gh_cell =
                                   if
                                     gh <
                                       (Prims.parse_int "18446744073709551616")
                                   then FStar_UInt64.uint_to_t gh
                                   else Stdint.Uint64.zero in
                                 let u' =
                                   {
                                     Space_Universe.id =
                                       (u.Space_Universe.id);
                                     Space_Universe.name =
                                       (u.Space_Universe.name);
                                     Space_Universe.discipline =
                                       (u.Space_Universe.discipline);
                                     Space_Universe.stack = (gh_cell :: rest);
                                     Space_Universe.memory =
                                       (u.Space_Universe.memory);
                                     Space_Universe.capacity =
                                       (u.Space_Universe.capacity);
                                     Space_Universe.state =
                                       (u.Space_Universe.state)
                                   } in
                                 StepOk
                                   (Space_Interpreter.advance_ip
                                      (let uu___2 =
                                         Space_Interpreter.update_current m
                                           u' in
                                       {
                                         Space_Interpreter.mworld =
                                           (uu___2.Space_Interpreter.mworld);
                                         Space_Interpreter.borrows =
                                           (uu___2.Space_Interpreter.borrows);
                                         Space_Interpreter.warps1 =
                                           (uu___2.Space_Interpreter.warps1);
                                         Space_Interpreter.texts1 =
                                           (uu___2.Space_Interpreter.texts1);
                                         Space_Interpreter.text_warps =
                                           (uu___2.Space_Interpreter.text_warps);
                                         Space_Interpreter.graphemes1 = gt';
                                         Space_Interpreter.bytes_meta =
                                           (uu___2.Space_Interpreter.bytes_meta);
                                         Space_Interpreter.output =
                                           (uu___2.Space_Interpreter.output);
                                         Space_Interpreter.input =
                                           (uu___2.Space_Interpreter.input);
                                         Space_Interpreter.inst_ptr =
                                           (uu___2.Space_Interpreter.inst_ptr);
                                         Space_Interpreter.return_stack =
                                           (uu___2.Space_Interpreter.return_stack);
                                         Space_Interpreter.running =
                                           (uu___2.Space_Interpreter.running);
                                         Space_Interpreter.error =
                                           (uu___2.Space_Interpreter.error)
                                       }))))))
       | uu___ -> StepError (m, "text-grapheme-at: stack underflow"))
let step_text_grapheme_first (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | th_cell::rest ->
           let th = FStar_UInt64.v th_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1 th
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-grapheme-first: invalid text handle")
            | FStar_Pervasives_Native.Some t ->
                if
                  (t.Space_Text_Types.header).Space_Text_Types.grapheme_count
                    = Prims.int_zero
                then StepError (m, "text-grapheme-first: empty text")
                else
                  (let tw = Space_Text_Warp.text_warp_begin t in
                   match Space_Text_Warp.text_warp_current tw with
                   | FStar_Pervasives_Native.None ->
                       StepError (m, "text-grapheme-first: no grapheme")
                   | FStar_Pervasives_Native.Some g ->
                       let uu___1 =
                         Space_Interpreter.add_grapheme
                           m.Space_Interpreter.graphemes1 g in
                       (match uu___1 with
                        | (gt', gh) ->
                            let gh_cell =
                              if
                                gh < (Prims.parse_int "18446744073709551616")
                              then FStar_UInt64.uint_to_t gh
                              else Stdint.Uint64.zero in
                            let u' =
                              {
                                Space_Universe.id = (u.Space_Universe.id);
                                Space_Universe.name = (u.Space_Universe.name);
                                Space_Universe.discipline =
                                  (u.Space_Universe.discipline);
                                Space_Universe.stack = (gh_cell :: rest);
                                Space_Universe.memory =
                                  (u.Space_Universe.memory);
                                Space_Universe.capacity =
                                  (u.Space_Universe.capacity);
                                Space_Universe.state =
                                  (u.Space_Universe.state)
                              } in
                            StepOk
                              (Space_Interpreter.advance_ip
                                 (let uu___2 =
                                    Space_Interpreter.update_current m u' in
                                  {
                                    Space_Interpreter.mworld =
                                      (uu___2.Space_Interpreter.mworld);
                                    Space_Interpreter.borrows =
                                      (uu___2.Space_Interpreter.borrows);
                                    Space_Interpreter.warps1 =
                                      (uu___2.Space_Interpreter.warps1);
                                    Space_Interpreter.texts1 =
                                      (uu___2.Space_Interpreter.texts1);
                                    Space_Interpreter.text_warps =
                                      (uu___2.Space_Interpreter.text_warps);
                                    Space_Interpreter.graphemes1 = gt';
                                    Space_Interpreter.bytes_meta =
                                      (uu___2.Space_Interpreter.bytes_meta);
                                    Space_Interpreter.output =
                                      (uu___2.Space_Interpreter.output);
                                    Space_Interpreter.input =
                                      (uu___2.Space_Interpreter.input);
                                    Space_Interpreter.inst_ptr =
                                      (uu___2.Space_Interpreter.inst_ptr);
                                    Space_Interpreter.return_stack =
                                      (uu___2.Space_Interpreter.return_stack);
                                    Space_Interpreter.running =
                                      (uu___2.Space_Interpreter.running);
                                    Space_Interpreter.error =
                                      (uu___2.Space_Interpreter.error)
                                  })))))
       | uu___ -> StepError (m, "text-grapheme-first: stack underflow"))
let step_text_grapheme_last (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | th_cell::rest ->
           let th = FStar_UInt64.v th_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1 th
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-grapheme-last: invalid text handle")
            | FStar_Pervasives_Native.Some t ->
                if
                  (t.Space_Text_Types.header).Space_Text_Types.grapheme_count
                    = Prims.int_zero
                then StepError (m, "text-grapheme-last: empty text")
                else
                  (let tw = Space_Text_Warp.text_warp_begin t in
                   let last_idx =
                     (t.Space_Text_Types.header).Space_Text_Types.grapheme_count
                       - Prims.int_one in
                   match Space_Text_Warp.text_warp_goto tw last_idx with
                   | FStar_Pervasives_Native.None ->
                       StepError (m, "text-grapheme-last: seek failed")
                   | FStar_Pervasives_Native.Some tw' ->
                       (match Space_Text_Warp.text_warp_current tw' with
                        | FStar_Pervasives_Native.None ->
                            StepError (m, "text-grapheme-last: no grapheme")
                        | FStar_Pervasives_Native.Some g ->
                            let uu___1 =
                              Space_Interpreter.add_grapheme
                                m.Space_Interpreter.graphemes1 g in
                            (match uu___1 with
                             | (gt', gh) ->
                                 let gh_cell =
                                   if
                                     gh <
                                       (Prims.parse_int "18446744073709551616")
                                   then FStar_UInt64.uint_to_t gh
                                   else Stdint.Uint64.zero in
                                 let u' =
                                   {
                                     Space_Universe.id =
                                       (u.Space_Universe.id);
                                     Space_Universe.name =
                                       (u.Space_Universe.name);
                                     Space_Universe.discipline =
                                       (u.Space_Universe.discipline);
                                     Space_Universe.stack = (gh_cell :: rest);
                                     Space_Universe.memory =
                                       (u.Space_Universe.memory);
                                     Space_Universe.capacity =
                                       (u.Space_Universe.capacity);
                                     Space_Universe.state =
                                       (u.Space_Universe.state)
                                   } in
                                 StepOk
                                   (Space_Interpreter.advance_ip
                                      (let uu___2 =
                                         Space_Interpreter.update_current m
                                           u' in
                                       {
                                         Space_Interpreter.mworld =
                                           (uu___2.Space_Interpreter.mworld);
                                         Space_Interpreter.borrows =
                                           (uu___2.Space_Interpreter.borrows);
                                         Space_Interpreter.warps1 =
                                           (uu___2.Space_Interpreter.warps1);
                                         Space_Interpreter.texts1 =
                                           (uu___2.Space_Interpreter.texts1);
                                         Space_Interpreter.text_warps =
                                           (uu___2.Space_Interpreter.text_warps);
                                         Space_Interpreter.graphemes1 = gt';
                                         Space_Interpreter.bytes_meta =
                                           (uu___2.Space_Interpreter.bytes_meta);
                                         Space_Interpreter.output =
                                           (uu___2.Space_Interpreter.output);
                                         Space_Interpreter.input =
                                           (uu___2.Space_Interpreter.input);
                                         Space_Interpreter.inst_ptr =
                                           (uu___2.Space_Interpreter.inst_ptr);
                                         Space_Interpreter.return_stack =
                                           (uu___2.Space_Interpreter.return_stack);
                                         Space_Interpreter.running =
                                           (uu___2.Space_Interpreter.running);
                                         Space_Interpreter.error =
                                           (uu___2.Space_Interpreter.error)
                                       }))))))
       | uu___ -> StepError (m, "text-grapheme-last: stack underflow"))
let step_text_compare (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | h2_cell::h1_cell::rest ->
           let h1 = FStar_UInt64.v h1_cell in
           let h2 = FStar_UInt64.v h2_cell in
           (match ((Space_Interpreter.get_text m.Space_Interpreter.texts1 h1),
                    (Space_Interpreter.get_text m.Space_Interpreter.texts1 h2))
            with
            | (FStar_Pervasives_Native.Some t1, FStar_Pervasives_Native.Some
               t2) ->
                let rec compare_bytes b1 b2 =
                  match (b1, b2) with
                  | ([], []) -> Prims.int_zero
                  | ([], uu___) -> (Prims.of_int (-1))
                  | (uu___, []) -> Prims.int_one
                  | (x::xs, y::ys) ->
                      let xv = FStar_UInt8.v x in
                      let yv = FStar_UInt8.v y in
                      if xv < yv
                      then (Prims.of_int (-1))
                      else
                        if xv > yv
                        then Prims.int_one
                        else compare_bytes xs ys in
                let cmp =
                  compare_bytes t1.Space_Text_Types.data
                    t2.Space_Text_Types.data in
                let cmp_cell =
                  if cmp = (Prims.of_int (-1))
                  then (Stdint.Uint64.of_string "0xFFFFFFFFFFFFFFFF")
                  else
                    if cmp = Prims.int_zero
                    then Stdint.Uint64.zero
                    else Stdint.Uint64.one in
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = (cmp_cell :: rest);
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (Space_Interpreter.update_current m u'))
            | (uu___, uu___1) ->
                StepError (m, "text-compare: invalid handle"))
       | uu___ -> StepError (m, "text-compare: stack underflow"))
let step_text_code_point_count (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | th_cell::rest ->
           let th = FStar_UInt64.v th_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1 th
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-code-point-count: invalid handle")
            | FStar_Pervasives_Native.Some t ->
                let rec count_codepoints bs =
                  match bs with
                  | [] -> Prims.int_zero
                  | b::rest' ->
                      let bv = FStar_UInt8.v b in
                      if
                        (bv < (Prims.of_int (128))) ||
                          (bv >= (Prims.of_int (192)))
                      then Prims.int_one + (count_codepoints rest')
                      else count_codepoints rest' in
                let count = count_codepoints t.Space_Text_Types.data in
                let count_cell =
                  if count < (Prims.parse_int "18446744073709551616")
                  then FStar_UInt64.uint_to_t count
                  else Stdint.Uint64.zero in
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = (count_cell :: rest);
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                StepOk
                  (Space_Interpreter.advance_ip
                     (Space_Interpreter.update_current m u')))
       | uu___ -> StepError (m, "text-code-point-count: stack underflow"))
let step_text_code_point_at (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | idx_cell::th_cell::rest ->
           let th = FStar_UInt64.v th_cell in
           let idx = FStar_UInt64.v idx_cell in
           (match Space_Interpreter.get_text m.Space_Interpreter.texts1 th
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "text-code-point-at: invalid handle")
            | FStar_Pervasives_Native.Some t ->
                let cps =
                  Space_Text_Normalize.bytes_to_codepoints
                    t.Space_Text_Types.data in
                if idx >= (FStar_List_Tot_Base.length cps)
                then StepError (m, "text-code-point-at: index out of bounds")
                else
                  (match FStar_List_Tot_Base.nth cps idx with
                   | FStar_Pervasives_Native.None ->
                       StepError
                         (m, "text-code-point-at: index out of bounds")
                   | FStar_Pervasives_Native.Some cp ->
                       let cp_cell =
                         if cp < (Prims.parse_int "18446744073709551616")
                         then FStar_UInt64.uint_to_t cp
                         else Stdint.Uint64.zero in
                       let u' =
                         {
                           Space_Universe.id = (u.Space_Universe.id);
                           Space_Universe.name = (u.Space_Universe.name);
                           Space_Universe.discipline =
                             (u.Space_Universe.discipline);
                           Space_Universe.stack = (cp_cell :: rest);
                           Space_Universe.memory = (u.Space_Universe.memory);
                           Space_Universe.capacity =
                             (u.Space_Universe.capacity);
                           Space_Universe.state = (u.Space_Universe.state)
                         } in
                       StepOk
                         (Space_Interpreter.advance_ip
                            (Space_Interpreter.update_current m u'))))
       | uu___ -> StepError (m, "text-code-point-at: stack underflow"))
let step_emit_grapheme (m : Space_Interpreter.machine) : step_result=
  match Space_Interpreter.current_universe m with
  | FStar_Pervasives_Native.None -> StepError (m, "no current universe")
  | FStar_Pervasives_Native.Some u ->
      (match u.Space_Universe.stack with
       | gh_cell::rest ->
           let gh = FStar_UInt64.v gh_cell in
           (match Space_Interpreter.get_grapheme
                    m.Space_Interpreter.graphemes1 gh
            with
            | FStar_Pervasives_Native.None ->
                StepError (m, "emit-grapheme: invalid handle")
            | FStar_Pervasives_Native.Some g ->
                let byte_to_nat b = FStar_UInt8.v b in
                let byte_vals =
                  FStar_List_Tot_Base.map byte_to_nat
                    g.Space_Text_Types.bytes in
                let u' =
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = rest;
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  } in
                let m' =
                  let uu___ = Space_Interpreter.update_current m u' in
                  {
                    Space_Interpreter.mworld =
                      (uu___.Space_Interpreter.mworld);
                    Space_Interpreter.borrows =
                      (uu___.Space_Interpreter.borrows);
                    Space_Interpreter.warps1 =
                      (uu___.Space_Interpreter.warps1);
                    Space_Interpreter.texts1 =
                      (uu___.Space_Interpreter.texts1);
                    Space_Interpreter.text_warps =
                      (uu___.Space_Interpreter.text_warps);
                    Space_Interpreter.graphemes1 =
                      (uu___.Space_Interpreter.graphemes1);
                    Space_Interpreter.bytes_meta =
                      (uu___.Space_Interpreter.bytes_meta);
                    Space_Interpreter.output =
                      (FStar_List_Tot_Base.append m.Space_Interpreter.output
                         byte_vals);
                    Space_Interpreter.input = (uu___.Space_Interpreter.input);
                    Space_Interpreter.inst_ptr =
                      (uu___.Space_Interpreter.inst_ptr);
                    Space_Interpreter.return_stack =
                      (uu___.Space_Interpreter.return_stack);
                    Space_Interpreter.running =
                      (uu___.Space_Interpreter.running);
                    Space_Interpreter.error = (uu___.Space_Interpreter.error)
                  } in
                StepOk (Space_Interpreter.advance_ip m'))
       | uu___ -> StepError (m, "emit-grapheme: stack underflow"))
let step_text_prim (m : Space_Interpreter.machine)
  (op : Space_Instruction.prim_op) : step_result=
  match op with
  | Space_Instruction.PrimCreateText -> step_create_text m
  | Space_Instruction.PrimTextByteLength -> step_text_byte_length m
  | Space_Instruction.PrimTextGraphemeCount -> step_text_grapheme_count m
  | Space_Instruction.PrimTextIsSimple -> step_text_is_simple m
  | Space_Instruction.PrimTextEqual -> step_text_equal m
  | Space_Instruction.PrimTextConcat -> step_text_concat m
  | Space_Instruction.PrimTextSlice -> step_text_slice m
  | Space_Instruction.PrimTextNormalizeNfc -> step_text_normalize_nfc m
  | Space_Instruction.PrimTextNormalizeNfd -> step_text_normalize_nfd m
  | Space_Instruction.PrimTextNormalizeNfkc -> step_text_normalize_nfkc m
  | Space_Instruction.PrimTextNormalizeNfkd -> step_text_normalize_nfkd m
  | Space_Instruction.PrimTextToUpper -> step_text_to_upper m
  | Space_Instruction.PrimTextToLower -> step_text_to_lower m
  | Space_Instruction.PrimTextToTitle -> step_text_to_title m
  | Space_Instruction.PrimTextWarpHasGrapheme ->
      step_text_warp_has_grapheme m
  | Space_Instruction.PrimTextWarpCurrentGrapheme ->
      step_text_warp_current_grapheme m
  | Space_Instruction.PrimTextWarpNextGrapheme ->
      step_text_warp_next_grapheme m
  | Space_Instruction.PrimTextWarpGraphemeIndex ->
      step_text_warp_grapheme_index m
  | Space_Instruction.PrimTextWarpGotoGrapheme ->
      step_text_warp_goto_grapheme m
  | Space_Instruction.PrimGraphemeByteLength -> step_grapheme_byte_length m
  | Space_Instruction.PrimGraphemeIsAscii -> step_grapheme_is_ascii m
  | Space_Instruction.PrimGraphemeCodePoints -> step_grapheme_code_points m
  | Space_Instruction.PrimTextGraphemeAt -> step_text_grapheme_at m
  | Space_Instruction.PrimTextGraphemeFirst -> step_text_grapheme_first m
  | Space_Instruction.PrimTextGraphemeLast -> step_text_grapheme_last m
  | Space_Instruction.PrimTextCompare -> step_text_compare m
  | Space_Instruction.PrimTextCodePointCount -> step_text_code_point_count m
  | Space_Instruction.PrimTextCodePointAt -> step_text_code_point_at m
  | Space_Instruction.PrimEmitGrapheme -> step_emit_grapheme m
  | uu___ -> StepError (m, "unknown text primitive")
let step_prim (m : Space_Interpreter.machine)
  (op : Space_Instruction.prim_op) : step_result=
  if is_text_prim op
  then step_text_prim m op
  else
    if is_io_prim op
    then step_io_prim m op
    else
      if is_borrow_prim op
      then step_borrow_prim m op
      else
        if is_warp_prim op
        then step_warp_prim m op
        else
          if is_bytes_meta_prim op
          then step_bytes_meta_prim m op
          else
            (match Space_Interpreter.current_universe m with
             | FStar_Pervasives_Native.None ->
                 StepError (m, "no current universe")
             | FStar_Pervasives_Native.Some u ->
                 (match Space_Execute.exec_prim op u with
                  | Space_Execute.ExecOk u' ->
                      StepOk
                        (Space_Interpreter.advance_ip
                           (Space_Interpreter.update_current m u'))
                  | Space_Execute.ExecHalt ->
                      StepHalt (Space_Interpreter.halt m)
                  | Space_Execute.ExecError msg -> StepError (m, msg)))
let step_one (m : Space_Interpreter.machine)
  (instr : Space_Instruction.instruction) : step_result=
  match instr with
  | Space_Instruction.IPush v -> step_push m v
  | Space_Instruction.ICall target -> step_call m target
  | Space_Instruction.IReturn -> step_return m
  | Space_Instruction.IPrimitive op -> step_prim m op
  | Space_Instruction.IBranch target -> step_branch m target
  | Space_Instruction.IBranchZero target -> step_branch_zero m target
  | Space_Instruction.IBranchNonZero target -> step_branch_nonzero m target
  | Space_Instruction.ICreateUniverse (name, disc) ->
      step_create_universe m name (Prims.of_int (1024)) disc
  | Space_Instruction.IEndUniverse name -> step_end_universe m name
  | Space_Instruction.IReleaseUniverse name -> step_release_universe m name
  | Space_Instruction.ITransferTo name -> step_transfer m name
let fetch_instr (prog : Space_Instruction.instruction Prims.list)
  (ip : Space_Instruction.ip) :
  Space_Instruction.instruction FStar_Pervasives_Native.option=
  let idx = FStar_UInt64.v ip in
  if idx < (FStar_List_Tot_Base.length prog)
  then FStar_List_Tot_Base.nth prog idx
  else FStar_Pervasives_Native.None
let rec run_loop (m : Space_Interpreter.machine)
  (prog : Space_Instruction.instruction Prims.list) (fuel : Prims.nat) :
  step_result=
  if fuel = Prims.int_zero
  then StepError (m, "fuel exhausted")
  else
    if Prims.op_Negation m.Space_Interpreter.running
    then StepOk m
    else
      (match fetch_instr prog m.Space_Interpreter.inst_ptr with
       | FStar_Pervasives_Native.None ->
           StepError (m, "invalid instruction pointer")
       | FStar_Pervasives_Native.Some instr ->
           (match step_one m instr with
            | StepOk m' -> run_loop m' prog (fuel - Prims.int_one)
            | StepHalt m' -> StepHalt m'
            | StepError (m', msg) -> StepError (m', msg)))
let run_program (prog : Space_Instruction.instruction Prims.list)
  (fuel : Prims.nat) : step_result=
  let m =
    {
      Space_Interpreter.mworld =
        (Space_Interpreter.initial_machine.Space_Interpreter.mworld);
      Space_Interpreter.borrows =
        (Space_Interpreter.initial_machine.Space_Interpreter.borrows);
      Space_Interpreter.warps1 =
        (Space_Interpreter.initial_machine.Space_Interpreter.warps1);
      Space_Interpreter.texts1 =
        (Space_Interpreter.initial_machine.Space_Interpreter.texts1);
      Space_Interpreter.text_warps =
        (Space_Interpreter.initial_machine.Space_Interpreter.text_warps);
      Space_Interpreter.graphemes1 =
        (Space_Interpreter.initial_machine.Space_Interpreter.graphemes1);
      Space_Interpreter.bytes_meta =
        (Space_Interpreter.initial_machine.Space_Interpreter.bytes_meta);
      Space_Interpreter.output =
        (Space_Interpreter.initial_machine.Space_Interpreter.output);
      Space_Interpreter.input =
        (Space_Interpreter.initial_machine.Space_Interpreter.input);
      Space_Interpreter.inst_ptr =
        (Space_Interpreter.initial_machine.Space_Interpreter.inst_ptr);
      Space_Interpreter.return_stack =
        (Space_Interpreter.initial_machine.Space_Interpreter.return_stack);
      Space_Interpreter.running = true;
      Space_Interpreter.error =
        (Space_Interpreter.initial_machine.Space_Interpreter.error)
    } in
  let uu___ =
    Space_World.create_universe m.Space_Interpreter.mworld "data"
      (Prims.parse_int "65536") Space_Types.Unrestricted in
  match uu___ with
  | (w', uu___1) ->
      (match Space_World.set_current w' "data" with
       | FStar_Pervasives_Native.None ->
           StepError (m, "failed to set current universe")
       | FStar_Pervasives_Native.Some w'' ->
           run_loop
             {
               Space_Interpreter.mworld = w'';
               Space_Interpreter.borrows = (m.Space_Interpreter.borrows);
               Space_Interpreter.warps1 = (m.Space_Interpreter.warps1);
               Space_Interpreter.texts1 = (m.Space_Interpreter.texts1);
               Space_Interpreter.text_warps =
                 (m.Space_Interpreter.text_warps);
               Space_Interpreter.graphemes1 =
                 (m.Space_Interpreter.graphemes1);
               Space_Interpreter.bytes_meta =
                 (m.Space_Interpreter.bytes_meta);
               Space_Interpreter.output = (m.Space_Interpreter.output);
               Space_Interpreter.input = (m.Space_Interpreter.input);
               Space_Interpreter.inst_ptr = (m.Space_Interpreter.inst_ptr);
               Space_Interpreter.return_stack =
                 (m.Space_Interpreter.return_stack);
               Space_Interpreter.running = (m.Space_Interpreter.running);
               Space_Interpreter.error = (m.Space_Interpreter.error)
             } prog fuel)
