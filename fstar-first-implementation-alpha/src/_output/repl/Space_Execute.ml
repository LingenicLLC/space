open Prims
type exec_result =
  | ExecOk of Space_Universe.universe 
  | ExecError of Prims.string 
  | ExecHalt 
let uu___is_ExecOk (projectee : exec_result) : Prims.bool=
  match projectee with | ExecOk _0 -> true | uu___ -> false
let __proj__ExecOk__item___0 (projectee : exec_result) :
  Space_Universe.universe= match projectee with | ExecOk _0 -> _0
let uu___is_ExecError (projectee : exec_result) : Prims.bool=
  match projectee with | ExecError _0 -> true | uu___ -> false
let __proj__ExecError__item___0 (projectee : exec_result) : Prims.string=
  match projectee with | ExecError _0 -> _0
let uu___is_ExecHalt (projectee : exec_result) : Prims.bool=
  match projectee with | ExecHalt -> true | uu___ -> false
let exec_stack_prim (op : Space_Instruction.prim_op)
  (u : Space_Universe.universe) : exec_result=
  if Prims.op_Negation (Space_Universe.is_live u)
  then ExecError "universe not live"
  else
    (let s = u.Space_Universe.stack in
     let result =
       match op with
       | Space_Instruction.PrimDup -> Space_Stack.dup s
       | Space_Instruction.PrimDrop -> Space_Stack.drop s
       | Space_Instruction.PrimSwap -> Space_Stack.swap s
       | Space_Instruction.PrimOver -> Space_Stack.over s
       | Space_Instruction.PrimRot -> Space_Stack.rot s
       | Space_Instruction.PrimNip -> Space_Stack.nip s
       | Space_Instruction.PrimTuck -> Space_Stack.tuck s
       | Space_Instruction.PrimPick -> Space_Stack.pick s
       | uu___1 -> FStar_Pervasives_Native.None in
     match result with
     | FStar_Pervasives_Native.Some s' ->
         ExecOk
           {
             Space_Universe.id = (u.Space_Universe.id);
             Space_Universe.name = (u.Space_Universe.name);
             Space_Universe.discipline = (u.Space_Universe.discipline);
             Space_Universe.stack = s';
             Space_Universe.memory = (u.Space_Universe.memory);
             Space_Universe.capacity = (u.Space_Universe.capacity);
             Space_Universe.state = (u.Space_Universe.state)
           }
     | FStar_Pervasives_Native.None -> ExecError "stack operation failed")
let exec_arith_prim (op : Space_Instruction.prim_op)
  (u : Space_Universe.universe) : exec_result=
  if Prims.op_Negation (Space_Universe.is_live u)
  then ExecError "universe not live"
  else
    (let s = u.Space_Universe.stack in
     let result =
       match op with
       | Space_Instruction.PrimAdd -> Space_Arithmetic.stack_add s
       | Space_Instruction.PrimSub -> Space_Arithmetic.stack_sub s
       | Space_Instruction.PrimMul -> Space_Arithmetic.stack_mul s
       | Space_Instruction.PrimDivU -> Space_Arithmetic.stack_div s
       | Space_Instruction.PrimDivS -> Space_Arithmetic.stack_div_signed s
       | Space_Instruction.PrimMod -> Space_Arithmetic.stack_mod s
       | Space_Instruction.PrimNeg -> Space_Arithmetic.stack_negate s
       | Space_Instruction.PrimMin -> Space_Arithmetic.stack_min s
       | Space_Instruction.PrimMax -> Space_Arithmetic.stack_max s
       | uu___1 -> FStar_Pervasives_Native.None in
     match result with
     | FStar_Pervasives_Native.Some s' ->
         ExecOk
           {
             Space_Universe.id = (u.Space_Universe.id);
             Space_Universe.name = (u.Space_Universe.name);
             Space_Universe.discipline = (u.Space_Universe.discipline);
             Space_Universe.stack = s';
             Space_Universe.memory = (u.Space_Universe.memory);
             Space_Universe.capacity = (u.Space_Universe.capacity);
             Space_Universe.state = (u.Space_Universe.state)
           }
     | FStar_Pervasives_Native.None ->
         ExecError "arithmetic operation failed")
let exec_bitwise_prim (op : Space_Instruction.prim_op)
  (u : Space_Universe.universe) : exec_result=
  if Prims.op_Negation (Space_Universe.is_live u)
  then ExecError "universe not live"
  else
    (let s = u.Space_Universe.stack in
     let result =
       match op with
       | Space_Instruction.PrimAnd -> Space_Bitwise.stack_and s
       | Space_Instruction.PrimOr -> Space_Bitwise.stack_or s
       | Space_Instruction.PrimXor -> Space_Bitwise.stack_xor s
       | Space_Instruction.PrimNot -> Space_Bitwise.stack_not s
       | Space_Instruction.PrimShl -> Space_Bitwise.stack_shl s
       | Space_Instruction.PrimShr -> Space_Bitwise.stack_shr s
       | uu___1 -> FStar_Pervasives_Native.None in
     match result with
     | FStar_Pervasives_Native.Some s' ->
         ExecOk
           {
             Space_Universe.id = (u.Space_Universe.id);
             Space_Universe.name = (u.Space_Universe.name);
             Space_Universe.discipline = (u.Space_Universe.discipline);
             Space_Universe.stack = s';
             Space_Universe.memory = (u.Space_Universe.memory);
             Space_Universe.capacity = (u.Space_Universe.capacity);
             Space_Universe.state = (u.Space_Universe.state)
           }
     | FStar_Pervasives_Native.None -> ExecError "bitwise operation failed")
let exec_compare_prim (op : Space_Instruction.prim_op)
  (u : Space_Universe.universe) : exec_result=
  if Prims.op_Negation (Space_Universe.is_live u)
  then ExecError "universe not live"
  else
    (let s = u.Space_Universe.stack in
     let result =
       match op with
       | Space_Instruction.PrimEq -> Space_Comparison.stack_eq s
       | Space_Instruction.PrimNeq -> Space_Comparison.stack_neq s
       | Space_Instruction.PrimLtU -> Space_Comparison.stack_lt s
       | Space_Instruction.PrimGtU -> Space_Comparison.stack_gt s
       | Space_Instruction.PrimLtS -> Space_Comparison.stack_lt_signed s
       | Space_Instruction.PrimGtS -> Space_Comparison.stack_gt_signed s
       | uu___1 -> FStar_Pervasives_Native.None in
     match result with
     | FStar_Pervasives_Native.Some s' ->
         ExecOk
           {
             Space_Universe.id = (u.Space_Universe.id);
             Space_Universe.name = (u.Space_Universe.name);
             Space_Universe.discipline = (u.Space_Universe.discipline);
             Space_Universe.stack = s';
             Space_Universe.memory = (u.Space_Universe.memory);
             Space_Universe.capacity = (u.Space_Universe.capacity);
             Space_Universe.state = (u.Space_Universe.state)
           }
     | FStar_Pervasives_Native.None ->
         ExecError "comparison operation failed")
let exec_mem_fetch (u : Space_Universe.universe) : exec_result=
  if Prims.op_Negation (Space_Universe.is_live u)
  then ExecError "universe not live"
  else
    (match u.Space_Universe.stack with
     | addr_cell::rest ->
         let addr = FStar_UInt64.v addr_cell in
         (match Space_Memory.mem_fetch u.Space_Universe.memory addr with
          | FStar_Pervasives_Native.None ->
              ExecError "fetch: invalid address"
          | FStar_Pervasives_Native.Some value ->
              ExecOk
                {
                  Space_Universe.id = (u.Space_Universe.id);
                  Space_Universe.name = (u.Space_Universe.name);
                  Space_Universe.discipline = (u.Space_Universe.discipline);
                  Space_Universe.stack = (value :: rest);
                  Space_Universe.memory = (u.Space_Universe.memory);
                  Space_Universe.capacity = (u.Space_Universe.capacity);
                  Space_Universe.state = (u.Space_Universe.state)
                })
     | uu___1 -> ExecError "fetch: stack underflow")
let exec_mem_store (u : Space_Universe.universe) : exec_result=
  if Prims.op_Negation (Space_Universe.is_live u)
  then ExecError "universe not live"
  else
    (match u.Space_Universe.stack with
     | addr_cell::value::rest ->
         let addr = FStar_UInt64.v addr_cell in
         (match Space_Memory.mem_store u.Space_Universe.memory addr value
          with
          | FStar_Pervasives_Native.None ->
              ExecError "store: invalid address"
          | FStar_Pervasives_Native.Some mem' ->
              ExecOk
                {
                  Space_Universe.id = (u.Space_Universe.id);
                  Space_Universe.name = (u.Space_Universe.name);
                  Space_Universe.discipline = (u.Space_Universe.discipline);
                  Space_Universe.stack = rest;
                  Space_Universe.memory = mem';
                  Space_Universe.capacity = (u.Space_Universe.capacity);
                  Space_Universe.state = (u.Space_Universe.state)
                })
     | uu___1 -> ExecError "store: stack underflow")
let exec_mem_alloc (u : Space_Universe.universe) : exec_result=
  if Prims.op_Negation (Space_Universe.is_live u)
  then ExecError "universe not live"
  else
    (match u.Space_Universe.stack with
     | n_cell::rest ->
         let n = FStar_UInt64.v n_cell in
         let uu___1 = Space_Memory.mem_alloc u.Space_Universe.memory n in
         (match uu___1 with
          | (mem', base_addr) ->
              let addr_cell =
                if base_addr < (Prims.pow2 (Prims.of_int (64)))
                then FStar_UInt64.uint_to_t base_addr
                else Stdint.Uint64.zero in
              ExecOk
                {
                  Space_Universe.id = (u.Space_Universe.id);
                  Space_Universe.name = (u.Space_Universe.name);
                  Space_Universe.discipline = (u.Space_Universe.discipline);
                  Space_Universe.stack = (addr_cell :: rest);
                  Space_Universe.memory = mem';
                  Space_Universe.capacity = (u.Space_Universe.capacity);
                  Space_Universe.state = (u.Space_Universe.state)
                })
     | uu___1 -> ExecError "alloc: stack underflow")
let exec_bytes_fetch (u : Space_Universe.universe) : exec_result=
  if Prims.op_Negation (Space_Universe.is_live u)
  then ExecError "universe not live"
  else
    (match u.Space_Universe.stack with
     | offset_cell::addr_cell::rest ->
         let addr = FStar_UInt64.v addr_cell in
         let offset = FStar_UInt64.v offset_cell in
         let cell_idx = addr + (offset / (Prims.of_int (8))) in
         let byte_pos =
           FStar_UInt64.rem offset_cell (Stdint.Uint64.of_int (8)) in
         let shift_u64 =
           FStar_UInt64.mul_mod byte_pos (Stdint.Uint64.of_int (8)) in
         let n32 = FStar_Int_Cast.uint64_to_uint32 shift_u64 in
         (match Space_Memory.mem_fetch u.Space_Universe.memory cell_idx with
          | FStar_Pervasives_Native.None ->
              ExecError "bytes_fetch: invalid address"
          | FStar_Pervasives_Native.Some cell_val ->
              if (FStar_UInt32.v n32) < (Prims.of_int (64))
              then
                let shifted = Space_Bitwise.shift_right cell_val n32 in
                let byte_val =
                  FStar_UInt64.rem shifted (Stdint.Uint64.of_int (256)) in
                ExecOk
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = (byte_val :: rest);
                    Space_Universe.memory = (u.Space_Universe.memory);
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  }
              else ExecError "bytes_fetch: internal error")
     | uu___1 -> ExecError "bytes_fetch: stack underflow")
let exec_bytes_store (u : Space_Universe.universe) : exec_result=
  if Prims.op_Negation (Space_Universe.is_live u)
  then ExecError "universe not live"
  else
    (match u.Space_Universe.stack with
     | offset_cell::addr_cell::byte_cell::rest ->
         let addr = FStar_UInt64.v addr_cell in
         let offset = FStar_UInt64.v offset_cell in
         let cell_idx = addr + (offset / (Prims.of_int (8))) in
         let byte_pos =
           FStar_UInt64.rem offset_cell (Stdint.Uint64.of_int (8)) in
         let shift_u64 =
           FStar_UInt64.mul_mod byte_pos (Stdint.Uint64.of_int (8)) in
         let n32 = FStar_Int_Cast.uint64_to_uint32 shift_u64 in
         let byte_masked =
           FStar_UInt64.logand byte_cell (Stdint.Uint64.of_int (0xFF)) in
         (match Space_Memory.mem_fetch u.Space_Universe.memory cell_idx with
          | FStar_Pervasives_Native.None ->
              ExecError "bytes_store: invalid address"
          | FStar_Pervasives_Native.Some old_cell ->
              if (FStar_UInt32.v n32) < (Prims.of_int (64))
              then
                let mask =
                  Space_Bitwise.shift_left (Stdint.Uint64.of_int (0xFF)) n32 in
                let inv_mask = FStar_UInt64.lognot mask in
                let cleared = FStar_UInt64.logand old_cell inv_mask in
                let new_byte = Space_Bitwise.shift_left byte_masked n32 in
                let new_cell = FStar_UInt64.logor cleared new_byte in
                (match Space_Memory.mem_store u.Space_Universe.memory
                         cell_idx new_cell
                 with
                 | FStar_Pervasives_Native.None ->
                     ExecError "bytes_store: store failed"
                 | FStar_Pervasives_Native.Some mem' ->
                     ExecOk
                       {
                         Space_Universe.id = (u.Space_Universe.id);
                         Space_Universe.name = (u.Space_Universe.name);
                         Space_Universe.discipline =
                           (u.Space_Universe.discipline);
                         Space_Universe.stack = rest;
                         Space_Universe.memory = mem';
                         Space_Universe.capacity =
                           (u.Space_Universe.capacity);
                         Space_Universe.state = (u.Space_Universe.state)
                       })
              else ExecError "bytes_store: internal error")
     | uu___1 -> ExecError "bytes_store: stack underflow")
let copy_one_byte (mem : Space_Memory.memory) (src_addr : Prims.nat)
  (dst_addr : Prims.nat) (offset : Prims.nat) :
  Space_Memory.memory FStar_Pervasives_Native.option=
  let src_cell_idx = src_addr + (offset / (Prims.of_int (8))) in
  let dst_cell_idx = dst_addr + (offset / (Prims.of_int (8))) in
  let byte_pos = (mod) offset (Prims.of_int (8)) in
  if byte_pos >= (Prims.of_int (8))
  then FStar_Pervasives_Native.None
  else
    (let byte_pos_u64 = FStar_UInt64.uint_to_t byte_pos in
     let shift_u64 =
       FStar_UInt64.mul_mod byte_pos_u64 (Stdint.Uint64.of_int (8)) in
     let n32 = FStar_Int_Cast.uint64_to_uint32 shift_u64 in
     if (FStar_UInt32.v n32) >= (Prims.of_int (64))
     then FStar_Pervasives_Native.None
     else
       (match Space_Memory.mem_fetch mem src_cell_idx with
        | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
        | FStar_Pervasives_Native.Some src_cell ->
            let shifted = Space_Bitwise.shift_right src_cell n32 in
            let byte_val =
              FStar_UInt64.logand shifted (Stdint.Uint64.of_int (0xFF)) in
            (match Space_Memory.mem_fetch mem dst_cell_idx with
             | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
             | FStar_Pervasives_Native.Some dst_cell ->
                 let mask =
                   Space_Bitwise.shift_left (Stdint.Uint64.of_int (0xFF)) n32 in
                 let inv_mask = FStar_UInt64.lognot mask in
                 let cleared = FStar_UInt64.logand dst_cell inv_mask in
                 let new_byte = Space_Bitwise.shift_left byte_val n32 in
                 let new_cell = FStar_UInt64.logor cleared new_byte in
                 Space_Memory.mem_store mem dst_cell_idx new_cell)))
let rec bytes_copy_loop (mem : Space_Memory.memory) (src_addr : Prims.nat)
  (dst_addr : Prims.nat) (offset : Prims.nat) (remaining : Prims.nat) :
  Space_Memory.memory FStar_Pervasives_Native.option=
  if remaining = Prims.int_zero
  then FStar_Pervasives_Native.Some mem
  else
    (match copy_one_byte mem src_addr dst_addr offset with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some mem' ->
         bytes_copy_loop mem' src_addr dst_addr (offset + Prims.int_one)
           (remaining - Prims.int_one))
let exec_bytes_copy (u : Space_Universe.universe) : exec_result=
  if Prims.op_Negation (Space_Universe.is_live u)
  then ExecError "universe not live"
  else
    (match u.Space_Universe.stack with
     | len_cell::dst_cell::src_cell::rest ->
         let src_addr = FStar_UInt64.v src_cell in
         let dst_addr = FStar_UInt64.v dst_cell in
         let len = FStar_UInt64.v len_cell in
         if len = Prims.int_zero
         then
           ExecOk
             {
               Space_Universe.id = (u.Space_Universe.id);
               Space_Universe.name = (u.Space_Universe.name);
               Space_Universe.discipline = (u.Space_Universe.discipline);
               Space_Universe.stack = rest;
               Space_Universe.memory = (u.Space_Universe.memory);
               Space_Universe.capacity = (u.Space_Universe.capacity);
               Space_Universe.state = (u.Space_Universe.state)
             }
         else
           (match bytes_copy_loop u.Space_Universe.memory src_addr dst_addr
                    Prims.int_zero len
            with
            | FStar_Pervasives_Native.None ->
                ExecError "bytes_copy: copy failed"
            | FStar_Pervasives_Native.Some mem' ->
                ExecOk
                  {
                    Space_Universe.id = (u.Space_Universe.id);
                    Space_Universe.name = (u.Space_Universe.name);
                    Space_Universe.discipline = (u.Space_Universe.discipline);
                    Space_Universe.stack = rest;
                    Space_Universe.memory = mem';
                    Space_Universe.capacity = (u.Space_Universe.capacity);
                    Space_Universe.state = (u.Space_Universe.state)
                  })
     | uu___1 -> ExecError "bytes_copy: stack underflow")
let exec_prim (op : Space_Instruction.prim_op) (u : Space_Universe.universe)
  : exec_result=
  match op with
  | Space_Instruction.PrimDup -> exec_stack_prim op u
  | Space_Instruction.PrimDrop -> exec_stack_prim op u
  | Space_Instruction.PrimSwap -> exec_stack_prim op u
  | Space_Instruction.PrimOver -> exec_stack_prim op u
  | Space_Instruction.PrimRot -> exec_stack_prim op u
  | Space_Instruction.PrimNip -> exec_stack_prim op u
  | Space_Instruction.PrimTuck -> exec_stack_prim op u
  | Space_Instruction.PrimPick -> exec_stack_prim op u
  | Space_Instruction.PrimAdd -> exec_arith_prim op u
  | Space_Instruction.PrimSub -> exec_arith_prim op u
  | Space_Instruction.PrimMul -> exec_arith_prim op u
  | Space_Instruction.PrimDivU -> exec_arith_prim op u
  | Space_Instruction.PrimDivS -> exec_arith_prim op u
  | Space_Instruction.PrimMod -> exec_arith_prim op u
  | Space_Instruction.PrimNeg -> exec_arith_prim op u
  | Space_Instruction.PrimMin -> exec_arith_prim op u
  | Space_Instruction.PrimMax -> exec_arith_prim op u
  | Space_Instruction.PrimAnd -> exec_bitwise_prim op u
  | Space_Instruction.PrimOr -> exec_bitwise_prim op u
  | Space_Instruction.PrimXor -> exec_bitwise_prim op u
  | Space_Instruction.PrimNot -> exec_bitwise_prim op u
  | Space_Instruction.PrimShl -> exec_bitwise_prim op u
  | Space_Instruction.PrimShr -> exec_bitwise_prim op u
  | Space_Instruction.PrimEq -> exec_compare_prim op u
  | Space_Instruction.PrimNeq -> exec_compare_prim op u
  | Space_Instruction.PrimLtU -> exec_compare_prim op u
  | Space_Instruction.PrimGtU -> exec_compare_prim op u
  | Space_Instruction.PrimLtS -> exec_compare_prim op u
  | Space_Instruction.PrimGtS -> exec_compare_prim op u
  | Space_Instruction.PrimFetch -> exec_mem_fetch u
  | Space_Instruction.PrimStore -> exec_mem_store u
  | Space_Instruction.PrimAlloc -> exec_mem_alloc u
  | Space_Instruction.PrimBytesAlloc ->
      ExecError "bytes-alloc: routed through step_prim"
  | Space_Instruction.PrimBytesFetch -> exec_bytes_fetch u
  | Space_Instruction.PrimBytesStore -> exec_bytes_store u
  | Space_Instruction.PrimBytesLen ->
      ExecError "bytes-len: routed through step_prim"
  | Space_Instruction.PrimBytesCopy -> exec_bytes_copy u
  | Space_Instruction.PrimBorrowPointer ->
      ExecError "borrow ops routed through step_prim"
  | Space_Instruction.PrimReturnPointer ->
      ExecError "borrow ops routed through step_prim"
  | Space_Instruction.PrimDropPointer ->
      ExecError "borrow ops routed through step_prim"
  | Space_Instruction.PrimFetchBorrowed ->
      ExecError "borrow ops routed through step_prim"
  | Space_Instruction.PrimStoreBorrowed ->
      ExecError "borrow ops routed through step_prim"
  | Space_Instruction.PrimFetchAndEnd ->
      ExecError "borrow ops routed through step_prim"
  | Space_Instruction.PrimStoreAndEnd ->
      ExecError "borrow ops routed through step_prim"
  | Space_Instruction.PrimOffsetBorrowed ->
      ExecError "borrow ops routed through step_prim"
  | Space_Instruction.PrimWarpFetch ->
      ExecError "warp ops routed through step_prim"
  | Space_Instruction.PrimWarpStore ->
      ExecError "warp ops routed through step_prim"
  | Space_Instruction.PrimWarpAdvance ->
      ExecError "warp ops routed through step_prim"
  | Space_Instruction.PrimWarpFollow ->
      ExecError "warp ops routed through step_prim"
  | Space_Instruction.PrimWarpPosition ->
      ExecError "warp ops routed through step_prim"
  | Space_Instruction.PrimWarpRestore ->
      ExecError "warp ops routed through step_prim"
  | Space_Instruction.PrimWarpNull ->
      ExecError "warp ops routed through step_prim"
  | Space_Instruction.PrimCreateText ->
      ExecError "text ops routed through step_prim"
  | Space_Instruction.PrimTextByteLength ->
      ExecError "text ops routed through step_prim"
  | Space_Instruction.PrimTextGraphemeCount ->
      ExecError "text ops routed through step_prim"
  | Space_Instruction.PrimTextIsSimple ->
      ExecError "text ops routed through step_prim"
  | Space_Instruction.PrimTextGraphemeAt ->
      ExecError "text ops routed through step_prim"
  | Space_Instruction.PrimTextGraphemeFirst ->
      ExecError "text ops routed through step_prim"
  | Space_Instruction.PrimTextGraphemeLast ->
      ExecError "text ops routed through step_prim"
  | Space_Instruction.PrimTextSlice ->
      ExecError "text ops routed through step_prim"
  | Space_Instruction.PrimTextConcat ->
      ExecError "text ops routed through step_prim"
  | Space_Instruction.PrimTextEqual ->
      ExecError "text ops routed through step_prim"
  | Space_Instruction.PrimTextCompare ->
      ExecError "text ops routed through step_prim"
  | Space_Instruction.PrimTextWarpHasGrapheme ->
      ExecError "text warp ops routed through step_prim"
  | Space_Instruction.PrimTextWarpCurrentGrapheme ->
      ExecError "text warp ops routed through step_prim"
  | Space_Instruction.PrimTextWarpNextGrapheme ->
      ExecError "text warp ops routed through step_prim"
  | Space_Instruction.PrimTextWarpGraphemeIndex ->
      ExecError "text warp ops routed through step_prim"
  | Space_Instruction.PrimTextWarpGotoGrapheme ->
      ExecError "text warp ops routed through step_prim"
  | Space_Instruction.PrimGraphemeByteLength ->
      ExecError "grapheme ops routed through step_prim"
  | Space_Instruction.PrimGraphemeIsAscii ->
      ExecError "grapheme ops routed through step_prim"
  | Space_Instruction.PrimGraphemeCodePoints ->
      ExecError "grapheme ops routed through step_prim"
  | Space_Instruction.PrimTextCodePointCount ->
      ExecError "codepoint ops routed through step_prim"
  | Space_Instruction.PrimTextCodePointAt ->
      ExecError "codepoint ops routed through step_prim"
  | Space_Instruction.PrimEmit ->
      ExecError "I/O ops routed through step_prim"
  | Space_Instruction.PrimKey -> ExecError "I/O ops routed through step_prim"
  | Space_Instruction.PrimEmitGrapheme ->
      ExecError "I/O ops routed through step_prim"
  | Space_Instruction.PrimHalt -> ExecHalt
  | Space_Instruction.PrimTextNormalizeNfc ->
      ExecError "normalization ops routed through step_prim"
  | Space_Instruction.PrimTextNormalizeNfd ->
      ExecError "normalization ops routed through step_prim"
  | Space_Instruction.PrimTextNormalizeNfkc ->
      ExecError "normalization ops routed through step_prim"
  | Space_Instruction.PrimTextNormalizeNfkd ->
      ExecError "normalization ops routed through step_prim"
  | Space_Instruction.PrimTextToUpper ->
      ExecError "case mapping ops routed through step_prim"
  | Space_Instruction.PrimTextToLower ->
      ExecError "case mapping ops routed through step_prim"
  | Space_Instruction.PrimTextToTitle ->
      ExecError "case mapping ops routed through step_prim"
