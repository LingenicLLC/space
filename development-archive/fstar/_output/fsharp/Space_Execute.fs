#light "off"
module Space_Execute
type exec_result =
| ExecOk of Space_Universe.universe
| ExecError of Prims.string
| ExecHalt


let uu___is_ExecOk : exec_result  ->  Prims.bool = (fun ( projectee  :  exec_result ) -> (match (projectee) with
| ExecOk (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__ExecOk__item___0 : exec_result  ->  Space_Universe.universe = (fun ( projectee  :  exec_result ) -> (match (projectee) with
| ExecOk (_0) -> begin
_0
end))


let uu___is_ExecError : exec_result  ->  Prims.bool = (fun ( projectee  :  exec_result ) -> (match (projectee) with
| ExecError (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__ExecError__item___0 : exec_result  ->  Prims.string = (fun ( projectee  :  exec_result ) -> (match (projectee) with
| ExecError (_0) -> begin
_0
end))


let uu___is_ExecHalt : exec_result  ->  Prims.bool = (fun ( projectee  :  exec_result ) -> (match (projectee) with
| ExecHalt -> begin
true
end
| uu___ -> begin
false
end))


let exec_stack_prim : Space_Instruction.prim_op  ->  Space_Universe.universe  ->  exec_result = (fun ( op  :  Space_Instruction.prim_op ) ( u  :  Space_Universe.universe ) -> (match ((not ((Space_Universe.is_live u)))) with
| true -> begin
ExecError ("universe not live")
end
| uu___ -> begin
(

let s = u.stack
in (

let result = (match (op) with
| Space_Instruction.PrimDup -> begin
(Space_Stack.dup s)
end
| Space_Instruction.PrimDrop -> begin
(Space_Stack.drop s)
end
| Space_Instruction.PrimSwap -> begin
(Space_Stack.swap s)
end
| Space_Instruction.PrimOver -> begin
(Space_Stack.over s)
end
| Space_Instruction.PrimRot -> begin
(Space_Stack.rot s)
end
| Space_Instruction.PrimNip -> begin
(Space_Stack.nip s)
end
| Space_Instruction.PrimTuck -> begin
(Space_Stack.tuck s)
end
| Space_Instruction.PrimPick -> begin
(Space_Stack.pick s)
end
| uu___1 -> begin
FStar_Pervasives_Native.None
end)
in (match (result) with
| FStar_Pervasives_Native.Some (s') -> begin
ExecOk ({Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = s'; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state})
end
| FStar_Pervasives_Native.None -> begin
ExecError ("stack operation failed")
end)))
end))


let exec_arith_prim : Space_Instruction.prim_op  ->  Space_Universe.universe  ->  exec_result = (fun ( op  :  Space_Instruction.prim_op ) ( u  :  Space_Universe.universe ) -> (match ((not ((Space_Universe.is_live u)))) with
| true -> begin
ExecError ("universe not live")
end
| uu___ -> begin
(

let s = u.stack
in (

let result = (match (op) with
| Space_Instruction.PrimAdd -> begin
(Space_Arithmetic.stack_add s)
end
| Space_Instruction.PrimSub -> begin
(Space_Arithmetic.stack_sub s)
end
| Space_Instruction.PrimMul -> begin
(Space_Arithmetic.stack_mul s)
end
| Space_Instruction.PrimDivU -> begin
(Space_Arithmetic.stack_div s)
end
| Space_Instruction.PrimDivS -> begin
(Space_Arithmetic.stack_div_signed s)
end
| Space_Instruction.PrimMod -> begin
(Space_Arithmetic.stack_mod s)
end
| Space_Instruction.PrimNeg -> begin
(Space_Arithmetic.stack_negate s)
end
| Space_Instruction.PrimMin -> begin
(Space_Arithmetic.stack_min s)
end
| Space_Instruction.PrimMax -> begin
(Space_Arithmetic.stack_max s)
end
| uu___1 -> begin
FStar_Pervasives_Native.None
end)
in (match (result) with
| FStar_Pervasives_Native.Some (s') -> begin
ExecOk ({Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = s'; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state})
end
| FStar_Pervasives_Native.None -> begin
ExecError ("arithmetic operation failed")
end)))
end))


let exec_bitwise_prim : Space_Instruction.prim_op  ->  Space_Universe.universe  ->  exec_result = (fun ( op  :  Space_Instruction.prim_op ) ( u  :  Space_Universe.universe ) -> (match ((not ((Space_Universe.is_live u)))) with
| true -> begin
ExecError ("universe not live")
end
| uu___ -> begin
(

let s = u.stack
in (

let result = (match (op) with
| Space_Instruction.PrimAnd -> begin
(Space_Bitwise.stack_and s)
end
| Space_Instruction.PrimOr -> begin
(Space_Bitwise.stack_or s)
end
| Space_Instruction.PrimXor -> begin
(Space_Bitwise.stack_xor s)
end
| Space_Instruction.PrimNot -> begin
(Space_Bitwise.stack_not s)
end
| Space_Instruction.PrimShl -> begin
(Space_Bitwise.stack_shl s)
end
| Space_Instruction.PrimShr -> begin
(Space_Bitwise.stack_shr s)
end
| uu___1 -> begin
FStar_Pervasives_Native.None
end)
in (match (result) with
| FStar_Pervasives_Native.Some (s') -> begin
ExecOk ({Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = s'; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state})
end
| FStar_Pervasives_Native.None -> begin
ExecError ("bitwise operation failed")
end)))
end))


let exec_compare_prim : Space_Instruction.prim_op  ->  Space_Universe.universe  ->  exec_result = (fun ( op  :  Space_Instruction.prim_op ) ( u  :  Space_Universe.universe ) -> (match ((not ((Space_Universe.is_live u)))) with
| true -> begin
ExecError ("universe not live")
end
| uu___ -> begin
(

let s = u.stack
in (

let result = (match (op) with
| Space_Instruction.PrimEq -> begin
(Space_Comparison.stack_eq s)
end
| Space_Instruction.PrimNeq -> begin
(Space_Comparison.stack_neq s)
end
| Space_Instruction.PrimLtU -> begin
(Space_Comparison.stack_lt s)
end
| Space_Instruction.PrimGtU -> begin
(Space_Comparison.stack_gt s)
end
| Space_Instruction.PrimLtS -> begin
(Space_Comparison.stack_lt_signed s)
end
| Space_Instruction.PrimGtS -> begin
(Space_Comparison.stack_gt_signed s)
end
| uu___1 -> begin
FStar_Pervasives_Native.None
end)
in (match (result) with
| FStar_Pervasives_Native.Some (s') -> begin
ExecOk ({Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = s'; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state})
end
| FStar_Pervasives_Native.None -> begin
ExecError ("comparison operation failed")
end)))
end))


let exec_mem_fetch : Space_Universe.universe  ->  exec_result = (fun ( u  :  Space_Universe.universe ) -> (match ((not ((Space_Universe.is_live u)))) with
| true -> begin
ExecError ("universe not live")
end
| uu___ -> begin
(match (u.stack) with
| (addr_cell)::rest -> begin
(

let addr = (FStar_UInt64.v addr_cell)
in (match ((Space_Memory.mem_fetch u.memory addr)) with
| FStar_Pervasives_Native.None -> begin
ExecError ("fetch: invalid address")
end
| FStar_Pervasives_Native.Some (value) -> begin
ExecOk ({Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (value)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state})
end))
end
| uu___1 -> begin
ExecError ("fetch: stack underflow")
end)
end))


let exec_mem_store : Space_Universe.universe  ->  exec_result = (fun ( u  :  Space_Universe.universe ) -> (match ((not ((Space_Universe.is_live u)))) with
| true -> begin
ExecError ("universe not live")
end
| uu___ -> begin
(match (u.stack) with
| (addr_cell)::(value)::rest -> begin
(

let addr = (FStar_UInt64.v addr_cell)
in (match ((Space_Memory.mem_store u.memory addr value)) with
| FStar_Pervasives_Native.None -> begin
ExecError ("store: invalid address")
end
| FStar_Pervasives_Native.Some (mem') -> begin
ExecOk ({Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = rest; Space_Universe.memory = mem'; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state})
end))
end
| uu___1 -> begin
ExecError ("store: stack underflow")
end)
end))


let exec_mem_alloc : Space_Universe.universe  ->  exec_result = (fun ( u  :  Space_Universe.universe ) -> (match ((not ((Space_Universe.is_live u)))) with
| true -> begin
ExecError ("universe not live")
end
| uu___ -> begin
(match (u.stack) with
| (n_cell)::rest -> begin
(

let n = (FStar_UInt64.v n_cell)
in (

let uu___1 = (Space_Memory.mem_alloc u.memory n)
in (match (uu___1) with
| (mem', base_addr) -> begin
(

let addr_cell = (match ((base_addr < (Prims.pow2 (Prims.parse_int "64")))) with
| true -> begin
(FStar_UInt64.uint_to_t base_addr)
end
| uu___2 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in ExecOk ({Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (addr_cell)::rest; Space_Universe.memory = mem'; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}))
end)))
end
| uu___1 -> begin
ExecError ("alloc: stack underflow")
end)
end))


let exec_bytes_fetch : Space_Universe.universe  ->  exec_result = (fun ( u  :  Space_Universe.universe ) -> (match ((not ((Space_Universe.is_live u)))) with
| true -> begin
ExecError ("universe not live")
end
| uu___ -> begin
(match (u.stack) with
| (offset_cell)::(addr_cell)::rest -> begin
(

let addr = (FStar_UInt64.v addr_cell)
in (

let offset = (FStar_UInt64.v offset_cell)
in (

let cell_idx = (addr + (offset / (Prims.parse_int "8")))
in (

let byte_pos = (FStar_UInt64.rem offset_cell (FStar_UInt64.uint_to_t ((Prims.parse_int "8"))))
in (

let shift_u64 = (FStar_UInt64.mul_mod byte_pos (FStar_UInt64.uint_to_t ((Prims.parse_int "8"))))
in (

let n32 = (FStar_Int_Cast.uint64_to_uint32 shift_u64)
in (match ((Space_Memory.mem_fetch u.memory cell_idx)) with
| FStar_Pervasives_Native.None -> begin
ExecError ("bytes_fetch: invalid address")
end
| FStar_Pervasives_Native.Some (cell_val) -> begin
(match (((FStar_UInt32.v n32) < (Prims.parse_int "64"))) with
| true -> begin
(

let shifted = (Space_Bitwise.shift_right cell_val n32)
in (

let byte_val = (FStar_UInt64.rem shifted (FStar_UInt64.uint_to_t ((Prims.parse_int "256"))))
in ExecOk ({Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (byte_val)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state})))
end
| uu___1 -> begin
ExecError ("bytes_fetch: internal error")
end)
end)))))))
end
| uu___1 -> begin
ExecError ("bytes_fetch: stack underflow")
end)
end))


let exec_bytes_store : Space_Universe.universe  ->  exec_result = (fun ( u  :  Space_Universe.universe ) -> (match ((not ((Space_Universe.is_live u)))) with
| true -> begin
ExecError ("universe not live")
end
| uu___ -> begin
(match (u.stack) with
| (offset_cell)::(addr_cell)::(byte_cell)::rest -> begin
(

let addr = (FStar_UInt64.v addr_cell)
in (

let offset = (FStar_UInt64.v offset_cell)
in (

let cell_idx = (addr + (offset / (Prims.parse_int "8")))
in (

let byte_pos = (FStar_UInt64.rem offset_cell (FStar_UInt64.uint_to_t ((Prims.parse_int "8"))))
in (

let shift_u64 = (FStar_UInt64.mul_mod byte_pos (FStar_UInt64.uint_to_t ((Prims.parse_int "8"))))
in (

let n32 = (FStar_Int_Cast.uint64_to_uint32 shift_u64)
in (

let byte_masked = (FStar_UInt64.logand byte_cell (FStar_UInt64.uint_to_t ((Prims.parse_int "0xFF"))))
in (match ((Space_Memory.mem_fetch u.memory cell_idx)) with
| FStar_Pervasives_Native.None -> begin
ExecError ("bytes_store: invalid address")
end
| FStar_Pervasives_Native.Some (old_cell) -> begin
(match (((FStar_UInt32.v n32) < (Prims.parse_int "64"))) with
| true -> begin
(

let mask = (Space_Bitwise.shift_left (FStar_UInt64.uint_to_t ((Prims.parse_int "0xFF"))) n32)
in (

let inv_mask = (FStar_UInt64.lognot mask)
in (

let cleared = (FStar_UInt64.logand old_cell inv_mask)
in (

let new_byte = (Space_Bitwise.shift_left byte_masked n32)
in (

let new_cell = (FStar_UInt64.logor cleared new_byte)
in (match ((Space_Memory.mem_store u.memory cell_idx new_cell)) with
| FStar_Pervasives_Native.None -> begin
ExecError ("bytes_store: store failed")
end
| FStar_Pervasives_Native.Some (mem') -> begin
ExecOk ({Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = rest; Space_Universe.memory = mem'; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state})
end))))))
end
| uu___1 -> begin
ExecError ("bytes_store: internal error")
end)
end))))))))
end
| uu___1 -> begin
ExecError ("bytes_store: stack underflow")
end)
end))


let copy_one_byte : Space_Memory.memory  ->  Prims.nat  ->  Prims.nat  ->  Prims.nat  ->  Space_Memory.memory FStar_Pervasives_Native.option = (fun ( mem  :  Space_Memory.memory ) ( src_addr  :  Prims.nat ) ( dst_addr  :  Prims.nat ) ( offset  :  Prims.nat ) -> (

let src_cell_idx = (src_addr + (offset / (Prims.parse_int "8")))
in (

let dst_cell_idx = (dst_addr + (offset / (Prims.parse_int "8")))
in (

let byte_pos = (Prims.mod_f offset (Prims.parse_int "8"))
in (match ((byte_pos >= (Prims.parse_int "8"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(

let byte_pos_u64 = (FStar_UInt64.uint_to_t byte_pos)
in (

let shift_u64 = (FStar_UInt64.mul_mod byte_pos_u64 (FStar_UInt64.uint_to_t ((Prims.parse_int "8"))))
in (

let n32 = (FStar_Int_Cast.uint64_to_uint32 shift_u64)
in (match (((FStar_UInt32.v n32) >= (Prims.parse_int "64"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
(match ((Space_Memory.mem_fetch mem src_cell_idx)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (src_cell) -> begin
(

let shifted = (Space_Bitwise.shift_right src_cell n32)
in (

let byte_val = (FStar_UInt64.logand shifted (FStar_UInt64.uint_to_t ((Prims.parse_int "0xFF"))))
in (match ((Space_Memory.mem_fetch mem dst_cell_idx)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (dst_cell) -> begin
(

let mask = (Space_Bitwise.shift_left (FStar_UInt64.uint_to_t ((Prims.parse_int "0xFF"))) n32)
in (

let inv_mask = (FStar_UInt64.lognot mask)
in (

let cleared = (FStar_UInt64.logand dst_cell inv_mask)
in (

let new_byte = (Space_Bitwise.shift_left byte_val n32)
in (

let new_cell = (FStar_UInt64.logor cleared new_byte)
in (Space_Memory.mem_store mem dst_cell_idx new_cell))))))
end)))
end)
end))))
end)))))


let rec bytes_copy_loop : Space_Memory.memory  ->  Prims.nat  ->  Prims.nat  ->  Prims.nat  ->  Prims.nat  ->  Space_Memory.memory FStar_Pervasives_Native.option = (fun ( mem  :  Space_Memory.memory ) ( src_addr  :  Prims.nat ) ( dst_addr  :  Prims.nat ) ( offset  :  Prims.nat ) ( remaining  :  Prims.nat ) -> (match ((Prims.op_Equality remaining (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.Some (mem)
end
| uu___ -> begin
(match ((copy_one_byte mem src_addr dst_addr offset)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (mem') -> begin
(bytes_copy_loop mem' src_addr dst_addr (offset + (Prims.parse_int "1")) (remaining - (Prims.parse_int "1")))
end)
end))


let exec_bytes_copy : Space_Universe.universe  ->  exec_result = (fun ( u  :  Space_Universe.universe ) -> (match ((not ((Space_Universe.is_live u)))) with
| true -> begin
ExecError ("universe not live")
end
| uu___ -> begin
(match (u.stack) with
| (len_cell)::(dst_cell)::(src_cell)::rest -> begin
(

let src_addr = (FStar_UInt64.v src_cell)
in (

let dst_addr = (FStar_UInt64.v dst_cell)
in (

let len = (FStar_UInt64.v len_cell)
in (match ((Prims.op_Equality len (Prims.parse_int "0"))) with
| true -> begin
ExecOk ({Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state})
end
| uu___1 -> begin
(match ((bytes_copy_loop u.memory src_addr dst_addr (Prims.parse_int "0") len)) with
| FStar_Pervasives_Native.None -> begin
ExecError ("bytes_copy: copy failed")
end
| FStar_Pervasives_Native.Some (mem') -> begin
ExecOk ({Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = rest; Space_Universe.memory = mem'; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state})
end)
end))))
end
| uu___1 -> begin
ExecError ("bytes_copy: stack underflow")
end)
end))


let exec_prim : Space_Instruction.prim_op  ->  Space_Universe.universe  ->  exec_result = (fun ( op  :  Space_Instruction.prim_op ) ( u  :  Space_Universe.universe ) -> (match (op) with
| Space_Instruction.PrimDup -> begin
(exec_stack_prim op u)
end
| Space_Instruction.PrimDrop -> begin
(exec_stack_prim op u)
end
| Space_Instruction.PrimSwap -> begin
(exec_stack_prim op u)
end
| Space_Instruction.PrimOver -> begin
(exec_stack_prim op u)
end
| Space_Instruction.PrimRot -> begin
(exec_stack_prim op u)
end
| Space_Instruction.PrimNip -> begin
(exec_stack_prim op u)
end
| Space_Instruction.PrimTuck -> begin
(exec_stack_prim op u)
end
| Space_Instruction.PrimPick -> begin
(exec_stack_prim op u)
end
| Space_Instruction.PrimAdd -> begin
(exec_arith_prim op u)
end
| Space_Instruction.PrimSub -> begin
(exec_arith_prim op u)
end
| Space_Instruction.PrimMul -> begin
(exec_arith_prim op u)
end
| Space_Instruction.PrimDivU -> begin
(exec_arith_prim op u)
end
| Space_Instruction.PrimDivS -> begin
(exec_arith_prim op u)
end
| Space_Instruction.PrimMod -> begin
(exec_arith_prim op u)
end
| Space_Instruction.PrimNeg -> begin
(exec_arith_prim op u)
end
| Space_Instruction.PrimMin -> begin
(exec_arith_prim op u)
end
| Space_Instruction.PrimMax -> begin
(exec_arith_prim op u)
end
| Space_Instruction.PrimAnd -> begin
(exec_bitwise_prim op u)
end
| Space_Instruction.PrimOr -> begin
(exec_bitwise_prim op u)
end
| Space_Instruction.PrimXor -> begin
(exec_bitwise_prim op u)
end
| Space_Instruction.PrimNot -> begin
(exec_bitwise_prim op u)
end
| Space_Instruction.PrimShl -> begin
(exec_bitwise_prim op u)
end
| Space_Instruction.PrimShr -> begin
(exec_bitwise_prim op u)
end
| Space_Instruction.PrimEq -> begin
(exec_compare_prim op u)
end
| Space_Instruction.PrimNeq -> begin
(exec_compare_prim op u)
end
| Space_Instruction.PrimLtU -> begin
(exec_compare_prim op u)
end
| Space_Instruction.PrimGtU -> begin
(exec_compare_prim op u)
end
| Space_Instruction.PrimLtS -> begin
(exec_compare_prim op u)
end
| Space_Instruction.PrimGtS -> begin
(exec_compare_prim op u)
end
| Space_Instruction.PrimFetch -> begin
(exec_mem_fetch u)
end
| Space_Instruction.PrimStore -> begin
(exec_mem_store u)
end
| Space_Instruction.PrimAlloc -> begin
(exec_mem_alloc u)
end
| Space_Instruction.PrimBytesAlloc -> begin
ExecError ("bytes-alloc: routed through step_prim")
end
| Space_Instruction.PrimBytesFetch -> begin
(exec_bytes_fetch u)
end
| Space_Instruction.PrimBytesStore -> begin
(exec_bytes_store u)
end
| Space_Instruction.PrimBytesLen -> begin
ExecError ("bytes-len: routed through step_prim")
end
| Space_Instruction.PrimBytesCopy -> begin
(exec_bytes_copy u)
end
| Space_Instruction.PrimBorrowPointer -> begin
ExecError ("borrow ops routed through step_prim")
end
| Space_Instruction.PrimReturnPointer -> begin
ExecError ("borrow ops routed through step_prim")
end
| Space_Instruction.PrimDropPointer -> begin
ExecError ("borrow ops routed through step_prim")
end
| Space_Instruction.PrimFetchBorrowed -> begin
ExecError ("borrow ops routed through step_prim")
end
| Space_Instruction.PrimStoreBorrowed -> begin
ExecError ("borrow ops routed through step_prim")
end
| Space_Instruction.PrimFetchAndEnd -> begin
ExecError ("borrow ops routed through step_prim")
end
| Space_Instruction.PrimStoreAndEnd -> begin
ExecError ("borrow ops routed through step_prim")
end
| Space_Instruction.PrimOffsetBorrowed -> begin
ExecError ("borrow ops routed through step_prim")
end
| Space_Instruction.PrimWarpFetch -> begin
ExecError ("warp ops routed through step_prim")
end
| Space_Instruction.PrimWarpStore -> begin
ExecError ("warp ops routed through step_prim")
end
| Space_Instruction.PrimWarpAdvance -> begin
ExecError ("warp ops routed through step_prim")
end
| Space_Instruction.PrimWarpFollow -> begin
ExecError ("warp ops routed through step_prim")
end
| Space_Instruction.PrimWarpPosition -> begin
ExecError ("warp ops routed through step_prim")
end
| Space_Instruction.PrimWarpRestore -> begin
ExecError ("warp ops routed through step_prim")
end
| Space_Instruction.PrimWarpNull -> begin
ExecError ("warp ops routed through step_prim")
end
| Space_Instruction.PrimCreateText -> begin
ExecError ("text ops routed through step_prim")
end
| Space_Instruction.PrimTextByteLength -> begin
ExecError ("text ops routed through step_prim")
end
| Space_Instruction.PrimTextGraphemeCount -> begin
ExecError ("text ops routed through step_prim")
end
| Space_Instruction.PrimTextIsSimple -> begin
ExecError ("text ops routed through step_prim")
end
| Space_Instruction.PrimTextGraphemeAt -> begin
ExecError ("text ops routed through step_prim")
end
| Space_Instruction.PrimTextGraphemeFirst -> begin
ExecError ("text ops routed through step_prim")
end
| Space_Instruction.PrimTextGraphemeLast -> begin
ExecError ("text ops routed through step_prim")
end
| Space_Instruction.PrimTextSlice -> begin
ExecError ("text ops routed through step_prim")
end
| Space_Instruction.PrimTextConcat -> begin
ExecError ("text ops routed through step_prim")
end
| Space_Instruction.PrimTextEqual -> begin
ExecError ("text ops routed through step_prim")
end
| Space_Instruction.PrimTextCompare -> begin
ExecError ("text ops routed through step_prim")
end
| Space_Instruction.PrimTextWarpHasGrapheme -> begin
ExecError ("text warp ops routed through step_prim")
end
| Space_Instruction.PrimTextWarpCurrentGrapheme -> begin
ExecError ("text warp ops routed through step_prim")
end
| Space_Instruction.PrimTextWarpNextGrapheme -> begin
ExecError ("text warp ops routed through step_prim")
end
| Space_Instruction.PrimTextWarpGraphemeIndex -> begin
ExecError ("text warp ops routed through step_prim")
end
| Space_Instruction.PrimTextWarpGotoGrapheme -> begin
ExecError ("text warp ops routed through step_prim")
end
| Space_Instruction.PrimGraphemeByteLength -> begin
ExecError ("grapheme ops routed through step_prim")
end
| Space_Instruction.PrimGraphemeIsAscii -> begin
ExecError ("grapheme ops routed through step_prim")
end
| Space_Instruction.PrimGraphemeCodePoints -> begin
ExecError ("grapheme ops routed through step_prim")
end
| Space_Instruction.PrimTextCodePointCount -> begin
ExecError ("codepoint ops routed through step_prim")
end
| Space_Instruction.PrimTextCodePointAt -> begin
ExecError ("codepoint ops routed through step_prim")
end
| Space_Instruction.PrimEmit -> begin
ExecError ("I/O ops routed through step_prim")
end
| Space_Instruction.PrimKey -> begin
ExecError ("I/O ops routed through step_prim")
end
| Space_Instruction.PrimEmitGrapheme -> begin
ExecError ("I/O ops routed through step_prim")
end
| Space_Instruction.PrimHalt -> begin
ExecHalt
end
| Space_Instruction.PrimTextNormalizeNfc -> begin
ExecError ("normalization ops routed through step_prim")
end
| Space_Instruction.PrimTextNormalizeNfd -> begin
ExecError ("normalization ops routed through step_prim")
end
| Space_Instruction.PrimTextNormalizeNfkc -> begin
ExecError ("normalization ops routed through step_prim")
end
| Space_Instruction.PrimTextNormalizeNfkd -> begin
ExecError ("normalization ops routed through step_prim")
end
| Space_Instruction.PrimTextToUpper -> begin
ExecError ("case mapping ops routed through step_prim")
end
| Space_Instruction.PrimTextToLower -> begin
ExecError ("case mapping ops routed through step_prim")
end
| Space_Instruction.PrimTextToTitle -> begin
ExecError ("case mapping ops routed through step_prim")
end))




