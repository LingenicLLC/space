#light "off"
module Space_Step
type step_result =
| StepOk of Space_Interpreter.machine
| StepHalt of Space_Interpreter.machine
| StepError of (Space_Interpreter.machine * Prims.string)


let uu___is_StepOk : step_result  ->  Prims.bool = (fun ( projectee  :  step_result ) -> (match (projectee) with
| StepOk (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__StepOk__item___0 : step_result  ->  Space_Interpreter.machine = (fun ( projectee  :  step_result ) -> (match (projectee) with
| StepOk (_0) -> begin
_0
end))


let uu___is_StepHalt : step_result  ->  Prims.bool = (fun ( projectee  :  step_result ) -> (match (projectee) with
| StepHalt (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__StepHalt__item___0 : step_result  ->  Space_Interpreter.machine = (fun ( projectee  :  step_result ) -> (match (projectee) with
| StepHalt (_0) -> begin
_0
end))


let uu___is_StepError : step_result  ->  Prims.bool = (fun ( projectee  :  step_result ) -> (match (projectee) with
| StepError (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__StepError__item___0 : step_result  ->  (Space_Interpreter.machine * Prims.string) = (fun ( projectee  :  step_result ) -> (match (projectee) with
| StepError (_0) -> begin
_0
end))


let step_push : Space_Interpreter.machine  ->  Space_Types.cell  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( v  :  Space_Types.cell ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match ((Space_Universe.universe_push u v)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("push failed")))
end
| FStar_Pervasives_Native.Some (u') -> begin
StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u')))
end)
end))


let step_branch_zero : Space_Interpreter.machine  ->  Space_Instruction.ip  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( target  :  Space_Instruction.ip ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match ((Space_Control.if_true u.stack)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("branch: stack underflow")))
end
| FStar_Pervasives_Native.Some (Space_Control.TakeBranch, s') -> begin
(

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = s'; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u'))))
end
| FStar_Pervasives_Native.Some (Space_Control.SkipBranch, s') -> begin
(

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = s'; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.jump (Space_Interpreter.update_current m u') target)))
end)
end))


let step_branch_nonzero : Space_Interpreter.machine  ->  Space_Instruction.ip  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( target  :  Space_Instruction.ip ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match ((Space_Control.if_false u.stack)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("branch: stack underflow")))
end
| FStar_Pervasives_Native.Some (Space_Control.TakeBranch, s') -> begin
(

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = s'; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u'))))
end
| FStar_Pervasives_Native.Some (Space_Control.SkipBranch, s') -> begin
(

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = s'; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.jump (Space_Interpreter.update_current m u') target)))
end)
end))


let step_call : Space_Interpreter.machine  ->  Space_Instruction.ip  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( target  :  Space_Instruction.ip ) -> StepOk ((Space_Interpreter.call m target)))


let step_return : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (

let m' = (Space_Interpreter.do_return m)
in (match (m'.error) with
| FStar_Pervasives_Native.Some (msg) -> begin
StepError (((m'), (msg)))
end
| FStar_Pervasives_Native.None -> begin
StepOk (m')
end)))


let step_branch : Space_Interpreter.machine  ->  Space_Instruction.ip  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( target  :  Space_Instruction.ip ) -> StepOk ((Space_Interpreter.jump m target)))


let step_create_universe : Space_Interpreter.machine  ->  Space_Types.universe_name  ->  Prims.nat  ->  Space_Types.discipline  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( name  :  Space_Types.universe_name ) ( cap  :  Prims.nat ) ( disc  :  Space_Types.discipline ) -> (

let uu___ = (Space_World.create_universe m.mworld name cap disc)
in (match (uu___) with
| (w', _uid) -> begin
StepOk ((Space_Interpreter.advance_ip {Space_Interpreter.mworld = w'; Space_Interpreter.borrows = m.borrows; Space_Interpreter.warps1 = m.warps1; Space_Interpreter.texts1 = m.texts1; Space_Interpreter.text_warps = m.text_warps; Space_Interpreter.graphemes1 = m.graphemes1; Space_Interpreter.bytes_meta = m.bytes_meta; Space_Interpreter.output = m.output; Space_Interpreter.input = m.input; Space_Interpreter.inst_ptr = m.inst_ptr; Space_Interpreter.return_stack = m.return_stack; Space_Interpreter.running = m.running; Space_Interpreter.error = m.error}))
end)))


let step_end_universe : Space_Interpreter.machine  ->  Space_Types.universe_name  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( name  :  Space_Types.universe_name ) -> (match ((Space_World.find_by_name m.mworld name)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("universe not found")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (((Prims.op_Equality u.discipline Space_Types.Linear) && (not ((Space_Universe.stack_empty u))))) with
| true -> begin
StepError (((m), ("linear universe not empty")))
end
| uu___ -> begin
(

let u' = (Space_Universe.destroy u)
in (

let w' = (Space_World.update_universe m.mworld u')
in StepOk ((Space_Interpreter.advance_ip {Space_Interpreter.mworld = w'; Space_Interpreter.borrows = m.borrows; Space_Interpreter.warps1 = m.warps1; Space_Interpreter.texts1 = m.texts1; Space_Interpreter.text_warps = m.text_warps; Space_Interpreter.graphemes1 = m.graphemes1; Space_Interpreter.bytes_meta = m.bytes_meta; Space_Interpreter.output = m.output; Space_Interpreter.input = m.input; Space_Interpreter.inst_ptr = m.inst_ptr; Space_Interpreter.return_stack = m.return_stack; Space_Interpreter.running = m.running; Space_Interpreter.error = m.error}))))
end)
end))


let step_release_universe : Space_Interpreter.machine  ->  Space_Types.universe_name  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( name  :  Space_Types.universe_name ) -> (match ((Space_World.find_by_name m.mworld name)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("universe not found")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match ((Space_Universe.release u)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("release failed: not affine or not live")))
end
| FStar_Pervasives_Native.Some (u') -> begin
(

let w' = (Space_World.update_universe m.mworld u')
in StepOk ((Space_Interpreter.advance_ip {Space_Interpreter.mworld = w'; Space_Interpreter.borrows = m.borrows; Space_Interpreter.warps1 = m.warps1; Space_Interpreter.texts1 = m.texts1; Space_Interpreter.text_warps = m.text_warps; Space_Interpreter.graphemes1 = m.graphemes1; Space_Interpreter.bytes_meta = m.bytes_meta; Space_Interpreter.output = m.output; Space_Interpreter.input = m.input; Space_Interpreter.inst_ptr = m.inst_ptr; Space_Interpreter.return_stack = m.return_stack; Space_Interpreter.running = m.running; Space_Interpreter.error = m.error})))
end)
end))


let step_transfer : Space_Interpreter.machine  ->  Space_Types.universe_name  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( name  :  Space_Types.universe_name ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (src) -> begin
(match ((Space_World.find_by_name m.mworld name)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("target universe not found")))
end
| FStar_Pervasives_Native.Some (dst) -> begin
(match (src.stack) with
| [] -> begin
StepError (((m), ("transfer: stack underflow")))
end
| (v)::rest -> begin
(

let src' = {Space_Universe.id = src.id; Space_Universe.name = src.name; Space_Universe.discipline = src.discipline; Space_Universe.stack = rest; Space_Universe.memory = src.memory; Space_Universe.capacity = src.capacity; Space_Universe.state = src.state}
in (

let dst' = {Space_Universe.id = dst.id; Space_Universe.name = dst.name; Space_Universe.discipline = dst.discipline; Space_Universe.stack = (v)::dst.stack; Space_Universe.memory = dst.memory; Space_Universe.capacity = dst.capacity; Space_Universe.state = dst.state}
in (

let w' = (Space_World.update_universe (Space_World.update_universe m.mworld src') dst')
in StepOk ((Space_Interpreter.advance_ip {Space_Interpreter.mworld = w'; Space_Interpreter.borrows = m.borrows; Space_Interpreter.warps1 = m.warps1; Space_Interpreter.texts1 = m.texts1; Space_Interpreter.text_warps = m.text_warps; Space_Interpreter.graphemes1 = m.graphemes1; Space_Interpreter.bytes_meta = m.bytes_meta; Space_Interpreter.output = m.output; Space_Interpreter.input = m.input; Space_Interpreter.inst_ptr = m.inst_ptr; Space_Interpreter.return_stack = m.return_stack; Space_Interpreter.running = m.running; Space_Interpreter.error = m.error})))))
end)
end)
end))


let read_byte_from_mem : Space_Memory.memory  ->  Prims.nat  ->  Prims.nat  ->  FStar_UInt8.t FStar_Pervasives_Native.option = (fun ( mem  :  Space_Memory.memory ) ( base_addr  :  Prims.nat ) ( offset  :  Prims.nat ) -> (match ((offset >= (Prims.parse_int "18446744073709551616"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(

let offset64 = (FStar_UInt64.uint_to_t offset)
in (

let cell_offset = (FStar_UInt64.div offset64 (FStar_UInt64.uint_to_t ((Prims.parse_int "8"))))
in (

let cell_idx = (base_addr + (FStar_UInt64.v cell_offset))
in (

let byte_pos = (FStar_UInt64.rem offset64 (FStar_UInt64.uint_to_t ((Prims.parse_int "8"))))
in (

let shift_amt = (FStar_UInt64.mul byte_pos (FStar_UInt64.uint_to_t ((Prims.parse_int "8"))))
in (match ((Space_Memory.mem_fetch mem cell_idx)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (cell_val) -> begin
(match (((FStar_UInt64.v shift_amt) >= (Prims.parse_int "64"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
(

let n32 = (FStar_UInt32.uint_to_t (FStar_UInt64.v shift_amt))
in (

let shifted = (FStar_UInt64.shift_right cell_val n32)
in (

let byte_val = (FStar_UInt64.v (FStar_UInt64.logand shifted (FStar_UInt64.uint_to_t ((Prims.parse_int "0xFF")))))
in (match ((byte_val < (Prims.parse_int "256"))) with
| true -> begin
FStar_Pervasives_Native.Some ((FStar_UInt8.uint_to_t byte_val))
end
| uu___2 -> begin
FStar_Pervasives_Native.None
end))))
end)
end))))))
end))


let rec read_bytes_from_mem : Space_Memory.memory  ->  Prims.nat  ->  Prims.nat  ->  FStar_UInt8.t Prims.list FStar_Pervasives_Native.option = (fun ( mem  :  Space_Memory.memory ) ( addr  :  Prims.nat ) ( remaining  :  Prims.nat ) -> (match ((Prims.op_Equality remaining (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.Some ([])
end
| uu___ -> begin
(match ((read_byte_from_mem mem addr (Prims.parse_int "0"))) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (b) -> begin
(match ((read_bytes_from_mem mem addr (remaining - (Prims.parse_int "1")))) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (rest) -> begin
FStar_Pervasives_Native.Some ((b)::rest)
end)
end)
end))


let rec read_bytes_seq : Space_Memory.memory  ->  Prims.nat  ->  Prims.nat  ->  Prims.nat  ->  FStar_UInt8.t Prims.list FStar_Pervasives_Native.option = (fun ( mem  :  Space_Memory.memory ) ( base_addr  :  Prims.nat ) ( offset  :  Prims.nat ) ( remaining  :  Prims.nat ) -> (match ((Prims.op_Equality remaining (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.Some ([])
end
| uu___ -> begin
(match ((read_byte_from_mem mem base_addr offset)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (b) -> begin
(match ((read_bytes_seq mem base_addr (offset + (Prims.parse_int "1")) (remaining - (Prims.parse_int "1")))) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (rest) -> begin
FStar_Pervasives_Native.Some ((b)::rest)
end)
end)
end))


let step_create_text : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (len_cell)::(addr_cell)::rest -> begin
(

let len = (FStar_UInt64.v len_cell)
in (

let addr = (FStar_UInt64.v addr_cell)
in (match ((Prims.op_Equality len (Prims.parse_int "0"))) with
| true -> begin
(

let uu___ = (Space_Interpreter.add_text m.texts1 Space_Text_Types.empty_text)
in (match (uu___) with
| (tt', handle) -> begin
(

let handle_cell = (match ((handle < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t handle)
end
| uu___1 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (handle_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___1 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___1.mworld; Space_Interpreter.borrows = uu___1.borrows; Space_Interpreter.warps1 = uu___1.warps1; Space_Interpreter.texts1 = tt'; Space_Interpreter.text_warps = uu___1.text_warps; Space_Interpreter.graphemes1 = uu___1.graphemes1; Space_Interpreter.bytes_meta = uu___1.bytes_meta; Space_Interpreter.output = uu___1.output; Space_Interpreter.input = uu___1.input; Space_Interpreter.inst_ptr = uu___1.inst_ptr; Space_Interpreter.return_stack = uu___1.return_stack; Space_Interpreter.running = uu___1.running; Space_Interpreter.error = uu___1.error})))))
end))
end
| uu___ -> begin
(match ((read_bytes_seq u.memory addr (Prims.parse_int "0") len)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("create-text: failed to read memory")))
end
| FStar_Pervasives_Native.Some (bytes) -> begin
(match ((Space_Text_Create.text_from_bytes bytes)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("create-text: invalid UTF-8")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(

let uu___1 = (Space_Interpreter.add_text m.texts1 t)
in (match (uu___1) with
| (tt', handle) -> begin
(

let handle_cell = (match ((handle < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t handle)
end
| uu___2 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (handle_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___2 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___2.mworld; Space_Interpreter.borrows = uu___2.borrows; Space_Interpreter.warps1 = uu___2.warps1; Space_Interpreter.texts1 = tt'; Space_Interpreter.text_warps = uu___2.text_warps; Space_Interpreter.graphemes1 = uu___2.graphemes1; Space_Interpreter.bytes_meta = uu___2.bytes_meta; Space_Interpreter.output = uu___2.output; Space_Interpreter.input = uu___2.input; Space_Interpreter.inst_ptr = uu___2.inst_ptr; Space_Interpreter.return_stack = uu___2.return_stack; Space_Interpreter.running = uu___2.running; Space_Interpreter.error = uu___2.error})))))
end))
end)
end)
end)))
end
| uu___ -> begin
StepError (((m), ("create-text: stack underflow")))
end)
end))


let step_text_byte_length : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (handle_cell)::rest -> begin
(

let handle = (FStar_UInt64.v handle_cell)
in (match ((Space_Interpreter.get_text m.texts1 handle)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-byte-length: invalid handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(

let len = (Space_Text_Create.text_byte_length t)
in (

let len_cell = (match ((len < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t len)
end
| uu___ -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (len_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u'))))))
end))
end
| uu___ -> begin
StepError (((m), ("text-byte-length: stack underflow")))
end)
end))


let step_text_grapheme_count : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (handle_cell)::rest -> begin
(

let handle = (FStar_UInt64.v handle_cell)
in (match ((Space_Interpreter.get_text m.texts1 handle)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-grapheme-count: invalid handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(

let count = (Space_Text_Create.text_grapheme_count t)
in (

let count_cell = (match ((count < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t count)
end
| uu___ -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (count_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u'))))))
end))
end
| uu___ -> begin
StepError (((m), ("text-grapheme-count: stack underflow")))
end)
end))


let step_text_is_simple : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (handle_cell)::rest -> begin
(

let handle = (FStar_UInt64.v handle_cell)
in (match ((Space_Interpreter.get_text m.texts1 handle)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-is-simple: invalid handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(

let flag = (match ((Space_Text_Create.text_is_simple t)) with
| true -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "1")))
end
| uu___ -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (flag)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u')))))
end))
end
| uu___ -> begin
StepError (((m), ("text-is-simple: stack underflow")))
end)
end))


let step_text_equal : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (h2_cell)::(h1_cell)::rest -> begin
(

let h1 = (FStar_UInt64.v h1_cell)
in (

let h2 = (FStar_UInt64.v h2_cell)
in (match ((((Space_Interpreter.get_text m.texts1 h1)), ((Space_Interpreter.get_text m.texts1 h2)))) with
| (FStar_Pervasives_Native.Some (t1), FStar_Pervasives_Native.Some (t2)) -> begin
(

let eq = (match ((Space_Text_Ops.text_equal t1 t2)) with
| true -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "1")))
end
| uu___ -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (eq)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u')))))
end
| (uu___, uu___1) -> begin
StepError (((m), ("text-equal: invalid handle")))
end)))
end
| uu___ -> begin
StepError (((m), ("text-equal: stack underflow")))
end)
end))


let step_text_concat : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (h2_cell)::(h1_cell)::rest -> begin
(

let h1 = (FStar_UInt64.v h1_cell)
in (

let h2 = (FStar_UInt64.v h2_cell)
in (match ((((Space_Interpreter.get_text m.texts1 h1)), ((Space_Interpreter.get_text m.texts1 h2)))) with
| (FStar_Pervasives_Native.Some (t1), FStar_Pervasives_Native.Some (t2)) -> begin
(

let t3 = (Space_Text_Ops.text_concat t1 t2)
in (

let uu___ = (Space_Interpreter.add_text m.texts1 t3)
in (match (uu___) with
| (tt', h3) -> begin
(

let h3_cell = (match ((h3 < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t h3)
end
| uu___1 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (h3_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___1 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___1.mworld; Space_Interpreter.borrows = uu___1.borrows; Space_Interpreter.warps1 = uu___1.warps1; Space_Interpreter.texts1 = tt'; Space_Interpreter.text_warps = uu___1.text_warps; Space_Interpreter.graphemes1 = uu___1.graphemes1; Space_Interpreter.bytes_meta = uu___1.bytes_meta; Space_Interpreter.output = uu___1.output; Space_Interpreter.input = uu___1.input; Space_Interpreter.inst_ptr = uu___1.inst_ptr; Space_Interpreter.return_stack = uu___1.return_stack; Space_Interpreter.running = uu___1.running; Space_Interpreter.error = uu___1.error})))))
end)))
end
| (uu___, uu___1) -> begin
StepError (((m), ("text-concat: invalid handle")))
end)))
end
| uu___ -> begin
StepError (((m), ("text-concat: stack underflow")))
end)
end))


let step_text_slice : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (end_cell)::(start_cell)::(handle_cell)::rest -> begin
(

let handle = (FStar_UInt64.v handle_cell)
in (

let start = (FStar_UInt64.v start_cell)
in (

let finish = (FStar_UInt64.v end_cell)
in (match ((Space_Interpreter.get_text m.texts1 handle)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-slice: invalid handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(match ((Space_Text_Ops.text_slice t start finish)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-slice: invalid range")))
end
| FStar_Pervasives_Native.Some (t') -> begin
(

let uu___ = (Space_Interpreter.add_text m.texts1 t')
in (match (uu___) with
| (tt', h') -> begin
(

let h_cell = (match ((h' < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t h')
end
| uu___1 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (h_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___1 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___1.mworld; Space_Interpreter.borrows = uu___1.borrows; Space_Interpreter.warps1 = uu___1.warps1; Space_Interpreter.texts1 = tt'; Space_Interpreter.text_warps = uu___1.text_warps; Space_Interpreter.graphemes1 = uu___1.graphemes1; Space_Interpreter.bytes_meta = uu___1.bytes_meta; Space_Interpreter.output = uu___1.output; Space_Interpreter.input = uu___1.input; Space_Interpreter.inst_ptr = uu___1.inst_ptr; Space_Interpreter.return_stack = uu___1.return_stack; Space_Interpreter.running = uu___1.running; Space_Interpreter.error = uu___1.error})))))
end))
end)
end))))
end
| uu___ -> begin
StepError (((m), ("text-slice: stack underflow")))
end)
end))


let step_bytes_alloc : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (n_cell)::rest -> begin
(

let n = (FStar_UInt64.v n_cell)
in (match ((Prims.op_Equality n (Prims.parse_int "0"))) with
| true -> begin
StepError (((m), ("bytes-alloc: cannot allocate zero bytes")))
end
| uu___ -> begin
(

let cells_needed = ((n + (Prims.parse_int "7")) / (Prims.parse_int "8"))
in (

let uu___1 = (Space_Memory.mem_alloc u.memory cells_needed)
in (match (uu___1) with
| (mem', base_addr) -> begin
(

let addr_cell = (match ((base_addr < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t base_addr)
end
| uu___2 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (addr_cell)::rest; Space_Universe.memory = mem'; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in (

let bm' = (Space_Interpreter.record_bytes_alloc m.bytes_meta base_addr n)
in StepOk ((Space_Interpreter.advance_ip (

let uu___2 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___2.mworld; Space_Interpreter.borrows = uu___2.borrows; Space_Interpreter.warps1 = uu___2.warps1; Space_Interpreter.texts1 = uu___2.texts1; Space_Interpreter.text_warps = uu___2.text_warps; Space_Interpreter.graphemes1 = uu___2.graphemes1; Space_Interpreter.bytes_meta = bm'; Space_Interpreter.output = uu___2.output; Space_Interpreter.input = uu___2.input; Space_Interpreter.inst_ptr = uu___2.inst_ptr; Space_Interpreter.return_stack = uu___2.return_stack; Space_Interpreter.running = uu___2.running; Space_Interpreter.error = uu___2.error}))))))
end)))
end))
end
| uu___ -> begin
StepError (((m), ("bytes-alloc: stack underflow")))
end)
end))


let step_bytes_len : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (addr_cell)::rest -> begin
(

let addr = (FStar_UInt64.v addr_cell)
in (match ((Space_Interpreter.get_bytes_len m.bytes_meta addr)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("bytes-len: unknown allocation")))
end
| FStar_Pervasives_Native.Some (len) -> begin
(

let len_cell = (match ((len < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t len)
end
| uu___ -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (len_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u')))))
end))
end
| uu___ -> begin
StepError (((m), ("bytes-len: stack underflow")))
end)
end))


let is_bytes_meta_prim : Space_Instruction.prim_op  ->  Prims.bool = (fun ( op  :  Space_Instruction.prim_op ) -> (match (op) with
| Space_Instruction.PrimBytesAlloc -> begin
true
end
| Space_Instruction.PrimBytesLen -> begin
true
end
| uu___ -> begin
false
end))


let step_bytes_meta_prim : Space_Interpreter.machine  ->  Space_Instruction.prim_op  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( op  :  Space_Instruction.prim_op ) -> (match (op) with
| Space_Instruction.PrimBytesAlloc -> begin
(step_bytes_alloc m)
end
| Space_Instruction.PrimBytesLen -> begin
(step_bytes_len m)
end
| uu___ -> begin
StepError (((m), ("unknown bytes primitive")))
end))


let step_emit : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (char_cell)::rest -> begin
(

let char_val = (FStar_UInt64.v char_cell)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in (

let m' = (

let uu___ = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___.mworld; Space_Interpreter.borrows = uu___.borrows; Space_Interpreter.warps1 = uu___.warps1; Space_Interpreter.texts1 = uu___.texts1; Space_Interpreter.text_warps = uu___.text_warps; Space_Interpreter.graphemes1 = uu___.graphemes1; Space_Interpreter.bytes_meta = uu___.bytes_meta; Space_Interpreter.output = (FStar_List_Tot_Base.append m.output ((char_val)::[])); Space_Interpreter.input = uu___.input; Space_Interpreter.inst_ptr = uu___.inst_ptr; Space_Interpreter.return_stack = uu___.return_stack; Space_Interpreter.running = uu___.running; Space_Interpreter.error = uu___.error})
in StepOk ((Space_Interpreter.advance_ip m')))))
end
| uu___ -> begin
StepError (((m), ("emit: stack underflow")))
end)
end))


let step_key : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (m.input) with
| (char_val)::rest_input -> begin
(

let char_cell = (match ((char_val < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t char_val)
end
| uu___ -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (char_cell)::u.stack; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in (

let m' = (

let uu___ = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___.mworld; Space_Interpreter.borrows = uu___.borrows; Space_Interpreter.warps1 = uu___.warps1; Space_Interpreter.texts1 = uu___.texts1; Space_Interpreter.text_warps = uu___.text_warps; Space_Interpreter.graphemes1 = uu___.graphemes1; Space_Interpreter.bytes_meta = uu___.bytes_meta; Space_Interpreter.output = uu___.output; Space_Interpreter.input = rest_input; Space_Interpreter.inst_ptr = uu___.inst_ptr; Space_Interpreter.return_stack = uu___.return_stack; Space_Interpreter.running = uu___.running; Space_Interpreter.error = uu___.error})
in StepOk ((Space_Interpreter.advance_ip m')))))
end
| [] -> begin
(

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = ((FStar_UInt64.uint_to_t ((Prims.parse_int "0xFFFFFFFFFFFFFFFF"))))::u.stack; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u'))))
end)
end))


let is_io_prim : Space_Instruction.prim_op  ->  Prims.bool = (fun ( op  :  Space_Instruction.prim_op ) -> (match (op) with
| Space_Instruction.PrimEmit -> begin
true
end
| Space_Instruction.PrimKey -> begin
true
end
| uu___ -> begin
false
end))


let step_io_prim : Space_Interpreter.machine  ->  Space_Instruction.prim_op  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( op  :  Space_Instruction.prim_op ) -> (match (op) with
| Space_Instruction.PrimEmit -> begin
(step_emit m)
end
| Space_Instruction.PrimKey -> begin
(step_key m)
end
| uu___ -> begin
StepError (((m), ("unknown I/O primitive")))
end))


let step_borrow_pointer : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match ((Space_Borrow.borrow_pointer u)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("borrow-pointer: failed")))
end
| FStar_Pervasives_Native.Some (b, u') -> begin
(

let bs' = (Space_Borrow.add_borrow m.borrows b)
in StepOk ((Space_Interpreter.advance_ip (

let uu___ = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___.mworld; Space_Interpreter.borrows = bs'; Space_Interpreter.warps1 = uu___.warps1; Space_Interpreter.texts1 = uu___.texts1; Space_Interpreter.text_warps = uu___.text_warps; Space_Interpreter.graphemes1 = uu___.graphemes1; Space_Interpreter.bytes_meta = uu___.bytes_meta; Space_Interpreter.output = uu___.output; Space_Interpreter.input = uu___.input; Space_Interpreter.inst_ptr = uu___.inst_ptr; Space_Interpreter.return_stack = uu___.return_stack; Space_Interpreter.running = uu___.running; Space_Interpreter.error = uu___.error}))))
end)
end))


let step_return_pointer : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (m.borrows.borrows) with
| [] -> begin
StepError (((m), ("return-pointer: no active borrow")))
end
| (b)::rest -> begin
(match ((Space_Borrow.return_borrowed b u)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("return-pointer: failed")))
end
| FStar_Pervasives_Native.Some (u') -> begin
(

let bs' = {Space_Borrow.borrows = rest}
in StepOk ((Space_Interpreter.advance_ip (

let uu___ = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___.mworld; Space_Interpreter.borrows = bs'; Space_Interpreter.warps1 = uu___.warps1; Space_Interpreter.texts1 = uu___.texts1; Space_Interpreter.text_warps = uu___.text_warps; Space_Interpreter.graphemes1 = uu___.graphemes1; Space_Interpreter.bytes_meta = uu___.bytes_meta; Space_Interpreter.output = uu___.output; Space_Interpreter.input = uu___.input; Space_Interpreter.inst_ptr = uu___.inst_ptr; Space_Interpreter.return_stack = uu___.return_stack; Space_Interpreter.running = uu___.running; Space_Interpreter.error = uu___.error}))))
end)
end)
end))


let step_drop_pointer : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match (m.borrows.borrows) with
| [] -> begin
StepError (((m), ("drop-pointer: no active borrow")))
end
| (b)::rest -> begin
(

let bs' = {Space_Borrow.borrows = rest}
in StepOk ((Space_Interpreter.advance_ip {Space_Interpreter.mworld = m.mworld; Space_Interpreter.borrows = bs'; Space_Interpreter.warps1 = m.warps1; Space_Interpreter.texts1 = m.texts1; Space_Interpreter.text_warps = m.text_warps; Space_Interpreter.graphemes1 = m.graphemes1; Space_Interpreter.bytes_meta = m.bytes_meta; Space_Interpreter.output = m.output; Space_Interpreter.input = m.input; Space_Interpreter.inst_ptr = m.inst_ptr; Space_Interpreter.return_stack = m.return_stack; Space_Interpreter.running = m.running; Space_Interpreter.error = m.error})))
end))


let step_fetch_borrowed : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (m.borrows.borrows) with
| [] -> begin
StepError (((m), ("fetch-borrowed: no active borrow")))
end
| (b)::uu___ -> begin
(

let um = {Space_BorrowOps.universe = u; Space_BorrowOps.memory = u.memory}
in (match ((Space_BorrowOps.fetch_borrowed b um)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("fetch-borrowed: failed")))
end
| FStar_Pervasives_Native.Some (v) -> begin
(

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (v)::u.stack; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u'))))
end))
end)
end))


let step_store_borrowed : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| [] -> begin
StepError (((m), ("store-borrowed: stack underflow")))
end
| (v)::rest -> begin
(match (m.borrows.borrows) with
| [] -> begin
StepError (((m), ("store-borrowed: no active borrow")))
end
| (b)::uu___ -> begin
(

let um = {Space_BorrowOps.universe = u; Space_BorrowOps.memory = u.memory}
in (match ((Space_BorrowOps.store_borrowed b v um)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("store-borrowed: failed")))
end
| FStar_Pervasives_Native.Some (um') -> begin
(

let u' = (

let uu___1 = um'.universe
in {Space_Universe.id = uu___1.id; Space_Universe.name = uu___1.name; Space_Universe.discipline = uu___1.discipline; Space_Universe.stack = rest; Space_Universe.memory = um'.memory; Space_Universe.capacity = uu___1.capacity; Space_Universe.state = uu___1.state})
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u'))))
end))
end)
end)
end))


let step_fetch_and_end : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (m.borrows.borrows) with
| [] -> begin
StepError (((m), ("fetch-and-end: no active borrow")))
end
| (b)::rest -> begin
(

let um = {Space_BorrowOps.universe = u; Space_BorrowOps.memory = u.memory}
in (match ((Space_BorrowOps.fetch_and_end b um)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("fetch-and-end: failed")))
end
| FStar_Pervasives_Native.Some (v, uu___) -> begin
(

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (v)::u.stack; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in (

let bs' = {Space_Borrow.borrows = rest}
in StepOk ((Space_Interpreter.advance_ip (

let uu___1 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___1.mworld; Space_Interpreter.borrows = bs'; Space_Interpreter.warps1 = uu___1.warps1; Space_Interpreter.texts1 = uu___1.texts1; Space_Interpreter.text_warps = uu___1.text_warps; Space_Interpreter.graphemes1 = uu___1.graphemes1; Space_Interpreter.bytes_meta = uu___1.bytes_meta; Space_Interpreter.output = uu___1.output; Space_Interpreter.input = uu___1.input; Space_Interpreter.inst_ptr = uu___1.inst_ptr; Space_Interpreter.return_stack = uu___1.return_stack; Space_Interpreter.running = uu___1.running; Space_Interpreter.error = uu___1.error})))))
end))
end)
end))


let step_store_and_end : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| [] -> begin
StepError (((m), ("store-and-end: stack underflow")))
end
| (v)::rest -> begin
(match (m.borrows.borrows) with
| [] -> begin
StepError (((m), ("store-and-end: no active borrow")))
end
| (b)::brest -> begin
(

let um = {Space_BorrowOps.universe = u; Space_BorrowOps.memory = u.memory}
in (match ((Space_BorrowOps.store_and_end b v um)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("store-and-end: failed")))
end
| FStar_Pervasives_Native.Some (um', uu___) -> begin
(

let u' = (

let uu___1 = um'.universe
in {Space_Universe.id = uu___1.id; Space_Universe.name = uu___1.name; Space_Universe.discipline = uu___1.discipline; Space_Universe.stack = rest; Space_Universe.memory = um'.memory; Space_Universe.capacity = uu___1.capacity; Space_Universe.state = uu___1.state})
in (

let bs' = {Space_Borrow.borrows = brest}
in StepOk ((Space_Interpreter.advance_ip (

let uu___1 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___1.mworld; Space_Interpreter.borrows = bs'; Space_Interpreter.warps1 = uu___1.warps1; Space_Interpreter.texts1 = uu___1.texts1; Space_Interpreter.text_warps = uu___1.text_warps; Space_Interpreter.graphemes1 = uu___1.graphemes1; Space_Interpreter.bytes_meta = uu___1.bytes_meta; Space_Interpreter.output = uu___1.output; Space_Interpreter.input = uu___1.input; Space_Interpreter.inst_ptr = uu___1.inst_ptr; Space_Interpreter.return_stack = uu___1.return_stack; Space_Interpreter.running = uu___1.running; Space_Interpreter.error = uu___1.error})))))
end))
end)
end)
end))


let step_offset_borrowed : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| [] -> begin
StepError (((m), ("offset-borrowed: stack underflow")))
end
| (offset)::rest -> begin
(match (m.borrows.borrows) with
| [] -> begin
StepError (((m), ("offset-borrowed: no active borrow")))
end
| (b)::brest -> begin
(

let b' = (Space_BorrowOps.offset_borrowed b offset)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in (

let bs' = {Space_Borrow.borrows = (b')::brest}
in StepOk ((Space_Interpreter.advance_ip (

let uu___ = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___.mworld; Space_Interpreter.borrows = bs'; Space_Interpreter.warps1 = uu___.warps1; Space_Interpreter.texts1 = uu___.texts1; Space_Interpreter.text_warps = uu___.text_warps; Space_Interpreter.graphemes1 = uu___.graphemes1; Space_Interpreter.bytes_meta = uu___.bytes_meta; Space_Interpreter.output = uu___.output; Space_Interpreter.input = uu___.input; Space_Interpreter.inst_ptr = uu___.inst_ptr; Space_Interpreter.return_stack = uu___.return_stack; Space_Interpreter.running = uu___.running; Space_Interpreter.error = uu___.error}))))))
end)
end)
end))


let is_borrow_prim : Space_Instruction.prim_op  ->  Prims.bool = (fun ( op  :  Space_Instruction.prim_op ) -> (match (op) with
| Space_Instruction.PrimBorrowPointer -> begin
true
end
| Space_Instruction.PrimReturnPointer -> begin
true
end
| Space_Instruction.PrimDropPointer -> begin
true
end
| Space_Instruction.PrimFetchBorrowed -> begin
true
end
| Space_Instruction.PrimStoreBorrowed -> begin
true
end
| Space_Instruction.PrimFetchAndEnd -> begin
true
end
| Space_Instruction.PrimStoreAndEnd -> begin
true
end
| Space_Instruction.PrimOffsetBorrowed -> begin
true
end
| uu___ -> begin
false
end))


let step_borrow_prim : Space_Interpreter.machine  ->  Space_Instruction.prim_op  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( op  :  Space_Instruction.prim_op ) -> (match (op) with
| Space_Instruction.PrimBorrowPointer -> begin
(step_borrow_pointer m)
end
| Space_Instruction.PrimReturnPointer -> begin
(step_return_pointer m)
end
| Space_Instruction.PrimDropPointer -> begin
(step_drop_pointer m)
end
| Space_Instruction.PrimFetchBorrowed -> begin
(step_fetch_borrowed m)
end
| Space_Instruction.PrimStoreBorrowed -> begin
(step_store_borrowed m)
end
| Space_Instruction.PrimFetchAndEnd -> begin
(step_fetch_and_end m)
end
| Space_Instruction.PrimStoreAndEnd -> begin
(step_store_and_end m)
end
| Space_Instruction.PrimOffsetBorrowed -> begin
(step_offset_borrowed m)
end
| uu___ -> begin
StepError (((m), ("unknown borrow primitive")))
end))


let step_warp_fetch : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match ((Space_Warp.get_implicit_warp m.warps1)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("warp-fetch: no active warp")))
end
| FStar_Pervasives_Native.Some (w) -> begin
(match ((Space_World.find_universe m.mworld w.target_id)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("warp-fetch: target universe not found")))
end
| FStar_Pervasives_Native.Some (target) -> begin
(

let tu = {Space_WarpOps.universe = target; Space_WarpOps.memory = target.memory}
in (match ((Space_WarpOps.warp_fetch w tu)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("warp-fetch: failed")))
end
| FStar_Pervasives_Native.Some (v) -> begin
(

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (v)::u.stack; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u'))))
end))
end)
end)
end))


let step_warp_store : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| [] -> begin
StepError (((m), ("warp-store: stack underflow")))
end
| (v)::rest -> begin
(match ((Space_Warp.get_implicit_warp m.warps1)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("warp-store: no active warp")))
end
| FStar_Pervasives_Native.Some (w) -> begin
(match ((Space_World.find_universe m.mworld w.target_id)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("warp-store: target universe not found")))
end
| FStar_Pervasives_Native.Some (target) -> begin
(

let tu = {Space_WarpOps.universe = target; Space_WarpOps.memory = target.memory}
in (match ((Space_WarpOps.warp_store w v tu)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("warp-store: failed")))
end
| FStar_Pervasives_Native.Some (tu') -> begin
(

let target' = (

let uu___ = tu'.universe
in {Space_Universe.id = uu___.id; Space_Universe.name = uu___.name; Space_Universe.discipline = uu___.discipline; Space_Universe.stack = uu___.stack; Space_Universe.memory = tu'.memory; Space_Universe.capacity = uu___.capacity; Space_Universe.state = uu___.state})
in (

let w' = (Space_World.update_universe m.mworld target')
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___ = (Space_Interpreter.update_current {Space_Interpreter.mworld = w'; Space_Interpreter.borrows = m.borrows; Space_Interpreter.warps1 = m.warps1; Space_Interpreter.texts1 = m.texts1; Space_Interpreter.text_warps = m.text_warps; Space_Interpreter.graphemes1 = m.graphemes1; Space_Interpreter.bytes_meta = m.bytes_meta; Space_Interpreter.output = m.output; Space_Interpreter.input = m.input; Space_Interpreter.inst_ptr = m.inst_ptr; Space_Interpreter.return_stack = m.return_stack; Space_Interpreter.running = m.running; Space_Interpreter.error = m.error} u')
in {Space_Interpreter.mworld = w'; Space_Interpreter.borrows = uu___.borrows; Space_Interpreter.warps1 = uu___.warps1; Space_Interpreter.texts1 = uu___.texts1; Space_Interpreter.text_warps = uu___.text_warps; Space_Interpreter.graphemes1 = uu___.graphemes1; Space_Interpreter.bytes_meta = uu___.bytes_meta; Space_Interpreter.output = uu___.output; Space_Interpreter.input = uu___.input; Space_Interpreter.inst_ptr = uu___.inst_ptr; Space_Interpreter.return_stack = uu___.return_stack; Space_Interpreter.running = uu___.running; Space_Interpreter.error = uu___.error}))))))
end))
end)
end)
end)
end))


let step_warp_advance : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| [] -> begin
StepError (((m), ("warp-advance: stack underflow")))
end
| (offset)::rest -> begin
(match ((Space_Warp.get_implicit_warp m.warps1)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("warp-advance: no active warp")))
end
| FStar_Pervasives_Native.Some (w) -> begin
(

let w' = (Space_WarpOps.warp_advance w offset)
in (

let wt' = (Space_Warp.update_warp m.warps1 w')
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___ = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___.mworld; Space_Interpreter.borrows = uu___.borrows; Space_Interpreter.warps1 = wt'; Space_Interpreter.texts1 = uu___.texts1; Space_Interpreter.text_warps = uu___.text_warps; Space_Interpreter.graphemes1 = uu___.graphemes1; Space_Interpreter.bytes_meta = uu___.bytes_meta; Space_Interpreter.output = uu___.output; Space_Interpreter.input = uu___.input; Space_Interpreter.inst_ptr = uu___.inst_ptr; Space_Interpreter.return_stack = uu___.return_stack; Space_Interpreter.running = uu___.running; Space_Interpreter.error = uu___.error}))))))
end)
end)
end))


let step_warp_follow : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Warp.get_implicit_warp m.warps1)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("warp-follow: no active warp")))
end
| FStar_Pervasives_Native.Some (w) -> begin
(match ((Space_World.find_universe m.mworld w.target_id)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("warp-follow: target universe not found")))
end
| FStar_Pervasives_Native.Some (target) -> begin
(

let tu = {Space_WarpOps.universe = target; Space_WarpOps.memory = target.memory}
in (match ((Space_WarpOps.warp_follow w tu)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("warp-follow: failed")))
end
| FStar_Pervasives_Native.Some (w') -> begin
(

let wt' = (Space_Warp.update_warp m.warps1 w')
in StepOk ((Space_Interpreter.advance_ip {Space_Interpreter.mworld = m.mworld; Space_Interpreter.borrows = m.borrows; Space_Interpreter.warps1 = wt'; Space_Interpreter.texts1 = m.texts1; Space_Interpreter.text_warps = m.text_warps; Space_Interpreter.graphemes1 = m.graphemes1; Space_Interpreter.bytes_meta = m.bytes_meta; Space_Interpreter.output = m.output; Space_Interpreter.input = m.input; Space_Interpreter.inst_ptr = m.inst_ptr; Space_Interpreter.return_stack = m.return_stack; Space_Interpreter.running = m.running; Space_Interpreter.error = m.error})))
end))
end)
end))


let step_warp_position : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match ((Space_Warp.get_implicit_warp m.warps1)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("warp-position: no active warp")))
end
| FStar_Pervasives_Native.Some (w) -> begin
(

let pos = (Space_Warp.warp_position w)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (pos)::u.stack; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u')))))
end)
end))


let step_warp_restore : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| [] -> begin
StepError (((m), ("warp-restore: stack underflow")))
end
| (pos)::rest -> begin
(match ((Space_Warp.get_implicit_warp m.warps1)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("warp-restore: no active warp")))
end
| FStar_Pervasives_Native.Some (w) -> begin
(match ((Space_WarpOps.warp_restore w pos)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("warp-restore: position not saved")))
end
| FStar_Pervasives_Native.Some (w') -> begin
(

let wt' = (Space_Warp.update_warp m.warps1 w')
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___ = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___.mworld; Space_Interpreter.borrows = uu___.borrows; Space_Interpreter.warps1 = wt'; Space_Interpreter.texts1 = uu___.texts1; Space_Interpreter.text_warps = uu___.text_warps; Space_Interpreter.graphemes1 = uu___.graphemes1; Space_Interpreter.bytes_meta = uu___.bytes_meta; Space_Interpreter.output = uu___.output; Space_Interpreter.input = uu___.input; Space_Interpreter.inst_ptr = uu___.inst_ptr; Space_Interpreter.return_stack = uu___.return_stack; Space_Interpreter.running = uu___.running; Space_Interpreter.error = uu___.error})))))
end)
end)
end)
end))


let step_warp_null : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match ((Space_Warp.get_implicit_warp m.warps1)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("warp-null: no active warp")))
end
| FStar_Pervasives_Native.Some (w) -> begin
(

let flag = (match ((Space_WarpOps.warp_at_null w)) with
| true -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "1")))
end
| uu___ -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (flag)::u.stack; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u')))))
end)
end))


let is_warp_prim : Space_Instruction.prim_op  ->  Prims.bool = (fun ( op  :  Space_Instruction.prim_op ) -> (match (op) with
| Space_Instruction.PrimWarpFetch -> begin
true
end
| Space_Instruction.PrimWarpStore -> begin
true
end
| Space_Instruction.PrimWarpAdvance -> begin
true
end
| Space_Instruction.PrimWarpFollow -> begin
true
end
| Space_Instruction.PrimWarpPosition -> begin
true
end
| Space_Instruction.PrimWarpRestore -> begin
true
end
| Space_Instruction.PrimWarpNull -> begin
true
end
| uu___ -> begin
false
end))


let step_warp_prim : Space_Interpreter.machine  ->  Space_Instruction.prim_op  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( op  :  Space_Instruction.prim_op ) -> (match (op) with
| Space_Instruction.PrimWarpFetch -> begin
(step_warp_fetch m)
end
| Space_Instruction.PrimWarpStore -> begin
(step_warp_store m)
end
| Space_Instruction.PrimWarpAdvance -> begin
(step_warp_advance m)
end
| Space_Instruction.PrimWarpFollow -> begin
(step_warp_follow m)
end
| Space_Instruction.PrimWarpPosition -> begin
(step_warp_position m)
end
| Space_Instruction.PrimWarpRestore -> begin
(step_warp_restore m)
end
| Space_Instruction.PrimWarpNull -> begin
(step_warp_null m)
end
| uu___ -> begin
StepError (((m), ("unknown warp primitive")))
end))


let is_text_prim : Space_Instruction.prim_op  ->  Prims.bool = (fun ( op  :  Space_Instruction.prim_op ) -> (match (op) with
| Space_Instruction.PrimCreateText -> begin
true
end
| Space_Instruction.PrimTextByteLength -> begin
true
end
| Space_Instruction.PrimTextGraphemeCount -> begin
true
end
| Space_Instruction.PrimTextIsSimple -> begin
true
end
| Space_Instruction.PrimTextGraphemeAt -> begin
true
end
| Space_Instruction.PrimTextGraphemeFirst -> begin
true
end
| Space_Instruction.PrimTextGraphemeLast -> begin
true
end
| Space_Instruction.PrimTextSlice -> begin
true
end
| Space_Instruction.PrimTextConcat -> begin
true
end
| Space_Instruction.PrimTextEqual -> begin
true
end
| Space_Instruction.PrimTextCompare -> begin
true
end
| Space_Instruction.PrimTextWarpHasGrapheme -> begin
true
end
| Space_Instruction.PrimTextWarpCurrentGrapheme -> begin
true
end
| Space_Instruction.PrimTextWarpNextGrapheme -> begin
true
end
| Space_Instruction.PrimTextWarpGraphemeIndex -> begin
true
end
| Space_Instruction.PrimTextWarpGotoGrapheme -> begin
true
end
| Space_Instruction.PrimGraphemeByteLength -> begin
true
end
| Space_Instruction.PrimGraphemeIsAscii -> begin
true
end
| Space_Instruction.PrimGraphemeCodePoints -> begin
true
end
| Space_Instruction.PrimTextCodePointCount -> begin
true
end
| Space_Instruction.PrimTextCodePointAt -> begin
true
end
| Space_Instruction.PrimTextNormalizeNfc -> begin
true
end
| Space_Instruction.PrimTextNormalizeNfd -> begin
true
end
| Space_Instruction.PrimTextNormalizeNfkc -> begin
true
end
| Space_Instruction.PrimTextNormalizeNfkd -> begin
true
end
| Space_Instruction.PrimTextToUpper -> begin
true
end
| Space_Instruction.PrimTextToLower -> begin
true
end
| Space_Instruction.PrimTextToTitle -> begin
true
end
| Space_Instruction.PrimEmitGrapheme -> begin
true
end
| uu___ -> begin
false
end))


let step_text_normalize_nfc : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (handle_cell)::rest -> begin
(

let handle = (FStar_UInt64.v handle_cell)
in (match ((Space_Interpreter.get_text m.texts1 handle)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-normalize-nfc: invalid handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(match ((Space_Text_Normalize.normalize_nfc t)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-normalize-nfc: normalization failed")))
end
| FStar_Pervasives_Native.Some (t') -> begin
(

let uu___ = (Space_Interpreter.add_text m.texts1 t')
in (match (uu___) with
| (tt', h') -> begin
(

let h_cell = (match ((h' < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t h')
end
| uu___1 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (h_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___1 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___1.mworld; Space_Interpreter.borrows = uu___1.borrows; Space_Interpreter.warps1 = uu___1.warps1; Space_Interpreter.texts1 = tt'; Space_Interpreter.text_warps = uu___1.text_warps; Space_Interpreter.graphemes1 = uu___1.graphemes1; Space_Interpreter.bytes_meta = uu___1.bytes_meta; Space_Interpreter.output = uu___1.output; Space_Interpreter.input = uu___1.input; Space_Interpreter.inst_ptr = uu___1.inst_ptr; Space_Interpreter.return_stack = uu___1.return_stack; Space_Interpreter.running = uu___1.running; Space_Interpreter.error = uu___1.error})))))
end))
end)
end))
end
| uu___ -> begin
StepError (((m), ("text-normalize-nfc: stack underflow")))
end)
end))


let step_text_normalize_nfd : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (handle_cell)::rest -> begin
(

let handle = (FStar_UInt64.v handle_cell)
in (match ((Space_Interpreter.get_text m.texts1 handle)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-normalize-nfd: invalid handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(match ((Space_Text_Normalize.normalize_nfd t)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-normalize-nfd: normalization failed")))
end
| FStar_Pervasives_Native.Some (t') -> begin
(

let uu___ = (Space_Interpreter.add_text m.texts1 t')
in (match (uu___) with
| (tt', h') -> begin
(

let h_cell = (match ((h' < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t h')
end
| uu___1 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (h_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___1 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___1.mworld; Space_Interpreter.borrows = uu___1.borrows; Space_Interpreter.warps1 = uu___1.warps1; Space_Interpreter.texts1 = tt'; Space_Interpreter.text_warps = uu___1.text_warps; Space_Interpreter.graphemes1 = uu___1.graphemes1; Space_Interpreter.bytes_meta = uu___1.bytes_meta; Space_Interpreter.output = uu___1.output; Space_Interpreter.input = uu___1.input; Space_Interpreter.inst_ptr = uu___1.inst_ptr; Space_Interpreter.return_stack = uu___1.return_stack; Space_Interpreter.running = uu___1.running; Space_Interpreter.error = uu___1.error})))))
end))
end)
end))
end
| uu___ -> begin
StepError (((m), ("text-normalize-nfd: stack underflow")))
end)
end))


let step_text_normalize_nfkc : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (handle_cell)::rest -> begin
(

let handle = (FStar_UInt64.v handle_cell)
in (match ((Space_Interpreter.get_text m.texts1 handle)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-normalize-nfkc: invalid handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(match ((Space_Text_Normalize.normalize_nfkc t)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-normalize-nfkc: normalization failed")))
end
| FStar_Pervasives_Native.Some (t') -> begin
(

let uu___ = (Space_Interpreter.add_text m.texts1 t')
in (match (uu___) with
| (tt', h') -> begin
(

let h_cell = (match ((h' < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t h')
end
| uu___1 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (h_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___1 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___1.mworld; Space_Interpreter.borrows = uu___1.borrows; Space_Interpreter.warps1 = uu___1.warps1; Space_Interpreter.texts1 = tt'; Space_Interpreter.text_warps = uu___1.text_warps; Space_Interpreter.graphemes1 = uu___1.graphemes1; Space_Interpreter.bytes_meta = uu___1.bytes_meta; Space_Interpreter.output = uu___1.output; Space_Interpreter.input = uu___1.input; Space_Interpreter.inst_ptr = uu___1.inst_ptr; Space_Interpreter.return_stack = uu___1.return_stack; Space_Interpreter.running = uu___1.running; Space_Interpreter.error = uu___1.error})))))
end))
end)
end))
end
| uu___ -> begin
StepError (((m), ("text-normalize-nfkc: stack underflow")))
end)
end))


let step_text_normalize_nfkd : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (handle_cell)::rest -> begin
(

let handle = (FStar_UInt64.v handle_cell)
in (match ((Space_Interpreter.get_text m.texts1 handle)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-normalize-nfkd: invalid handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(match ((Space_Text_Normalize.normalize_nfkd t)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-normalize-nfkd: normalization failed")))
end
| FStar_Pervasives_Native.Some (t') -> begin
(

let uu___ = (Space_Interpreter.add_text m.texts1 t')
in (match (uu___) with
| (tt', h') -> begin
(

let h_cell = (match ((h' < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t h')
end
| uu___1 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (h_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___1 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___1.mworld; Space_Interpreter.borrows = uu___1.borrows; Space_Interpreter.warps1 = uu___1.warps1; Space_Interpreter.texts1 = tt'; Space_Interpreter.text_warps = uu___1.text_warps; Space_Interpreter.graphemes1 = uu___1.graphemes1; Space_Interpreter.bytes_meta = uu___1.bytes_meta; Space_Interpreter.output = uu___1.output; Space_Interpreter.input = uu___1.input; Space_Interpreter.inst_ptr = uu___1.inst_ptr; Space_Interpreter.return_stack = uu___1.return_stack; Space_Interpreter.running = uu___1.running; Space_Interpreter.error = uu___1.error})))))
end))
end)
end))
end
| uu___ -> begin
StepError (((m), ("text-normalize-nfkd: stack underflow")))
end)
end))


let step_text_to_upper : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (handle_cell)::rest -> begin
(

let handle = (FStar_UInt64.v handle_cell)
in (match ((Space_Interpreter.get_text m.texts1 handle)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-to-upper: invalid handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(match ((Space_Text_Case.text_to_upper t)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-to-upper: conversion failed")))
end
| FStar_Pervasives_Native.Some (t') -> begin
(

let uu___ = (Space_Interpreter.add_text m.texts1 t')
in (match (uu___) with
| (tt', h') -> begin
(

let h_cell = (match ((h' < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t h')
end
| uu___1 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (h_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___1 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___1.mworld; Space_Interpreter.borrows = uu___1.borrows; Space_Interpreter.warps1 = uu___1.warps1; Space_Interpreter.texts1 = tt'; Space_Interpreter.text_warps = uu___1.text_warps; Space_Interpreter.graphemes1 = uu___1.graphemes1; Space_Interpreter.bytes_meta = uu___1.bytes_meta; Space_Interpreter.output = uu___1.output; Space_Interpreter.input = uu___1.input; Space_Interpreter.inst_ptr = uu___1.inst_ptr; Space_Interpreter.return_stack = uu___1.return_stack; Space_Interpreter.running = uu___1.running; Space_Interpreter.error = uu___1.error})))))
end))
end)
end))
end
| uu___ -> begin
StepError (((m), ("text-to-upper: stack underflow")))
end)
end))


let step_text_to_lower : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (handle_cell)::rest -> begin
(

let handle = (FStar_UInt64.v handle_cell)
in (match ((Space_Interpreter.get_text m.texts1 handle)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-to-lower: invalid handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(match ((Space_Text_Case.text_to_lower t)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-to-lower: conversion failed")))
end
| FStar_Pervasives_Native.Some (t') -> begin
(

let uu___ = (Space_Interpreter.add_text m.texts1 t')
in (match (uu___) with
| (tt', h') -> begin
(

let h_cell = (match ((h' < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t h')
end
| uu___1 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (h_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___1 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___1.mworld; Space_Interpreter.borrows = uu___1.borrows; Space_Interpreter.warps1 = uu___1.warps1; Space_Interpreter.texts1 = tt'; Space_Interpreter.text_warps = uu___1.text_warps; Space_Interpreter.graphemes1 = uu___1.graphemes1; Space_Interpreter.bytes_meta = uu___1.bytes_meta; Space_Interpreter.output = uu___1.output; Space_Interpreter.input = uu___1.input; Space_Interpreter.inst_ptr = uu___1.inst_ptr; Space_Interpreter.return_stack = uu___1.return_stack; Space_Interpreter.running = uu___1.running; Space_Interpreter.error = uu___1.error})))))
end))
end)
end))
end
| uu___ -> begin
StepError (((m), ("text-to-lower: stack underflow")))
end)
end))


let step_text_to_title : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (handle_cell)::rest -> begin
(

let handle = (FStar_UInt64.v handle_cell)
in (match ((Space_Interpreter.get_text m.texts1 handle)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-to-title: invalid handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(match ((Space_Text_Case.text_to_title t)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-to-title: conversion failed")))
end
| FStar_Pervasives_Native.Some (t') -> begin
(

let uu___ = (Space_Interpreter.add_text m.texts1 t')
in (match (uu___) with
| (tt', h') -> begin
(

let h_cell = (match ((h' < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t h')
end
| uu___1 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (h_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___1 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___1.mworld; Space_Interpreter.borrows = uu___1.borrows; Space_Interpreter.warps1 = uu___1.warps1; Space_Interpreter.texts1 = tt'; Space_Interpreter.text_warps = uu___1.text_warps; Space_Interpreter.graphemes1 = uu___1.graphemes1; Space_Interpreter.bytes_meta = uu___1.bytes_meta; Space_Interpreter.output = uu___1.output; Space_Interpreter.input = uu___1.input; Space_Interpreter.inst_ptr = uu___1.inst_ptr; Space_Interpreter.return_stack = uu___1.return_stack; Space_Interpreter.running = uu___1.running; Space_Interpreter.error = uu___1.error})))))
end))
end)
end))
end
| uu___ -> begin
StepError (((m), ("text-to-title: stack underflow")))
end)
end))


let step_text_warp_has_grapheme : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (wh_cell)::rest -> begin
(

let wh = (FStar_UInt64.v wh_cell)
in (match ((Space_Interpreter.get_text_warp m.text_warps wh)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-warp-has-grapheme: invalid warp handle")))
end
| FStar_Pervasives_Native.Some (tw) -> begin
(

let flag = (match ((Space_Text_Warp.text_warp_has_grapheme tw)) with
| true -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "1")))
end
| uu___ -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (flag)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u')))))
end))
end
| uu___ -> begin
StepError (((m), ("text-warp-has-grapheme: stack underflow")))
end)
end))


let step_text_warp_current_grapheme : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (wh_cell)::rest -> begin
(

let wh = (FStar_UInt64.v wh_cell)
in (match ((Space_Interpreter.get_text_warp m.text_warps wh)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-warp-current-grapheme: invalid warp handle")))
end
| FStar_Pervasives_Native.Some (tw) -> begin
(match ((Space_Text_Warp.text_warp_current tw)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-warp-current-grapheme: no current grapheme")))
end
| FStar_Pervasives_Native.Some (g) -> begin
(

let uu___ = (Space_Interpreter.add_grapheme m.graphemes1 g)
in (match (uu___) with
| (gt', gh) -> begin
(

let gh_cell = (match ((gh < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t gh)
end
| uu___1 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (gh_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___1 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___1.mworld; Space_Interpreter.borrows = uu___1.borrows; Space_Interpreter.warps1 = uu___1.warps1; Space_Interpreter.texts1 = uu___1.texts1; Space_Interpreter.text_warps = uu___1.text_warps; Space_Interpreter.graphemes1 = gt'; Space_Interpreter.bytes_meta = uu___1.bytes_meta; Space_Interpreter.output = uu___1.output; Space_Interpreter.input = uu___1.input; Space_Interpreter.inst_ptr = uu___1.inst_ptr; Space_Interpreter.return_stack = uu___1.return_stack; Space_Interpreter.running = uu___1.running; Space_Interpreter.error = uu___1.error})))))
end))
end)
end))
end
| uu___ -> begin
StepError (((m), ("text-warp-current-grapheme: stack underflow")))
end)
end))


let step_text_warp_next_grapheme : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (wh_cell)::rest -> begin
(

let wh = (FStar_UInt64.v wh_cell)
in (match ((Space_Interpreter.get_text_warp m.text_warps wh)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-warp-next-grapheme: invalid warp handle")))
end
| FStar_Pervasives_Native.Some (tw) -> begin
(match ((Space_Text_Warp.text_warp_next tw)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-warp-next-grapheme: no next grapheme")))
end
| FStar_Pervasives_Native.Some (g, tw') -> begin
(

let uu___ = (Space_Interpreter.add_grapheme m.graphemes1 g)
in (match (uu___) with
| (gt', gh) -> begin
(

let twt' = (Space_Interpreter.update_text_warp m.text_warps wh tw')
in (

let gh_cell = (match ((gh < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t gh)
end
| uu___1 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (wh_cell)::(gh_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___1 = (Space_Interpreter.update_current {Space_Interpreter.mworld = m.mworld; Space_Interpreter.borrows = m.borrows; Space_Interpreter.warps1 = m.warps1; Space_Interpreter.texts1 = m.texts1; Space_Interpreter.text_warps = twt'; Space_Interpreter.graphemes1 = m.graphemes1; Space_Interpreter.bytes_meta = m.bytes_meta; Space_Interpreter.output = m.output; Space_Interpreter.input = m.input; Space_Interpreter.inst_ptr = m.inst_ptr; Space_Interpreter.return_stack = m.return_stack; Space_Interpreter.running = m.running; Space_Interpreter.error = m.error} u')
in {Space_Interpreter.mworld = uu___1.mworld; Space_Interpreter.borrows = uu___1.borrows; Space_Interpreter.warps1 = uu___1.warps1; Space_Interpreter.texts1 = uu___1.texts1; Space_Interpreter.text_warps = uu___1.text_warps; Space_Interpreter.graphemes1 = gt'; Space_Interpreter.bytes_meta = uu___1.bytes_meta; Space_Interpreter.output = uu___1.output; Space_Interpreter.input = uu___1.input; Space_Interpreter.inst_ptr = uu___1.inst_ptr; Space_Interpreter.return_stack = uu___1.return_stack; Space_Interpreter.running = uu___1.running; Space_Interpreter.error = uu___1.error}))))))
end))
end)
end))
end
| uu___ -> begin
StepError (((m), ("text-warp-next-grapheme: stack underflow")))
end)
end))


let step_text_warp_grapheme_index : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (wh_cell)::rest -> begin
(

let wh = (FStar_UInt64.v wh_cell)
in (match ((Space_Interpreter.get_text_warp m.text_warps wh)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-warp-grapheme-index: invalid warp handle")))
end
| FStar_Pervasives_Native.Some (tw) -> begin
(

let pos = (Space_Text_Warp.text_warp_position tw)
in (

let pos_cell = (match ((pos < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t pos)
end
| uu___ -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (pos_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u'))))))
end))
end
| uu___ -> begin
StepError (((m), ("text-warp-grapheme-index: stack underflow")))
end)
end))


let step_text_warp_goto_grapheme : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (idx_cell)::(wh_cell)::rest -> begin
(

let wh = (FStar_UInt64.v wh_cell)
in (

let idx = (FStar_UInt64.v idx_cell)
in (match ((Space_Interpreter.get_text_warp m.text_warps wh)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-warp-goto-grapheme: invalid warp handle")))
end
| FStar_Pervasives_Native.Some (tw) -> begin
(match ((Space_Text_Warp.text_warp_goto tw idx)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-warp-goto-grapheme: invalid index")))
end
| FStar_Pervasives_Native.Some (tw') -> begin
(

let twt' = (Space_Interpreter.update_text_warp m.text_warps wh tw')
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (wh_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___ = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___.mworld; Space_Interpreter.borrows = uu___.borrows; Space_Interpreter.warps1 = uu___.warps1; Space_Interpreter.texts1 = uu___.texts1; Space_Interpreter.text_warps = twt'; Space_Interpreter.graphemes1 = uu___.graphemes1; Space_Interpreter.bytes_meta = uu___.bytes_meta; Space_Interpreter.output = uu___.output; Space_Interpreter.input = uu___.input; Space_Interpreter.inst_ptr = uu___.inst_ptr; Space_Interpreter.return_stack = uu___.return_stack; Space_Interpreter.running = uu___.running; Space_Interpreter.error = uu___.error})))))
end)
end)))
end
| uu___ -> begin
StepError (((m), ("text-warp-goto-grapheme: stack underflow")))
end)
end))


let step_grapheme_byte_length : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (gh_cell)::rest -> begin
(

let gh = (FStar_UInt64.v gh_cell)
in (match ((Space_Interpreter.get_grapheme m.graphemes1 gh)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("grapheme-byte-length: invalid handle")))
end
| FStar_Pervasives_Native.Some (g) -> begin
(

let len = g.len
in (

let len_cell = (match ((len < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t len)
end
| uu___ -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (len_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u'))))))
end))
end
| uu___ -> begin
StepError (((m), ("grapheme-byte-length: stack underflow")))
end)
end))


let step_grapheme_is_ascii : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (gh_cell)::rest -> begin
(

let gh = (FStar_UInt64.v gh_cell)
in (match ((Space_Interpreter.get_grapheme m.graphemes1 gh)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("grapheme-is-ascii: invalid handle")))
end
| FStar_Pervasives_Native.Some (g) -> begin
(

let is_ascii = ((Prims.op_Equality g.len (Prims.parse_int "1")) && (match (g.bytes) with
| (b)::[] -> begin
((FStar_UInt8.v b) < (Prims.parse_int "128"))
end
| uu___ -> begin
false
end))
in (

let flag = (match (is_ascii) with
| true -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "1")))
end
| uu___ -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (flag)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u'))))))
end))
end
| uu___ -> begin
StepError (((m), ("grapheme-is-ascii: stack underflow")))
end)
end))


let step_grapheme_code_points : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (gh_cell)::rest -> begin
(

let gh = (FStar_UInt64.v gh_cell)
in (match ((Space_Interpreter.get_grapheme m.graphemes1 gh)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("grapheme-code-points: invalid handle")))
end
| FStar_Pervasives_Native.Some (g) -> begin
(

let rec count_codepoints : FStar_UInt8.t Prims.list  ->  Prims.nat = (fun ( bs  :  FStar_UInt8.t Prims.list ) -> (match (bs) with
| [] -> begin
(Prims.parse_int "0")
end
| (b)::rest' -> begin
(

let bv = (FStar_UInt8.v b)
in (match (((bv < (Prims.parse_int "128")) || (bv >= (Prims.parse_int "192")))) with
| true -> begin
((Prims.parse_int "1") + (count_codepoints rest'))
end
| uu___ -> begin
(count_codepoints rest')
end))
end))
in (

let count = (count_codepoints g.bytes)
in (

let count_cell = (match ((count < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t count)
end
| uu___ -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (count_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u')))))))
end))
end
| uu___ -> begin
StepError (((m), ("grapheme-code-points: stack underflow")))
end)
end))


let step_text_grapheme_at : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (idx_cell)::(th_cell)::rest -> begin
(

let th = (FStar_UInt64.v th_cell)
in (

let idx = (FStar_UInt64.v idx_cell)
in (match ((Space_Interpreter.get_text m.texts1 th)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-grapheme-at: invalid text handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(match ((idx >= t.header.grapheme_count)) with
| true -> begin
StepError (((m), ("text-grapheme-at: index out of bounds")))
end
| uu___ -> begin
(

let tw = (Space_Text_Warp.text_warp_begin t)
in (match ((Space_Text_Warp.text_warp_goto tw idx)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-grapheme-at: seek failed")))
end
| FStar_Pervasives_Native.Some (tw') -> begin
(match ((Space_Text_Warp.text_warp_current tw')) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-grapheme-at: no grapheme at index")))
end
| FStar_Pervasives_Native.Some (g) -> begin
(

let uu___1 = (Space_Interpreter.add_grapheme m.graphemes1 g)
in (match (uu___1) with
| (gt', gh) -> begin
(

let gh_cell = (match ((gh < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t gh)
end
| uu___2 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (gh_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___2 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___2.mworld; Space_Interpreter.borrows = uu___2.borrows; Space_Interpreter.warps1 = uu___2.warps1; Space_Interpreter.texts1 = uu___2.texts1; Space_Interpreter.text_warps = uu___2.text_warps; Space_Interpreter.graphemes1 = gt'; Space_Interpreter.bytes_meta = uu___2.bytes_meta; Space_Interpreter.output = uu___2.output; Space_Interpreter.input = uu___2.input; Space_Interpreter.inst_ptr = uu___2.inst_ptr; Space_Interpreter.return_stack = uu___2.return_stack; Space_Interpreter.running = uu___2.running; Space_Interpreter.error = uu___2.error})))))
end))
end)
end))
end)
end)))
end
| uu___ -> begin
StepError (((m), ("text-grapheme-at: stack underflow")))
end)
end))


let step_text_grapheme_first : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (th_cell)::rest -> begin
(

let th = (FStar_UInt64.v th_cell)
in (match ((Space_Interpreter.get_text m.texts1 th)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-grapheme-first: invalid text handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(match ((Prims.op_Equality t.header.grapheme_count (Prims.parse_int "0"))) with
| true -> begin
StepError (((m), ("text-grapheme-first: empty text")))
end
| uu___ -> begin
(

let tw = (Space_Text_Warp.text_warp_begin t)
in (match ((Space_Text_Warp.text_warp_current tw)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-grapheme-first: no grapheme")))
end
| FStar_Pervasives_Native.Some (g) -> begin
(

let uu___1 = (Space_Interpreter.add_grapheme m.graphemes1 g)
in (match (uu___1) with
| (gt', gh) -> begin
(

let gh_cell = (match ((gh < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t gh)
end
| uu___2 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (gh_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___2 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___2.mworld; Space_Interpreter.borrows = uu___2.borrows; Space_Interpreter.warps1 = uu___2.warps1; Space_Interpreter.texts1 = uu___2.texts1; Space_Interpreter.text_warps = uu___2.text_warps; Space_Interpreter.graphemes1 = gt'; Space_Interpreter.bytes_meta = uu___2.bytes_meta; Space_Interpreter.output = uu___2.output; Space_Interpreter.input = uu___2.input; Space_Interpreter.inst_ptr = uu___2.inst_ptr; Space_Interpreter.return_stack = uu___2.return_stack; Space_Interpreter.running = uu___2.running; Space_Interpreter.error = uu___2.error})))))
end))
end))
end)
end))
end
| uu___ -> begin
StepError (((m), ("text-grapheme-first: stack underflow")))
end)
end))


let step_text_grapheme_last : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (th_cell)::rest -> begin
(

let th = (FStar_UInt64.v th_cell)
in (match ((Space_Interpreter.get_text m.texts1 th)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-grapheme-last: invalid text handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(match ((Prims.op_Equality t.header.grapheme_count (Prims.parse_int "0"))) with
| true -> begin
StepError (((m), ("text-grapheme-last: empty text")))
end
| uu___ -> begin
(

let tw = (Space_Text_Warp.text_warp_begin t)
in (

let last_idx = (t.header.grapheme_count - (Prims.parse_int "1"))
in (match ((Space_Text_Warp.text_warp_goto tw last_idx)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-grapheme-last: seek failed")))
end
| FStar_Pervasives_Native.Some (tw') -> begin
(match ((Space_Text_Warp.text_warp_current tw')) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-grapheme-last: no grapheme")))
end
| FStar_Pervasives_Native.Some (g) -> begin
(

let uu___1 = (Space_Interpreter.add_grapheme m.graphemes1 g)
in (match (uu___1) with
| (gt', gh) -> begin
(

let gh_cell = (match ((gh < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t gh)
end
| uu___2 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (gh_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (

let uu___2 = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___2.mworld; Space_Interpreter.borrows = uu___2.borrows; Space_Interpreter.warps1 = uu___2.warps1; Space_Interpreter.texts1 = uu___2.texts1; Space_Interpreter.text_warps = uu___2.text_warps; Space_Interpreter.graphemes1 = gt'; Space_Interpreter.bytes_meta = uu___2.bytes_meta; Space_Interpreter.output = uu___2.output; Space_Interpreter.input = uu___2.input; Space_Interpreter.inst_ptr = uu___2.inst_ptr; Space_Interpreter.return_stack = uu___2.return_stack; Space_Interpreter.running = uu___2.running; Space_Interpreter.error = uu___2.error})))))
end))
end)
end)))
end)
end))
end
| uu___ -> begin
StepError (((m), ("text-grapheme-last: stack underflow")))
end)
end))


let step_text_compare : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (h2_cell)::(h1_cell)::rest -> begin
(

let h1 = (FStar_UInt64.v h1_cell)
in (

let h2 = (FStar_UInt64.v h2_cell)
in (match ((((Space_Interpreter.get_text m.texts1 h1)), ((Space_Interpreter.get_text m.texts1 h2)))) with
| (FStar_Pervasives_Native.Some (t1), FStar_Pervasives_Native.Some (t2)) -> begin
(

let rec compare_bytes : FStar_UInt8.t Prims.list  ->  FStar_UInt8.t Prims.list  ->  Prims.int = (fun ( b1  :  FStar_UInt8.t Prims.list ) ( b2  :  FStar_UInt8.t Prims.list ) -> (match (((b1), (b2))) with
| ([], []) -> begin
(Prims.parse_int "0")
end
| ([], uu___) -> begin
(Prims.parse_int "-1")
end
| (uu___, []) -> begin
(Prims.parse_int "1")
end
| ((x)::xs, (y)::ys) -> begin
(

let xv = (FStar_UInt8.v x)
in (

let yv = (FStar_UInt8.v y)
in (match ((xv < yv)) with
| true -> begin
(Prims.parse_int "-1")
end
| uu___ -> begin
(match ((xv > yv)) with
| true -> begin
(Prims.parse_int "1")
end
| uu___1 -> begin
(compare_bytes xs ys)
end)
end)))
end))
in (

let cmp = (compare_bytes t1.data t2.data)
in (

let cmp_cell = (match ((Prims.op_Equality cmp (Prims.parse_int "-1"))) with
| true -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0xFFFFFFFFFFFFFFFF")))
end
| uu___ -> begin
(match ((Prims.op_Equality cmp (Prims.parse_int "0"))) with
| true -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end
| uu___1 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "1")))
end)
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (cmp_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u')))))))
end
| (uu___, uu___1) -> begin
StepError (((m), ("text-compare: invalid handle")))
end)))
end
| uu___ -> begin
StepError (((m), ("text-compare: stack underflow")))
end)
end))


let step_text_code_point_count : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (th_cell)::rest -> begin
(

let th = (FStar_UInt64.v th_cell)
in (match ((Space_Interpreter.get_text m.texts1 th)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-code-point-count: invalid handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(

let rec count_codepoints : FStar_UInt8.t Prims.list  ->  Prims.nat = (fun ( bs  :  FStar_UInt8.t Prims.list ) -> (match (bs) with
| [] -> begin
(Prims.parse_int "0")
end
| (b)::rest' -> begin
(

let bv = (FStar_UInt8.v b)
in (match (((bv < (Prims.parse_int "128")) || (bv >= (Prims.parse_int "192")))) with
| true -> begin
((Prims.parse_int "1") + (count_codepoints rest'))
end
| uu___ -> begin
(count_codepoints rest')
end))
end))
in (

let count = (count_codepoints t.data)
in (

let count_cell = (match ((count < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t count)
end
| uu___ -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (count_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u')))))))
end))
end
| uu___ -> begin
StepError (((m), ("text-code-point-count: stack underflow")))
end)
end))


let step_text_code_point_at : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (idx_cell)::(th_cell)::rest -> begin
(

let th = (FStar_UInt64.v th_cell)
in (

let idx = (FStar_UInt64.v idx_cell)
in (match ((Space_Interpreter.get_text m.texts1 th)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-code-point-at: invalid handle")))
end
| FStar_Pervasives_Native.Some (t) -> begin
(

let cps = (Space_Text_Normalize.bytes_to_codepoints t.data)
in (match ((idx >= (FStar_List_Tot_Base.length cps))) with
| true -> begin
StepError (((m), ("text-code-point-at: index out of bounds")))
end
| uu___ -> begin
(match ((FStar_List_Tot_Base.nth cps idx)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("text-code-point-at: index out of bounds")))
end
| FStar_Pervasives_Native.Some (cp) -> begin
(

let cp_cell = (match ((cp < (Prims.parse_int "18446744073709551616"))) with
| true -> begin
(FStar_UInt64.uint_to_t cp)
end
| uu___1 -> begin
(FStar_UInt64.uint_to_t ((Prims.parse_int "0")))
end)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = (cp_cell)::rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u')))))
end)
end))
end)))
end
| uu___ -> begin
StepError (((m), ("text-code-point-at: stack underflow")))
end)
end))


let step_emit_grapheme : Space_Interpreter.machine  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) -> (match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match (u.stack) with
| (gh_cell)::rest -> begin
(

let gh = (FStar_UInt64.v gh_cell)
in (match ((Space_Interpreter.get_grapheme m.graphemes1 gh)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("emit-grapheme: invalid handle")))
end
| FStar_Pervasives_Native.Some (g) -> begin
(

let byte_to_nat = (fun ( b  :  FStar_UInt8.t ) -> (FStar_UInt8.v b))
in (

let byte_vals = (FStar_List_Tot_Base.map byte_to_nat g.bytes)
in (

let u' = {Space_Universe.id = u.id; Space_Universe.name = u.name; Space_Universe.discipline = u.discipline; Space_Universe.stack = rest; Space_Universe.memory = u.memory; Space_Universe.capacity = u.capacity; Space_Universe.state = u.state}
in (

let m' = (

let uu___ = (Space_Interpreter.update_current m u')
in {Space_Interpreter.mworld = uu___.mworld; Space_Interpreter.borrows = uu___.borrows; Space_Interpreter.warps1 = uu___.warps1; Space_Interpreter.texts1 = uu___.texts1; Space_Interpreter.text_warps = uu___.text_warps; Space_Interpreter.graphemes1 = uu___.graphemes1; Space_Interpreter.bytes_meta = uu___.bytes_meta; Space_Interpreter.output = (FStar_List_Tot_Base.append m.output byte_vals); Space_Interpreter.input = uu___.input; Space_Interpreter.inst_ptr = uu___.inst_ptr; Space_Interpreter.return_stack = uu___.return_stack; Space_Interpreter.running = uu___.running; Space_Interpreter.error = uu___.error})
in StepOk ((Space_Interpreter.advance_ip m'))))))
end))
end
| uu___ -> begin
StepError (((m), ("emit-grapheme: stack underflow")))
end)
end))


let step_text_prim : Space_Interpreter.machine  ->  Space_Instruction.prim_op  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( op  :  Space_Instruction.prim_op ) -> (match (op) with
| Space_Instruction.PrimCreateText -> begin
(step_create_text m)
end
| Space_Instruction.PrimTextByteLength -> begin
(step_text_byte_length m)
end
| Space_Instruction.PrimTextGraphemeCount -> begin
(step_text_grapheme_count m)
end
| Space_Instruction.PrimTextIsSimple -> begin
(step_text_is_simple m)
end
| Space_Instruction.PrimTextEqual -> begin
(step_text_equal m)
end
| Space_Instruction.PrimTextConcat -> begin
(step_text_concat m)
end
| Space_Instruction.PrimTextSlice -> begin
(step_text_slice m)
end
| Space_Instruction.PrimTextNormalizeNfc -> begin
(step_text_normalize_nfc m)
end
| Space_Instruction.PrimTextNormalizeNfd -> begin
(step_text_normalize_nfd m)
end
| Space_Instruction.PrimTextNormalizeNfkc -> begin
(step_text_normalize_nfkc m)
end
| Space_Instruction.PrimTextNormalizeNfkd -> begin
(step_text_normalize_nfkd m)
end
| Space_Instruction.PrimTextToUpper -> begin
(step_text_to_upper m)
end
| Space_Instruction.PrimTextToLower -> begin
(step_text_to_lower m)
end
| Space_Instruction.PrimTextToTitle -> begin
(step_text_to_title m)
end
| Space_Instruction.PrimTextWarpHasGrapheme -> begin
(step_text_warp_has_grapheme m)
end
| Space_Instruction.PrimTextWarpCurrentGrapheme -> begin
(step_text_warp_current_grapheme m)
end
| Space_Instruction.PrimTextWarpNextGrapheme -> begin
(step_text_warp_next_grapheme m)
end
| Space_Instruction.PrimTextWarpGraphemeIndex -> begin
(step_text_warp_grapheme_index m)
end
| Space_Instruction.PrimTextWarpGotoGrapheme -> begin
(step_text_warp_goto_grapheme m)
end
| Space_Instruction.PrimGraphemeByteLength -> begin
(step_grapheme_byte_length m)
end
| Space_Instruction.PrimGraphemeIsAscii -> begin
(step_grapheme_is_ascii m)
end
| Space_Instruction.PrimGraphemeCodePoints -> begin
(step_grapheme_code_points m)
end
| Space_Instruction.PrimTextGraphemeAt -> begin
(step_text_grapheme_at m)
end
| Space_Instruction.PrimTextGraphemeFirst -> begin
(step_text_grapheme_first m)
end
| Space_Instruction.PrimTextGraphemeLast -> begin
(step_text_grapheme_last m)
end
| Space_Instruction.PrimTextCompare -> begin
(step_text_compare m)
end
| Space_Instruction.PrimTextCodePointCount -> begin
(step_text_code_point_count m)
end
| Space_Instruction.PrimTextCodePointAt -> begin
(step_text_code_point_at m)
end
| Space_Instruction.PrimEmitGrapheme -> begin
(step_emit_grapheme m)
end
| uu___ -> begin
StepError (((m), ("unknown text primitive")))
end))


let step_prim : Space_Interpreter.machine  ->  Space_Instruction.prim_op  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( op  :  Space_Instruction.prim_op ) -> (match ((is_text_prim op)) with
| true -> begin
(step_text_prim m op)
end
| uu___ -> begin
(match ((is_io_prim op)) with
| true -> begin
(step_io_prim m op)
end
| uu___1 -> begin
(match ((is_borrow_prim op)) with
| true -> begin
(step_borrow_prim m op)
end
| uu___2 -> begin
(match ((is_warp_prim op)) with
| true -> begin
(step_warp_prim m op)
end
| uu___3 -> begin
(match ((is_bytes_meta_prim op)) with
| true -> begin
(step_bytes_meta_prim m op)
end
| uu___4 -> begin
(match ((Space_Interpreter.current_universe m)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("no current universe")))
end
| FStar_Pervasives_Native.Some (u) -> begin
(match ((Space_Execute.exec_prim op u)) with
| Space_Execute.ExecOk (u') -> begin
StepOk ((Space_Interpreter.advance_ip (Space_Interpreter.update_current m u')))
end
| Space_Execute.ExecHalt -> begin
StepHalt ((Space_Interpreter.halt m))
end
| Space_Execute.ExecError (msg) -> begin
StepError (((m), (msg)))
end)
end)
end)
end)
end)
end)
end))


let step_one : Space_Interpreter.machine  ->  Space_Instruction.instruction  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( instr  :  Space_Instruction.instruction ) -> (match (instr) with
| Space_Instruction.IPush (v) -> begin
(step_push m v)
end
| Space_Instruction.ICall (target) -> begin
(step_call m target)
end
| Space_Instruction.IReturn -> begin
(step_return m)
end
| Space_Instruction.IPrimitive (op) -> begin
(step_prim m op)
end
| Space_Instruction.IBranch (target) -> begin
(step_branch m target)
end
| Space_Instruction.IBranchZero (target) -> begin
(step_branch_zero m target)
end
| Space_Instruction.IBranchNonZero (target) -> begin
(step_branch_nonzero m target)
end
| Space_Instruction.ICreateUniverse (name, disc) -> begin
(step_create_universe m name (Prims.parse_int "1024") disc)
end
| Space_Instruction.IEndUniverse (name) -> begin
(step_end_universe m name)
end
| Space_Instruction.IReleaseUniverse (name) -> begin
(step_release_universe m name)
end
| Space_Instruction.ITransferTo (name) -> begin
(step_transfer m name)
end))


let fetch_instr : Space_Instruction.instruction Prims.list  ->  Space_Instruction.ip  ->  Space_Instruction.instruction FStar_Pervasives_Native.option = (fun ( prog  :  Space_Instruction.instruction Prims.list ) ( ip  :  Space_Instruction.ip ) -> (

let idx = (FStar_UInt64.v ip)
in (match ((idx < (FStar_List_Tot_Base.length prog))) with
| true -> begin
(FStar_List_Tot_Base.nth prog idx)
end
| uu___ -> begin
FStar_Pervasives_Native.None
end)))


let rec run_loop : Space_Interpreter.machine  ->  Space_Instruction.instruction Prims.list  ->  Prims.nat  ->  step_result = (fun ( m  :  Space_Interpreter.machine ) ( prog  :  Space_Instruction.instruction Prims.list ) ( fuel  :  Prims.nat ) -> (match ((Prims.op_Equality fuel (Prims.parse_int "0"))) with
| true -> begin
StepError (((m), ("fuel exhausted")))
end
| uu___ -> begin
(match ((not (m.running))) with
| true -> begin
StepOk (m)
end
| uu___1 -> begin
(match ((fetch_instr prog m.inst_ptr)) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("invalid instruction pointer")))
end
| FStar_Pervasives_Native.Some (instr) -> begin
(match ((step_one m instr)) with
| StepOk (m') -> begin
(run_loop m' prog (fuel - (Prims.parse_int "1")))
end
| StepHalt (m') -> begin
StepHalt (m')
end
| StepError (m', msg) -> begin
StepError (((m'), (msg)))
end)
end)
end)
end))


let run_program : Space_Instruction.instruction Prims.list  ->  Prims.nat  ->  step_result = (fun ( prog  :  Space_Instruction.instruction Prims.list ) ( fuel  :  Prims.nat ) -> (

let m = {Space_Interpreter.mworld = Space_Interpreter.initial_machine.mworld; Space_Interpreter.borrows = Space_Interpreter.initial_machine.borrows; Space_Interpreter.warps1 = Space_Interpreter.initial_machine.warps1; Space_Interpreter.texts1 = Space_Interpreter.initial_machine.texts1; Space_Interpreter.text_warps = Space_Interpreter.initial_machine.text_warps; Space_Interpreter.graphemes1 = Space_Interpreter.initial_machine.graphemes1; Space_Interpreter.bytes_meta = Space_Interpreter.initial_machine.bytes_meta; Space_Interpreter.output = Space_Interpreter.initial_machine.output; Space_Interpreter.input = Space_Interpreter.initial_machine.input; Space_Interpreter.inst_ptr = Space_Interpreter.initial_machine.inst_ptr; Space_Interpreter.return_stack = Space_Interpreter.initial_machine.return_stack; Space_Interpreter.running = true; Space_Interpreter.error = Space_Interpreter.initial_machine.error}
in (

let uu___ = (Space_World.create_universe m.mworld "data" (Prims.parse_int "65536") Space_Types.Unrestricted)
in (match (uu___) with
| (w', uu___1) -> begin
(match ((Space_World.set_current w' "data")) with
| FStar_Pervasives_Native.None -> begin
StepError (((m), ("failed to set current universe")))
end
| FStar_Pervasives_Native.Some (w'') -> begin
(run_loop {Space_Interpreter.mworld = w''; Space_Interpreter.borrows = m.borrows; Space_Interpreter.warps1 = m.warps1; Space_Interpreter.texts1 = m.texts1; Space_Interpreter.text_warps = m.text_warps; Space_Interpreter.graphemes1 = m.graphemes1; Space_Interpreter.bytes_meta = m.bytes_meta; Space_Interpreter.output = m.output; Space_Interpreter.input = m.input; Space_Interpreter.inst_ptr = m.inst_ptr; Space_Interpreter.return_stack = m.return_stack; Space_Interpreter.running = m.running; Space_Interpreter.error = m.error} prog fuel)
end)
end))))




