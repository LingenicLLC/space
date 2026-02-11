#light "off"
module Space_Interpreter

type text_handle =
Prims.nat

type text_table =
{texts : Space_Text_Types.text Prims.list; next_handle : text_handle}


let __proj__Mktext_table__item__texts : text_table  ->  Space_Text_Types.text Prims.list = (fun ( projectee  :  text_table ) -> (match (projectee) with
| {texts = texts; next_handle = next_handle} -> begin
texts
end))


let __proj__Mktext_table__item__next_handle : text_table  ->  text_handle = (fun ( projectee  :  text_table ) -> (match (projectee) with
| {texts = texts; next_handle = next_handle} -> begin
next_handle
end))


let empty_text_table : text_table = {texts = []; next_handle = (Prims.parse_int "0")}


let add_text : text_table  ->  Space_Text_Types.text  ->  (text_table * text_handle) = (fun ( tt  :  text_table ) ( t  :  Space_Text_Types.text ) -> (

let h = tt.next_handle
in (({texts = (FStar_List_Tot_Base.append tt.texts ((t)::[])); next_handle = (h + (Prims.parse_int "1"))}), (h))))


let rec get_text_aux : Space_Text_Types.text Prims.list  ->  Prims.nat  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( ts  :  Space_Text_Types.text Prims.list ) ( h  :  Prims.nat ) -> (match (ts) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (t)::rest -> begin
(match ((Prims.op_Equality h (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.Some (t)
end
| uu___ -> begin
(get_text_aux rest (h - (Prims.parse_int "1")))
end)
end))


let get_text : text_table  ->  text_handle  ->  Space_Text_Types.text FStar_Pervasives_Native.option = (fun ( tt  :  text_table ) ( h  :  text_handle ) -> (match ((h >= tt.next_handle)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(get_text_aux tt.texts h)
end))


let rec update_text_aux : Space_Text_Types.text Prims.list  ->  Prims.nat  ->  Space_Text_Types.text  ->  Space_Text_Types.text Prims.list = (fun ( ts  :  Space_Text_Types.text Prims.list ) ( h  :  Prims.nat ) ( t  :  Space_Text_Types.text ) -> (match (ts) with
| [] -> begin
[]
end
| (old)::rest -> begin
(match ((Prims.op_Equality h (Prims.parse_int "0"))) with
| true -> begin
(t)::rest
end
| uu___ -> begin
(old)::(update_text_aux rest (h - (Prims.parse_int "1")) t)
end)
end))


let update_text : text_table  ->  text_handle  ->  Space_Text_Types.text  ->  text_table = (fun ( tt  :  text_table ) ( h  :  text_handle ) ( t  :  Space_Text_Types.text ) -> {texts = (update_text_aux tt.texts h t); next_handle = tt.next_handle})


type text_warp_handle =
Prims.nat

type text_warp_table =
{warps : Space_Text_Warp.text_warp Prims.list; next_handle1 : text_warp_handle}


let __proj__Mktext_warp_table__item__warps : text_warp_table  ->  Space_Text_Warp.text_warp Prims.list = (fun ( projectee  :  text_warp_table ) -> (match (projectee) with
| {warps = warps; next_handle1 = next_handle} -> begin
warps
end))


let __proj__Mktext_warp_table__item__next_handle : text_warp_table  ->  text_warp_handle = (fun ( projectee  :  text_warp_table ) -> (match (projectee) with
| {warps = warps; next_handle1 = next_handle} -> begin
next_handle
end))


let empty_text_warp_table : text_warp_table = {warps = []; next_handle1 = (Prims.parse_int "0")}


let add_text_warp : text_warp_table  ->  Space_Text_Warp.text_warp  ->  (text_warp_table * text_warp_handle) = (fun ( twt  :  text_warp_table ) ( tw  :  Space_Text_Warp.text_warp ) -> (

let h = twt.next_handle1
in (({warps = (FStar_List_Tot_Base.append twt.warps ((tw)::[])); next_handle1 = (h + (Prims.parse_int "1"))}), (h))))


let rec get_text_warp_aux : Space_Text_Warp.text_warp Prims.list  ->  Prims.nat  ->  Space_Text_Warp.text_warp FStar_Pervasives_Native.option = (fun ( tws  :  Space_Text_Warp.text_warp Prims.list ) ( h  :  Prims.nat ) -> (match (tws) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (tw)::rest -> begin
(match ((Prims.op_Equality h (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.Some (tw)
end
| uu___ -> begin
(get_text_warp_aux rest (h - (Prims.parse_int "1")))
end)
end))


let get_text_warp : text_warp_table  ->  text_warp_handle  ->  Space_Text_Warp.text_warp FStar_Pervasives_Native.option = (fun ( twt  :  text_warp_table ) ( h  :  text_warp_handle ) -> (match ((h >= twt.next_handle1)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(get_text_warp_aux twt.warps h)
end))


let rec update_text_warp_aux : Space_Text_Warp.text_warp Prims.list  ->  Prims.nat  ->  Space_Text_Warp.text_warp  ->  Space_Text_Warp.text_warp Prims.list = (fun ( tws  :  Space_Text_Warp.text_warp Prims.list ) ( h  :  Prims.nat ) ( tw  :  Space_Text_Warp.text_warp ) -> (match (tws) with
| [] -> begin
[]
end
| (old)::rest -> begin
(match ((Prims.op_Equality h (Prims.parse_int "0"))) with
| true -> begin
(tw)::rest
end
| uu___ -> begin
(old)::(update_text_warp_aux rest (h - (Prims.parse_int "1")) tw)
end)
end))


let update_text_warp : text_warp_table  ->  text_warp_handle  ->  Space_Text_Warp.text_warp  ->  text_warp_table = (fun ( twt  :  text_warp_table ) ( h  :  text_warp_handle ) ( tw  :  Space_Text_Warp.text_warp ) -> {warps = (update_text_warp_aux twt.warps h tw); next_handle1 = twt.next_handle1})


let get_implicit_text_warp : text_warp_table  ->  (Space_Text_Warp.text_warp * text_warp_handle) FStar_Pervasives_Native.option = (fun ( twt  :  text_warp_table ) -> (match ((Prims.op_Equality twt.next_handle1 (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(

let h = (twt.next_handle1 - (Prims.parse_int "1"))
in (match ((get_text_warp twt h)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (tw) -> begin
FStar_Pervasives_Native.Some (((tw), (h)))
end))
end))


type grapheme_handle =
Prims.nat

type grapheme_table =
{graphemes : Space_Text_Types.grapheme Prims.list; next_handle2 : grapheme_handle}


let __proj__Mkgrapheme_table__item__graphemes : grapheme_table  ->  Space_Text_Types.grapheme Prims.list = (fun ( projectee  :  grapheme_table ) -> (match (projectee) with
| {graphemes = graphemes; next_handle2 = next_handle} -> begin
graphemes
end))


let __proj__Mkgrapheme_table__item__next_handle : grapheme_table  ->  grapheme_handle = (fun ( projectee  :  grapheme_table ) -> (match (projectee) with
| {graphemes = graphemes; next_handle2 = next_handle} -> begin
next_handle
end))


let empty_grapheme_table : grapheme_table = {graphemes = []; next_handle2 = (Prims.parse_int "0")}


let add_grapheme : grapheme_table  ->  Space_Text_Types.grapheme  ->  (grapheme_table * grapheme_handle) = (fun ( gt  :  grapheme_table ) ( g  :  Space_Text_Types.grapheme ) -> (

let h = gt.next_handle2
in (({graphemes = (FStar_List_Tot_Base.append gt.graphemes ((g)::[])); next_handle2 = (h + (Prims.parse_int "1"))}), (h))))


let rec get_grapheme_aux : Space_Text_Types.grapheme Prims.list  ->  Prims.nat  ->  Space_Text_Types.grapheme FStar_Pervasives_Native.option = (fun ( gs  :  Space_Text_Types.grapheme Prims.list ) ( h  :  Prims.nat ) -> (match (gs) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (g)::rest -> begin
(match ((Prims.op_Equality h (Prims.parse_int "0"))) with
| true -> begin
FStar_Pervasives_Native.Some (g)
end
| uu___ -> begin
(get_grapheme_aux rest (h - (Prims.parse_int "1")))
end)
end))


let get_grapheme : grapheme_table  ->  grapheme_handle  ->  Space_Text_Types.grapheme FStar_Pervasives_Native.option = (fun ( gt  :  grapheme_table ) ( h  :  grapheme_handle ) -> (match ((h >= gt.next_handle2)) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(get_grapheme_aux gt.graphemes h)
end))

type bytes_metadata_entry =
{base_addr : Prims.nat; byte_length : Prims.nat}


let __proj__Mkbytes_metadata_entry__item__base_addr : bytes_metadata_entry  ->  Prims.nat = (fun ( projectee  :  bytes_metadata_entry ) -> (match (projectee) with
| {base_addr = base_addr; byte_length = byte_length} -> begin
base_addr
end))


let __proj__Mkbytes_metadata_entry__item__byte_length : bytes_metadata_entry  ->  Prims.nat = (fun ( projectee  :  bytes_metadata_entry ) -> (match (projectee) with
| {base_addr = base_addr; byte_length = byte_length} -> begin
byte_length
end))

type bytes_metadata =
{entries : bytes_metadata_entry Prims.list}


let __proj__Mkbytes_metadata__item__entries : bytes_metadata  ->  bytes_metadata_entry Prims.list = (fun ( projectee  :  bytes_metadata ) -> (match (projectee) with
| {entries = entries} -> begin
entries
end))


let empty_bytes_metadata : bytes_metadata = {entries = []}


let record_bytes_alloc : bytes_metadata  ->  Prims.nat  ->  Prims.nat  ->  bytes_metadata = (fun ( bm  :  bytes_metadata ) ( addr  :  Prims.nat ) ( len  :  Prims.nat ) -> {entries = ({base_addr = addr; byte_length = len})::bm.entries})


let rec lookup_bytes_len : bytes_metadata_entry Prims.list  ->  Prims.nat  ->  Prims.nat FStar_Pervasives_Native.option = (fun ( entries  :  bytes_metadata_entry Prims.list ) ( addr  :  Prims.nat ) -> (match (entries) with
| [] -> begin
FStar_Pervasives_Native.None
end
| (e)::rest -> begin
(match ((Prims.op_Equality e.base_addr addr)) with
| true -> begin
FStar_Pervasives_Native.Some (e.byte_length)
end
| uu___ -> begin
(lookup_bytes_len rest addr)
end)
end))


let get_bytes_len : bytes_metadata  ->  Prims.nat  ->  Prims.nat FStar_Pervasives_Native.option = (fun ( bm  :  bytes_metadata ) ( addr  :  Prims.nat ) -> (lookup_bytes_len bm.entries addr))

type machine =
{mworld : Space_World.world; borrows : Space_Borrow.borrow_state; warps1 : Space_Warp.warp_table; texts1 : text_table; text_warps : text_warp_table; graphemes1 : grapheme_table; bytes_meta : bytes_metadata; output : Prims.nat Prims.list; input : Prims.nat Prims.list; inst_ptr : Space_Instruction.ip; return_stack : Space_Instruction.ip Prims.list; running : Prims.bool; error : Prims.string FStar_Pervasives_Native.option}


let __proj__Mkmachine__item__mworld : machine  ->  Space_World.world = (fun ( projectee  :  machine ) -> (match (projectee) with
| {mworld = mworld; borrows = borrows; warps1 = warps; texts1 = texts; text_warps = text_warps; graphemes1 = graphemes; bytes_meta = bytes_meta; output = output; input = input; inst_ptr = inst_ptr; return_stack = return_stack; running = running; error = error} -> begin
mworld
end))


let __proj__Mkmachine__item__borrows : machine  ->  Space_Borrow.borrow_state = (fun ( projectee  :  machine ) -> (match (projectee) with
| {mworld = mworld; borrows = borrows; warps1 = warps; texts1 = texts; text_warps = text_warps; graphemes1 = graphemes; bytes_meta = bytes_meta; output = output; input = input; inst_ptr = inst_ptr; return_stack = return_stack; running = running; error = error} -> begin
borrows
end))


let __proj__Mkmachine__item__warps : machine  ->  Space_Warp.warp_table = (fun ( projectee  :  machine ) -> (match (projectee) with
| {mworld = mworld; borrows = borrows; warps1 = warps; texts1 = texts; text_warps = text_warps; graphemes1 = graphemes; bytes_meta = bytes_meta; output = output; input = input; inst_ptr = inst_ptr; return_stack = return_stack; running = running; error = error} -> begin
warps
end))


let __proj__Mkmachine__item__texts : machine  ->  text_table = (fun ( projectee  :  machine ) -> (match (projectee) with
| {mworld = mworld; borrows = borrows; warps1 = warps; texts1 = texts; text_warps = text_warps; graphemes1 = graphemes; bytes_meta = bytes_meta; output = output; input = input; inst_ptr = inst_ptr; return_stack = return_stack; running = running; error = error} -> begin
texts
end))


let __proj__Mkmachine__item__text_warps : machine  ->  text_warp_table = (fun ( projectee  :  machine ) -> (match (projectee) with
| {mworld = mworld; borrows = borrows; warps1 = warps; texts1 = texts; text_warps = text_warps; graphemes1 = graphemes; bytes_meta = bytes_meta; output = output; input = input; inst_ptr = inst_ptr; return_stack = return_stack; running = running; error = error} -> begin
text_warps
end))


let __proj__Mkmachine__item__graphemes : machine  ->  grapheme_table = (fun ( projectee  :  machine ) -> (match (projectee) with
| {mworld = mworld; borrows = borrows; warps1 = warps; texts1 = texts; text_warps = text_warps; graphemes1 = graphemes; bytes_meta = bytes_meta; output = output; input = input; inst_ptr = inst_ptr; return_stack = return_stack; running = running; error = error} -> begin
graphemes
end))


let __proj__Mkmachine__item__bytes_meta : machine  ->  bytes_metadata = (fun ( projectee  :  machine ) -> (match (projectee) with
| {mworld = mworld; borrows = borrows; warps1 = warps; texts1 = texts; text_warps = text_warps; graphemes1 = graphemes; bytes_meta = bytes_meta; output = output; input = input; inst_ptr = inst_ptr; return_stack = return_stack; running = running; error = error} -> begin
bytes_meta
end))


let __proj__Mkmachine__item__output : machine  ->  Prims.nat Prims.list = (fun ( projectee  :  machine ) -> (match (projectee) with
| {mworld = mworld; borrows = borrows; warps1 = warps; texts1 = texts; text_warps = text_warps; graphemes1 = graphemes; bytes_meta = bytes_meta; output = output; input = input; inst_ptr = inst_ptr; return_stack = return_stack; running = running; error = error} -> begin
output
end))


let __proj__Mkmachine__item__input : machine  ->  Prims.nat Prims.list = (fun ( projectee  :  machine ) -> (match (projectee) with
| {mworld = mworld; borrows = borrows; warps1 = warps; texts1 = texts; text_warps = text_warps; graphemes1 = graphemes; bytes_meta = bytes_meta; output = output; input = input; inst_ptr = inst_ptr; return_stack = return_stack; running = running; error = error} -> begin
input
end))


let __proj__Mkmachine__item__inst_ptr : machine  ->  Space_Instruction.ip = (fun ( projectee  :  machine ) -> (match (projectee) with
| {mworld = mworld; borrows = borrows; warps1 = warps; texts1 = texts; text_warps = text_warps; graphemes1 = graphemes; bytes_meta = bytes_meta; output = output; input = input; inst_ptr = inst_ptr; return_stack = return_stack; running = running; error = error} -> begin
inst_ptr
end))


let __proj__Mkmachine__item__return_stack : machine  ->  Space_Instruction.ip Prims.list = (fun ( projectee  :  machine ) -> (match (projectee) with
| {mworld = mworld; borrows = borrows; warps1 = warps; texts1 = texts; text_warps = text_warps; graphemes1 = graphemes; bytes_meta = bytes_meta; output = output; input = input; inst_ptr = inst_ptr; return_stack = return_stack; running = running; error = error} -> begin
return_stack
end))


let __proj__Mkmachine__item__running : machine  ->  Prims.bool = (fun ( projectee  :  machine ) -> (match (projectee) with
| {mworld = mworld; borrows = borrows; warps1 = warps; texts1 = texts; text_warps = text_warps; graphemes1 = graphemes; bytes_meta = bytes_meta; output = output; input = input; inst_ptr = inst_ptr; return_stack = return_stack; running = running; error = error} -> begin
running
end))


let __proj__Mkmachine__item__error : machine  ->  Prims.string FStar_Pervasives_Native.option = (fun ( projectee  :  machine ) -> (match (projectee) with
| {mworld = mworld; borrows = borrows; warps1 = warps; texts1 = texts; text_warps = text_warps; graphemes1 = graphemes; bytes_meta = bytes_meta; output = output; input = input; inst_ptr = inst_ptr; return_stack = return_stack; running = running; error = error} -> begin
error
end))


let initial_machine : machine = {mworld = Space_World.empty_world; borrows = Space_Borrow.empty_borrow_state; warps1 = Space_Warp.empty_warp_table; texts1 = empty_text_table; text_warps = empty_text_warp_table; graphemes1 = empty_grapheme_table; bytes_meta = empty_bytes_metadata; output = []; input = []; inst_ptr = (FStar_UInt64.uint_to_t ((Prims.parse_int "0"))); return_stack = []; running = false; error = FStar_Pervasives_Native.None}


let machine_error : machine  ->  Prims.string  ->  machine = (fun ( m  :  machine ) ( msg  :  Prims.string ) -> {mworld = m.mworld; borrows = m.borrows; warps1 = m.warps1; texts1 = m.texts1; text_warps = m.text_warps; graphemes1 = m.graphemes1; bytes_meta = m.bytes_meta; output = m.output; input = m.input; inst_ptr = m.inst_ptr; return_stack = m.return_stack; running = false; error = FStar_Pervasives_Native.Some (msg)})


let halt : machine  ->  machine = (fun ( m  :  machine ) -> {mworld = m.mworld; borrows = m.borrows; warps1 = m.warps1; texts1 = m.texts1; text_warps = m.text_warps; graphemes1 = m.graphemes1; bytes_meta = m.bytes_meta; output = m.output; input = m.input; inst_ptr = m.inst_ptr; return_stack = m.return_stack; running = false; error = m.error})


let advance_ip : machine  ->  machine = (fun ( m  :  machine ) -> {mworld = m.mworld; borrows = m.borrows; warps1 = m.warps1; texts1 = m.texts1; text_warps = m.text_warps; graphemes1 = m.graphemes1; bytes_meta = m.bytes_meta; output = m.output; input = m.input; inst_ptr = (FStar_UInt64.add_mod m.inst_ptr (FStar_UInt64.uint_to_t ((Prims.parse_int "1")))); return_stack = m.return_stack; running = m.running; error = m.error})


let jump : machine  ->  Space_Instruction.ip  ->  machine = (fun ( m  :  machine ) ( target  :  Space_Instruction.ip ) -> {mworld = m.mworld; borrows = m.borrows; warps1 = m.warps1; texts1 = m.texts1; text_warps = m.text_warps; graphemes1 = m.graphemes1; bytes_meta = m.bytes_meta; output = m.output; input = m.input; inst_ptr = target; return_stack = m.return_stack; running = m.running; error = m.error})


let call : machine  ->  Space_Instruction.ip  ->  machine = (fun ( m  :  machine ) ( target  :  Space_Instruction.ip ) -> (

let return_addr = (FStar_UInt64.add_mod m.inst_ptr (FStar_UInt64.uint_to_t ((Prims.parse_int "1"))))
in {mworld = m.mworld; borrows = m.borrows; warps1 = m.warps1; texts1 = m.texts1; text_warps = m.text_warps; graphemes1 = m.graphemes1; bytes_meta = m.bytes_meta; output = m.output; input = m.input; inst_ptr = target; return_stack = (return_addr)::m.return_stack; running = m.running; error = m.error}))


let do_return : machine  ->  machine = (fun ( m  :  machine ) -> (match (m.return_stack) with
| [] -> begin
(machine_error m "return stack underflow")
end
| (addr)::rest -> begin
{mworld = m.mworld; borrows = m.borrows; warps1 = m.warps1; texts1 = m.texts1; text_warps = m.text_warps; graphemes1 = m.graphemes1; bytes_meta = m.bytes_meta; output = m.output; input = m.input; inst_ptr = addr; return_stack = rest; running = m.running; error = m.error}
end))


let current_universe : machine  ->  Space_Universe.universe FStar_Pervasives_Native.option = (fun ( m  :  machine ) -> (Space_World.get_current m.mworld))


let update_current : machine  ->  Space_Universe.universe  ->  machine = (fun ( m  :  machine ) ( u  :  Space_Universe.universe ) -> {mworld = (Space_World.update_universe m.mworld u); borrows = m.borrows; warps1 = m.warps1; texts1 = m.texts1; text_warps = m.text_warps; graphemes1 = m.graphemes1; bytes_meta = m.bytes_meta; output = m.output; input = m.input; inst_ptr = m.inst_ptr; return_stack = m.return_stack; running = m.running; error = m.error})




