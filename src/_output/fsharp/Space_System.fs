#light "off"
module Space_System
type system_state =
| Running
| Halted


let uu___is_Running : system_state  ->  Prims.bool = (fun ( projectee  :  system_state ) -> (match (projectee) with
| Running -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_Halted : system_state  ->  Prims.bool = (fun ( projectee  :  system_state ) -> (match (projectee) with
| Halted -> begin
true
end
| uu___ -> begin
false
end))

type 'a io_result =
| IOSuccess of 'a
| IOError


let uu___is_IOSuccess = (fun ( projectee  :  'a io_result ) -> (match (projectee) with
| IOSuccess (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__IOSuccess__item___0 = (fun ( projectee  :  'a io_result ) -> (match (projectee) with
| IOSuccess (_0) -> begin
_0
end))


let uu___is_IOError = (fun ( projectee  :  'a io_result ) -> (match (projectee) with
| IOError -> begin
true
end
| uu___ -> begin
false
end))


let halt_system : system_state = Halted


let is_running : system_state  ->  Prims.bool = (fun ( state  :  system_state ) -> (match (state) with
| Running -> begin
true
end
| Halted -> begin
false
end))


let emit_byte : Space_Stack.stack  ->  (FStar_UInt8.t * Space_Stack.stack) FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) -> (match (s) with
| (c)::rest -> begin
(

let byte = (FStar_Int_Cast.uint64_to_uint8 c)
in FStar_Pervasives_Native.Some (((byte), (rest))))
end
| [] -> begin
FStar_Pervasives_Native.None
end))


let read_byte : FStar_UInt8.t  ->  Space_Stack.stack  ->  Space_Stack.stack = (fun ( input  :  FStar_UInt8.t ) ( s  :  Space_Stack.stack ) -> (

let c = (FStar_Int_Cast.uint8_to_uint64 input)
in (c)::s))

type io_channel =
{output_buffer : FStar_UInt8.t Prims.list; input_buffer : FStar_UInt8.t Prims.list; input_pos : Prims.nat}


let __proj__Mkio_channel__item__output_buffer : io_channel  ->  FStar_UInt8.t Prims.list = (fun ( projectee  :  io_channel ) -> (match (projectee) with
| {output_buffer = output_buffer; input_buffer = input_buffer; input_pos = input_pos} -> begin
output_buffer
end))


let __proj__Mkio_channel__item__input_buffer : io_channel  ->  FStar_UInt8.t Prims.list = (fun ( projectee  :  io_channel ) -> (match (projectee) with
| {output_buffer = output_buffer; input_buffer = input_buffer; input_pos = input_pos} -> begin
input_buffer
end))


let __proj__Mkio_channel__item__input_pos : io_channel  ->  Prims.nat = (fun ( projectee  :  io_channel ) -> (match (projectee) with
| {output_buffer = output_buffer; input_buffer = input_buffer; input_pos = input_pos} -> begin
input_pos
end))


let empty_channel : io_channel = {output_buffer = []; input_buffer = []; input_pos = (Prims.parse_int "0")}


let channel_emit : io_channel  ->  FStar_UInt8.t  ->  io_channel = (fun ( ch  :  io_channel ) ( byte  :  FStar_UInt8.t ) -> {output_buffer = (FStar_List_Tot_Base.op_At ch.output_buffer ((byte)::[])); input_buffer = ch.input_buffer; input_pos = ch.input_pos})


let channel_read : io_channel  ->  (FStar_UInt8.t * io_channel) FStar_Pervasives_Native.option = (fun ( ch  :  io_channel ) -> (match ((ch.input_pos >= (FStar_List_Tot_Base.length ch.input_buffer))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(

let byte = (FStar_List_Tot_Base.index ch.input_buffer ch.input_pos)
in FStar_Pervasives_Native.Some (((byte), ({output_buffer = ch.output_buffer; input_buffer = ch.input_buffer; input_pos = (ch.input_pos + (Prims.parse_int "1"))}))))
end))


let stack_emit : Space_Stack.stack  ->  io_channel  ->  (Space_Stack.stack * io_channel) FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) ( ch  :  io_channel ) -> (match ((emit_byte s)) with
| FStar_Pervasives_Native.Some (byte, rest) -> begin
FStar_Pervasives_Native.Some (((rest), ((channel_emit ch byte))))
end
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end))


let stack_read : Space_Stack.stack  ->  io_channel  ->  (Space_Stack.stack * io_channel) FStar_Pervasives_Native.option = (fun ( s  :  Space_Stack.stack ) ( ch  :  io_channel ) -> (match ((channel_read ch)) with
| FStar_Pervasives_Native.Some (byte, ch') -> begin
FStar_Pervasives_Native.Some ((((read_byte byte s)), (ch')))
end
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end))




