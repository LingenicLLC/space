open Prims
type system_state =
  | Running 
  | Halted 
let uu___is_Running (projectee : system_state) : Prims.bool=
  match projectee with | Running -> true | uu___ -> false
let uu___is_Halted (projectee : system_state) : Prims.bool=
  match projectee with | Halted -> true | uu___ -> false
type 'a io_result =
  | IOSuccess of 'a 
  | IOError 
let uu___is_IOSuccess (projectee : 'a io_result) : Prims.bool=
  match projectee with | IOSuccess _0 -> true | uu___ -> false
let __proj__IOSuccess__item___0 (projectee : 'a io_result) : 'a=
  match projectee with | IOSuccess _0 -> _0
let uu___is_IOError (projectee : 'a io_result) : Prims.bool=
  match projectee with | IOError -> true | uu___ -> false
let halt_system : system_state= Halted
let is_running (state : system_state) : Prims.bool=
  match state with | Running -> true | Halted -> false
let emit_byte (s : Space_Stack.stack) :
  (FStar_UInt8.t * Space_Stack.stack) FStar_Pervasives_Native.option=
  match s with
  | c::rest ->
      let byte = FStar_Int_Cast.uint64_to_uint8 c in
      FStar_Pervasives_Native.Some (byte, rest)
  | [] -> FStar_Pervasives_Native.None
let read_byte (input : FStar_UInt8.t) (s : Space_Stack.stack) :
  Space_Stack.stack= let c = FStar_Int_Cast.uint8_to_uint64 input in c :: s
type io_channel =
  {
  output_buffer: FStar_UInt8.t Prims.list ;
  input_buffer: FStar_UInt8.t Prims.list ;
  input_pos: Prims.nat }
let __proj__Mkio_channel__item__output_buffer (projectee : io_channel) :
  FStar_UInt8.t Prims.list=
  match projectee with
  | { output_buffer; input_buffer; input_pos;_} -> output_buffer
let __proj__Mkio_channel__item__input_buffer (projectee : io_channel) :
  FStar_UInt8.t Prims.list=
  match projectee with
  | { output_buffer; input_buffer; input_pos;_} -> input_buffer
let __proj__Mkio_channel__item__input_pos (projectee : io_channel) :
  Prims.nat=
  match projectee with
  | { output_buffer; input_buffer; input_pos;_} -> input_pos
let empty_channel : io_channel=
  { output_buffer = []; input_buffer = []; input_pos = Prims.int_zero }
let channel_emit (ch : io_channel) (byte : FStar_UInt8.t) : io_channel=
  {
    output_buffer = (FStar_List_Tot_Base.op_At ch.output_buffer [byte]);
    input_buffer = (ch.input_buffer);
    input_pos = (ch.input_pos)
  }
let channel_read (ch : io_channel) :
  (FStar_UInt8.t * io_channel) FStar_Pervasives_Native.option=
  if ch.input_pos >= (FStar_List_Tot_Base.length ch.input_buffer)
  then FStar_Pervasives_Native.None
  else
    (let byte = FStar_List_Tot_Base.index ch.input_buffer ch.input_pos in
     FStar_Pervasives_Native.Some
       (byte,
         {
           output_buffer = (ch.output_buffer);
           input_buffer = (ch.input_buffer);
           input_pos = (ch.input_pos + Prims.int_one)
         }))
let stack_emit (s : Space_Stack.stack) (ch : io_channel) :
  (Space_Stack.stack * io_channel) FStar_Pervasives_Native.option=
  match emit_byte s with
  | FStar_Pervasives_Native.Some (byte, rest) ->
      FStar_Pervasives_Native.Some (rest, (channel_emit ch byte))
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
let stack_read (s : Space_Stack.stack) (ch : io_channel) :
  (Space_Stack.stack * io_channel) FStar_Pervasives_Native.option=
  match channel_read ch with
  | FStar_Pervasives_Native.Some (byte, ch') ->
      FStar_Pervasives_Native.Some ((read_byte byte s), ch')
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
