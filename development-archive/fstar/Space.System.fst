module Space.System

(** System primitives: halt, emit, read *)

open FStar.UInt8
open FStar.UInt64
open FStar.List.Tot
open Space.Types
open Space.Stack

(** System state *)
type system_state =
  | Running
  | Halted

(** I/O result *)
type io_result 'a =
  | IOSuccess of 'a
  | IOError

(** Halt system - signals termination *)
let halt_system : system_state = Halted

(** Check if system should continue *)
let is_running (state: system_state) : bool =
  match state with
  | Running -> true
  | Halted -> false

(** Emit byte - output a single byte
    In actual implementation, this would write to stdout/output buffer *)
let emit_byte (s: stack) : option (UInt8.t * stack) =
  match s with
  | c :: rest ->
    (* Truncate cell to byte *)
    let byte = FStar.Int.Cast.uint64_to_uint8 c in
    Some (byte, rest)
  | [] -> None

(** Read byte - input a single byte
    In actual implementation, this would read from stdin/input buffer
    Here we model it as returning a byte value *)
let read_byte (input: UInt8.t) (s: stack) : stack =
  (* Extend byte to cell and push *)
  let c = FStar.Int.Cast.uint8_to_uint64 input in
  c :: s

(** I/O channel abstraction *)
type io_channel = {
  output_buffer: list UInt8.t;
  input_buffer: list UInt8.t;
  input_pos: nat;
}

(** Empty I/O channel *)
let empty_channel : io_channel = {
  output_buffer = [];
  input_buffer = [];
  input_pos = 0;
}

(** Write byte to channel *)
let channel_emit (ch: io_channel) (byte: UInt8.t) : io_channel =
  { ch with output_buffer = ch.output_buffer @ [byte] }

(** Read byte from channel *)
let channel_read (ch: io_channel) : option (UInt8.t * io_channel) =
  if ch.input_pos >= List.Tot.length ch.input_buffer
  then None  (* EOF *)
  else
    let byte = List.Tot.index ch.input_buffer ch.input_pos in
    Some (byte, { ch with input_pos = ch.input_pos + 1 })

(** Stack operation: emit byte *)
let stack_emit (s: stack) (ch: io_channel) : option (stack * io_channel) =
  match emit_byte s with
  | Some (byte, rest) -> Some (rest, channel_emit ch byte)
  | None -> None

(** Stack operation: read byte *)
let stack_read (s: stack) (ch: io_channel) : option (stack * io_channel) =
  match channel_read ch with
  | Some (byte, ch') -> Some (read_byte byte s, ch')
  | None -> None  (* EOF - could also push -1 or signal error *)

