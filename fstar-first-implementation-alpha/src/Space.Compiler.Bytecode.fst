module Space.Compiler.Bytecode

(** Bytecode format for compiled Space programs *)

open FStar.List.Tot
open FStar.UInt8
open FStar.Mul
open Space.Types

(** Opcode type *)
type opcode = UInt8.t

(** Stack operations: 0x00-0x0F *)
let op_nop : opcode = 0x00uy
let op_dup : opcode = 0x01uy
let op_drop : opcode = 0x02uy
let op_swap : opcode = 0x03uy
let op_over : opcode = 0x04uy
let op_rot : opcode = 0x05uy
let op_nip : opcode = 0x06uy
let op_tuck : opcode = 0x07uy
let op_pick : opcode = 0x08uy

(** Arithmetic: 0x10-0x1F *)
let op_add : opcode = 0x10uy
let op_sub : opcode = 0x11uy
let op_mul : opcode = 0x12uy
let op_divu : opcode = 0x13uy
let op_divs : opcode = 0x14uy
let op_mod : opcode = 0x15uy
let op_neg : opcode = 0x16uy
let op_min : opcode = 0x17uy
let op_max : opcode = 0x18uy

(** Bitwise: 0x20-0x2F *)
let op_and : opcode = 0x20uy
let op_or : opcode = 0x21uy
let op_xor : opcode = 0x22uy
let op_not : opcode = 0x23uy
let op_shl : opcode = 0x24uy
let op_shr : opcode = 0x25uy

(** Comparison: 0x30-0x3F *)
let op_eq : opcode = 0x30uy
let op_neq : opcode = 0x31uy
let op_lt : opcode = 0x32uy
let op_gt : opcode = 0x33uy
let op_ltu : opcode = 0x34uy
let op_gtu : opcode = 0x35uy

(** Memory/Bytes: 0x40-0x4F *)
let op_alloc_bytes : opcode = 0x40uy
let op_bytes_fetch : opcode = 0x41uy
let op_bytes_store : opcode = 0x42uy
let op_bytes_len : opcode = 0x43uy
let op_bytes_copy : opcode = 0x44uy

(** Borrowing: 0x50-0x5F *)
let op_borrow : opcode = 0x50uy
let op_return_ptr : opcode = 0x51uy
let op_drop_ptr : opcode = 0x52uy
let op_fetch_bor : opcode = 0x53uy
let op_store_bor : opcode = 0x54uy
let op_fetch_end : opcode = 0x55uy
let op_store_end : opcode = 0x56uy
let op_offset_bor : opcode = 0x57uy

(** Warp: 0x60-0x6F *)
let op_warp_into : opcode = 0x60uy
let op_warp_into_ro : opcode = 0x61uy
let op_end_warp : opcode = 0x62uy
let op_warp_fetch : opcode = 0x63uy
let op_warp_store : opcode = 0x64uy
let op_warp_advance : opcode = 0x65uy
let op_warp_follow : opcode = 0x66uy
let op_warp_pos : opcode = 0x67uy
let op_warp_restore : opcode = 0x68uy
let op_warp_null : opcode = 0x69uy

(** Text: 0x70-0x7F *)
let op_create_text : opcode = 0x70uy
let op_text_byte_len : opcode = 0x71uy
let op_text_graph_cnt : opcode = 0x72uy
let op_text_is_simple : opcode = 0x73uy
let op_text_graph_at : opcode = 0x74uy
let op_text_graph_first : opcode = 0x75uy
let op_text_graph_last : opcode = 0x76uy
let op_text_slice : opcode = 0x77uy
let op_text_concat : opcode = 0x78uy
let op_text_equal : opcode = 0x79uy
let op_text_compare : opcode = 0x7Auy
let op_text_cp_count : opcode = 0x7Buy
let op_text_cp_at : opcode = 0x7Cuy
let op_graph_byte_len : opcode = 0x7Duy
let op_graph_is_ascii : opcode = 0x7Euy
let op_graph_cp_count : opcode = 0x7Fuy

(** Text Warp: 0x80-0x8F *)
let op_text_warp_into : opcode = 0x80uy
let op_end_text_warp : opcode = 0x81uy
let op_warp_has_graph : opcode = 0x82uy
let op_warp_cur_graph : opcode = 0x83uy
let op_warp_next_graph : opcode = 0x84uy
let op_warp_graph_idx : opcode = 0x85uy
let op_warp_goto_graph : opcode = 0x86uy

(** Control flow: 0x90-0x9F *)
let op_call : opcode = 0x90uy
let op_ret : opcode = 0x91uy
let op_jmp : opcode = 0x92uy
let op_jz : opcode = 0x93uy
let op_jnz : opcode = 0x94uy
let op_loop : opcode = 0x95uy
let op_exit : opcode = 0x96uy

(** Universe: 0xA0-0xAF *)
let op_create_univ : opcode = 0xA0uy
let op_end_univ : opcode = 0xA1uy
let op_release_univ : opcode = 0xA2uy
let op_transfer : opcode = 0xA3uy

(** System: 0xB0-0xBF *)
let op_halt : opcode = 0xB0uy
let op_emit : opcode = 0xB1uy
let op_read : opcode = 0xB2uy
let op_emit_graph : opcode = 0xB3uy

(** Full profile: Normalization 0xC8-0xCF *)
let op_normalize_nfc : opcode = 0xC8uy
let op_normalize_nfd : opcode = 0xC9uy
let op_normalize_nfkc : opcode = 0xCAuy
let op_normalize_nfkd : opcode = 0xCBuy

(** Full profile: Case 0xD0-0xDF *)
let op_text_upper : opcode = 0xD0uy
let op_text_lower : opcode = 0xD1uy
let op_text_title : opcode = 0xD2uy

(** Meta: 0xF0-0xFF *)
let op_push_i8 : opcode = 0xF0uy
let op_push_i16 : opcode = 0xF1uy
let op_push_i32 : opcode = 0xF2uy
let op_push_i64 : opcode = 0xF3uy
let op_push_str : opcode = 0xF4uy
let op_push_text : opcode = 0xF5uy

(** Discipline encoding *)
let disc_linear : UInt8.t = 0x00uy
let disc_affine : UInt8.t = 0x01uy
let disc_unrestricted : UInt8.t = 0x02uy

(** String table entry *)
noeq type string_entry = {
  se_offset: nat;
  se_length: nat;
  se_data: list UInt8.t;
}

(** Function entry *)
noeq type function_entry = {
  fe_name: string;
  fe_code_offset: nat;
  fe_code_length: nat;
}

(** Bytecode module (compiled program) *)
noeq type bytecode_module = {
  bm_magic: list UInt8.t;
  bm_version: nat;
  bm_strings: list string_entry;
  bm_functions: list function_entry;
  bm_constants: list int;
  bm_code: list UInt8.t;
}

(** Create empty module *)
let empty_module : bytecode_module = {
  bm_magic = [0x53uy; 0x50uy; 0x41uy; 0x43uy; 0x45uy];
  bm_version = 1;
  bm_strings = [];
  bm_functions = [];
  bm_constants = [];
  bm_code = [];
}

(** Helper: encode 16-bit value as little-endian bytes *)
let encode_u16 (n: nat{n < 65536}) : list UInt8.t =
  let lo = n % 256 in
  let hi = n / 256 in
  [UInt8.uint_to_t lo; UInt8.uint_to_t hi]

(** Helper: encode 32-bit value as little-endian bytes *)
let encode_u32 (n: nat{n < 4294967296}) : list UInt8.t =
  let b0 = n % 256 in
  let b1 = (n / 256) % 256 in
  let b2 = (n / 65536) % 256 in
  let b3 = (n / 16777216) % 256 in
  [UInt8.uint_to_t b0; UInt8.uint_to_t b1; UInt8.uint_to_t b2; UInt8.uint_to_t b3]

(** Helper: encode signed 16-bit offset *)
let encode_i16 (n: int{n >= -32768 && n < 32768}) : list UInt8.t =
  let unsigned = if n < 0 then 65536 + n else n in
  encode_u16 unsigned

(** Convert char to byte (assumes ASCII range) *)
let char_to_byte (c: FStar.Char.char) : UInt8.t =
  let n = FStar.Char.int_of_char c in
  if n < 256 then UInt8.uint_to_t n else 0x3Fuy  (* '?' for out of range *)

(** Add string to string table, return index *)
let add_string (m: bytecode_module) (s: string) : (bytecode_module & nat) =
  let idx = length m.bm_strings in
  let chars = FStar.String.list_of_string s in
  let utf8_bytes = List.Tot.map char_to_byte chars in
  let entry = {
    se_offset = 0;
    se_length = length utf8_bytes;
    se_data = utf8_bytes;
  } in
  ({ m with bm_strings = m.bm_strings @ [entry] }, idx)

(** Add function to function table *)
let add_function (m: bytecode_module) (name: string) (offset: nat) (len: nat)
  : bytecode_module =
  let entry = {
    fe_name = name;
    fe_code_offset = offset;
    fe_code_length = len;
  } in
  { m with bm_functions = m.bm_functions @ [entry] }

(** Append bytes to code section *)
let append_code (m: bytecode_module) (bytes: list UInt8.t) : bytecode_module =
  { m with bm_code = m.bm_code @ bytes }

(** Get current code offset *)
let code_offset (m: bytecode_module) : nat =
  length m.bm_code
