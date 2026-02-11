module Space.Compiler.Codegen

(** Code generator: AST → Bytecode *)

open FStar.List.Tot
open FStar.UInt8
open FStar.Mul
open Space.Compiler.Token
open Space.Compiler.AST
open Space.Compiler.Bytecode

(** Compilation context *)
noeq type compile_ctx = {
  ctx_module: bytecode_module;
  ctx_word_offsets: list (string & nat);
  ctx_universe_names: list string;
  ctx_next_string: nat;
  ctx_errors: list (string & source_loc);
}

(** Create initial context *)
let init_ctx : compile_ctx = {
  ctx_module = empty_module;
  ctx_word_offsets = [];
  ctx_universe_names = [];
  ctx_next_string = 0;
  ctx_errors = [];
}

(** Add error to context *)
let add_error (ctx: compile_ctx) (msg: string) (loc: source_loc) : compile_ctx =
  { ctx with ctx_errors = ctx.ctx_errors @ [(msg, loc)] }

(** Lookup word offset *)
let lookup_word (ctx: compile_ctx) (name: string) : option nat =
  let rec search (pairs: list (string & nat)) : option nat =
    match pairs with
    | [] -> None
    | (n, off) :: rest -> if n = name then Some off else search rest
  in
  search ctx.ctx_word_offsets

(** Lookup or add universe name, return index *)
let lookup_or_add_universe (ctx: compile_ctx) (name: string) : (compile_ctx & nat) =
  let rec search (names: list string) (idx: nat) : option nat =
    match names with
    | [] -> None
    | n :: rest -> if n = name then Some idx else search rest (idx + 1)
  in
  match search ctx.ctx_universe_names 0 with
  | Some idx -> (ctx, idx)
  | None ->
    let idx = length ctx.ctx_universe_names in
    let ctx' = { ctx with ctx_universe_names = ctx.ctx_universe_names @ [name] } in
    (ctx', idx)

(** Emit single opcode *)
let emit_op (ctx: compile_ctx) (op: opcode) : compile_ctx =
  { ctx with ctx_module = append_code ctx.ctx_module [op] }

(** Emit opcode with u8 argument *)
let emit_op_u8 (ctx: compile_ctx) (op: opcode) (arg: UInt8.t) : compile_ctx =
  { ctx with ctx_module = append_code ctx.ctx_module [op; arg] }

(** Emit opcode with u16 argument *)
let emit_op_u16 (ctx: compile_ctx) (op: opcode) (arg: nat{arg < 65536}) : compile_ctx =
  { ctx with ctx_module = append_code ctx.ctx_module (op :: encode_u16 arg) }

(** Emit opcode with i16 argument *)
let emit_op_i16 (ctx: compile_ctx) (op: opcode) (arg: int{arg >= -32768 && arg < 32768}) : compile_ctx =
  { ctx with ctx_module = append_code ctx.ctx_module (op :: encode_i16 arg) }

(** Emit opcode with u32 argument *)
let emit_op_u32 (ctx: compile_ctx) (op: opcode) (arg: nat{arg < 4294967296}) : compile_ctx =
  { ctx with ctx_module = append_code ctx.ctx_module (op :: encode_u32 arg) }

(** Current code position *)
let current_pos (ctx: compile_ctx) : nat =
  code_offset ctx.ctx_module

(** Emit push literal *)
let emit_push_int (ctx: compile_ctx) (n: int) : compile_ctx =
  if n >= -128 && n < 128 then
    let byte = if n < 0 then UInt8.uint_to_t (256 + n) else UInt8.uint_to_t n in
    emit_op_u8 ctx op_push_i8 byte
  else if n >= -32768 && n < 32768 then
    emit_op_i16 ctx op_push_i16 n
  else if n >= 0 && n < 4294967296 then
    emit_op_u32 ctx op_push_i32 n
  else if n >= -2147483648 && n < 0 then
    let unsigned : nat = 4294967296 + n in
    emit_op_u32 ctx op_push_i32 unsigned
  else
    ctx

(** Emit string literal, add to string table *)
let emit_push_string (ctx: compile_ctx) (s: string) : compile_ctx =
  let (m', idx) = add_string ctx.ctx_module s in
  let ctx' = { ctx with ctx_module = m'; ctx_next_string = idx + 1 } in
  if idx < 65536 then
    emit_op_u16 ctx' op_push_str idx
  else
    ctx'

(** Map primitive to opcode *)
let primitive_to_opcode (p: primitive) : opcode =
  match p with
  | PRIM_Dup -> op_dup
  | PRIM_Drop -> op_drop
  | PRIM_Swap -> op_swap
  | PRIM_Over -> op_over
  | PRIM_Rot -> op_rot
  | PRIM_Nip -> op_nip
  | PRIM_Tuck -> op_tuck
  | PRIM_Add -> op_add
  | PRIM_Subtract -> op_sub
  | PRIM_Multiply -> op_mul
  | PRIM_DivideUnsigned -> op_divu
  | PRIM_DivideSigned -> op_divs
  | PRIM_Modulo -> op_mod
  | PRIM_Negate -> op_neg
  | PRIM_Min -> op_min
  | PRIM_Max -> op_max
  | PRIM_BitAnd -> op_and
  | PRIM_BitOr -> op_or
  | PRIM_BitXor -> op_xor
  | PRIM_BitNot -> op_not
  | PRIM_ShiftLeft -> op_shl
  | PRIM_ShiftRight -> op_shr
  | PRIM_Equal -> op_eq
  | PRIM_NotEqual -> op_neq
  | PRIM_LessThan -> op_lt
  | PRIM_GreaterThan -> op_gt
  | PRIM_LessUnsigned -> op_ltu
  | PRIM_GreaterUnsigned -> op_gtu
  | PRIM_AllocateBytes -> op_alloc_bytes
  | PRIM_BytesFetch -> op_bytes_fetch
  | PRIM_BytesStore -> op_bytes_store
  | PRIM_BytesLength -> op_bytes_len
  | PRIM_BytesCopy -> op_bytes_copy
  | PRIM_BorrowPointer -> op_borrow
  | PRIM_ReturnPointer -> op_return_ptr
  | PRIM_DropPointer -> op_drop_ptr
  | PRIM_FetchBorrowed -> op_fetch_bor
  | PRIM_StoreBorrowed -> op_store_bor
  | PRIM_FetchAndEnd -> op_fetch_end
  | PRIM_StoreAndEnd -> op_store_end
  | PRIM_OffsetBorrowed -> op_offset_bor
  | PRIM_WarpFetch -> op_warp_fetch
  | PRIM_WarpStore -> op_warp_store
  | PRIM_WarpAdvance -> op_warp_advance
  | PRIM_WarpFollow -> op_warp_follow
  | PRIM_WarpPosition -> op_warp_pos
  | PRIM_WarpRestore -> op_warp_restore
  | PRIM_WarpNull -> op_warp_null
  | PRIM_CreateText -> op_create_text
  | PRIM_TextByteLength -> op_text_byte_len
  | PRIM_TextGraphemeCount -> op_text_graph_cnt
  | PRIM_TextIsSimple -> op_text_is_simple
  | PRIM_TextGraphemeAt -> op_text_graph_at
  | PRIM_TextGraphemeFirst -> op_text_graph_first
  | PRIM_TextGraphemeLast -> op_text_graph_last
  | PRIM_TextSlice -> op_text_slice
  | PRIM_TextConcat -> op_text_concat
  | PRIM_TextEqual -> op_text_equal
  | PRIM_TextCompare -> op_text_compare
  | PRIM_TextCodePointCount -> op_text_cp_count
  | PRIM_TextCodePointAt -> op_text_cp_at
  | PRIM_GraphemeByteLength -> op_graph_byte_len
  | PRIM_GraphemeIsAscii -> op_graph_is_ascii
  | PRIM_GraphemeCodePoints -> op_graph_cp_count
  | PRIM_WarpHasGrapheme -> op_warp_has_graph
  | PRIM_WarpCurrentGrapheme -> op_warp_cur_graph
  | PRIM_WarpNextGrapheme -> op_warp_next_graph
  | PRIM_WarpGraphemeIndex -> op_warp_graph_idx
  | PRIM_WarpGotoGrapheme -> op_warp_goto_graph
  | PRIM_HaltSystem -> op_halt
  | PRIM_EmitByte -> op_emit
  | PRIM_ReadByte -> op_read
  | PRIM_EmitGrapheme -> op_emit_graph
  | PRIM_TextNormalizeNfc -> op_normalize_nfc
  | PRIM_TextNormalizeNfd -> op_normalize_nfd
  | PRIM_TextNormalizeNfkc -> op_normalize_nfkc
  | PRIM_TextNormalizeNfkd -> op_normalize_nfkd
  | PRIM_TextToUpper -> op_text_upper
  | PRIM_TextToLower -> op_text_lower
  | PRIM_TextToTitle -> op_text_title

(** Map discipline to byte *)
let discipline_to_byte (d: discipline) : UInt8.t =
  match d with
  | Discipline_Linear -> disc_linear
  | Discipline_Affine -> disc_affine
  | Discipline_Unrestricted -> disc_unrestricted

(** Compile expression - forward declaration *)
val compile_expr : compile_ctx -> expr -> nat -> compile_ctx
val compile_block : compile_ctx -> block -> nat -> compile_ctx

(** Compile a single expression *)
let rec compile_expr (ctx: compile_ctx) (e: expr) (fuel: nat)
  : Tot compile_ctx (decreases fuel) =
  if fuel = 0 then ctx
  else
    match e with
    | E_Literal (Lit_Int n) ->
      emit_push_int ctx n

    | E_Literal (Lit_String s) ->
      emit_push_string ctx s

    | E_Primitive prim ->
      emit_op ctx (primitive_to_opcode prim)

    | E_Call name ->
      (match lookup_word ctx name with
       | Some offset ->
         if offset < 4294967296 then
           emit_op_u32 ctx op_call offset
         else ctx
       | None ->
         emit_op_u32 ctx op_call 0)

    | E_TransferTo name ->
      let (ctx', idx) = lookup_or_add_universe ctx name in
      if idx < 65536 then
        emit_op_u16 ctx' op_transfer idx
      else ctx'

    | E_IfTrue (then_block, else_opt) ->
      let jz_pos = current_pos ctx in
      let ctx1 = emit_op_i16 ctx op_jz 0 in
      let ctx2 = compile_block ctx1 then_block (fuel - 1) in
      (match else_opt with
       | None -> ctx2
       | Some else_block ->
         let jmp_pos = current_pos ctx2 in
         let ctx3 = emit_op_i16 ctx2 op_jmp 0 in
         let ctx4 = compile_block ctx3 else_block (fuel - 1) in
         ctx4)

    | E_IfZero (then_block, else_opt) ->
      let jnz_pos = current_pos ctx in
      let ctx1 = emit_op_i16 ctx op_jnz 0 in
      let ctx2 = compile_block ctx1 then_block (fuel - 1) in
      (match else_opt with
       | None -> ctx2
       | Some else_block ->
         let jmp_pos = current_pos ctx2 in
         let ctx3 = emit_op_i16 ctx2 op_jmp 0 in
         let ctx4 = compile_block ctx3 else_block (fuel - 1) in
         ctx4)

    | E_Loop body ->
      let loop_start = current_pos ctx in
      let ctx1 = compile_block ctx body (fuel - 1) in
      let offset = loop_start - current_pos ctx1 - 3 in
      if offset >= -32768 && offset < 32768 then
        emit_op_i16 ctx1 op_loop offset
      else ctx1

    | E_While (cond, body) ->
      let loop_start = current_pos ctx in
      let ctx1 = compile_block ctx cond (fuel - 1) in
      let jz_pos = current_pos ctx1 in
      let ctx2 = emit_op_i16 ctx1 op_jz 0 in
      let ctx3 = compile_block ctx2 body (fuel - 1) in
      let back_offset = loop_start - current_pos ctx3 - 3 in
      if back_offset >= -32768 && back_offset < 32768 then
        emit_op_i16 ctx3 op_loop back_offset
      else ctx3

    | E_Times body ->
      let loop_start = current_pos ctx in
      let ctx1 = emit_push_int ctx 1 in
      let ctx2 = emit_op ctx1 op_sub in
      let ctx3 = emit_op ctx2 op_dup in
      let jz_pos = current_pos ctx3 in
      let ctx4 = emit_op_i16 ctx3 op_jz 0 in
      let ctx5 = compile_block ctx4 body (fuel - 1) in
      let back_offset = loop_start - current_pos ctx5 - 3 in
      if back_offset >= -32768 && back_offset < 32768 then
        let ctx6 = emit_op_i16 ctx5 op_loop back_offset in
        emit_op ctx6 op_drop
      else
        emit_op ctx5 op_drop

    | E_Exit ->
      emit_op ctx op_exit

    | E_CreateUniverse (size, disc, name, body) ->
      let (ctx1, idx) = lookup_or_add_universe ctx name in
      let disc_byte = discipline_to_byte disc in
      let ctx2 = emit_op_u8 ctx1 op_create_univ disc_byte in
      let ctx3 = if idx < 65536 then
        { ctx2 with ctx_module = append_code ctx2.ctx_module (encode_u16 idx) }
      else ctx2 in
      let ctx4 = compile_block ctx3 body (fuel - 1) in
      if idx < 65536 then
        emit_op_u16 ctx4 op_end_univ idx
      else ctx4

    | E_ReleaseUniverse name ->
      let (ctx1, idx) = lookup_or_add_universe ctx name in
      if idx < 65536 then
        emit_op_u16 ctx1 op_release_univ idx
      else ctx1

    | E_WarpInto (name, body) ->
      let ctx1 = emit_op ctx op_warp_into in
      let ctx2 = compile_block ctx1 body (fuel - 1) in
      emit_op ctx2 op_end_warp

    | E_WarpIntoReadonly (name, body) ->
      let ctx1 = emit_op ctx op_warp_into_ro in
      let ctx2 = compile_block ctx1 body (fuel - 1) in
      emit_op ctx2 op_end_warp

    | E_TextWarpInto body ->
      let ctx1 = emit_op ctx op_text_warp_into in
      let ctx2 = compile_block ctx1 body (fuel - 1) in
      emit_op ctx2 op_end_text_warp

(** Compile a block of expressions *)
and compile_block (ctx: compile_ctx) (b: block) (fuel: nat)
  : Tot compile_ctx (decreases fuel) =
  if fuel = 0 then ctx
  else
    match b with
    | [] -> ctx
    | e :: rest ->
      let ctx1 = compile_expr ctx e (fuel - 1) in
      compile_block ctx1 rest (fuel - 1)

(** Compile a word definition *)
let compile_word (ctx: compile_ctx) (w: definition) (fuel: nat) : compile_ctx =
  match w with
  | Def_Word wd ->
    let offset = current_pos ctx in
    let ctx1 = { ctx with ctx_word_offsets = ctx.ctx_word_offsets @ [(wd.wd_name, offset)] } in
    let ctx2 = compile_block ctx1 wd.wd_body fuel in
    let ctx3 = emit_op ctx2 op_ret in
    let end_pos = current_pos ctx3 in
    let len = if end_pos >= offset then end_pos - offset else 0 in
    { ctx3 with ctx_module = add_function ctx3.ctx_module wd.wd_name offset len }

  | Def_Constant cd ->
    ctx

  | Def_Text td ->
    let (m', _) = add_string ctx.ctx_module td.td_value in
    { ctx with ctx_module = m' }

(** Compile entire program *)
let rec compile_program_aux (ctx: compile_ctx) (defs: list definition) (fuel: nat)
  : Tot compile_ctx (decreases fuel) =
  if fuel = 0 then ctx
  else
    match defs with
    | [] -> ctx
    | d :: rest ->
      let ctx1 = compile_word ctx d (fuel - 1) in
      compile_program_aux ctx1 rest (fuel - 1)

(** Compile a program to bytecode module *)
let compile_program (prog: program) : either bytecode_module (list (string & source_loc)) =
  let fuel = 10000 in
  let ctx = compile_program_aux init_ctx prog.definitions fuel in
  if length ctx.ctx_errors > 0 then
    Inr ctx.ctx_errors
  else
    Inl ctx.ctx_module

(** Compile from source string *)
let compile_string (source: string) : either bytecode_module (string & source_loc) =
  match Space.Compiler.Parser.parse_string source with
  | Inr (msg, loc) -> Inr (msg, loc)
  | Inl prog ->
    match compile_program prog with
    | Inr errors ->
      (match errors with
       | (msg, loc) :: _ -> Inr (msg, loc)
       | [] -> Inr ("Unknown error", { line = 0; column = 0 }))
    | Inl m -> Inl m
