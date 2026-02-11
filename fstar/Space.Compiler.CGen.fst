module Space.Compiler.CGen

(** C code generator - compiles Space AST to C source *)

open FStar.String
open FStar.List.Tot
open Space.Compiler.Token
open Space.Compiler.AST

(** Generated C code as a list of lines *)
type ccode = list string

(** Concatenate code blocks *)
let (@@) (a b: ccode) : ccode = a @ b

(** Indent code *)
let indent (code: ccode) : ccode =
  List.Tot.map (fun s -> "    " ^ s) code

(** Generate primitive operation *)
let gen_primitive (p: primitive) : ccode =
  match p with
  (* Stack operations *)
  | PRIM_Dup -> ["sp++; stack[sp] = stack[sp-1];"]
  | PRIM_Drop -> ["sp--;"]
  | PRIM_Swap -> ["{ uint64_t t = stack[sp]; stack[sp] = stack[sp-1]; stack[sp-1] = t; }"]
  | PRIM_Over -> ["sp++; stack[sp] = stack[sp-2];"]
  | PRIM_Rot -> ["{ uint64_t t = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = stack[sp]; stack[sp] = t; }"]
  | PRIM_Nip -> ["stack[sp-1] = stack[sp]; sp--;"]
  | PRIM_Tuck -> ["{ uint64_t t = stack[sp]; stack[sp] = stack[sp-1]; stack[sp-1] = t; sp++; stack[sp] = stack[sp-2]; }"]
  (* Arithmetic *)
  | PRIM_Add -> ["stack[sp-1] += stack[sp]; sp--;"]
  | PRIM_Subtract -> ["stack[sp-1] -= stack[sp]; sp--;"]
  | PRIM_Multiply -> ["stack[sp-1] *= stack[sp]; sp--;"]
  | PRIM_DivideUnsigned -> ["stack[sp-1] /= stack[sp]; sp--;"]
  | PRIM_DivideSigned -> ["stack[sp-1] = (uint64_t)((int64_t)stack[sp-1] / (int64_t)stack[sp]); sp--;"]
  | PRIM_Modulo -> ["stack[sp-1] %= stack[sp]; sp--;"]
  | PRIM_Negate -> ["stack[sp] = (uint64_t)(-(int64_t)stack[sp]);"]
  | PRIM_Min -> ["if (stack[sp] < stack[sp-1]) stack[sp-1] = stack[sp]; sp--;"]
  | PRIM_Max -> ["if (stack[sp] > stack[sp-1]) stack[sp-1] = stack[sp]; sp--;"]
  (* Bitwise *)
  | PRIM_BitAnd -> ["stack[sp-1] &= stack[sp]; sp--;"]
  | PRIM_BitOr -> ["stack[sp-1] |= stack[sp]; sp--;"]
  | PRIM_BitXor -> ["stack[sp-1] ^= stack[sp]; sp--;"]
  | PRIM_BitNot -> ["stack[sp] = ~stack[sp];"]
  | PRIM_ShiftLeft -> ["stack[sp-1] <<= stack[sp]; sp--;"]
  | PRIM_ShiftRight -> ["stack[sp-1] >>= stack[sp]; sp--;"]
  (* Comparison - push 1 for true, 0 for false *)
  | PRIM_Equal -> ["stack[sp-1] = (stack[sp-1] == stack[sp]) ? 1 : 0; sp--;"]
  | PRIM_NotEqual -> ["stack[sp-1] = (stack[sp-1] != stack[sp]) ? 1 : 0; sp--;"]
  | PRIM_LessThan -> ["stack[sp-1] = ((int64_t)stack[sp-1] < (int64_t)stack[sp]) ? 1 : 0; sp--;"]
  | PRIM_GreaterThan -> ["stack[sp-1] = ((int64_t)stack[sp-1] > (int64_t)stack[sp]) ? 1 : 0; sp--;"]
  | PRIM_LessUnsigned -> ["stack[sp-1] = (stack[sp-1] < stack[sp]) ? 1 : 0; sp--;"]
  | PRIM_GreaterUnsigned -> ["stack[sp-1] = (stack[sp-1] > stack[sp]) ? 1 : 0; sp--;"]
  (* Memory - simplified, using global memory array *)
  | PRIM_AllocateBytes -> ["stack[sp] = (uint64_t)space_alloc(stack[sp]);"]
  | PRIM_BytesFetch -> ["{ uint64_t off = stack[sp]; sp--; stack[sp] = ((uint8_t*)stack[sp])[off]; }"]
  | PRIM_BytesStore -> ["{ uint64_t v = stack[sp]; sp--; uint64_t off = stack[sp]; sp--; ((uint8_t*)stack[sp])[off] = v; sp--; }"]
  | PRIM_BytesLength -> ["stack[sp] = space_bytes_len((void*)stack[sp]);"]
  | PRIM_BytesCopy -> ["space_bytes_copy();"]  (* needs runtime support *)
  (* I/O *)
  | PRIM_EmitByte -> ["putchar((int)stack[sp]); sp--;"]
  | PRIM_ReadByte -> ["sp++; stack[sp] = getchar();"]
  | PRIM_HaltSystem -> ["return;"]
  (* Borrowing, warps, text - require runtime support *)
  | _ -> ["/* TODO: " ^ "complex primitive" ^ " */"]

(** Forward declare for mutual recursion *)
val gen_expr : expr -> nat -> ccode
val gen_block : list expr -> nat -> ccode

(** Generate expression with label counter for control flow *)
let rec gen_expr (e: expr) (lbl: nat) : Tot ccode (decreases e) =
  match e with
  | E_Literal (Lit_Int n) ->
    ["sp++; stack[sp] = " ^ string_of_int n ^ "ULL;"]
  | E_Literal (Lit_String s) ->
    ["sp++; stack[sp] = (uint64_t)\"" ^ s ^ "\";"]
  | E_Primitive p ->
    gen_primitive p
  | E_Call name ->
    ["word_" ^ name ^ "();"]
  | E_Exit ->
    ["return;"]
  | E_IfTrue (then_block, else_opt) ->
    let lbl_s = string_of_int lbl in
    ["if (stack[sp--]) {"] @@
    indent (gen_block then_block (lbl + 1)) @@
    (match else_opt with
     | None -> ["}"]
     | Some else_block ->
       ["} else {"] @@
       indent (gen_block else_block (lbl + 100)) @@
       ["}"])
  | E_IfZero (then_block, else_opt) ->
    ["if (!stack[sp--]) {"] @@
    indent (gen_block then_block (lbl + 1)) @@
    (match else_opt with
     | None -> ["}"]
     | Some else_block ->
       ["} else {"] @@
       indent (gen_block else_block (lbl + 100)) @@
       ["}"])
  | E_Loop body ->
    let lbl_s = string_of_int lbl in
    ["loop_" ^ lbl_s ^ ":"] @@
    gen_block body (lbl + 1) @@
    ["goto loop_" ^ lbl_s ^ ";"]
  | E_While (cond, body) ->
    let lbl_s = string_of_int lbl in
    ["while_" ^ lbl_s ^ ":"] @@
    gen_block cond (lbl + 1) @@
    ["if (!stack[sp--]) goto endwhile_" ^ lbl_s ^ ";"] @@
    gen_block body (lbl + 100) @@
    ["goto while_" ^ lbl_s ^ ";"] @@
    ["endwhile_" ^ lbl_s ^ ":;"]
  | E_Times body ->
    let lbl_s = string_of_int lbl in
    ["{ uint64_t count_" ^ lbl_s ^ " = stack[sp--];"] @@
    ["  while (count_" ^ lbl_s ^ "-- > 0) {"] @@
    indent (indent (gen_block body (lbl + 1))) @@
    ["  }"] @@
    ["}"]
  | E_TransferTo name ->
    ["/* transfer-to " ^ name ^ " - runtime support needed */"]
  | E_CreateUniverse (size, disc, name, body) ->
    ["/* create-universe " ^ name ^ " */"] @@
    gen_block body (lbl + 1) @@
    ["/* end-universe " ^ name ^ " */"]
  | E_ReleaseUniverse name ->
    ["/* release-universe " ^ name ^ " */"]
  | E_WarpInto (name, body) ->
    ["/* warp-into " ^ name ^ " */"] @@
    gen_block body (lbl + 1) @@
    ["/* end-warp */"]
  | E_WarpIntoReadonly (name, body) ->
    ["/* warp-into-readonly " ^ name ^ " */"] @@
    gen_block body (lbl + 1) @@
    ["/* end-warp */"]
  | E_TextWarpInto body ->
    ["/* text-warp-into */"] @@
    gen_block body (lbl + 1) @@
    ["/* end-text-warp */"]

and gen_block (exprs: list expr) (lbl: nat) : Tot ccode (decreases exprs) =
  match exprs with
  | [] -> []
  | e :: rest -> gen_expr e lbl @@ gen_block rest (lbl + 1000)

(** Generate word definition *)
let gen_word (w: word_def) : ccode =
  [""; "void word_" ^ w.wd_name ^ "(void) {"] @@
  indent (gen_block w.wd_body 0) @@
  ["}"]

(** Generate constant definition *)
let gen_constant (c: const_def) : ccode =
  ["#define CONST_" ^ c.cd_name ^ " " ^ string_of_int c.cd_value]

(** Generate text definition *)
let gen_text (t: text_def) : ccode =
  ["static const char* TEXT_" ^ t.td_name ^ " = \"" ^ t.td_value ^ "\";"]

(** Generate definition *)
let gen_definition (d: definition) : ccode =
  match d with
  | Def_Word w -> gen_word w
  | Def_Constant c -> gen_constant c
  | Def_Text t -> gen_text t

(** Generate forward declarations for words *)
let rec gen_forward_decls (defs: list definition) : ccode =
  match defs with
  | [] -> []
  | Def_Word w :: rest -> ("void word_" ^ w.wd_name ^ "(void);") :: gen_forward_decls rest
  | _ :: rest -> gen_forward_decls rest

(** Generate all definitions *)
let rec gen_definitions (defs: list definition) : ccode =
  match defs with
  | [] -> []
  | d :: rest -> gen_definition d @@ gen_definitions rest

(** C runtime header *)
let runtime_header : ccode = [
  "/* Generated by Space Compiler */";
  "#include <stdint.h>";
  "#include <stdio.h>";
  "#include <stdlib.h>";
  "#include <string.h>";
  "";
  "/* Runtime */";
  "#define STACK_SIZE 65536";
  "#define MEMORY_SIZE (1024 * 1024)";
  "";
  "static uint64_t stack[STACK_SIZE];";
  "static int sp = -1;";
  "static uint8_t memory[MEMORY_SIZE];";
  "static size_t mem_ptr = 0;";
  "";
  "static void* space_alloc(size_t n) {";
  "    void* p = &memory[mem_ptr];";
  "    mem_ptr += n;";
  "    return p;";
  "}";
  "";
  "static size_t space_bytes_len(void* p) {";
  "    return 0; /* TODO: track allocations */";
  "}";
  "";
  "static void space_bytes_copy(void) {";
  "    /* TODO */";
  "}";
  ""
]

(** Generate main function that calls 'main' word if it exists *)
let gen_main (defs: list definition) : ccode =
  let has_main = match find_word defs "main" with | Some _ -> true | None -> false in
  if has_main then [
    "";
    "int main(void) {";
    "    word_main();";
    "    return 0;";
    "}"
  ]
  else [
    "";
    "int main(void) {";
    "    /* No 'main' word defined */";
    "    return 0;";
    "}"
  ]

(** Generate complete C program *)
let gen_program (p: program) : ccode =
  runtime_header @@
  ["/* Forward declarations */"] @@
  gen_forward_decls p.definitions @@
  [""; "/* Constants and texts */"] @@
  gen_definitions p.definitions @@
  gen_main p.definitions

(** Convert code to string *)
let rec code_to_string (code: ccode) : string =
  match code with
  | [] -> ""
  | line :: rest -> line ^ "\n" ^ code_to_string rest
