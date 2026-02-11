module Space.Compiler.Test

(** Tests for the Space compiler *)

open FStar.List.Tot
open Space.Compiler.Token
open Space.Compiler.Lexer
open Space.Compiler.AST
open Space.Compiler.Parser
open Space.Compiler.Bytecode
open Space.Compiler.Codegen
open Space.Compiler.Decoder
open Space.Instruction

(** Test 1: Lexer - simple tokens *)
let test_lexer_simple : bool =
  match tokenize "42 dup add" with
  | Inl tokens -> length tokens = 4  (* 3 tokens + EOF *)
  | Inr _ -> false

(** Test 2: Lexer - string literal *)
let test_lexer_string : bool =
  match tokenize "\"hello\"" with
  | Inl tokens ->
    (match tokens with
     | t :: _ ->
       (match t.value with
        | TOK_String s -> s = "hello"
        | _ -> false)
     | _ -> false)
  | Inr _ -> false

(** Test 3: Lexer - keywords *)
let test_lexer_keywords : bool =
  match tokenize "define-word end-word if-true end-if" with
  | Inl tokens -> length tokens = 5
  | Inr _ -> false

(** Test 4: Lexer - hex number *)
let test_lexer_hex : bool =
  match tokenize "0xFF" with
  | Inl tokens ->
    (match tokens with
     | t :: _ ->
       (match t.value with
        | TOK_Number (NumHex n) -> n = 255
        | _ -> false)
     | _ -> false)
  | Inr _ -> false

(** Test 5: Lexer - comment skipping *)
let test_lexer_comment : bool =
  match tokenize "1 \\ this is a comment\n2" with
  | Inl tokens -> length tokens = 3  (* 1, 2, EOF *)
  | Inr _ -> false

(** Test 6: Parser - simple word definition *)
let test_parser_word : bool =
  let source = "define-word double ( n -- n ) dup add end-word" in
  match parse_string source with
  | Inl prog -> length prog.definitions = 1
  | Inr _ -> false

(** Test 7: Parser - constant definition *)
let test_parser_constant : bool =
  let source = "define-constant answer 42" in
  match parse_string source with
  | Inl prog ->
    (match prog.definitions with
     | [Def_Constant c] -> c.cd_name = "answer" && c.cd_value = 42
     | _ -> false)
  | Inr _ -> false

(** Test 8: Parser - text definition *)
let test_parser_text : bool =
  let source = "define-text greeting \"Hello\"" in
  match parse_string source with
  | Inl prog ->
    (match prog.definitions with
     | [Def_Text t] -> t.td_name = "greeting" && t.td_value = "Hello"
     | _ -> false)
  | Inr _ -> false

(** Test 9: Parser - if-true *)
let test_parser_if : bool =
  let source = "define-word test ( -- ) 1 if-true 2 end-if end-word" in
  match parse_string source with
  | Inl prog -> length prog.definitions = 1
  | Inr _ -> false

(** Test 10: Parser - if-else *)
let test_parser_if_else : bool =
  let source = "define-word test ( -- ) 1 if-true 2 if-else 3 end-if end-word" in
  match parse_string source with
  | Inl prog -> length prog.definitions = 1
  | Inr _ -> false

(** Test 11: Parser - loop *)
let test_parser_loop : bool =
  let source = "define-word test ( -- ) begin-loop 1 end-loop end-word" in
  match parse_string source with
  | Inl prog -> length prog.definitions = 1
  | Inr _ -> false

(** Test 12: Parser - while *)
let test_parser_while : bool =
  let source = "define-word test ( -- ) begin-while 1 do-while 2 end-while end-word" in
  match parse_string source with
  | Inl prog -> length prog.definitions = 1
  | Inr _ -> false

(** Test 13: Codegen - simple word *)
let test_codegen_simple : bool =
  let source = "define-word double ( n -- n ) dup add end-word" in
  match compile_string source with
  | Inl m -> length m.bm_code > 0 && length m.bm_functions = 1
  | Inr _ -> false

(** Test 14: Codegen - multiple words *)
let test_codegen_multi : bool =
  let source = "define-word inc ( n -- n ) 1 add end-word define-word dec ( n -- n ) 1 subtract end-word" in
  match compile_string source with
  | Inl m -> length m.bm_functions = 2
  | Inr _ -> false

(** Test 15: Codegen - with string *)
let test_codegen_string : bool =
  let source = "define-text msg \"test\" define-word greet ( -- ) end-word" in
  match compile_string source with
  | Inl m -> length m.bm_strings >= 1
  | Inr _ -> false

(** Test 16: Full pipeline - factorial-like structure *)
let test_full_factorial : bool =
  let source = "
    define-word factorial ( n -- n! )
      dup 1 less-than if-true
        drop 1
      if-else
        dup 1 subtract factorial multiply
      end-if
    end-word
  " in
  match compile_string source with
  | Inl m -> length m.bm_functions = 1 && length m.bm_code > 0
  | Inr _ -> false

(** Test 17: Primitives compilation *)
let test_primitives : bool =
  let source = "define-word test ( -- ) dup drop swap over rot add subtract multiply end-word" in
  match compile_string source with
  | Inl m -> length m.bm_code > 8  (* at least 8 opcodes + ret *)
  | Inr _ -> false

(** Test 18: Nested control flow *)
let test_nested_control : bool =
  let source = "
    define-word nested ( n -- )
      if-true
        begin-loop
          1 if-true
            exit-word
          end-if
        end-loop
      end-if
    end-word
  " in
  match compile_string source with
  | Inl m -> length m.bm_functions = 1
  | Inr _ -> false

(** Test 19: Unicode escape - \xXX byte escape *)
let test_unicode_hex_escape : bool =
  match tokenize "\"\\x41\\x42\\x43\"" with  (* ABC *)
  | Inl tokens ->
    (match tokens with
     | t :: _ ->
       (match t.value with
        | TOK_String s -> s = "ABC"
        | _ -> false)
     | _ -> false)
  | Inr _ -> false

(** Test 20: Unicode escape - \uXXXX BMP escape *)
let test_unicode_bmp_escape : bool =
  (* \u0048\u0069 = "Hi" *)
  match tokenize "\"\\u0048\\u0069\"" with
  | Inl tokens ->
    (match tokens with
     | t :: _ ->
       (match t.value with
        | TOK_String s -> s = "Hi"
        | _ -> false)
     | _ -> false)
  | Inr _ -> false

(** Test 21: Unicode escape - \u for non-ASCII (euro sign U+20AC) *)
let test_unicode_euro : bool =
  (* Euro sign: U+20AC = UTF-8 bytes E2 82 AC *)
  match tokenize "\"\\u20AC\"" with
  | Inl tokens ->
    (match tokens with
     | t :: _ ->
       (match t.value with
        | TOK_String s ->
          (* Check that we got a 3-byte UTF-8 sequence *)
          FStar.String.strlen s = 3
        | _ -> false)
     | _ -> false)
  | Inr _ -> false

(** Test 22: Unicode escape - \UXXXXXXXX full Unicode *)
let test_unicode_full_escape : bool =
  (* \U0001F600 = grinning face emoji = UTF-8 F0 9F 98 80 *)
  match tokenize "\"\\U0001F600\"" with
  | Inl tokens ->
    (match tokens with
     | t :: _ ->
       (match t.value with
        | TOK_String s ->
          (* 4-byte UTF-8 sequence *)
          FStar.String.strlen s = 4
        | _ -> false)
     | _ -> false)
  | Inr _ -> false

(** Test 23: Unicode escape - \0 null escape *)
let test_unicode_null_escape : bool =
  match tokenize "\"a\\0b\"" with
  | Inl tokens ->
    (match tokens with
     | t :: _ ->
       (match t.value with
        | TOK_String s -> FStar.String.strlen s = 3  (* a, NUL, b *)
        | _ -> false)
     | _ -> false)
  | Inr _ -> false

(** Test 24: Unicode - mixed escapes *)
let test_unicode_mixed : bool =
  (* "A\n\u0042\x43" = "A" + newline + "B" + "C" *)
  match tokenize "\"A\\n\\u0042\\x43\"" with
  | Inl tokens ->
    (match tokens with
     | t :: _ ->
       (match t.value with
        | TOK_String s -> FStar.String.strlen s = 4
        | _ -> false)
     | _ -> false)
  | Inr _ -> false

(** Test 25: Decoder - decode simple bytecode *)
let test_decoder_simple : bool =
  (* dup, add, ret = 0x01, 0x10, 0x91 *)
  let bytecode = [0x01uy; 0x10uy; 0x91uy] in
  match decode_bytecode bytecode with
  | Inl instrs -> length instrs = 3
  | Inr _ -> false

(** Test 26: Decoder - decode push_i8 *)
let test_decoder_push : bool =
  (* push_i8 42, ret = 0xF0 0x2A 0x91 *)
  let bytecode = [0xF0uy; 0x2Auy; 0x91uy] in
  match decode_bytecode bytecode with
  | Inl instrs ->
    (match instrs with
     | IPush n :: IReturn :: [] -> FStar.UInt64.v n = 42
     | _ -> false)
  | Inr _ -> false

(** Test 27: Decoder - decode compiled function *)
let test_decoder_compile_decode : bool =
  let source = "define-word double ( n -- n ) dup add end-word" in
  match compile_string source with
  | Inl m ->
    (match decode_function m 0 with
     | Inl instrs -> length instrs >= 2  (* at least dup, add *)
     | Inr _ -> false)
  | Inr _ -> false

(** Test 28: Decoder - decode branch *)
let test_decoder_branch : bool =
  (* jz +5 = 0x93 0x05 0x00 *)
  let bytecode = [0x93uy; 0x05uy; 0x00uy] in
  match decode_bytecode bytecode with
  | Inl instrs ->
    (match instrs with
     | IBranchZero _ :: [] -> true
     | _ -> false)
  | Inr _ -> false

(** Test 29: Decoder - decode comparison *)
let test_decoder_compare : bool =
  (* eq = 0x30, neq = 0x31, ltu = 0x34 *)
  let bytecode = [0x30uy; 0x31uy; 0x34uy] in
  match decode_bytecode bytecode with
  | Inl instrs ->
    (match instrs with
     | IPrimitive PrimEq :: IPrimitive PrimNeq :: IPrimitive PrimLtU :: [] -> true
     | _ -> false)
  | Inr _ -> false

(** Test 30: Decoder - full pipeline compile and decode *)
let test_decoder_full_pipeline : bool =
  let source = "define-word test ( n -- n ) dup 1 add swap drop end-word" in
  match compile_string source with
  | Inl m ->
    (match decode_function_by_name m "test" with
     | Inl instrs -> length instrs >= 4
     | Inr _ -> false)
  | Inr _ -> false

(** Run all tests and count passes *)
let run_tests : nat =
  let results = [
    test_lexer_simple;
    test_lexer_string;
    test_lexer_keywords;
    test_lexer_hex;
    test_lexer_comment;
    test_parser_word;
    test_parser_constant;
    test_parser_text;
    test_parser_if;
    test_parser_if_else;
    test_parser_loop;
    test_parser_while;
    test_codegen_simple;
    test_codegen_multi;
    test_codegen_string;
    test_full_factorial;
    test_primitives;
    test_nested_control;
    test_unicode_hex_escape;
    test_unicode_bmp_escape;
    test_unicode_euro;
    test_unicode_full_escape;
    test_unicode_null_escape;
    test_unicode_mixed;
    test_decoder_simple;
    test_decoder_push;
    test_decoder_compile_decode;
    test_decoder_branch;
    test_decoder_compare;
    test_decoder_full_pipeline;
  ] in
  List.Tot.length (List.Tot.filter (fun b -> b) results)

(** Total number of tests *)
let total_tests : nat = 30

(** All tests pass *)
let all_tests_pass : bool = run_tests = total_tests
