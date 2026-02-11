module Space.Compiler.Lexer

(** Lexer for the Space language - with proper UTF-8 support *)

open FStar.List.Tot
open FStar.Mul
open FStar.UInt8
open Space.Compiler.Token
open Space.Text.UTF8

(** Lexer operates on UTF-8 bytes *)
noeq type lexer_state = {
  input: list UInt8.t;
  line: nat;
  column: nat;
}

(** Lexer result *)
noeq type lexer_result =
  | LexOk of located_token & lexer_state
  | LexError of string & source_loc
  | LexEOF of source_loc

(** Convert string to bytes - assumes UTF-8 encoding *)
let string_to_bytes (s: string) : list UInt8.t =
  let chars = FStar.String.list_of_string s in
  List.Tot.map (fun c ->
    let code = FStar.Char.int_of_char c in
    if code >= 0 && code <= 255 then UInt8.uint_to_t code
    else 0uy  (* Truncate non-ASCII - F* strings should be UTF-8 bytes *)
  ) chars

(** Convert bytes to string *)
let bytes_to_string (bs: list UInt8.t) : string =
  let chars = List.Tot.map (fun b -> FStar.Char.char_of_int (UInt8.v b)) bs in
  FStar.String.string_of_list chars

(** Create initial lexer state from string *)
let init_lexer (s: string) : lexer_state = {
  input = string_to_bytes s;
  line = 1;
  column = 1;
}

(** Get current source location *)
let current_loc (st: lexer_state) : source_loc = {
  line = st.line;
  column = st.column;
}

(** Check if byte is ASCII whitespace *)
let is_whitespace_byte (b: UInt8.t) : bool =
  b = 0x20uy || b = 0x09uy || b = 0x0Auy || b = 0x0Duy  (* space, tab, LF, CR *)

(** Check if byte is ASCII digit *)
let is_digit_byte (b: UInt8.t) : bool =
  let v = UInt8.v b in
  v >= 0x30 && v <= 0x39  (* '0' to '9' *)

(** Check if byte is ASCII hex digit *)
let is_hex_digit_byte (b: UInt8.t) : bool =
  let v = UInt8.v b in
  (v >= 0x30 && v <= 0x39) ||   (* 0-9 *)
  (v >= 0x41 && v <= 0x46) ||   (* A-F *)
  (v >= 0x61 && v <= 0x66)      (* a-f *)

(** Check if byte is valid start of a word (not whitespace or special) *)
let is_word_start_byte (b: UInt8.t) : bool =
  not (is_whitespace_byte b) &&
  b <> 0x22uy &&  (* " *)
  b <> 0x28uy &&  (* ( *)
  b <> 0x29uy     (* ) *)

(** Check if byte is valid continuation of a word *)
let is_word_cont_byte (b: UInt8.t) : bool =
  is_word_start_byte b

(** Convert digit byte to value *)
let digit_value_byte (b: UInt8.t) : nat =
  let v = UInt8.v b in
  if v >= 0x30 && v <= 0x39 then v - 0x30
  else 0

(** Convert hex digit byte to value *)
let hex_digit_value_byte (b: UInt8.t) : nat =
  let v = UInt8.v b in
  if v >= 0x30 && v <= 0x39 then v - 0x30
  else if v >= 0x41 && v <= 0x46 then v - 0x37
  else if v >= 0x61 && v <= 0x66 then v - 0x57
  else 0

(** Advance state by n bytes, tracking newlines *)
let rec advance_n (st: lexer_state) (n: nat) : Tot lexer_state (decreases n) =
  if n = 0 then st
  else match st.input with
    | [] -> st
    | b :: rest ->
      let st' = if b = 0x0Auy then  (* newline *)
        { input = rest; line = st.line + 1; column = 1 }
      else
        { input = rest; line = st.line; column = st.column + 1 }
      in
      advance_n st' (n - 1)

(** Advance state by one byte *)
let advance (st: lexer_state) : lexer_state =
  advance_n st 1

(** Skip whitespace bytes *)
let rec skip_whitespace (st: lexer_state) : Tot lexer_state (decreases (List.Tot.length st.input)) =
  match st.input with
  | [] -> st
  | b :: _ ->
    if is_whitespace_byte b then skip_whitespace (advance st)
    else st

(** Skip to end of line (for comments) *)
let rec skip_to_eol (st: lexer_state) : Tot lexer_state (decreases (List.Tot.length st.input)) =
  match st.input with
  | [] -> st
  | b :: _ ->
    if b = 0x0Auy then advance st  (* newline *)
    else skip_to_eol (advance st)

(** Read word bytes until whitespace or special char *)
let rec read_word_bytes_aux (st: lexer_state) (acc: list UInt8.t)
  : Tot (list UInt8.t & lexer_state) (decreases (List.Tot.length st.input)) =
  match st.input with
  | [] -> (acc, st)
  | b :: _ ->
    if is_word_cont_byte b then
      read_word_bytes_aux (advance st) (acc @ [b])
    else
      (acc, st)

let read_word (st: lexer_state) : (string & lexer_state) =
  let (bytes, st') = read_word_bytes_aux st [] in
  (bytes_to_string bytes, st')

(** Encode a Unicode code point as UTF-8 bytes *)
let encode_codepoint_utf8 (cp: nat) : list UInt8.t =
  if cp <= 0x7F then
    [UInt8.uint_to_t cp]
  else if cp <= 0x7FF then
    let b0 = 0xC0 + (cp / 64) in
    let b1 = 0x80 + (cp % 64) in
    [UInt8.uint_to_t b0; UInt8.uint_to_t b1]
  else if cp <= 0xFFFF then
    let b0 = 0xE0 + (cp / 4096) in
    let b1 = 0x80 + ((cp / 64) % 64) in
    let b2 = 0x80 + (cp % 64) in
    [UInt8.uint_to_t b0; UInt8.uint_to_t b1; UInt8.uint_to_t b2]
  else if cp <= 0x10FFFF then
    let b0 = 0xF0 + (cp / 262144) in
    let b1 = 0x80 + ((cp / 4096) % 64) in
    let b2 = 0x80 + ((cp / 64) % 64) in
    let b3 = 0x80 + (cp % 64) in
    [UInt8.uint_to_t b0; UInt8.uint_to_t b1; UInt8.uint_to_t b2; UInt8.uint_to_t b3]
  else
    [0xEFuy; 0xBFuy; 0xBDuy]  (* Replacement character U+FFFD *)

(** Parse exactly n hex digits from byte list, return (value, remaining) *)
let rec parse_n_hex_digits (bytes: list UInt8.t) (n: nat) (acc: nat)
  : Tot (option (nat & list UInt8.t)) (decreases n) =
  if n = 0 then Some (acc, bytes)
  else match bytes with
    | [] -> None
    | b :: rest ->
      if is_hex_digit_byte b then
        parse_n_hex_digits rest (n - 1) (acc * 16 + hex_digit_value_byte b)
      else None

(** String escape result *)
noeq type escape_result =
  | EscOk of list UInt8.t & lexer_state  (* Encoded bytes and new state *)
  | EscError of string & source_loc

(** Handle escape sequence - returns UTF-8 encoded bytes *)
let handle_escape (st: lexer_state) (start: source_loc) : escape_result =
  match st.input with
  | [] -> EscError ("Unterminated escape sequence", start)
  | b :: rest ->
    let st' = advance st in
    let v = UInt8.v b in
    (* Simple escapes *)
    if b = 0x6Euy then EscOk ([0x0Auy], st')        (* \n -> LF *)
    else if b = 0x74uy then EscOk ([0x09uy], st')   (* \t -> TAB *)
    else if b = 0x72uy then EscOk ([0x0Duy], st')   (* \r -> CR *)
    else if b = 0x5Cuy then EscOk ([0x5Cuy], st')   (* \\ -> backslash *)
    else if b = 0x22uy then EscOk ([0x22uy], st')   (* \" -> quote *)
    else if b = 0x30uy then EscOk ([0x00uy], st')   (* \0 -> NUL *)
    (* \xXX - single byte escape *)
    else if b = 0x78uy then  (* 'x' *)
      (match parse_n_hex_digits rest 2 0 with
       | Some (value, _) ->
         let st'' = advance_n st' 2 in
         EscOk ([UInt8.uint_to_t value], st'')
       | None -> EscError ("Invalid \\x escape: expected 2 hex digits", current_loc st))
    (* \uXXXX - BMP Unicode escape *)
    else if b = 0x75uy then  (* 'u' *)
      (match parse_n_hex_digits rest 4 0 with
       | Some (cp, _) ->
         let st'' = advance_n st' 4 in
         if is_valid_codepoint cp then
           EscOk (encode_codepoint_utf8 cp, st'')
         else
           EscError ("Invalid Unicode codepoint in \\u escape", current_loc st)
       | None -> EscError ("Invalid \\u escape: expected 4 hex digits", current_loc st))
    (* \UXXXXXXXX - Full Unicode escape *)
    else if b = 0x55uy then  (* 'U' *)
      (match parse_n_hex_digits rest 8 0 with
       | Some (cp, _) ->
         let st'' = advance_n st' 8 in
         if is_valid_codepoint cp then
           EscOk (encode_codepoint_utf8 cp, st'')
         else
           EscError ("Invalid Unicode codepoint in \\U escape", current_loc st)
       | None -> EscError ("Invalid \\U escape: expected 8 hex digits", current_loc st))
    else
      EscError ("Unknown escape sequence", current_loc st)

(** Read string literal, collecting UTF-8 bytes - fuel based for escape handling *)
let rec read_string_aux (st: lexer_state) (acc: list UInt8.t) (start: source_loc) (fuel: nat)
  : Tot lexer_result (decreases fuel) =
  if fuel = 0 then LexError ("String too long", start)
  else match st.input with
  | [] -> LexError ("Unterminated string literal", start)
  | b :: _ ->
    if b = 0x22uy then  (* closing quote *)
      let st' = advance st in
      (* Validate UTF-8 *)
      if is_valid_utf8 acc then
        let tok = { value = TOK_String (bytes_to_string acc); loc = start } in
        LexOk (tok, st')
      else
        LexError ("Invalid UTF-8 in string literal", start)
    else if b = 0x5Cuy then  (* backslash - escape *)
      let st' = advance st in
      (match handle_escape st' start with
       | EscOk (bytes, st'') ->
         read_string_aux st'' (acc @ bytes) start (fuel - 1)
       | EscError (msg, loc) -> LexError (msg, loc))
    else
      (* Regular byte - include in string *)
      read_string_aux (advance st) (acc @ [b]) start (fuel - 1)

(** Read a stack effect ( ... ) *)
let rec read_stack_effect_aux (st: lexer_state) (acc: list UInt8.t) (depth: nat) (start: source_loc)
  : Tot lexer_result (decreases (List.Tot.length st.input)) =
  match st.input with
  | [] -> LexError ("Unterminated stack effect", start)
  | b :: _ ->
    if b = 0x29uy then  (* ')' *)
      if depth = 0 then
        let st' = advance st in
        let tok = { value = TOK_StackEffect (bytes_to_string acc); loc = start } in
        LexOk (tok, st')
      else
        read_stack_effect_aux (advance st) (acc @ [b]) (depth - 1) start
    else if b = 0x28uy then  (* '(' *)
      read_stack_effect_aux (advance st) (acc @ [b]) (depth + 1) start
    else
      read_stack_effect_aux (advance st) (acc @ [b]) depth start

(** Helper: check if char is digit *)
let is_digit (c: FStar.Char.char) : bool =
  let code = FStar.Char.int_of_char c in
  code >= 48 && code <= 57

(** Helper: check if char is hex digit *)
let is_hex_digit (c: FStar.Char.char) : bool =
  let code = FStar.Char.int_of_char c in
  (code >= 48 && code <= 57) ||
  (code >= 65 && code <= 70) ||
  (code >= 97 && code <= 102)

(** Helper: digit value from char *)
let digit_value (c: FStar.Char.char) : nat =
  let code = FStar.Char.int_of_char c in
  if code >= 48 && code <= 57 then code - 48
  else 0

(** Helper: hex digit value from char *)
let hex_digit_value (c: FStar.Char.char) : nat =
  let code = FStar.Char.int_of_char c in
  if code >= 48 && code <= 57 then code - 48
  else if code >= 65 && code <= 70 then code - 55
  else if code >= 97 && code <= 102 then code - 87
  else 0

(** Parse a decimal number from string *)
let rec parse_decimal_aux (chars: list FStar.Char.char) (acc: nat)
  : Tot nat (decreases (List.Tot.length chars)) =
  match chars with
  | [] -> acc
  | c :: rest ->
    if is_digit c then
      parse_decimal_aux rest (acc * 10 + digit_value c)
    else acc

let parse_decimal (s: string) : option int =
  let chars = FStar.String.list_of_string s in
  match chars with
  | [] -> None
  | '-' :: rest ->
    if List.Tot.length rest > 0 then Some (0 - parse_decimal_aux rest 0)
    else None
  | _ ->
    if List.Tot.for_all is_digit chars then
      Some (parse_decimal_aux chars 0)
    else None

(** Parse a hex number (after 0x prefix) *)
let rec parse_hex_aux (chars: list FStar.Char.char) (acc: nat)
  : Tot nat (decreases (List.Tot.length chars)) =
  match chars with
  | [] -> acc
  | c :: rest ->
    if is_hex_digit c then
      parse_hex_aux rest (acc * 16 + hex_digit_value c)
    else acc

let parse_hex (s: string) : option nat =
  let chars = FStar.String.list_of_string s in
  match chars with
  | '0' :: 'x' :: rest
  | '0' :: 'X' :: rest ->
    if List.Tot.length rest > 0 && List.Tot.for_all is_hex_digit rest then
      Some (parse_hex_aux rest 0)
    else None
  | _ -> None

(** Parse a binary number (after 0b prefix) *)
let rec parse_binary_aux (chars: list FStar.Char.char) (acc: nat)
  : Tot nat (decreases (List.Tot.length chars)) =
  match chars with
  | [] -> acc
  | '0' :: rest -> parse_binary_aux rest (acc * 2)
  | '1' :: rest -> parse_binary_aux rest (acc * 2 + 1)
  | _ -> acc

let parse_binary (s: string) : option nat =
  let chars = FStar.String.list_of_string s in
  match chars with
  | '0' :: 'b' :: rest
  | '0' :: 'B' :: rest ->
    let is_binary c = c = '0' || c = '1' in
    if List.Tot.length rest > 0 && List.Tot.for_all is_binary rest then
      Some (parse_binary_aux rest 0)
    else None
  | _ -> None

(** Try to parse a number from a word *)
let try_parse_number (s: string) : option number_literal =
  match parse_hex s with
  | Some n -> Some (NumHex n)
  | None ->
    match parse_binary s with
    | Some n -> Some (NumBinary n)
    | None ->
      match parse_decimal s with
      | Some n -> Some (NumDecimal n)
      | None -> None

(** Read next token (with fuel for termination) *)
let rec next_token_aux (st: lexer_state) (fuel: nat)
  : Tot lexer_result (decreases fuel) =
  if fuel = 0 then LexError ("Lexer fuel exhausted", current_loc st)
  else
    let st = skip_whitespace st in
    match st.input with
    | [] -> LexEOF (current_loc st)
    | b :: _ ->
      if b = 0x5Cuy then  (* backslash - comment *)
        let st' = skip_to_eol (advance st) in
        next_token_aux st' (fuel - 1)
      else if b = 0x22uy then  (* quote - string literal *)
        let start = current_loc st in
        let st' = advance st in
        read_string_aux st' [] start (List.Tot.length st.input)
      else if b = 0x28uy then  (* open paren - stack effect *)
        let start = current_loc st in
        let st' = advance st in
        read_stack_effect_aux st' [] 0 start
      else  (* word or number *)
        let start = current_loc st in
        let (word, st') = read_word st in
        if FStar.String.strlen word = 0 then
          LexError ("Unexpected character", start)
        else
          match try_parse_number word with
          | Some num ->
            let tok = { value = TOK_Number num; loc = start } in
            LexOk (tok, st')
          | None ->
            let tok = { value = classify_word word; loc = start } in
            LexOk (tok, st')

(** Read next token *)
let next_token (st: lexer_state) : lexer_result =
  next_token_aux st (List.Tot.length st.input + 1)

(** Tokenize entire input *)
let rec tokenize_aux (st: lexer_state) (acc: list located_token) (fuel: nat)
  : Tot (either (list located_token) (string & source_loc)) (decreases fuel) =
  if fuel = 0 then Inl (List.Tot.rev acc)
  else
    match next_token st with
    | LexOk (tok, st') ->
      tokenize_aux st' (tok :: acc) (fuel - 1)
    | LexError (msg, loc) ->
      Inr (msg, loc)
    | LexEOF loc ->
      let eof_tok = { value = TOK_EOF; loc = loc } in
      Inl (List.Tot.rev (eof_tok :: acc))

(** Tokenize a string *)
let tokenize (s: string) : either (list located_token) (string & source_loc) =
  let st = init_lexer s in
  let fuel = List.Tot.length st.input + 1 in
  tokenize_aux st [] fuel
