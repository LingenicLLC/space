module Space.Compiler.Parser

(** Recursive descent parser for the Space language *)

open FStar.List.Tot
open Space.Compiler.Token
open Space.Compiler.AST

(** Parser state *)
noeq type parser_state = {
  tokens: list located_token;
  pos: nat;
}

(** Parser result *)
noeq type parse_result 'a =
  | ParseOk of 'a & parser_state
  | ParseError of string & source_loc

(** Initialize parser from token list *)
let init_parser (tokens: list located_token) : parser_state = {
  tokens = tokens;
  pos = 0;
}

(** Get current token *)
let current_token (st: parser_state) : option located_token =
  if st.pos < length st.tokens then
    Some (index st.tokens st.pos)
  else None

(** Get current location *)
let current_location (st: parser_state) : source_loc =
  match current_token st with
  | Some t -> t.loc
  | None -> { line = 0; column = 0 }

(** Advance parser by one token *)
let advance (st: parser_state) : parser_state =
  { st with pos = st.pos + 1 }

(** Check if at end of tokens *)
let at_end (st: parser_state) : bool =
  match current_token st with
  | Some t -> (match t.value with TOK_EOF -> true | _ -> false)
  | None -> true

(** Expect a specific keyword *)
let expect_keyword (st: parser_state) (kw: keyword) (msg: string)
  : parse_result parser_state =
  match current_token st with
  | Some t ->
    (match t.value with
     | TOK_Keyword k ->
       if k = kw then ParseOk (advance st, advance st)
       else ParseError (msg, t.loc)
     | _ -> ParseError (msg, t.loc))
  | None -> ParseError (msg, current_location st)

(** Expect an identifier and return it *)
let expect_identifier (st: parser_state) (msg: string)
  : parse_result (string & parser_state) =
  match current_token st with
  | Some t ->
    (match t.value with
     | TOK_Identifier name -> ParseOk ((name, advance st), advance st)
     | _ -> ParseError (msg, t.loc))
  | None -> ParseError (msg, current_location st)

(** Expect a number and return it *)
let expect_number (st: parser_state) (msg: string)
  : parse_result (int & parser_state) =
  match current_token st with
  | Some t ->
    (match t.value with
     | TOK_Number (NumDecimal n) -> ParseOk ((n, advance st), advance st)
     | TOK_Number (NumHex n) -> ParseOk ((n, advance st), advance st)
     | TOK_Number (NumBinary n) -> ParseOk ((n, advance st), advance st)
     | _ -> ParseError (msg, t.loc))
  | None -> ParseError (msg, current_location st)

(** Expect a string and return it *)
let expect_string (st: parser_state) (msg: string)
  : parse_result (string & parser_state) =
  match current_token st with
  | Some t ->
    (match t.value with
     | TOK_String s -> ParseOk ((s, advance st), advance st)
     | _ -> ParseError (msg, t.loc))
  | None -> ParseError (msg, current_location st)

(** Check if current token is a block-ending keyword *)
let is_block_end (st: parser_state) : bool =
  match current_token st with
  | Some t ->
    (match t.value with
     | TOK_Keyword KW_EndWord -> true
     | TOK_Keyword KW_EndIf -> true
     | TOK_Keyword KW_IfElse -> true
     | TOK_Keyword KW_EndLoop -> true
     | TOK_Keyword KW_DoWhile -> true
     | TOK_Keyword KW_EndWhile -> true
     | TOK_Keyword KW_EndTimes -> true
     | TOK_Keyword KW_EndUniverse -> true
     | TOK_Keyword KW_EndWarp -> true
     | TOK_Keyword KW_EndTextWarp -> true
     | TOK_EOF -> true
     | _ -> false)
  | None -> true

(** Parse discipline *)
let parse_discipline (st: parser_state) : parse_result (discipline & parser_state) =
  match current_token st with
  | Some t ->
    (match t.value with
     | TOK_Keyword KW_Linear -> ParseOk ((Discipline_Linear, advance st), advance st)
     | TOK_Keyword KW_Affine -> ParseOk ((Discipline_Affine, advance st), advance st)
     | TOK_Keyword KW_Unrestricted -> ParseOk ((Discipline_Unrestricted, advance st), advance st)
     | _ -> ParseError ("Expected discipline (linear, affine, unrestricted)", t.loc))
  | None -> ParseError ("Expected discipline", current_location st)

(** Forward declaration for mutual recursion with fuel *)
val parse_block : parser_state -> nat -> parse_result (block & parser_state)
val parse_expr : parser_state -> nat -> parse_result (expr & parser_state)

(** Parse a single expression *)
let rec parse_expr (st: parser_state) (fuel: nat)
  : Tot (parse_result (expr & parser_state)) (decreases fuel) =
  if fuel = 0 then ParseError ("Parser fuel exhausted", current_location st)
  else
    match current_token st with
    | None -> ParseError ("Unexpected end of input", current_location st)
    | Some tok ->
      match tok.value with
      (* Literals *)
      | TOK_Number (NumDecimal n) ->
        ParseOk ((E_Literal (Lit_Int n), advance st), advance st)
      | TOK_Number (NumHex n) ->
        ParseOk ((E_Literal (Lit_Int n), advance st), advance st)
      | TOK_Number (NumBinary n) ->
        ParseOk ((E_Literal (Lit_Int n), advance st), advance st)
      | TOK_String s ->
        ParseOk ((E_Literal (Lit_String s), advance st), advance st)

      (* Primitives *)
      | TOK_Primitive prim ->
        ParseOk ((E_Primitive prim, advance st), advance st)

      (* Identifiers (word calls) *)
      | TOK_Identifier name ->
        ParseOk ((E_Call name, advance st), advance st)

      (* Skip stack effects *)
      | TOK_StackEffect _ ->
        parse_expr (advance st) (fuel - 1)

      (* if-true ... [if-else ...] end-if *)
      | TOK_Keyword KW_IfTrue ->
        let st1 = advance st in
        (match parse_block st1 (fuel - 1) with
         | ParseError (msg, loc) -> ParseError (msg, loc)
         | ParseOk ((then_block, st2), _) ->
           match current_token st2 with
           | Some t ->
             (match t.value with
              | TOK_Keyword KW_IfElse ->
                let st3 = advance st2 in
                (match parse_block st3 (fuel - 1) with
                 | ParseError (msg, loc) -> ParseError (msg, loc)
                 | ParseOk ((else_block, st4), _) ->
                   match expect_keyword st4 KW_EndIf "Expected 'end-if'" with
                   | ParseError (msg, loc) -> ParseError (msg, loc)
                   | ParseOk (st5, _) ->
                     ParseOk ((E_IfTrue (then_block, Some else_block), st5), st5))
              | TOK_Keyword KW_EndIf ->
                let st3 = advance st2 in
                ParseOk ((E_IfTrue (then_block, None), st3), st3)
              | _ -> ParseError ("Expected 'if-else' or 'end-if'", t.loc))
           | None -> ParseError ("Unexpected end of input", current_location st2))

      (* if-zero ... [if-else ...] end-if *)
      | TOK_Keyword KW_IfZero ->
        let st1 = advance st in
        (match parse_block st1 (fuel - 1) with
         | ParseError (msg, loc) -> ParseError (msg, loc)
         | ParseOk ((then_block, st2), _) ->
           match current_token st2 with
           | Some t ->
             (match t.value with
              | TOK_Keyword KW_IfElse ->
                let st3 = advance st2 in
                (match parse_block st3 (fuel - 1) with
                 | ParseError (msg, loc) -> ParseError (msg, loc)
                 | ParseOk ((else_block, st4), _) ->
                   match expect_keyword st4 KW_EndIf "Expected 'end-if'" with
                   | ParseError (msg, loc) -> ParseError (msg, loc)
                   | ParseOk (st5, _) ->
                     ParseOk ((E_IfZero (then_block, Some else_block), st5), st5))
              | TOK_Keyword KW_EndIf ->
                let st3 = advance st2 in
                ParseOk ((E_IfZero (then_block, None), st3), st3)
              | _ -> ParseError ("Expected 'if-else' or 'end-if'", t.loc))
           | None -> ParseError ("Unexpected end of input", current_location st2))

      (* begin-loop ... end-loop *)
      | TOK_Keyword KW_BeginLoop ->
        let st1 = advance st in
        (match parse_block st1 (fuel - 1) with
         | ParseError (msg, loc) -> ParseError (msg, loc)
         | ParseOk ((body, st2), _) ->
           match expect_keyword st2 KW_EndLoop "Expected 'end-loop'" with
           | ParseError (msg, loc) -> ParseError (msg, loc)
           | ParseOk (st3, _) -> ParseOk ((E_Loop body, st3), st3))

      (* begin-while ... do-while ... end-while *)
      | TOK_Keyword KW_BeginWhile ->
        let st1 = advance st in
        (match parse_block st1 (fuel - 1) with
         | ParseError (msg, loc) -> ParseError (msg, loc)
         | ParseOk ((cond_block, st2), _) ->
           match expect_keyword st2 KW_DoWhile "Expected 'do-while'" with
           | ParseError (msg, loc) -> ParseError (msg, loc)
           | ParseOk (st3, _) ->
             match parse_block st3 (fuel - 1) with
             | ParseError (msg, loc) -> ParseError (msg, loc)
             | ParseOk ((body_block, st4), _) ->
               match expect_keyword st4 KW_EndWhile "Expected 'end-while'" with
               | ParseError (msg, loc) -> ParseError (msg, loc)
               | ParseOk (st5, _) ->
                 ParseOk ((E_While (cond_block, body_block), st5), st5))

      (* do-times ... end-times *)
      | TOK_Keyword KW_DoTimes ->
        let st1 = advance st in
        (match parse_block st1 (fuel - 1) with
         | ParseError (msg, loc) -> ParseError (msg, loc)
         | ParseOk ((body, st2), _) ->
           match expect_keyword st2 KW_EndTimes "Expected 'end-times'" with
           | ParseError (msg, loc) -> ParseError (msg, loc)
           | ParseOk (st3, _) -> ParseOk ((E_Times body, st3), st3))

      (* exit-word *)
      | TOK_Keyword KW_ExitWord ->
        ParseOk ((E_Exit, advance st), advance st)

      (* transfer-to universe-name *)
      | TOK_Keyword KW_TransferTo ->
        let st1 = advance st in
        (match expect_identifier st1 "Expected universe name after 'transfer-to'" with
         | ParseError (msg, loc) -> ParseError (msg, loc)
         | ParseOk ((name, st2), _) ->
           ParseOk ((E_TransferTo name, st2), st2))

      (* size discipline create-universe as name ... end-universe name *)
      | TOK_Keyword KW_CreateUniverse ->
        (* At this point, size and discipline should already be on the stack *)
        (* But we parse them as preceding tokens *)
        ParseError ("create-universe must be preceded by size and discipline", tok.loc)

      (* release-universe name *)
      | TOK_Keyword KW_ReleaseUniverse ->
        let st1 = advance st in
        (match expect_identifier st1 "Expected universe name after 'release-universe'" with
         | ParseError (msg, loc) -> ParseError (msg, loc)
         | ParseOk ((name, st2), _) ->
           ParseOk ((E_ReleaseUniverse name, st2), st2))

      (* warp-into as name ... end-warp name *)
      | TOK_Keyword KW_WarpInto ->
        let st1 = advance st in
        (match expect_keyword st1 KW_As "Expected 'as' after 'warp-into'" with
         | ParseError (msg, loc) -> ParseError (msg, loc)
         | ParseOk (st2, _) ->
           match expect_identifier st2 "Expected warp name" with
           | ParseError (msg, loc) -> ParseError (msg, loc)
           | ParseOk ((name, st3), _) ->
             match parse_block st3 (fuel - 1) with
             | ParseError (msg, loc) -> ParseError (msg, loc)
             | ParseOk ((body, st4), _) ->
               match expect_keyword st4 KW_EndWarp "Expected 'end-warp'" with
               | ParseError (msg, loc) -> ParseError (msg, loc)
               | ParseOk (st5, _) ->
                 (* Optionally consume the warp name after end-warp *)
                 let st6 = match current_token st5 with
                   | Some t ->
                     (match t.value with
                      | TOK_Identifier n -> if n = name then advance st5 else st5
                      | _ -> st5)
                   | None -> st5
                 in
                 ParseOk ((E_WarpInto (name, body), st6), st6))

      (* warp-into-readonly as name ... end-warp name *)
      | TOK_Keyword KW_WarpIntoReadonly ->
        let st1 = advance st in
        (match expect_keyword st1 KW_As "Expected 'as' after 'warp-into-readonly'" with
         | ParseError (msg, loc) -> ParseError (msg, loc)
         | ParseOk (st2, _) ->
           match expect_identifier st2 "Expected warp name" with
           | ParseError (msg, loc) -> ParseError (msg, loc)
           | ParseOk ((name, st3), _) ->
             match parse_block st3 (fuel - 1) with
             | ParseError (msg, loc) -> ParseError (msg, loc)
             | ParseOk ((body, st4), _) ->
               match expect_keyword st4 KW_EndWarp "Expected 'end-warp'" with
               | ParseError (msg, loc) -> ParseError (msg, loc)
               | ParseOk (st5, _) ->
                 let st6 = match current_token st5 with
                   | Some t ->
                     (match t.value with
                      | TOK_Identifier n -> if n = name then advance st5 else st5
                      | _ -> st5)
                   | None -> st5
                 in
                 ParseOk ((E_WarpIntoReadonly (name, body), st6), st6))

      (* text-warp-into ... end-text-warp *)
      | TOK_Keyword KW_TextWarpInto ->
        let st1 = advance st in
        (match parse_block st1 (fuel - 1) with
         | ParseError (msg, loc) -> ParseError (msg, loc)
         | ParseOk ((body, st2), _) ->
           match expect_keyword st2 KW_EndTextWarp "Expected 'end-text-warp'" with
           | ParseError (msg, loc) -> ParseError (msg, loc)
           | ParseOk (st3, _) -> ParseOk ((E_TextWarpInto body, st3), st3))

      (* Unexpected token *)
      | _ -> ParseError ("Unexpected token", tok.loc)

(** Parse a block of expressions until a block-ending keyword *)
and parse_block (st: parser_state) (fuel: nat)
  : Tot (parse_result (block & parser_state)) (decreases fuel) =
  if fuel = 0 then ParseError ("Parser fuel exhausted", current_location st)
  else
    parse_block_aux st [] (fuel - 1)

and parse_block_aux (st: parser_state) (acc: block) (fuel: nat)
  : Tot (parse_result (block & parser_state)) (decreases fuel) =
  if fuel = 0 then ParseError ("Parser fuel exhausted", current_location st)
  else if is_block_end st then
    ParseOk ((List.Tot.rev acc, st), st)
  else
    match parse_expr st (fuel - 1) with
    | ParseError (msg, loc) -> ParseError (msg, loc)
    | ParseOk ((expr, st'), _) ->
      parse_block_aux st' (expr :: acc) (fuel - 1)

(** Parse optional stack effect *)
let parse_stack_effect (st: parser_state) : (option string & parser_state) =
  match current_token st with
  | Some t ->
    (match t.value with
     | TOK_StackEffect s -> (Some s, advance st)
     | _ -> (None, st))
  | None -> (None, st)

(** Parse a word definition *)
let parse_word_definition (st: parser_state) (fuel: nat)
  : parse_result (definition & parser_state) =
  let loc = current_location st in
  let st1 = advance st in  (* consume 'define-word' *)
  match expect_identifier st1 "Expected word name after 'define-word'" with
  | ParseError (msg, l) -> ParseError (msg, l)
  | ParseOk ((name, st2), _) ->
    let (stack_eff, st3) = parse_stack_effect st2 in
    match parse_block st3 fuel with
    | ParseError (msg, l) -> ParseError (msg, l)
    | ParseOk ((body, st4), _) ->
      match expect_keyword st4 KW_EndWord "Expected 'end-word'" with
      | ParseError (msg, l) -> ParseError (msg, l)
      | ParseOk (st5, _) ->
        let def = Def_Word {
          wd_name = name;
          wd_stack_effect = stack_eff;
          wd_body = body;
          wd_loc = loc;
        } in
        ParseOk ((def, st5), st5)

(** Parse a constant definition *)
let parse_constant_definition (st: parser_state)
  : parse_result (definition & parser_state) =
  let loc = current_location st in
  let st1 = advance st in  (* consume 'define-constant' *)
  match expect_identifier st1 "Expected constant name after 'define-constant'" with
  | ParseError (msg, l) -> ParseError (msg, l)
  | ParseOk ((name, st2), _) ->
    match expect_number st2 "Expected value after constant name" with
    | ParseError (msg, l) -> ParseError (msg, l)
    | ParseOk ((value, st3), _) ->
      let def = Def_Constant {
        cd_name = name;
        cd_value = value;
        cd_loc = loc;
      } in
      ParseOk ((def, st3), st3)

(** Parse a text definition *)
let parse_text_definition (st: parser_state)
  : parse_result (definition & parser_state) =
  let loc = current_location st in
  let st1 = advance st in  (* consume 'define-text' *)
  match expect_identifier st1 "Expected text name after 'define-text'" with
  | ParseError (msg, l) -> ParseError (msg, l)
  | ParseOk ((name, st2), _) ->
    match expect_string st2 "Expected string literal after text name" with
    | ParseError (msg, l) -> ParseError (msg, l)
    | ParseOk ((value, st3), _) ->
      let def = Def_Text {
        td_name = name;
        td_value = value;
        td_loc = loc;
      } in
      ParseOk ((def, st3), st3)

(** Parse a single top-level definition *)
let parse_definition (st: parser_state) (fuel: nat)
  : parse_result (definition & parser_state) =
  match current_token st with
  | None -> ParseError ("Unexpected end of input", current_location st)
  | Some tok ->
    match tok.value with
    | TOK_Keyword KW_DefineWord -> parse_word_definition st fuel
    | TOK_Keyword KW_DefineConstant -> parse_constant_definition st
    | TOK_Keyword KW_DefineText -> parse_text_definition st
    | _ -> ParseError ("Expected definition (define-word, define-constant, define-text)", tok.loc)

(** Parse entire program *)
let rec parse_program_aux (st: parser_state) (acc: list definition) (fuel: nat)
  : Tot (parse_result (program & parser_state)) (decreases fuel) =
  if fuel = 0 then ParseError ("Parser fuel exhausted", current_location st)
  else if at_end st then
    ParseOk (({ definitions = List.Tot.rev acc }, st), st)
  else
    match parse_definition st (fuel - 1) with
    | ParseError (msg, loc) -> ParseError (msg, loc)
    | ParseOk ((def, st'), _) ->
      parse_program_aux st' (def :: acc) (fuel - 1)

(** Parse a program from tokens *)
let parse_program (tokens: list located_token) : parse_result (program & parser_state) =
  let st = init_parser tokens in
  let fuel = length tokens + 1 in
  parse_program_aux st [] fuel

(** Parse a string (lex + parse) *)
let parse_string (s: string) : either program (string & source_loc) =
  match Space.Compiler.Lexer.tokenize s with
  | Inr (msg, loc) -> Inr (msg, loc)
  | Inl tokens ->
    match parse_program tokens with
    | ParseError (msg, loc) -> Inr (msg, loc)
    | ParseOk ((prog, _), _) -> Inl prog
