module Space.Compiler.Token

(** Token types for the Space language lexer *)

open FStar.String

(** Source location for error reporting *)
type source_loc = {
  line: nat;
  column: nat;
}

(** Token with location *)
noeq type located 'a = {
  value: 'a;
  loc: source_loc;
}

(** Keyword tokens *)
type keyword =
  (* Definitions *)
  | KW_DefineWord      (* define-word *)
  | KW_EndWord         (* end-word *)
  | KW_DefineConstant  (* define-constant *)
  | KW_DefineText      (* define-text *)
  (* Control flow *)
  | KW_IfTrue          (* if-true *)
  | KW_IfElse          (* if-else *)
  | KW_IfZero          (* if-zero *)
  | KW_EndIf           (* end-if *)
  | KW_BeginLoop       (* begin-loop *)
  | KW_EndLoop         (* end-loop *)
  | KW_BeginWhile      (* begin-while *)
  | KW_DoWhile         (* do-while *)
  | KW_EndWhile        (* end-while *)
  | KW_DoTimes         (* do-times *)
  | KW_EndTimes        (* end-times *)
  | KW_ExitWord        (* exit-word *)
  (* Universes *)
  | KW_CreateUniverse  (* create-universe *)
  | KW_EndUniverse     (* end-universe *)
  | KW_ReleaseUniverse (* release-universe *)
  | KW_As              (* as *)
  | KW_Linear          (* linear *)
  | KW_Affine          (* affine *)
  | KW_Unrestricted    (* unrestricted *)
  | KW_TransferTo      (* transfer-to *)
  (* Warps *)
  | KW_WarpInto        (* warp-into *)
  | KW_WarpIntoReadonly (* warp-into-readonly *)
  | KW_EndWarp         (* end-warp *)
  (* Text warps *)
  | KW_TextWarpInto    (* text-warp-into *)
  | KW_EndTextWarp     (* end-text-warp *)

(** Primitive operations (built-in words) *)
type primitive =
  (* Stack operations *)
  | PRIM_Dup           (* dup *)
  | PRIM_Drop          (* drop *)
  | PRIM_Swap          (* swap *)
  | PRIM_Over          (* over *)
  | PRIM_Rot           (* rot *)
  | PRIM_Nip           (* nip *)
  | PRIM_Tuck          (* tuck *)
  (* Arithmetic *)
  | PRIM_Add           (* add *)
  | PRIM_Subtract      (* subtract *)
  | PRIM_Multiply      (* multiply *)
  | PRIM_DivideUnsigned (* divide-unsigned *)
  | PRIM_DivideSigned  (* divide-signed *)
  | PRIM_Modulo        (* modulo *)
  | PRIM_Negate        (* negate *)
  | PRIM_Min           (* min *)
  | PRIM_Max           (* max *)
  (* Bitwise *)
  | PRIM_BitAnd        (* bit-and *)
  | PRIM_BitOr         (* bit-or *)
  | PRIM_BitXor        (* bit-xor *)
  | PRIM_BitNot        (* bit-not *)
  | PRIM_ShiftLeft     (* shift-left *)
  | PRIM_ShiftRight    (* shift-right *)
  (* Comparison *)
  | PRIM_Equal         (* equal *)
  | PRIM_NotEqual      (* not-equal *)
  | PRIM_LessThan      (* less-than *)
  | PRIM_GreaterThan   (* greater-than *)
  | PRIM_LessUnsigned  (* less-unsigned *)
  | PRIM_GreaterUnsigned (* greater-unsigned *)
  (* Memory *)
  | PRIM_AllocateBytes (* allocate-bytes *)
  | PRIM_BytesFetch    (* bytes-fetch *)
  | PRIM_BytesStore    (* bytes-store *)
  | PRIM_BytesLength   (* bytes-length *)
  | PRIM_BytesCopy     (* bytes-copy *)
  (* Borrowing *)
  | PRIM_BorrowPointer (* borrow-pointer *)
  | PRIM_ReturnPointer (* return-pointer *)
  | PRIM_DropPointer   (* drop-pointer *)
  | PRIM_FetchBorrowed (* fetch-borrowed *)
  | PRIM_StoreBorrowed (* store-borrowed *)
  | PRIM_FetchAndEnd   (* fetch-and-end *)
  | PRIM_StoreAndEnd   (* store-and-end *)
  | PRIM_OffsetBorrowed (* offset-borrowed *)
  (* Warp operations *)
  | PRIM_WarpFetch     (* warp-fetch *)
  | PRIM_WarpStore     (* warp-store *)
  | PRIM_WarpAdvance   (* warp-advance *)
  | PRIM_WarpFollow    (* warp-follow *)
  | PRIM_WarpPosition  (* warp-position *)
  | PRIM_WarpRestore   (* warp-restore *)
  | PRIM_WarpNull      (* warp-null? *)
  (* Text operations *)
  | PRIM_CreateText    (* create-text *)
  | PRIM_TextByteLength (* text-byte-length *)
  | PRIM_TextGraphemeCount (* text-grapheme-count *)
  | PRIM_TextIsSimple  (* text-is-simple *)
  | PRIM_TextGraphemeAt (* text-grapheme-at *)
  | PRIM_TextGraphemeFirst (* text-grapheme-first *)
  | PRIM_TextGraphemeLast (* text-grapheme-last *)
  | PRIM_TextSlice     (* text-slice *)
  | PRIM_TextConcat    (* text-concat *)
  | PRIM_TextEqual     (* text-equal *)
  | PRIM_TextCompare   (* text-compare *)
  (* Text warp operations *)
  | PRIM_WarpHasGrapheme (* warp-has-grapheme *)
  | PRIM_WarpCurrentGrapheme (* warp-current-grapheme *)
  | PRIM_WarpNextGrapheme (* warp-next-grapheme *)
  | PRIM_WarpGraphemeIndex (* warp-grapheme-index *)
  | PRIM_WarpGotoGrapheme (* warp-goto-grapheme *)
  (* Grapheme properties *)
  | PRIM_GraphemeByteLength (* grapheme-byte-length *)
  | PRIM_GraphemeIsAscii (* grapheme-is-ascii *)
  | PRIM_GraphemeCodePoints (* grapheme-code-points *)
  (* Code point access *)
  | PRIM_TextCodePointCount (* text-code-point-count *)
  | PRIM_TextCodePointAt (* text-code-point-at *)
  (* System *)
  | PRIM_HaltSystem    (* halt-system *)
  | PRIM_EmitByte      (* emit-byte *)
  | PRIM_ReadByte      (* read-byte *)
  | PRIM_EmitGrapheme  (* emit-grapheme *)
  (* Full profile: UTF-16 *)
  | PRIM_TextToUtf16   (* text-to-utf16 *)
  | PRIM_Utf16ToText   (* utf16-to-text *)
  (* Full profile: Normalization *)
  | PRIM_TextNormalizeNfc  (* text-normalize-nfc *)
  | PRIM_TextNormalizeNfd  (* text-normalize-nfd *)
  | PRIM_TextNormalizeNfkc (* text-normalize-nfkc *)
  | PRIM_TextNormalizeNfkd (* text-normalize-nfkd *)
  (* Full profile: Case mapping *)
  | PRIM_TextToUpper   (* text-to-upper *)
  | PRIM_TextToLower   (* text-to-lower *)
  | PRIM_TextToTitle   (* text-to-title *)

(** Number literal types *)
type number_literal =
  | NumDecimal of int      (* 42, -17 *)
  | NumHex of nat          (* 0xFF *)
  | NumBinary of nat       (* 0b1010 *)

(** Token types *)
noeq type token =
  | TOK_Keyword of keyword
  | TOK_Primitive of primitive
  | TOK_Number of number_literal
  | TOK_String of string           (* "hello" *)
  | TOK_Identifier of string       (* user-defined word *)
  | TOK_StackEffect of string      (* ( n -- m ) *)
  | TOK_Comment of string          (* \ comment *)
  | TOK_EOF

(** Located token *)
type located_token = located token

(** Check if a string matches a keyword *)
let string_to_keyword (s: string) : option keyword =
  if s = "define-word" then Some KW_DefineWord
  else if s = "end-word" then Some KW_EndWord
  else if s = "define-constant" then Some KW_DefineConstant
  else if s = "define-text" then Some KW_DefineText
  else if s = "if-true" then Some KW_IfTrue
  else if s = "if-else" then Some KW_IfElse
  else if s = "if-zero" then Some KW_IfZero
  else if s = "end-if" then Some KW_EndIf
  else if s = "begin-loop" then Some KW_BeginLoop
  else if s = "end-loop" then Some KW_EndLoop
  else if s = "begin-while" then Some KW_BeginWhile
  else if s = "do-while" then Some KW_DoWhile
  else if s = "end-while" then Some KW_EndWhile
  else if s = "do-times" then Some KW_DoTimes
  else if s = "end-times" then Some KW_EndTimes
  else if s = "exit-word" then Some KW_ExitWord
  else if s = "create-universe" then Some KW_CreateUniverse
  else if s = "end-universe" then Some KW_EndUniverse
  else if s = "release-universe" then Some KW_ReleaseUniverse
  else if s = "as" then Some KW_As
  else if s = "linear" then Some KW_Linear
  else if s = "affine" then Some KW_Affine
  else if s = "unrestricted" then Some KW_Unrestricted
  else if s = "transfer-to" then Some KW_TransferTo
  else if s = "warp-into" then Some KW_WarpInto
  else if s = "warp-into-readonly" then Some KW_WarpIntoReadonly
  else if s = "end-warp" then Some KW_EndWarp
  else if s = "text-warp-into" then Some KW_TextWarpInto
  else if s = "end-text-warp" then Some KW_EndTextWarp
  else None

(** Check if a string matches a primitive *)
let string_to_primitive (s: string) : option primitive =
  (* Stack *)
  if s = "dup" then Some PRIM_Dup
  else if s = "drop" then Some PRIM_Drop
  else if s = "swap" then Some PRIM_Swap
  else if s = "over" then Some PRIM_Over
  else if s = "rot" then Some PRIM_Rot
  else if s = "nip" then Some PRIM_Nip
  else if s = "tuck" then Some PRIM_Tuck
  (* Arithmetic *)
  else if s = "add" then Some PRIM_Add
  else if s = "subtract" then Some PRIM_Subtract
  else if s = "multiply" then Some PRIM_Multiply
  else if s = "divide-unsigned" then Some PRIM_DivideUnsigned
  else if s = "divide-signed" then Some PRIM_DivideSigned
  else if s = "modulo" then Some PRIM_Modulo
  else if s = "negate" then Some PRIM_Negate
  else if s = "min" then Some PRIM_Min
  else if s = "max" then Some PRIM_Max
  (* Bitwise *)
  else if s = "bit-and" then Some PRIM_BitAnd
  else if s = "bit-or" then Some PRIM_BitOr
  else if s = "bit-xor" then Some PRIM_BitXor
  else if s = "bit-not" then Some PRIM_BitNot
  else if s = "shift-left" then Some PRIM_ShiftLeft
  else if s = "shift-right" then Some PRIM_ShiftRight
  (* Comparison *)
  else if s = "equal" then Some PRIM_Equal
  else if s = "not-equal" then Some PRIM_NotEqual
  else if s = "less-than" then Some PRIM_LessThan
  else if s = "greater-than" then Some PRIM_GreaterThan
  else if s = "less-unsigned" then Some PRIM_LessUnsigned
  else if s = "greater-unsigned" then Some PRIM_GreaterUnsigned
  (* Memory *)
  else if s = "allocate-bytes" then Some PRIM_AllocateBytes
  else if s = "bytes-fetch" then Some PRIM_BytesFetch
  else if s = "bytes-store" then Some PRIM_BytesStore
  else if s = "bytes-length" then Some PRIM_BytesLength
  else if s = "bytes-copy" then Some PRIM_BytesCopy
  (* Borrowing *)
  else if s = "borrow-pointer" then Some PRIM_BorrowPointer
  else if s = "return-pointer" then Some PRIM_ReturnPointer
  else if s = "drop-pointer" then Some PRIM_DropPointer
  else if s = "fetch-borrowed" then Some PRIM_FetchBorrowed
  else if s = "store-borrowed" then Some PRIM_StoreBorrowed
  else if s = "fetch-and-end" then Some PRIM_FetchAndEnd
  else if s = "store-and-end" then Some PRIM_StoreAndEnd
  else if s = "offset-borrowed" then Some PRIM_OffsetBorrowed
  (* Warp operations *)
  else if s = "warp-fetch" then Some PRIM_WarpFetch
  else if s = "warp-store" then Some PRIM_WarpStore
  else if s = "warp-advance" then Some PRIM_WarpAdvance
  else if s = "warp-follow" then Some PRIM_WarpFollow
  else if s = "warp-position" then Some PRIM_WarpPosition
  else if s = "warp-restore" then Some PRIM_WarpRestore
  else if s = "warp-null?" then Some PRIM_WarpNull
  (* Text *)
  else if s = "create-text" then Some PRIM_CreateText
  else if s = "text-byte-length" then Some PRIM_TextByteLength
  else if s = "text-grapheme-count" then Some PRIM_TextGraphemeCount
  else if s = "text-is-simple" then Some PRIM_TextIsSimple
  else if s = "text-grapheme-at" then Some PRIM_TextGraphemeAt
  else if s = "text-grapheme-first" then Some PRIM_TextGraphemeFirst
  else if s = "text-grapheme-last" then Some PRIM_TextGraphemeLast
  else if s = "text-slice" then Some PRIM_TextSlice
  else if s = "text-concat" then Some PRIM_TextConcat
  else if s = "text-equal" then Some PRIM_TextEqual
  else if s = "text-compare" then Some PRIM_TextCompare
  (* Text warp *)
  else if s = "warp-has-grapheme" then Some PRIM_WarpHasGrapheme
  else if s = "warp-current-grapheme" then Some PRIM_WarpCurrentGrapheme
  else if s = "warp-next-grapheme" then Some PRIM_WarpNextGrapheme
  else if s = "warp-grapheme-index" then Some PRIM_WarpGraphemeIndex
  else if s = "warp-goto-grapheme" then Some PRIM_WarpGotoGrapheme
  (* Grapheme properties *)
  else if s = "grapheme-byte-length" then Some PRIM_GraphemeByteLength
  else if s = "grapheme-is-ascii" then Some PRIM_GraphemeIsAscii
  else if s = "grapheme-code-points" then Some PRIM_GraphemeCodePoints
  (* Code points *)
  else if s = "text-code-point-count" then Some PRIM_TextCodePointCount
  else if s = "text-code-point-at" then Some PRIM_TextCodePointAt
  (* System *)
  else if s = "halt-system" then Some PRIM_HaltSystem
  else if s = "emit-byte" then Some PRIM_EmitByte
  else if s = "read-byte" then Some PRIM_ReadByte
  else if s = "emit-grapheme" then Some PRIM_EmitGrapheme
  (* UTF-16 *)
  else if s = "text-to-utf16" then Some PRIM_TextToUtf16
  else if s = "utf16-to-text" then Some PRIM_Utf16ToText
  (* Normalization *)
  else if s = "text-normalize-nfc" then Some PRIM_TextNormalizeNfc
  else if s = "text-normalize-nfd" then Some PRIM_TextNormalizeNfd
  else if s = "text-normalize-nfkc" then Some PRIM_TextNormalizeNfkc
  else if s = "text-normalize-nfkd" then Some PRIM_TextNormalizeNfkd
  (* Case mapping *)
  else if s = "text-to-upper" then Some PRIM_TextToUpper
  else if s = "text-to-lower" then Some PRIM_TextToLower
  else if s = "text-to-title" then Some PRIM_TextToTitle
  else None

(** Classify a word as keyword, primitive, or identifier *)
let classify_word (s: string) : token =
  match string_to_keyword s with
  | Some kw -> TOK_Keyword kw
  | None ->
    match string_to_primitive s with
    | Some prim -> TOK_Primitive prim
    | None -> TOK_Identifier s
