module Space.Compiler.Keywords.En

(** English keywords for Space language *)

open Space.Compiler.Token

let keywords : list (string * token) = [
  (* Definitions *)
  ("define-word", TOK_Keyword KW_DefineWord);
  ("end-word", TOK_Keyword KW_EndWord);
  ("define-constant", TOK_Keyword KW_DefineConstant);
  ("define-text", TOK_Keyword KW_DefineText);
  (* Control flow *)
  ("if-true", TOK_Keyword KW_IfTrue);
  ("if-else", TOK_Keyword KW_IfElse);
  ("if-zero", TOK_Keyword KW_IfZero);
  ("end-if", TOK_Keyword KW_EndIf);
  ("begin-loop", TOK_Keyword KW_BeginLoop);
  ("end-loop", TOK_Keyword KW_EndLoop);
  ("begin-while", TOK_Keyword KW_BeginWhile);
  ("do-while", TOK_Keyword KW_DoWhile);
  ("end-while", TOK_Keyword KW_EndWhile);
  ("do-times", TOK_Keyword KW_DoTimes);
  ("end-times", TOK_Keyword KW_EndTimes);
  ("exit-word", TOK_Keyword KW_ExitWord);
  (* Universes *)
  ("create-universe", TOK_Keyword KW_CreateUniverse);
  ("end-universe", TOK_Keyword KW_EndUniverse);
  ("release-universe", TOK_Keyword KW_ReleaseUniverse);
  ("as", TOK_Keyword KW_As);
  ("linear", TOK_Keyword KW_Linear);
  ("affine", TOK_Keyword KW_Affine);
  ("unrestricted", TOK_Keyword KW_Unrestricted);
  ("transfer-to", TOK_Keyword KW_TransferTo);
  (* Warps *)
  ("warp-into", TOK_Keyword KW_WarpInto);
  ("warp-into-readonly", TOK_Keyword KW_WarpIntoReadonly);
  ("end-warp", TOK_Keyword KW_EndWarp);
  ("text-warp-into", TOK_Keyword KW_TextWarpInto);
  ("end-text-warp", TOK_Keyword KW_EndTextWarp);
  (* Stack *)
  ("dup", TOK_Primitive PRIM_Dup);
  ("drop", TOK_Primitive PRIM_Drop);
  ("swap", TOK_Primitive PRIM_Swap);
  ("over", TOK_Primitive PRIM_Over);
  ("rot", TOK_Primitive PRIM_Rot);
  ("nip", TOK_Primitive PRIM_Nip);
  ("tuck", TOK_Primitive PRIM_Tuck);
  (* Arithmetic *)
  ("add", TOK_Primitive PRIM_Add);
  ("subtract", TOK_Primitive PRIM_Subtract);
  ("multiply", TOK_Primitive PRIM_Multiply);
  ("divide-unsigned", TOK_Primitive PRIM_DivideUnsigned);
  ("divide-signed", TOK_Primitive PRIM_DivideSigned);
  ("modulo", TOK_Primitive PRIM_Modulo);
  ("negate", TOK_Primitive PRIM_Negate);
  ("min", TOK_Primitive PRIM_Min);
  ("max", TOK_Primitive PRIM_Max);
  (* Bitwise *)
  ("bit-and", TOK_Primitive PRIM_BitAnd);
  ("bit-or", TOK_Primitive PRIM_BitOr);
  ("bit-xor", TOK_Primitive PRIM_BitXor);
  ("bit-not", TOK_Primitive PRIM_BitNot);
  ("shift-left", TOK_Primitive PRIM_ShiftLeft);
  ("shift-right", TOK_Primitive PRIM_ShiftRight);
  (* Comparison *)
  ("equal", TOK_Primitive PRIM_Equal);
  ("not-equal", TOK_Primitive PRIM_NotEqual);
  ("less-than", TOK_Primitive PRIM_LessThan);
  ("greater-than", TOK_Primitive PRIM_GreaterThan);
  ("less-unsigned", TOK_Primitive PRIM_LessUnsigned);
  ("greater-unsigned", TOK_Primitive PRIM_GreaterUnsigned);
  (* Memory *)
  ("allocate-bytes", TOK_Primitive PRIM_AllocateBytes);
  ("bytes-fetch", TOK_Primitive PRIM_BytesFetch);
  ("bytes-store", TOK_Primitive PRIM_BytesStore);
  ("bytes-length", TOK_Primitive PRIM_BytesLength);
  ("bytes-copy", TOK_Primitive PRIM_BytesCopy);
  (* Borrowing *)
  ("borrow-pointer", TOK_Primitive PRIM_BorrowPointer);
  ("return-pointer", TOK_Primitive PRIM_ReturnPointer);
  ("drop-pointer", TOK_Primitive PRIM_DropPointer);
  ("fetch-borrowed", TOK_Primitive PRIM_FetchBorrowed);
  ("store-borrowed", TOK_Primitive PRIM_StoreBorrowed);
  ("fetch-and-end", TOK_Primitive PRIM_FetchAndEnd);
  ("store-and-end", TOK_Primitive PRIM_StoreAndEnd);
  ("offset-borrowed", TOK_Primitive PRIM_OffsetBorrowed);
  (* Warp operations *)
  ("warp-fetch", TOK_Primitive PRIM_WarpFetch);
  ("warp-store", TOK_Primitive PRIM_WarpStore);
  ("warp-advance", TOK_Primitive PRIM_WarpAdvance);
  ("warp-follow", TOK_Primitive PRIM_WarpFollow);
  ("warp-position", TOK_Primitive PRIM_WarpPosition);
  ("warp-restore", TOK_Primitive PRIM_WarpRestore);
  ("warp-null?", TOK_Primitive PRIM_WarpNull);
  (* Text *)
  ("create-text", TOK_Primitive PRIM_CreateText);
  ("text-byte-length", TOK_Primitive PRIM_TextByteLength);
  ("text-grapheme-count", TOK_Primitive PRIM_TextGraphemeCount);
  ("text-is-simple", TOK_Primitive PRIM_TextIsSimple);
  ("text-grapheme-at", TOK_Primitive PRIM_TextGraphemeAt);
  ("text-grapheme-first", TOK_Primitive PRIM_TextGraphemeFirst);
  ("text-grapheme-last", TOK_Primitive PRIM_TextGraphemeLast);
  ("text-slice", TOK_Primitive PRIM_TextSlice);
  ("text-concat", TOK_Primitive PRIM_TextConcat);
  ("text-equal", TOK_Primitive PRIM_TextEqual);
  ("text-compare", TOK_Primitive PRIM_TextCompare);
  (* Text warp *)
  ("warp-has-grapheme", TOK_Primitive PRIM_WarpHasGrapheme);
  ("warp-current-grapheme", TOK_Primitive PRIM_WarpCurrentGrapheme);
  ("warp-next-grapheme", TOK_Primitive PRIM_WarpNextGrapheme);
  ("warp-grapheme-index", TOK_Primitive PRIM_WarpGraphemeIndex);
  ("warp-goto-grapheme", TOK_Primitive PRIM_WarpGotoGrapheme);
  (* Grapheme properties *)
  ("grapheme-byte-length", TOK_Primitive PRIM_GraphemeByteLength);
  ("grapheme-is-ascii", TOK_Primitive PRIM_GraphemeIsAscii);
  ("grapheme-code-points", TOK_Primitive PRIM_GraphemeCodePoints);
  (* Code points *)
  ("text-code-point-count", TOK_Primitive PRIM_TextCodePointCount);
  ("text-code-point-at", TOK_Primitive PRIM_TextCodePointAt);
  (* System *)
  ("halt-system", TOK_Primitive PRIM_HaltSystem);
  ("emit-byte", TOK_Primitive PRIM_EmitByte);
  ("read-byte", TOK_Primitive PRIM_ReadByte);
  ("emit-grapheme", TOK_Primitive PRIM_EmitGrapheme);
  (* Normalization *)
  ("text-normalize-nfc", TOK_Primitive PRIM_TextNormalizeNfc);
  ("text-normalize-nfd", TOK_Primitive PRIM_TextNormalizeNfd);
  ("text-normalize-nfkc", TOK_Primitive PRIM_TextNormalizeNfkc);
  ("text-normalize-nfkd", TOK_Primitive PRIM_TextNormalizeNfkd);
  (* Case *)
  ("text-to-upper", TOK_Primitive PRIM_TextToUpper);
  ("text-to-lower", TOK_Primitive PRIM_TextToLower);
  ("text-to-title", TOK_Primitive PRIM_TextToTitle)
]
