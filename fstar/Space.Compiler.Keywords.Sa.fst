module Space.Compiler.Keywords.Sa

(** संस्कृत-कुञ्चिकाः - Sanskrit keywords for Space language *)

open Space.Compiler.Token

let keywords : list (string * token) = [
  (* परिभाषाः - Definitions *)
  ("पद-परिभाषा", TOK_Keyword KW_DefineWord);
  ("पद-अन्त", TOK_Keyword KW_EndWord);
  ("स्थिर-परिभाषा", TOK_Keyword KW_DefineConstant);
  ("पाठ-परिभाषा", TOK_Keyword KW_DefineText);
  (* नियन्त्रणम् - Control flow *)
  ("यदि-सत्य", TOK_Keyword KW_IfTrue);
  ("यदि-अन्यथा", TOK_Keyword KW_IfElse);
  ("यदि-शून्य", TOK_Keyword KW_IfZero);
  ("यदि-अन्त", TOK_Keyword KW_EndIf);
  ("आवृत्ति-आरम्भ", TOK_Keyword KW_BeginLoop);
  ("आवृत्ति-अन्त", TOK_Keyword KW_EndLoop);
  ("यावत्-आरम्भ", TOK_Keyword KW_BeginWhile);
  ("यावत्-कृ", TOK_Keyword KW_DoWhile);
  ("यावत्-अन्त", TOK_Keyword KW_EndWhile);
  ("वार-कृ", TOK_Keyword KW_DoTimes);
  ("वार-अन्त", TOK_Keyword KW_EndTimes);
  ("पद-निर्गम", TOK_Keyword KW_ExitWord);
  (* विश्वाः - Universes *)
  ("विश्व-सृज्", TOK_Keyword KW_CreateUniverse);
  ("विश्व-अन्त", TOK_Keyword KW_EndUniverse);
  ("विश्व-मुच्", TOK_Keyword KW_ReleaseUniverse);
  ("इव", TOK_Keyword KW_As);
  ("रैखिक", TOK_Keyword KW_Linear);
  ("सजातीय", TOK_Keyword KW_Affine);
  ("अनियन्त्रित", TOK_Keyword KW_Unrestricted);
  ("प्रति-सञ्चर्", TOK_Keyword KW_TransferTo);
  (* तरणम् - Warps *)
  ("तरण-प्रविश्", TOK_Keyword KW_WarpInto);
  ("तरण-प्रविश्-पठन", TOK_Keyword KW_WarpIntoReadonly);
  ("तरण-अन्त", TOK_Keyword KW_EndWarp);
  ("पाठ-तरण-प्रविश्", TOK_Keyword KW_TextWarpInto);
  ("पाठ-तरण-अन्त", TOK_Keyword KW_EndTextWarp);
  (* चितिक्रियाः - Stack operations *)
  ("द्वि", TOK_Primitive PRIM_Dup);
  ("त्यज्", TOK_Primitive PRIM_Drop);
  ("विनिमय", TOK_Primitive PRIM_Swap);
  ("उपरि", TOK_Primitive PRIM_Over);
  ("भ्रम्", TOK_Primitive PRIM_Rot);
  ("छिद्", TOK_Primitive PRIM_Nip);
  ("निक्षिप्", TOK_Primitive PRIM_Tuck);
  (* गणितम् - Arithmetic *)
  ("योग", TOK_Primitive PRIM_Add);
  ("ऊन", TOK_Primitive PRIM_Subtract);
  ("गुण", TOK_Primitive PRIM_Multiply);
  ("भाग", TOK_Primitive PRIM_DivideUnsigned);
  ("भाग-चिह्नित", TOK_Primitive PRIM_DivideSigned);
  ("शेष", TOK_Primitive PRIM_Modulo);
  ("निषेध", TOK_Primitive PRIM_Negate);
  ("न्यूनतम", TOK_Primitive PRIM_Min);
  ("अधिकतम", TOK_Primitive PRIM_Max);
  (* अंशक्रियाः - Bitwise *)
  ("अंश-च", TOK_Primitive PRIM_BitAnd);
  ("अंश-वा", TOK_Primitive PRIM_BitOr);
  ("अंश-विशेष", TOK_Primitive PRIM_BitXor);
  ("अंश-न", TOK_Primitive PRIM_BitNot);
  ("वाम-सर्", TOK_Primitive PRIM_ShiftLeft);
  ("दक्षिण-सर्", TOK_Primitive PRIM_ShiftRight);
  (* तुलना - Comparison *)
  ("सम", TOK_Primitive PRIM_Equal);
  ("असम", TOK_Primitive PRIM_NotEqual);
  ("न्यून", TOK_Primitive PRIM_LessThan);
  ("अधिक", TOK_Primitive PRIM_GreaterThan);
  ("न्यून-अचिह्नित", TOK_Primitive PRIM_LessUnsigned);
  ("अधिक-अचिह्नित", TOK_Primitive PRIM_GreaterUnsigned);
  (* स्मृतिः - Memory *)
  ("अणु-आवण्ट्", TOK_Primitive PRIM_AllocateBytes);
  ("अणु-आन्", TOK_Primitive PRIM_BytesFetch);
  ("अणु-स्था", TOK_Primitive PRIM_BytesStore);
  ("अणु-दीर्घ", TOK_Primitive PRIM_BytesLength);
  ("अणु-प्रतिलिपि", TOK_Primitive PRIM_BytesCopy);
  (* ऋणम् - Borrowing *)
  ("ऋण-सूचक", TOK_Primitive PRIM_BorrowPointer);
  ("ऋण-प्रत्यर्पण", TOK_Primitive PRIM_ReturnPointer);
  ("ऋण-त्यज्", TOK_Primitive PRIM_DropPointer);
  ("ऋण-आन्", TOK_Primitive PRIM_FetchBorrowed);
  ("ऋण-स्था", TOK_Primitive PRIM_StoreBorrowed);
  ("आन्-अन्त", TOK_Primitive PRIM_FetchAndEnd);
  ("स्था-अन्त", TOK_Primitive PRIM_StoreAndEnd);
  ("ऋण-विस्थापन", TOK_Primitive PRIM_OffsetBorrowed);
  (* तरणक्रियाः - Warp operations *)
  ("तरण-आन्", TOK_Primitive PRIM_WarpFetch);
  ("तरण-स्था", TOK_Primitive PRIM_WarpStore);
  ("तरण-अग्र", TOK_Primitive PRIM_WarpAdvance);
  ("तरण-अनु", TOK_Primitive PRIM_WarpFollow);
  ("तरण-स्थान", TOK_Primitive PRIM_WarpPosition);
  ("तरण-पुनः", TOK_Primitive PRIM_WarpRestore);
  ("तरण-शून्य-किम्", TOK_Primitive PRIM_WarpNull);
  (* पाठक्रियाः - Text operations *)
  ("पाठ-सृज्", TOK_Primitive PRIM_CreateText);
  ("पाठ-अणु-दीर्घ", TOK_Primitive PRIM_TextByteLength);
  ("अक्षर-सङ्ख्या", TOK_Primitive PRIM_TextGraphemeCount);
  ("पाठ-सरल-किम्", TOK_Primitive PRIM_TextIsSimple);
  ("अक्षर-स्थान", TOK_Primitive PRIM_TextGraphemeAt);
  ("अक्षर-प्रथम", TOK_Primitive PRIM_TextGraphemeFirst);
  ("अक्षर-अन्तिम", TOK_Primitive PRIM_TextGraphemeLast);
  ("पाठ-खण्ड", TOK_Primitive PRIM_TextSlice);
  ("पाठ-सन्धि", TOK_Primitive PRIM_TextConcat);
  ("पाठ-सम", TOK_Primitive PRIM_TextEqual);
  ("पाठ-तुलना", TOK_Primitive PRIM_TextCompare);
  (* पाठतरणक्रियाः - Text warp operations *)
  ("तरण-अक्षर-अस्ति", TOK_Primitive PRIM_WarpHasGrapheme);
  ("तरण-अक्षर-वर्तमान", TOK_Primitive PRIM_WarpCurrentGrapheme);
  ("तरण-अक्षर-अग्र", TOK_Primitive PRIM_WarpNextGrapheme);
  ("तरण-अक्षर-सूची", TOK_Primitive PRIM_WarpGraphemeIndex);
  ("तरण-अक्षर-गम्", TOK_Primitive PRIM_WarpGotoGrapheme);
  (* अक्षरगुणाः - Grapheme properties *)
  ("अक्षर-अणु-दीर्घ", TOK_Primitive PRIM_GraphemeByteLength);
  ("अक्षर-ASCII-किम्", TOK_Primitive PRIM_GraphemeIsAscii);
  ("अक्षर-बिन्दु", TOK_Primitive PRIM_GraphemeCodePoints);
  (* बिन्दुप्राप्तिः - Code point access *)
  ("बिन्दु-सङ्ख्या", TOK_Primitive PRIM_TextCodePointCount);
  ("बिन्दु-स्थान", TOK_Primitive PRIM_TextCodePointAt);
  (* तन्त्रम् - System *)
  ("विराम", TOK_Primitive PRIM_HaltSystem);
  ("अणु-निर्गम", TOK_Primitive PRIM_EmitByte);
  ("अणु-प्रविश्", TOK_Primitive PRIM_ReadByte);
  ("अक्षर-निर्गम", TOK_Primitive PRIM_EmitGrapheme);
  (* मानकीकरणम् - Normalization *)
  ("मानक-NFC", TOK_Primitive PRIM_TextNormalizeNfc);
  ("मानक-NFD", TOK_Primitive PRIM_TextNormalizeNfd);
  ("मानक-NFKC", TOK_Primitive PRIM_TextNormalizeNfkc);
  ("मानक-NFKD", TOK_Primitive PRIM_TextNormalizeNfkd);
  (* वर्णपरिवर्तनम् - Case mapping *)
  ("बृहत्-वर्ण", TOK_Primitive PRIM_TextToUpper);
  ("लघु-वर्ण", TOK_Primitive PRIM_TextToLower);
  ("शीर्षक-वर्ण", TOK_Primitive PRIM_TextToTitle)
]
