module Space.Instruction

(** Instruction set for Space interpreter *)

open FStar.UInt64
open Space.Types

(** Instruction pointer *)
type ip = UInt64.t

(** Primitive operations - 80 primitives *)
type prim_op =
  (* Stack manipulation - 8 ops *)
  | PrimDup
  | PrimDrop
  | PrimSwap
  | PrimOver
  | PrimRot
  | PrimNip
  | PrimTuck
  | PrimPick
  (* Arithmetic - 9 ops *)
  | PrimAdd
  | PrimSub
  | PrimMul
  | PrimDivU
  | PrimDivS
  | PrimMod
  | PrimNeg
  | PrimMin
  | PrimMax
  (* Bitwise - 6 ops *)
  | PrimAnd
  | PrimOr
  | PrimXor
  | PrimNot
  | PrimShl
  | PrimShr
  (* Comparison - 6 ops *)
  | PrimEq
  | PrimNeq
  | PrimLtU
  | PrimGtU
  | PrimLtS
  | PrimGtS
  (* Memory - cell level - 3 ops *)
  | PrimFetch
  | PrimStore
  | PrimAlloc
  (* Memory - byte level - 5 ops *)
  | PrimBytesAlloc
  | PrimBytesFetch
  | PrimBytesStore
  | PrimBytesLen
  | PrimBytesCopy
  (* Borrowing - 8 ops *)
  | PrimBorrowPointer
  | PrimReturnPointer
  | PrimDropPointer
  | PrimFetchBorrowed
  | PrimStoreBorrowed
  | PrimFetchAndEnd
  | PrimStoreAndEnd
  | PrimOffsetBorrowed
  (* Warp - 7 ops *)
  | PrimWarpFetch
  | PrimWarpStore
  | PrimWarpAdvance
  | PrimWarpFollow
  | PrimWarpPosition
  | PrimWarpRestore
  | PrimWarpNull
  (* Text - 11 ops *)
  | PrimCreateText
  | PrimTextByteLength
  | PrimTextGraphemeCount
  | PrimTextIsSimple
  | PrimTextGraphemeAt
  | PrimTextGraphemeFirst
  | PrimTextGraphemeLast
  | PrimTextSlice
  | PrimTextConcat
  | PrimTextEqual
  | PrimTextCompare
  (* Text warp - 5 ops *)
  | PrimTextWarpHasGrapheme
  | PrimTextWarpCurrentGrapheme
  | PrimTextWarpNextGrapheme
  | PrimTextWarpGraphemeIndex
  | PrimTextWarpGotoGrapheme
  (* Grapheme properties - 3 ops *)
  | PrimGraphemeByteLength
  | PrimGraphemeIsAscii
  | PrimGraphemeCodePoints
  (* Code point access - 2 ops *)
  | PrimTextCodePointCount
  | PrimTextCodePointAt
  (* System - 4 ops *)
  | PrimEmit
  | PrimKey
  | PrimEmitGrapheme
  | PrimHalt
  (* Normalization - 4 ops *)
  | PrimTextNormalizeNfc
  | PrimTextNormalizeNfd
  | PrimTextNormalizeNfkc
  | PrimTextNormalizeNfkd
  (* Case mapping - 3 ops *)
  | PrimTextToUpper
  | PrimTextToLower
  | PrimTextToTitle

(** Instructions *)
type instruction =
  | IPush of cell              (* Push literal value *)
  | ICall of ip                (* Call word at address *)
  | IReturn                    (* Return from word *)
  | IPrimitive of prim_op      (* Execute primitive *)
  | IBranch of ip              (* Unconditional jump *)
  | IBranchZero of ip          (* Jump if top = 0 *)
  | IBranchNonZero of ip       (* Jump if top != 0 *)
  | ICreateUniverse of universe_name * discipline  (* Create named universe *)
  | IEndUniverse of universe_name     (* End universe scope *)
  | IReleaseUniverse of universe_name (* Release affine universe *)
  | ITransferTo of universe_name      (* Transfer value to named universe *)

(** Decode primitive from opcode - 80 primitives *)
let decode_prim (op: UInt64.t) : option prim_op =
  (* Stack manipulation: 0-7 *)
  if op = 0uL then Some PrimDup
  else if op = 1uL then Some PrimDrop
  else if op = 2uL then Some PrimSwap
  else if op = 3uL then Some PrimOver
  else if op = 4uL then Some PrimRot
  else if op = 5uL then Some PrimNip
  else if op = 6uL then Some PrimTuck
  else if op = 7uL then Some PrimPick
  (* Arithmetic: 10-18 *)
  else if op = 10uL then Some PrimAdd
  else if op = 11uL then Some PrimSub
  else if op = 12uL then Some PrimMul
  else if op = 13uL then Some PrimDivU
  else if op = 14uL then Some PrimDivS
  else if op = 15uL then Some PrimMod
  else if op = 16uL then Some PrimNeg
  else if op = 17uL then Some PrimMin
  else if op = 18uL then Some PrimMax
  (* Bitwise: 20-25 *)
  else if op = 20uL then Some PrimAnd
  else if op = 21uL then Some PrimOr
  else if op = 22uL then Some PrimXor
  else if op = 23uL then Some PrimNot
  else if op = 24uL then Some PrimShl
  else if op = 25uL then Some PrimShr
  (* Comparison: 30-35 *)
  else if op = 30uL then Some PrimEq
  else if op = 31uL then Some PrimNeq
  else if op = 32uL then Some PrimLtU
  else if op = 33uL then Some PrimGtU
  else if op = 34uL then Some PrimLtS
  else if op = 35uL then Some PrimGtS
  (* Memory cell: 40-42 *)
  else if op = 40uL then Some PrimFetch
  else if op = 41uL then Some PrimStore
  else if op = 42uL then Some PrimAlloc
  (* Memory bytes: 43-47 *)
  else if op = 43uL then Some PrimBytesAlloc
  else if op = 44uL then Some PrimBytesFetch
  else if op = 45uL then Some PrimBytesStore
  else if op = 46uL then Some PrimBytesLen
  else if op = 47uL then Some PrimBytesCopy
  (* Borrowing: 50-57 *)
  else if op = 50uL then Some PrimBorrowPointer
  else if op = 51uL then Some PrimReturnPointer
  else if op = 52uL then Some PrimDropPointer
  else if op = 53uL then Some PrimFetchBorrowed
  else if op = 54uL then Some PrimStoreBorrowed
  else if op = 55uL then Some PrimFetchAndEnd
  else if op = 56uL then Some PrimStoreAndEnd
  else if op = 57uL then Some PrimOffsetBorrowed
  (* Warp: 60-66 *)
  else if op = 60uL then Some PrimWarpFetch
  else if op = 61uL then Some PrimWarpStore
  else if op = 62uL then Some PrimWarpAdvance
  else if op = 63uL then Some PrimWarpFollow
  else if op = 64uL then Some PrimWarpPosition
  else if op = 65uL then Some PrimWarpRestore
  else if op = 66uL then Some PrimWarpNull
  (* Text: 70-80 *)
  else if op = 70uL then Some PrimCreateText
  else if op = 71uL then Some PrimTextByteLength
  else if op = 72uL then Some PrimTextGraphemeCount
  else if op = 73uL then Some PrimTextIsSimple
  else if op = 74uL then Some PrimTextGraphemeAt
  else if op = 75uL then Some PrimTextGraphemeFirst
  else if op = 76uL then Some PrimTextGraphemeLast
  else if op = 77uL then Some PrimTextSlice
  else if op = 78uL then Some PrimTextConcat
  else if op = 79uL then Some PrimTextEqual
  else if op = 80uL then Some PrimTextCompare
  (* Text warp: 81-85 *)
  else if op = 81uL then Some PrimTextWarpHasGrapheme
  else if op = 82uL then Some PrimTextWarpCurrentGrapheme
  else if op = 83uL then Some PrimTextWarpNextGrapheme
  else if op = 84uL then Some PrimTextWarpGraphemeIndex
  else if op = 85uL then Some PrimTextWarpGotoGrapheme
  (* Grapheme: 86-88 *)
  else if op = 86uL then Some PrimGraphemeByteLength
  else if op = 87uL then Some PrimGraphemeIsAscii
  else if op = 88uL then Some PrimGraphemeCodePoints
  (* Codepoint: 89-90 *)
  else if op = 89uL then Some PrimTextCodePointCount
  else if op = 90uL then Some PrimTextCodePointAt
  (* I/O: 100-103 *)
  else if op = 100uL then Some PrimEmit
  else if op = 101uL then Some PrimKey
  else if op = 102uL then Some PrimEmitGrapheme
  else if op = 103uL then Some PrimHalt
  (* Normalization: 120-123 *)
  else if op = 120uL then Some PrimTextNormalizeNfc
  else if op = 121uL then Some PrimTextNormalizeNfd
  else if op = 122uL then Some PrimTextNormalizeNfkc
  else if op = 123uL then Some PrimTextNormalizeNfkd
  (* Case mapping: 130-132 *)
  else if op = 130uL then Some PrimTextToUpper
  else if op = 131uL then Some PrimTextToLower
  else if op = 132uL then Some PrimTextToTitle
  else None
