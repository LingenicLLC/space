open Prims
type ip = FStar_UInt64.t
type prim_op =
  | PrimDup 
  | PrimDrop 
  | PrimSwap 
  | PrimOver 
  | PrimRot 
  | PrimNip 
  | PrimTuck 
  | PrimPick 
  | PrimAdd 
  | PrimSub 
  | PrimMul 
  | PrimDivU 
  | PrimDivS 
  | PrimMod 
  | PrimNeg 
  | PrimMin 
  | PrimMax 
  | PrimAnd 
  | PrimOr 
  | PrimXor 
  | PrimNot 
  | PrimShl 
  | PrimShr 
  | PrimEq 
  | PrimNeq 
  | PrimLtU 
  | PrimGtU 
  | PrimLtS 
  | PrimGtS 
  | PrimFetch 
  | PrimStore 
  | PrimAlloc 
  | PrimBytesAlloc 
  | PrimBytesFetch 
  | PrimBytesStore 
  | PrimBytesLen 
  | PrimBytesCopy 
  | PrimBorrowPointer 
  | PrimReturnPointer 
  | PrimDropPointer 
  | PrimFetchBorrowed 
  | PrimStoreBorrowed 
  | PrimFetchAndEnd 
  | PrimStoreAndEnd 
  | PrimOffsetBorrowed 
  | PrimWarpFetch 
  | PrimWarpStore 
  | PrimWarpAdvance 
  | PrimWarpFollow 
  | PrimWarpPosition 
  | PrimWarpRestore 
  | PrimWarpNull 
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
  | PrimTextWarpHasGrapheme 
  | PrimTextWarpCurrentGrapheme 
  | PrimTextWarpNextGrapheme 
  | PrimTextWarpGraphemeIndex 
  | PrimTextWarpGotoGrapheme 
  | PrimGraphemeByteLength 
  | PrimGraphemeIsAscii 
  | PrimGraphemeCodePoints 
  | PrimTextCodePointCount 
  | PrimTextCodePointAt 
  | PrimEmit 
  | PrimKey 
  | PrimEmitGrapheme 
  | PrimHalt 
  | PrimTextNormalizeNfc 
  | PrimTextNormalizeNfd 
  | PrimTextNormalizeNfkc 
  | PrimTextNormalizeNfkd 
  | PrimTextToUpper 
  | PrimTextToLower 
  | PrimTextToTitle 
let uu___is_PrimDup (projectee : prim_op) : Prims.bool=
  match projectee with | PrimDup -> true | uu___ -> false
let uu___is_PrimDrop (projectee : prim_op) : Prims.bool=
  match projectee with | PrimDrop -> true | uu___ -> false
let uu___is_PrimSwap (projectee : prim_op) : Prims.bool=
  match projectee with | PrimSwap -> true | uu___ -> false
let uu___is_PrimOver (projectee : prim_op) : Prims.bool=
  match projectee with | PrimOver -> true | uu___ -> false
let uu___is_PrimRot (projectee : prim_op) : Prims.bool=
  match projectee with | PrimRot -> true | uu___ -> false
let uu___is_PrimNip (projectee : prim_op) : Prims.bool=
  match projectee with | PrimNip -> true | uu___ -> false
let uu___is_PrimTuck (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTuck -> true | uu___ -> false
let uu___is_PrimPick (projectee : prim_op) : Prims.bool=
  match projectee with | PrimPick -> true | uu___ -> false
let uu___is_PrimAdd (projectee : prim_op) : Prims.bool=
  match projectee with | PrimAdd -> true | uu___ -> false
let uu___is_PrimSub (projectee : prim_op) : Prims.bool=
  match projectee with | PrimSub -> true | uu___ -> false
let uu___is_PrimMul (projectee : prim_op) : Prims.bool=
  match projectee with | PrimMul -> true | uu___ -> false
let uu___is_PrimDivU (projectee : prim_op) : Prims.bool=
  match projectee with | PrimDivU -> true | uu___ -> false
let uu___is_PrimDivS (projectee : prim_op) : Prims.bool=
  match projectee with | PrimDivS -> true | uu___ -> false
let uu___is_PrimMod (projectee : prim_op) : Prims.bool=
  match projectee with | PrimMod -> true | uu___ -> false
let uu___is_PrimNeg (projectee : prim_op) : Prims.bool=
  match projectee with | PrimNeg -> true | uu___ -> false
let uu___is_PrimMin (projectee : prim_op) : Prims.bool=
  match projectee with | PrimMin -> true | uu___ -> false
let uu___is_PrimMax (projectee : prim_op) : Prims.bool=
  match projectee with | PrimMax -> true | uu___ -> false
let uu___is_PrimAnd (projectee : prim_op) : Prims.bool=
  match projectee with | PrimAnd -> true | uu___ -> false
let uu___is_PrimOr (projectee : prim_op) : Prims.bool=
  match projectee with | PrimOr -> true | uu___ -> false
let uu___is_PrimXor (projectee : prim_op) : Prims.bool=
  match projectee with | PrimXor -> true | uu___ -> false
let uu___is_PrimNot (projectee : prim_op) : Prims.bool=
  match projectee with | PrimNot -> true | uu___ -> false
let uu___is_PrimShl (projectee : prim_op) : Prims.bool=
  match projectee with | PrimShl -> true | uu___ -> false
let uu___is_PrimShr (projectee : prim_op) : Prims.bool=
  match projectee with | PrimShr -> true | uu___ -> false
let uu___is_PrimEq (projectee : prim_op) : Prims.bool=
  match projectee with | PrimEq -> true | uu___ -> false
let uu___is_PrimNeq (projectee : prim_op) : Prims.bool=
  match projectee with | PrimNeq -> true | uu___ -> false
let uu___is_PrimLtU (projectee : prim_op) : Prims.bool=
  match projectee with | PrimLtU -> true | uu___ -> false
let uu___is_PrimGtU (projectee : prim_op) : Prims.bool=
  match projectee with | PrimGtU -> true | uu___ -> false
let uu___is_PrimLtS (projectee : prim_op) : Prims.bool=
  match projectee with | PrimLtS -> true | uu___ -> false
let uu___is_PrimGtS (projectee : prim_op) : Prims.bool=
  match projectee with | PrimGtS -> true | uu___ -> false
let uu___is_PrimFetch (projectee : prim_op) : Prims.bool=
  match projectee with | PrimFetch -> true | uu___ -> false
let uu___is_PrimStore (projectee : prim_op) : Prims.bool=
  match projectee with | PrimStore -> true | uu___ -> false
let uu___is_PrimAlloc (projectee : prim_op) : Prims.bool=
  match projectee with | PrimAlloc -> true | uu___ -> false
let uu___is_PrimBytesAlloc (projectee : prim_op) : Prims.bool=
  match projectee with | PrimBytesAlloc -> true | uu___ -> false
let uu___is_PrimBytesFetch (projectee : prim_op) : Prims.bool=
  match projectee with | PrimBytesFetch -> true | uu___ -> false
let uu___is_PrimBytesStore (projectee : prim_op) : Prims.bool=
  match projectee with | PrimBytesStore -> true | uu___ -> false
let uu___is_PrimBytesLen (projectee : prim_op) : Prims.bool=
  match projectee with | PrimBytesLen -> true | uu___ -> false
let uu___is_PrimBytesCopy (projectee : prim_op) : Prims.bool=
  match projectee with | PrimBytesCopy -> true | uu___ -> false
let uu___is_PrimBorrowPointer (projectee : prim_op) : Prims.bool=
  match projectee with | PrimBorrowPointer -> true | uu___ -> false
let uu___is_PrimReturnPointer (projectee : prim_op) : Prims.bool=
  match projectee with | PrimReturnPointer -> true | uu___ -> false
let uu___is_PrimDropPointer (projectee : prim_op) : Prims.bool=
  match projectee with | PrimDropPointer -> true | uu___ -> false
let uu___is_PrimFetchBorrowed (projectee : prim_op) : Prims.bool=
  match projectee with | PrimFetchBorrowed -> true | uu___ -> false
let uu___is_PrimStoreBorrowed (projectee : prim_op) : Prims.bool=
  match projectee with | PrimStoreBorrowed -> true | uu___ -> false
let uu___is_PrimFetchAndEnd (projectee : prim_op) : Prims.bool=
  match projectee with | PrimFetchAndEnd -> true | uu___ -> false
let uu___is_PrimStoreAndEnd (projectee : prim_op) : Prims.bool=
  match projectee with | PrimStoreAndEnd -> true | uu___ -> false
let uu___is_PrimOffsetBorrowed (projectee : prim_op) : Prims.bool=
  match projectee with | PrimOffsetBorrowed -> true | uu___ -> false
let uu___is_PrimWarpFetch (projectee : prim_op) : Prims.bool=
  match projectee with | PrimWarpFetch -> true | uu___ -> false
let uu___is_PrimWarpStore (projectee : prim_op) : Prims.bool=
  match projectee with | PrimWarpStore -> true | uu___ -> false
let uu___is_PrimWarpAdvance (projectee : prim_op) : Prims.bool=
  match projectee with | PrimWarpAdvance -> true | uu___ -> false
let uu___is_PrimWarpFollow (projectee : prim_op) : Prims.bool=
  match projectee with | PrimWarpFollow -> true | uu___ -> false
let uu___is_PrimWarpPosition (projectee : prim_op) : Prims.bool=
  match projectee with | PrimWarpPosition -> true | uu___ -> false
let uu___is_PrimWarpRestore (projectee : prim_op) : Prims.bool=
  match projectee with | PrimWarpRestore -> true | uu___ -> false
let uu___is_PrimWarpNull (projectee : prim_op) : Prims.bool=
  match projectee with | PrimWarpNull -> true | uu___ -> false
let uu___is_PrimCreateText (projectee : prim_op) : Prims.bool=
  match projectee with | PrimCreateText -> true | uu___ -> false
let uu___is_PrimTextByteLength (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextByteLength -> true | uu___ -> false
let uu___is_PrimTextGraphemeCount (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextGraphemeCount -> true | uu___ -> false
let uu___is_PrimTextIsSimple (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextIsSimple -> true | uu___ -> false
let uu___is_PrimTextGraphemeAt (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextGraphemeAt -> true | uu___ -> false
let uu___is_PrimTextGraphemeFirst (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextGraphemeFirst -> true | uu___ -> false
let uu___is_PrimTextGraphemeLast (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextGraphemeLast -> true | uu___ -> false
let uu___is_PrimTextSlice (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextSlice -> true | uu___ -> false
let uu___is_PrimTextConcat (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextConcat -> true | uu___ -> false
let uu___is_PrimTextEqual (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextEqual -> true | uu___ -> false
let uu___is_PrimTextCompare (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextCompare -> true | uu___ -> false
let uu___is_PrimTextWarpHasGrapheme (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextWarpHasGrapheme -> true | uu___ -> false
let uu___is_PrimTextWarpCurrentGrapheme (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextWarpCurrentGrapheme -> true | uu___ -> false
let uu___is_PrimTextWarpNextGrapheme (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextWarpNextGrapheme -> true | uu___ -> false
let uu___is_PrimTextWarpGraphemeIndex (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextWarpGraphemeIndex -> true | uu___ -> false
let uu___is_PrimTextWarpGotoGrapheme (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextWarpGotoGrapheme -> true | uu___ -> false
let uu___is_PrimGraphemeByteLength (projectee : prim_op) : Prims.bool=
  match projectee with | PrimGraphemeByteLength -> true | uu___ -> false
let uu___is_PrimGraphemeIsAscii (projectee : prim_op) : Prims.bool=
  match projectee with | PrimGraphemeIsAscii -> true | uu___ -> false
let uu___is_PrimGraphemeCodePoints (projectee : prim_op) : Prims.bool=
  match projectee with | PrimGraphemeCodePoints -> true | uu___ -> false
let uu___is_PrimTextCodePointCount (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextCodePointCount -> true | uu___ -> false
let uu___is_PrimTextCodePointAt (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextCodePointAt -> true | uu___ -> false
let uu___is_PrimEmit (projectee : prim_op) : Prims.bool=
  match projectee with | PrimEmit -> true | uu___ -> false
let uu___is_PrimKey (projectee : prim_op) : Prims.bool=
  match projectee with | PrimKey -> true | uu___ -> false
let uu___is_PrimEmitGrapheme (projectee : prim_op) : Prims.bool=
  match projectee with | PrimEmitGrapheme -> true | uu___ -> false
let uu___is_PrimHalt (projectee : prim_op) : Prims.bool=
  match projectee with | PrimHalt -> true | uu___ -> false
let uu___is_PrimTextNormalizeNfc (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextNormalizeNfc -> true | uu___ -> false
let uu___is_PrimTextNormalizeNfd (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextNormalizeNfd -> true | uu___ -> false
let uu___is_PrimTextNormalizeNfkc (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextNormalizeNfkc -> true | uu___ -> false
let uu___is_PrimTextNormalizeNfkd (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextNormalizeNfkd -> true | uu___ -> false
let uu___is_PrimTextToUpper (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextToUpper -> true | uu___ -> false
let uu___is_PrimTextToLower (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextToLower -> true | uu___ -> false
let uu___is_PrimTextToTitle (projectee : prim_op) : Prims.bool=
  match projectee with | PrimTextToTitle -> true | uu___ -> false
type instruction =
  | IPush of Space_Types.cell 
  | ICall of ip 
  | IReturn 
  | IPrimitive of prim_op 
  | IBranch of ip 
  | IBranchZero of ip 
  | IBranchNonZero of ip 
  | ICreateUniverse of (Space_Types.universe_name * Space_Types.discipline) 
  | IEndUniverse of Space_Types.universe_name 
  | IReleaseUniverse of Space_Types.universe_name 
  | ITransferTo of Space_Types.universe_name 
let uu___is_IPush (projectee : instruction) : Prims.bool=
  match projectee with | IPush _0 -> true | uu___ -> false
let __proj__IPush__item___0 (projectee : instruction) : Space_Types.cell=
  match projectee with | IPush _0 -> _0
let uu___is_ICall (projectee : instruction) : Prims.bool=
  match projectee with | ICall _0 -> true | uu___ -> false
let __proj__ICall__item___0 (projectee : instruction) : ip=
  match projectee with | ICall _0 -> _0
let uu___is_IReturn (projectee : instruction) : Prims.bool=
  match projectee with | IReturn -> true | uu___ -> false
let uu___is_IPrimitive (projectee : instruction) : Prims.bool=
  match projectee with | IPrimitive _0 -> true | uu___ -> false
let __proj__IPrimitive__item___0 (projectee : instruction) : prim_op=
  match projectee with | IPrimitive _0 -> _0
let uu___is_IBranch (projectee : instruction) : Prims.bool=
  match projectee with | IBranch _0 -> true | uu___ -> false
let __proj__IBranch__item___0 (projectee : instruction) : ip=
  match projectee with | IBranch _0 -> _0
let uu___is_IBranchZero (projectee : instruction) : Prims.bool=
  match projectee with | IBranchZero _0 -> true | uu___ -> false
let __proj__IBranchZero__item___0 (projectee : instruction) : ip=
  match projectee with | IBranchZero _0 -> _0
let uu___is_IBranchNonZero (projectee : instruction) : Prims.bool=
  match projectee with | IBranchNonZero _0 -> true | uu___ -> false
let __proj__IBranchNonZero__item___0 (projectee : instruction) : ip=
  match projectee with | IBranchNonZero _0 -> _0
let uu___is_ICreateUniverse (projectee : instruction) : Prims.bool=
  match projectee with | ICreateUniverse _0 -> true | uu___ -> false
let __proj__ICreateUniverse__item___0 (projectee : instruction) :
  (Space_Types.universe_name * Space_Types.discipline)=
  match projectee with | ICreateUniverse _0 -> _0
let uu___is_IEndUniverse (projectee : instruction) : Prims.bool=
  match projectee with | IEndUniverse _0 -> true | uu___ -> false
let __proj__IEndUniverse__item___0 (projectee : instruction) :
  Space_Types.universe_name= match projectee with | IEndUniverse _0 -> _0
let uu___is_IReleaseUniverse (projectee : instruction) : Prims.bool=
  match projectee with | IReleaseUniverse _0 -> true | uu___ -> false
let __proj__IReleaseUniverse__item___0 (projectee : instruction) :
  Space_Types.universe_name= match projectee with | IReleaseUniverse _0 -> _0
let uu___is_ITransferTo (projectee : instruction) : Prims.bool=
  match projectee with | ITransferTo _0 -> true | uu___ -> false
let __proj__ITransferTo__item___0 (projectee : instruction) :
  Space_Types.universe_name= match projectee with | ITransferTo _0 -> _0
let decode_prim (op : FStar_UInt64.t) :
  prim_op FStar_Pervasives_Native.option=
  if op = Stdint.Uint64.zero
  then FStar_Pervasives_Native.Some PrimDup
  else
    if op = Stdint.Uint64.one
    then FStar_Pervasives_Native.Some PrimDrop
    else
      if op = (Stdint.Uint64.of_int (2))
      then FStar_Pervasives_Native.Some PrimSwap
      else
        if op = (Stdint.Uint64.of_int (3))
        then FStar_Pervasives_Native.Some PrimOver
        else
          if op = (Stdint.Uint64.of_int (4))
          then FStar_Pervasives_Native.Some PrimRot
          else
            if op = (Stdint.Uint64.of_int (5))
            then FStar_Pervasives_Native.Some PrimNip
            else
              if op = (Stdint.Uint64.of_int (6))
              then FStar_Pervasives_Native.Some PrimTuck
              else
                if op = (Stdint.Uint64.of_int (7))
                then FStar_Pervasives_Native.Some PrimPick
                else
                  if op = (Stdint.Uint64.of_int (10))
                  then FStar_Pervasives_Native.Some PrimAdd
                  else
                    if op = (Stdint.Uint64.of_int (11))
                    then FStar_Pervasives_Native.Some PrimSub
                    else
                      if op = (Stdint.Uint64.of_int (12))
                      then FStar_Pervasives_Native.Some PrimMul
                      else
                        if op = (Stdint.Uint64.of_int (13))
                        then FStar_Pervasives_Native.Some PrimDivU
                        else
                          if op = (Stdint.Uint64.of_int (14))
                          then FStar_Pervasives_Native.Some PrimDivS
                          else
                            if op = (Stdint.Uint64.of_int (15))
                            then FStar_Pervasives_Native.Some PrimMod
                            else
                              if op = (Stdint.Uint64.of_int (16))
                              then FStar_Pervasives_Native.Some PrimNeg
                              else
                                if op = (Stdint.Uint64.of_int (17))
                                then FStar_Pervasives_Native.Some PrimMin
                                else
                                  if op = (Stdint.Uint64.of_int (18))
                                  then FStar_Pervasives_Native.Some PrimMax
                                  else
                                    if op = (Stdint.Uint64.of_int (20))
                                    then FStar_Pervasives_Native.Some PrimAnd
                                    else
                                      if op = (Stdint.Uint64.of_int (21))
                                      then
                                        FStar_Pervasives_Native.Some PrimOr
                                      else
                                        if op = (Stdint.Uint64.of_int (22))
                                        then
                                          FStar_Pervasives_Native.Some
                                            PrimXor
                                        else
                                          if op = (Stdint.Uint64.of_int (23))
                                          then
                                            FStar_Pervasives_Native.Some
                                              PrimNot
                                          else
                                            if
                                              op =
                                                (Stdint.Uint64.of_int (24))
                                            then
                                              FStar_Pervasives_Native.Some
                                                PrimShl
                                            else
                                              if
                                                op =
                                                  (Stdint.Uint64.of_int (25))
                                              then
                                                FStar_Pervasives_Native.Some
                                                  PrimShr
                                              else
                                                if
                                                  op =
                                                    (Stdint.Uint64.of_int (30))
                                                then
                                                  FStar_Pervasives_Native.Some
                                                    PrimEq
                                                else
                                                  if
                                                    op =
                                                      (Stdint.Uint64.of_int (31))
                                                  then
                                                    FStar_Pervasives_Native.Some
                                                      PrimNeq
                                                  else
                                                    if
                                                      op =
                                                        (Stdint.Uint64.of_int (32))
                                                    then
                                                      FStar_Pervasives_Native.Some
                                                        PrimLtU
                                                    else
                                                      if
                                                        op =
                                                          (Stdint.Uint64.of_int (33))
                                                      then
                                                        FStar_Pervasives_Native.Some
                                                          PrimGtU
                                                      else
                                                        if
                                                          op =
                                                            (Stdint.Uint64.of_int (34))
                                                        then
                                                          FStar_Pervasives_Native.Some
                                                            PrimLtS
                                                        else
                                                          if
                                                            op =
                                                              (Stdint.Uint64.of_int (35))
                                                          then
                                                            FStar_Pervasives_Native.Some
                                                              PrimGtS
                                                          else
                                                            if
                                                              op =
                                                                (Stdint.Uint64.of_int (40))
                                                            then
                                                              FStar_Pervasives_Native.Some
                                                                PrimFetch
                                                            else
                                                              if
                                                                op =
                                                                  (Stdint.Uint64.of_int (41))
                                                              then
                                                                FStar_Pervasives_Native.Some
                                                                  PrimStore
                                                              else
                                                                if
                                                                  op =
                                                                    (Stdint.Uint64.of_int (42))
                                                                then
                                                                  FStar_Pervasives_Native.Some
                                                                    PrimAlloc
                                                                else
                                                                  if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (43))
                                                                  then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimBytesAlloc
                                                                  else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (44))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimBytesFetch
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (45))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimBytesStore
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (46))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimBytesLen
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (47))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimBytesCopy
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (50))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimBorrowPointer
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (51))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimReturnPointer
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (52))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimDropPointer
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (53))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimFetchBorrowed
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (54))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimStoreBorrowed
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (55))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimFetchAndEnd
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (56))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimStoreAndEnd
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (57))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimOffsetBorrowed
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (60))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimWarpFetch
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (61))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimWarpStore
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (62))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimWarpAdvance
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (63))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimWarpFollow
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (64))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimWarpPosition
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (65))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimWarpRestore
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (66))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimWarpNull
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (70))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimCreateText
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (71))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextByteLength
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (72))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextGraphemeCount
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (73))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextIsSimple
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (74))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextGraphemeAt
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (75))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextGraphemeFirst
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (76))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextGraphemeLast
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (77))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextSlice
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (78))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextConcat
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (79))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextEqual
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (80))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextCompare
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (81))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextWarpHasGrapheme
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (82))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextWarpCurrentGrapheme
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (83))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextWarpNextGrapheme
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (84))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextWarpGraphemeIndex
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (85))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextWarpGotoGrapheme
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (86))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimGraphemeByteLength
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (87))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimGraphemeIsAscii
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (88))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimGraphemeCodePoints
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (89))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextCodePointCount
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (90))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextCodePointAt
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (100))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimEmit
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (101))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimKey
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (102))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimEmitGrapheme
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (103))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimHalt
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (120))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextNormalizeNfc
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (121))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextNormalizeNfd
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (122))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextNormalizeNfkc
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (123))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextNormalizeNfkd
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (130))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextToUpper
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (131))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextToLower
                                                                    else
                                                                    if
                                                                    op =
                                                                    (Stdint.Uint64.of_int (132))
                                                                    then
                                                                    FStar_Pervasives_Native.Some
                                                                    PrimTextToTitle
                                                                    else
                                                                    FStar_Pervasives_Native.None
