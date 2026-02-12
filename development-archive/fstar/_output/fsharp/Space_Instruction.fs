#light "off"
module Space_Instruction

type ip =
FStar_UInt64.t

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


let uu___is_PrimDup : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimDup -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimDrop : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimDrop -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimSwap : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimSwap -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimOver : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimOver -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimRot : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimRot -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimNip : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimNip -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTuck : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTuck -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimPick : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimPick -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimAdd : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimAdd -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimSub : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimSub -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimMul : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimMul -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimDivU : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimDivU -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimDivS : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimDivS -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimMod : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimMod -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimNeg : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimNeg -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimMin : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimMin -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimMax : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimMax -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimAnd : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimAnd -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimOr : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimOr -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimXor : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimXor -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimNot : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimNot -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimShl : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimShl -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimShr : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimShr -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimEq : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimEq -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimNeq : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimNeq -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimLtU : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimLtU -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimGtU : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimGtU -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimLtS : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimLtS -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimGtS : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimGtS -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimFetch : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimFetch -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimStore : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimStore -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimAlloc : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimAlloc -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimBytesAlloc : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimBytesAlloc -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimBytesFetch : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimBytesFetch -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimBytesStore : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimBytesStore -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimBytesLen : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimBytesLen -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimBytesCopy : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimBytesCopy -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimBorrowPointer : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimBorrowPointer -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimReturnPointer : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimReturnPointer -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimDropPointer : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimDropPointer -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimFetchBorrowed : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimFetchBorrowed -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimStoreBorrowed : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimStoreBorrowed -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimFetchAndEnd : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimFetchAndEnd -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimStoreAndEnd : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimStoreAndEnd -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimOffsetBorrowed : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimOffsetBorrowed -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimWarpFetch : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimWarpFetch -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimWarpStore : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimWarpStore -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimWarpAdvance : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimWarpAdvance -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimWarpFollow : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimWarpFollow -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimWarpPosition : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimWarpPosition -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimWarpRestore : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimWarpRestore -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimWarpNull : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimWarpNull -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimCreateText : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimCreateText -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextByteLength : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextByteLength -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextGraphemeCount : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextGraphemeCount -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextIsSimple : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextIsSimple -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextGraphemeAt : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextGraphemeAt -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextGraphemeFirst : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextGraphemeFirst -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextGraphemeLast : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextGraphemeLast -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextSlice : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextSlice -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextConcat : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextConcat -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextEqual : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextEqual -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextCompare : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextCompare -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextWarpHasGrapheme : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextWarpHasGrapheme -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextWarpCurrentGrapheme : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextWarpCurrentGrapheme -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextWarpNextGrapheme : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextWarpNextGrapheme -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextWarpGraphemeIndex : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextWarpGraphemeIndex -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextWarpGotoGrapheme : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextWarpGotoGrapheme -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimGraphemeByteLength : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimGraphemeByteLength -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimGraphemeIsAscii : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimGraphemeIsAscii -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimGraphemeCodePoints : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimGraphemeCodePoints -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextCodePointCount : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextCodePointCount -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextCodePointAt : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextCodePointAt -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimEmit : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimEmit -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimKey : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimKey -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimEmitGrapheme : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimEmitGrapheme -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimHalt : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimHalt -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextNormalizeNfc : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextNormalizeNfc -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextNormalizeNfd : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextNormalizeNfd -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextNormalizeNfkc : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextNormalizeNfkc -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextNormalizeNfkd : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextNormalizeNfkd -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextToUpper : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextToUpper -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextToLower : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextToLower -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_PrimTextToTitle : prim_op  ->  Prims.bool = (fun ( projectee  :  prim_op ) -> (match (projectee) with
| PrimTextToTitle -> begin
true
end
| uu___ -> begin
false
end))

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


let uu___is_IPush : instruction  ->  Prims.bool = (fun ( projectee  :  instruction ) -> (match (projectee) with
| IPush (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__IPush__item___0 : instruction  ->  Space_Types.cell = (fun ( projectee  :  instruction ) -> (match (projectee) with
| IPush (_0) -> begin
_0
end))


let uu___is_ICall : instruction  ->  Prims.bool = (fun ( projectee  :  instruction ) -> (match (projectee) with
| ICall (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__ICall__item___0 : instruction  ->  ip = (fun ( projectee  :  instruction ) -> (match (projectee) with
| ICall (_0) -> begin
_0
end))


let uu___is_IReturn : instruction  ->  Prims.bool = (fun ( projectee  :  instruction ) -> (match (projectee) with
| IReturn -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_IPrimitive : instruction  ->  Prims.bool = (fun ( projectee  :  instruction ) -> (match (projectee) with
| IPrimitive (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__IPrimitive__item___0 : instruction  ->  prim_op = (fun ( projectee  :  instruction ) -> (match (projectee) with
| IPrimitive (_0) -> begin
_0
end))


let uu___is_IBranch : instruction  ->  Prims.bool = (fun ( projectee  :  instruction ) -> (match (projectee) with
| IBranch (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__IBranch__item___0 : instruction  ->  ip = (fun ( projectee  :  instruction ) -> (match (projectee) with
| IBranch (_0) -> begin
_0
end))


let uu___is_IBranchZero : instruction  ->  Prims.bool = (fun ( projectee  :  instruction ) -> (match (projectee) with
| IBranchZero (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__IBranchZero__item___0 : instruction  ->  ip = (fun ( projectee  :  instruction ) -> (match (projectee) with
| IBranchZero (_0) -> begin
_0
end))


let uu___is_IBranchNonZero : instruction  ->  Prims.bool = (fun ( projectee  :  instruction ) -> (match (projectee) with
| IBranchNonZero (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__IBranchNonZero__item___0 : instruction  ->  ip = (fun ( projectee  :  instruction ) -> (match (projectee) with
| IBranchNonZero (_0) -> begin
_0
end))


let uu___is_ICreateUniverse : instruction  ->  Prims.bool = (fun ( projectee  :  instruction ) -> (match (projectee) with
| ICreateUniverse (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__ICreateUniverse__item___0 : instruction  ->  (Space_Types.universe_name * Space_Types.discipline) = (fun ( projectee  :  instruction ) -> (match (projectee) with
| ICreateUniverse (_0) -> begin
_0
end))


let uu___is_IEndUniverse : instruction  ->  Prims.bool = (fun ( projectee  :  instruction ) -> (match (projectee) with
| IEndUniverse (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__IEndUniverse__item___0 : instruction  ->  Space_Types.universe_name = (fun ( projectee  :  instruction ) -> (match (projectee) with
| IEndUniverse (_0) -> begin
_0
end))


let uu___is_IReleaseUniverse : instruction  ->  Prims.bool = (fun ( projectee  :  instruction ) -> (match (projectee) with
| IReleaseUniverse (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__IReleaseUniverse__item___0 : instruction  ->  Space_Types.universe_name = (fun ( projectee  :  instruction ) -> (match (projectee) with
| IReleaseUniverse (_0) -> begin
_0
end))


let uu___is_ITransferTo : instruction  ->  Prims.bool = (fun ( projectee  :  instruction ) -> (match (projectee) with
| ITransferTo (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__ITransferTo__item___0 : instruction  ->  Space_Types.universe_name = (fun ( projectee  :  instruction ) -> (match (projectee) with
| ITransferTo (_0) -> begin
_0
end))


let decode_prim : FStar_UInt64.t  ->  prim_op FStar_Pervasives_Native.option = (fun ( op  :  FStar_UInt64.t ) -> (match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "0"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimDup)
end
| uu___ -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "1"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimDrop)
end
| uu___1 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "2"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimSwap)
end
| uu___2 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "3"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimOver)
end
| uu___3 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "4"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimRot)
end
| uu___4 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "5"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimNip)
end
| uu___5 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "6"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTuck)
end
| uu___6 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "7"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimPick)
end
| uu___7 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "10"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimAdd)
end
| uu___8 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "11"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimSub)
end
| uu___9 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "12"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimMul)
end
| uu___10 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "13"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimDivU)
end
| uu___11 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "14"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimDivS)
end
| uu___12 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "15"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimMod)
end
| uu___13 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "16"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimNeg)
end
| uu___14 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "17"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimMin)
end
| uu___15 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "18"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimMax)
end
| uu___16 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "20"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimAnd)
end
| uu___17 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "21"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimOr)
end
| uu___18 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "22"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimXor)
end
| uu___19 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "23"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimNot)
end
| uu___20 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "24"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimShl)
end
| uu___21 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "25"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimShr)
end
| uu___22 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "30"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimEq)
end
| uu___23 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "31"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimNeq)
end
| uu___24 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "32"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimLtU)
end
| uu___25 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "33"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimGtU)
end
| uu___26 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "34"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimLtS)
end
| uu___27 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "35"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimGtS)
end
| uu___28 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "40"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimFetch)
end
| uu___29 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "41"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimStore)
end
| uu___30 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "42"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimAlloc)
end
| uu___31 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "43"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimBytesAlloc)
end
| uu___32 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "44"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimBytesFetch)
end
| uu___33 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "45"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimBytesStore)
end
| uu___34 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "46"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimBytesLen)
end
| uu___35 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "47"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimBytesCopy)
end
| uu___36 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "50"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimBorrowPointer)
end
| uu___37 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "51"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimReturnPointer)
end
| uu___38 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "52"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimDropPointer)
end
| uu___39 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "53"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimFetchBorrowed)
end
| uu___40 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "54"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimStoreBorrowed)
end
| uu___41 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "55"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimFetchAndEnd)
end
| uu___42 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "56"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimStoreAndEnd)
end
| uu___43 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "57"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimOffsetBorrowed)
end
| uu___44 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "60"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimWarpFetch)
end
| uu___45 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "61"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimWarpStore)
end
| uu___46 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "62"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimWarpAdvance)
end
| uu___47 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "63"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimWarpFollow)
end
| uu___48 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "64"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimWarpPosition)
end
| uu___49 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "65"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimWarpRestore)
end
| uu___50 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "66"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimWarpNull)
end
| uu___51 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "70"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimCreateText)
end
| uu___52 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "71"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextByteLength)
end
| uu___53 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "72"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextGraphemeCount)
end
| uu___54 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "73"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextIsSimple)
end
| uu___55 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "74"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextGraphemeAt)
end
| uu___56 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "75"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextGraphemeFirst)
end
| uu___57 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "76"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextGraphemeLast)
end
| uu___58 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "77"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextSlice)
end
| uu___59 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "78"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextConcat)
end
| uu___60 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "79"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextEqual)
end
| uu___61 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "80"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextCompare)
end
| uu___62 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "81"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextWarpHasGrapheme)
end
| uu___63 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "82"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextWarpCurrentGrapheme)
end
| uu___64 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "83"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextWarpNextGrapheme)
end
| uu___65 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "84"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextWarpGraphemeIndex)
end
| uu___66 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "85"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextWarpGotoGrapheme)
end
| uu___67 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "86"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimGraphemeByteLength)
end
| uu___68 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "87"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimGraphemeIsAscii)
end
| uu___69 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "88"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimGraphemeCodePoints)
end
| uu___70 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "89"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextCodePointCount)
end
| uu___71 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "90"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextCodePointAt)
end
| uu___72 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "100"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimEmit)
end
| uu___73 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "101"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimKey)
end
| uu___74 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "102"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimEmitGrapheme)
end
| uu___75 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "103"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimHalt)
end
| uu___76 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "120"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextNormalizeNfc)
end
| uu___77 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "121"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextNormalizeNfd)
end
| uu___78 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "122"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextNormalizeNfkc)
end
| uu___79 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "123"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextNormalizeNfkd)
end
| uu___80 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "130"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextToUpper)
end
| uu___81 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "131"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextToLower)
end
| uu___82 -> begin
(match ((Prims.op_Equality op (FStar_UInt64.uint_to_t ((Prims.parse_int "132"))))) with
| true -> begin
FStar_Pervasives_Native.Some (PrimTextToTitle)
end
| uu___83 -> begin
FStar_Pervasives_Native.None
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end)
end))




