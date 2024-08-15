import gleam/option.{type Option}
import gleam/result
import internal/finger_tree.{type FingerTree}
import internal/structure/common.{between}
import internal/structure/numbers.{
  type F32, type F64, type I32, type I64, type U32, type V128Value,
}

pub fn unwrap_lane_16(val: LaneIDX16) {
  val.val
}

pub fn lane_16(val: Int) {
  case val |> between(#(0, 15)) {
    True -> Ok(LaneIDX16(val))
    _ -> Error("Lane index out of range")
  }
}

pub opaque type LaneIDX16 {
  LaneIDX16(val: Int)
}

pub fn unwrap_lane_2(val: LaneIDX2) {
  val.val
}

pub fn lane_2(val: Int) {
  case val |> between(#(0, 1)) {
    True -> Ok(LaneIDX2(val))
    _ -> Error("Lane index out of range")
  }
}

pub opaque type LaneIDX2 {
  LaneIDX2(val: Int)
}

pub fn unwrap_lane_4(val: LaneIDX4) {
  val.val
}

pub fn lane_4(val: Int) {
  case val |> between(#(0, 3)) {
    True -> Ok(LaneIDX4(val))
    _ -> Error("Lane index out of range")
  }
}

pub opaque type LaneIDX4 {
  LaneIDX4(val: Int)
}

pub fn unwrap_lane_8(val: LaneIDX8) {
  val.val
}

pub fn lane_8(val: Int) {
  case val |> between(#(0, 7)) {
    True -> Ok(LaneIDX8(val))
    _ -> Error("Lane index out of range")
  }
}

pub opaque type LaneIDX8 {
  LaneIDX8(val: Int)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#number-types
pub type NumType {
  I32NumType
  I64NumType
  F32NumType
  F64NumType
}

pub const bool = I32NumType

/// pleasae see: https://webassembly.github.io/gc/core/syntax/types.html#vector-types
pub type VecType {
  VecType
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#heap-types
/// For validation and execution, please see: https://webassembly.github.io/gc/core/valid/conventions.html#types
/// Please note that Abstract Heap Types are a seperate type, and are combined with heap types
/// in this convention for the sake of clarity, and ease of encoding and decoding
pub type HeapType {
  FuncHeapType
  NoFuncHeapType
  ExternHeapType
  NoExternHeapType
  AnyHeapType
  EqHeapType
  I31HeapType
  StructHeapType
  ArrayHeapType
  NoneHeapType
  ConcreteHeapType(idx: TypeIDX)
  BotHeapType
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#heap-types
pub type AbstractHeapType {
  FuncAbstractHeapType
  NoAbstractFuncHeapType
  ExternAbstractHeapType
  NoExternAbstractHeapType
  AnyAbstractHeapType
  EqAbstractHeapType
  I31AbstractHeapType
  StructAbstractHeapType
  ArrayAbstractHeapType
  NoneAbstractHeapType
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#reference-types
pub type RefType {
  HeapTypeRefType(ht: HeapType, null: Bool)

  /// Shorthand for (ref null Any)
  AnyRefType
  /// Shorthand for (ref null Eq)
  EqRefType
  /// Shorthand for (ref null I31)
  I31RefType
  /// Shorthand for (ref null Struct)
  StructRefType
  /// Shorthand for (ref null Array)
  ArrayRefType
  /// Shorthand for (ref null Func)
  FuncRefType
  /// Shorthand for (ref null Extern)
  ExternRefType
  /// Shorthand for (ref null None)
  NoneRefType
  /// Shorthand for (ref null NoFunc)
  NoFuncRefType
  /// Shorthand for (ref null NoExtern)
  NoExternRefType
}

pub fn ref_type_is_nullable(rt: RefType) {
  case rt {
    HeapTypeRefType(_, null) -> null
    _ -> True
  }
}

pub fn ref_type_get_heap_type(rt: RefType) {
  case rt {
    HeapTypeRefType(ht, _) -> ht
    AnyRefType -> AnyHeapType
    EqRefType -> EqHeapType
    I31RefType -> I31HeapType
    StructRefType -> StructHeapType
    ArrayRefType -> ArrayHeapType
    FuncRefType -> FuncHeapType
    ExternRefType -> ExternHeapType
    NoneRefType -> NoneHeapType
    NoFuncRefType -> NoFuncHeapType
    NoExternRefType -> NoExternHeapType
  }
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#value-types
pub type ValType {
  V128ValType
  I32ValType
  I64ValType
  F32ValType
  F64ValType
  RefTypeValType(rt: RefType)
}

/// Please see: https://webassembly.github.io/gc/core/binary/instructions.html#control-instructions
pub type BlockType {
  VoidBlockType
  ValTypeBlockType(vt: ValType)
  FuncTypeBlockType(idx: TypeIDX)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#result-types
pub type ResultType {
  ResultType(parameters: FingerTree(ValType), result: FingerTree(ValType))
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#function-types
pub type FuncType {
  FuncType(rt: ResultType)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#aggregate-types
pub type StructType {
  StructType(ft: FingerTree(FieldType))
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#aggregate-types
pub type ArrayType {
  ArrayType(ft: FieldType)
}

/// Please See: https://webassembly.github.io/gc/core/syntax/types.html#aggregate-types
pub type FieldType {
  FieldType(st: StorageType, mut: Mut)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#aggregate-types
/// Please note that storage types can be PackedTypes, and they are a seperate type,
/// however, they are merged with StorageType for convenience
pub type StorageType {
  ValTypeStorageType(vt: ValType)
  I8StorageType
  I16StorageType
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#aggregate-types
pub type PackedType {
  I8
  I16
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#aux-unpacktype
pub fn unpack_storage_type(st: StorageType) -> ValType {
  case st {
    ValTypeStorageType(vt) -> vt
    _ -> I32ValType
  }
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#composite-types
pub type CompositeType {
  FuncCompositeType(ft: FuncType)
  StructCompositeType(st: StructType)
  ArrayCompositeType(at: ArrayType)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#recursive-types
pub type RecType {
  RecType(st: FingerTree(SubType))
}

pub type TypeIDX {
  /// Module Type Index
  TypeIDX(id: U32)
}

pub type FieldIDX {
  /// Module Field Index
  FieldIDX(id: U32)
}

pub type FuncIDX {
  /// Module Func Index
  FuncIDX(id: U32)
}

pub type TableIDX {
  /// Module Table Index
  TableIDX(id: U32)
}

pub type LabelIDX {
  /// Label Index
  LabelIDX(id: U32)
}

pub type ElemIDX {
  /// Module Elem Index
  ElemIDX(id: U32)
}

pub type MemIDX {
  /// Module Mem Index
  MemIDX(id: U32)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#recursive-types
pub type SubType {
  SubType(final: Bool, t: FingerTree(TypeIDX), ct: CompositeType)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#limits
pub type Limits {
  Limits(min: U32, max: Option(U32))
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#memory-types
pub type MemType {
  MemType(limits: Limits)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#table-types
pub type TableType {
  TableType(t: RefType, limits: Limits)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#global-types
pub type GlobalType {
  GlobalType(vt: ValType, mut: Mut)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#global-types
pub type Mut {
  Const
  Var
}

/// Please see: https://webassembly.github.io/gc/core/syntax/types.html#external-types
pub type ExternType {
  FuncExternType(dt: DefType)
  TableExternType(tt: TableType)
  MemExternType(mt: MemType)
  GlobalExternType(gt: GlobalType)
}

/// https://webassembly.github.io/gc/core/syntax/types.html#id5
pub fn get_extern_func_types(
  et: FingerTree(ExternType),
) -> FingerTree(ExternType) {
  et
  |> finger_tree.filter(fn(et) {
    case et {
      FuncExternType(_) -> True
      _ -> False
    }
  })
}

/// https://webassembly.github.io/gc/core/syntax/types.html#id5
pub fn get_extern_table_types(
  et: FingerTree(ExternType),
) -> FingerTree(ExternType) {
  et
  |> finger_tree.filter(fn(et) {
    case et {
      TableExternType(_) -> True
      _ -> False
    }
  })
}

/// https://webassembly.github.io/gc/core/syntax/types.html#id5
pub fn get_extern_memory_types(
  et: FingerTree(ExternType),
) -> FingerTree(ExternType) {
  et
  |> finger_tree.filter(fn(et) {
    case et {
      MemExternType(_) -> True
      _ -> False
    }
  })
}

/// https://webassembly.github.io/gc/core/syntax/types.html#id5
pub fn get_extern_global_types(
  et: FingerTree(ExternType),
) -> FingerTree(ExternType) {
  et
  |> finger_tree.filter(fn(et) {
    case et {
      GlobalExternType(_) -> True
      _ -> False
    }
  })
}

/// Please see: https://webassembly.github.io/gc/core/valid/types.html#instruction-types
pub type InstructionType {
  InstructionType(t1: FingerTree(ValType), t2: FingerTree(ValType))
}

/// Please see: https://webassembly.github.io/gc/core/valid/conventions.html#defined-types
pub type DefType {
  DefType(rt: RecType, idx: Int)
}

/// Please see: https://webassembly.github.io/gc/core/valid/conventions.html#local-types 
pub type LocalType {
  LocalType(initialized: Bool, t: ValType)
}

/// Please see: todo
pub type DataIDX {
  DataIDX(id: U32)
}

/// Please see: todo
pub type LocalIDX {
  LocalIDX(id: U32)
}

/// Please see: todo
pub type GlobalIDX {
  GlobalIDX(id: U32)
}

/// Please see: todo
pub type MemArg {
  MemArg(offset: U32, align: U32)
}

pub type Instruction {
  I64Const(val: I64)
  I32Const(val: I32)
  F64Const(val: F64)
  F32Const(val: F32)
  I64Popcnt
  I64Ctz
  I64Clz
  I32Popcnt
  I32Ctz
  I32Clz
  F64Nearest
  F64Trunc
  F64Floor
  F64Ceil
  F64Sqrt
  F64Neg
  F64Abs
  F32Nearest
  F32Trunc
  F32Floor
  F32Ceil
  F32Sqrt
  F32Neg
  F32Abs
  I64Rotr
  I64Rotl
  I64ShrS
  I64ShrU
  I64Shl
  I64Xor
  I64Or
  I64And
  I64RemS
  I64RemU
  I64DivS
  I64DivU
  I64Mul
  I64Sub
  I64Add
  I32Rotr
  I32Rotl
  I32ShrS
  I32ShrU
  I32Shl
  I32Xor
  I32Or
  I32And
  I32RemS
  I32RemU
  I32DivS
  I32DivU
  I32Mul
  I32Sub
  I32Add
  F64Copysign
  F64Max
  F64Min
  F64Div
  F64Mul
  F64Sub
  F64Add
  F32Copysign
  F32Max
  F32Min
  F32Div
  F32Mul
  F32Sub
  F32Add
  I64Eqz
  I32Eqz
  I64GeS
  I64GeU
  I64LeS
  I64LeU
  I64GtS
  I64GtU
  I64LtS
  I64LtU
  I64Ne
  I64Eq
  I32GeS
  I32GeU
  I32LeS
  I32LeU
  I32GtS
  I32GtU
  I32LtS
  I32LtU
  I32Ne
  V128AndNot
  I32Eq
  F64Ge
  F64Le
  F64Gt
  F64Lt
  F64Ne
  F64Eq
  F32Ge
  F32Le
  F32Gt
  F32Lt
  F32Ne
  F32Eq
  I32Extend16S
  I32Extend8S
  I64Extend32S
  I64Extend16S
  I64Extend8S
  I32WrapI64
  I64ExtendI32S
  I64ExtendI32U
  I64TruncF64S
  I64TruncF64U
  I64TruncF32S
  I64TruncF32U
  I32TruncF64S
  I32TruncF64U
  I32TruncF32S
  I32TruncF32U
  I64TruncSatF64S
  I64TruncSatF64U
  I64TruncSatF32S
  I64TruncSatF32U
  I32TruncSatF64S
  I32TruncSatF64U
  I32TruncSatF32S
  I32TruncSatF32U
  F32DemoteF64
  F64PromoteF32
  F64ConvertI64S
  F64ConvertI64U
  F64ConvertI32S
  F64ConvertI32U
  F32ConvertI64S
  F32ConvertI64U
  F32ConvertI32S
  F32ConvertI32U
  F64ReinterpretI64
  F32ReinterpretI32
  I64ReinterpretF64
  I32ReinterpretF32
  V128Const(val: V128Value)
  V128Not
  V128Xor
  V128Or
  V128And
  V128Bitselect
  V128AnyTrue
  I8x16Shuffle(
    i0: LaneIDX16,
    i1: LaneIDX16,
    i2: LaneIDX16,
    i3: LaneIDX16,
    i4: LaneIDX16,
    i5: LaneIDX16,
    i6: LaneIDX16,
    i7: LaneIDX16,
    i8: LaneIDX16,
    i9: LaneIDX16,
    i10: LaneIDX16,
    i11: LaneIDX16,
    i12: LaneIDX16,
    i13: LaneIDX16,
    i14: LaneIDX16,
    i15: LaneIDX16,
  )
  I8x16Swizzle
  I32x4Splat
  I16x8Splat
  I8x16Splat
  I64x2Splat
  F32x4Splat
  F64x2Splat
  I8x16ExtractLaneS(idx: LaneIDX16)
  I16x8ExtractLaneS(idx: LaneIDX8)
  I32x4ExtractLane(idx: LaneIDX4)
  I64x2ExtractLane(idx: LaneIDX2)
  I8x16ExtractLaneU(idx: LaneIDX16)
  I16x8ExtractLaneU(idx: LaneIDX8)
  F32x4ExtractLane(idx: LaneIDX4)
  F64x2ExtractLane(idx: LaneIDX2)
  I8x16ReplaceLane(idx: LaneIDX16)
  I16x8ReplaceLane(idx: LaneIDX8)
  I32x4ReplaceLane(idx: LaneIDX4)
  I64x2ReplaceLane(idx: LaneIDX2)
  F32x4ReplaceLane(idx: LaneIDX4)
  F64x2ReplaceLane(idx: LaneIDX2)
  I8x16GeS
  I8x16GeU
  I8x16LeS
  I8x16LeU
  I8x16GtS
  I8x16GtU
  I8x16LtS
  I8x16LtU
  I8x16Ne
  I8x16Eq
  I16x8GeS
  I16x8GeU
  I16x8LeS
  I16x8LeU
  I16x8GtS
  I16x8GtU
  I16x8LtS
  I16x8LtU
  I16x8Ne
  I16x8Eq
  I32x4GeS
  I32x4GeU
  I32x4LeS
  I32x4LeU
  I32x4GtS
  I32x4GtU
  I32x4LtS
  I32x4LtU
  I32x4Ne
  I32x4Eq
  I64x2GeS
  I64x2LeS
  I64x2GtS
  I64x2LtS
  I64x2Ne
  I64x2Eq
  F64x2Ge
  F64x2Le
  F64x2Gt
  F64x2Lt
  F64x2Ne
  F64x2Eq
  F32x4Ge
  F32x4Le
  F32x4Gt
  F32x4Lt
  F32x4Ne
  F32x4Eq
  I64x2Neg
  I64x2Abs
  I32x4Neg
  I32x4Abs
  I16x8Neg
  I16x8Abs
  I8x16Neg
  I8x16Abs
  I8x16Popcnt
  I16x8Q15mulrSatS
  I32x4DotI16x8S
  F64x2Nearest
  F64x2Trunc
  F64x2Floor
  F64x2Ceil
  F64x2Sqrt
  F64x2Neg
  F64x2Abs
  F32x4Nearest
  F32x4Trunc
  F32x4Floor
  F32x4Ceil
  F32x4Sqrt
  F32x4Neg
  F32x4Abs
  I64x2AllTrue
  I32x4AllTrue
  I16x8AllTrue
  I8x16AllTrue
  I64x2Bitmask
  I32x4Bitmask
  I16x8Bitmask
  I8x16Bitmask
  I8x16NarrowI16x8S
  I8x16NarrowI16x8U
  I16x8NarrowI32x4S
  I16x8NarrowI32x4U
  I16x8ExtendHighI8x16S
  I16x8ExtendHighI8x16U
  I16x8ExtendLowI8x16S
  I16x8ExtendLowI8x16U
  I32x4ExtendHighI16x8S
  I32x4ExtendHighI16x8U
  I32x4ExtendLowI16x8S
  I32x4ExtendLowI16x8U
  I64x2ExtendHighI32x4S
  I64x2ExtendHighI32x4U
  I64x2ExtendLowI32x4S
  I64x2ExtendLowI32x4U
  I64x2ShrS
  I64x2ShrU
  I64x2Shl
  I32x4ShrS
  I32x4ShrU
  I32x4Shl
  I16x8ShrS
  I16x8ShrU
  I16x8Shl
  I8x16ShrS
  I8x16ShrU
  I8x16Shl
  I64x2Sub
  I64x2Add
  I32x4Sub
  I32x4Add
  I16x8Sub
  I16x8Add
  I8x16Sub
  I8x16Add
  I8x16MaxS
  I8x16MaxU
  I8x16MinS
  I8x16MinU
  I16x8MaxS
  I16x8MaxU
  I16x8MinS
  I16x8MinU
  I32x4MaxS
  I32x4MaxU
  I32x4MinS
  I32x4MinU
  I8x16SubSatS
  I8x16SubSatU
  I8x16AddSatS
  I8x16AddSatU
  I16x8SubSatS
  I16x8SubSatU
  I16x8AddSatS
  I16x8AddSatU
  I16x8Mul
  I32x4Mul
  I8x16AvgrU
  I64x2Mul
  I16x8AvgrU
  I16x8ExtmulHighI8x16S
  I16x8ExtmulHighI8x16U
  I16x8ExtmulLowI8x16S
  I16x8ExtmulLowI8x16U
  I32x4ExtmulHighI16x8S
  I32x4ExtmulHighI16x8U
  I32x4ExtmulLowI16x8S
  I32x4ExtmulLowI16x8U
  I64x2ExtmulHighI32x4S
  I64x2ExtmulHighI32x4U
  I64x2ExtmulLowI32x4S
  I64x2ExtmulLowI32x4U
  I16x8ExtaddPairwiseI8x16S
  I16x8ExtaddPairwiseI8x16U
  I32x4ExtaddPairwiseI16x8S
  I32x4ExtaddPairwiseI16x8U
  F64x2Pmax
  F64x2Pmin
  F64x2Max
  F64x2Min
  F64x2Div
  F64x2Mul
  F64x2Sub
  F64x2Add
  F32x4Pmax
  F32x4Pmin
  F32x4Max
  F32x4Min
  F32x4Div
  F32x4Mul
  F32x4Sub
  F32x4Add
  I32x4TruncSatF32x4S
  I32x4TruncSatF32x4U
  I32x4TruncSatF64x2SZero
  I32x4TruncSatF64x2UZero
  F32x4ConvertI32x4S
  F32x4ConvertI32x4U
  F32x4DemoteF64x2Zero
  F64x2ConvertLowI32x4S
  F64x2ConvertLowI32x4U
  F64x2PromoteLowF32x4
  RefNull(ht: HeapType)
  RefFunc(idx: FuncIDX)
  RefIsNull
  RefAsNonNull
  RefEq
  RefTest(rt: RefType)
  RefCast(rt: RefType)
  StructNew(idx: TypeIDX)
  StructNewDefault(idx: TypeIDX)
  StructGet(tidx: TypeIDX, fidx: FieldIDX)
  StructGetS(tidx: TypeIDX, fidx: FieldIDX)
  StructGetU(tidx: TypeIDX, fidx: FieldIDX)
  StructSet(tidx: TypeIDX, fidx: FieldIDX)
  ArrayNew(idx: TypeIDX)
  ArrayNewDefault(idx: TypeIDX)
  ArrayNewData(idx: TypeIDX, data: DataIDX)
  ArrayNewElem(idx: TypeIDX, elem: ElemIDX)
  ArrayNewFixed(idx: TypeIDX, size: U32)
  ArrayGet(idx: TypeIDX)
  ArrayGetS(idx: TypeIDX)
  ArrayGetU(idx: TypeIDX)
  ArraySet(idx: TypeIDX)
  ArrayLen
  ArrayFill(idx: TypeIDX)
  ArrayCopy(idx1: TypeIDX, idx2: TypeIDX)
  ArrayInitData(idx: TypeIDX, data: DataIDX)
  ArrayInitElem(idx: TypeIDX, elem: ElemIDX)
  RefI31
  I31GetS
  I31GetU
  AnyConvertExtern
  ExternConvertAny
  Drop
  Select
  SelectT(vt: FingerTree(ValType))
  LocalGet(idx: LocalIDX)
  LocalSet(idx: LocalIDX)
  LocalTee(idx: LocalIDX)
  GlobalGet(idx: GlobalIDX)
  GlobalSet(idx: GlobalIDX)
  TableGet(idx: TableIDX)
  TableSet(idx: TableIDX)
  TableSize(idx: TableIDX)
  TableGrow(idx: TableIDX)
  TableFill(idx: TableIDX)
  TableCopy(idx1: TableIDX, idx2: TableIDX)
  TableInit(elem: ElemIDX, idx: TableIDX)
  ElemDrop(idx: ElemIDX)
  I64Load(arg: MemArg)
  I32Load(arg: MemArg)
  F64Load(arg: MemArg)
  F32Load(arg: MemArg)
  V128Load(arg: MemArg)
  I64Store(arg: MemArg)
  I64Store8(arg: MemArg)
  I64Store16(arg: MemArg)
  I64Store32(arg: MemArg)
  I32Store(arg: MemArg)
  I32Store8(arg: MemArg)
  I32Store16(arg: MemArg)
  F64Store(arg: MemArg)
  F32Store(arg: MemArg)
  V128Store(arg: MemArg)
  I64Load16S(arg: MemArg)
  I64Load16U(arg: MemArg)
  I64Load8S(arg: MemArg)
  I64Load8U(arg: MemArg)
  I32Load16S(arg: MemArg)
  I32Load16U(arg: MemArg)
  I32Load8S(arg: MemArg)
  I32Load8U(arg: MemArg)
  I64Load32S(arg: MemArg)
  I64Load32U(arg: MemArg)
  V128Load8x8S(arg: MemArg)
  V128Load8x8U(arg: MemArg)
  V128Load16x4S(arg: MemArg)
  V128Load16x4U(arg: MemArg)
  V128Load32x2S(arg: MemArg)
  V128Load32x2U(arg: MemArg)
  V128Load32Zero(arg: MemArg)
  V128Load64Zero(arg: MemArg)
  V128Load64Splat(arg: MemArg)
  V128Load32Splat(arg: MemArg)
  V128Load16Splat(arg: MemArg)
  V128Load8Splat(arg: MemArg)
  V128Load8Lane(arg: MemArg, idx: LaneIDX16)
  V128Load16Lane(arg: MemArg, idx: LaneIDX8)
  V128Load32Lane(arg: MemArg, idx: LaneIDX4)
  V128Load64Lane(arg: MemArg, idx: LaneIDX2)
  V128Store8Lane(arg: MemArg, idx: LaneIDX16)
  V128Store16Lane(arg: MemArg, idx: LaneIDX8)
  V128Store32Lane(arg: MemArg, idx: LaneIDX4)
  V128Store64Lane(arg: MemArg, idx: LaneIDX2)
  MemorySize
  MemoryGrow
  MemoryFill
  MemoryCopy
  MemoryInit(idx: DataIDX)
  DataDrop(idx: DataIDX)
  Nop
  Unreachable
  Block(bt: BlockType, instructions: FingerTree(Instruction))
  Loop(bt: BlockType, instructions: FingerTree(Instruction))
  If(
    bt: BlockType,
    instructions: FingerTree(Instruction),
    else_instructions: Option(FingerTree(Instruction)),
  )
  Br(label: LabelIDX)
  BrIf(label: LabelIDX)
  BrTable(labels: FingerTree(LabelIDX), default: LabelIDX)
  BrOnNull(label: LabelIDX)
  BrOnNonNull(label: LabelIDX)
  BrOnCast(label: LabelIDX, rt1: RefType, rt2: RefType)
  BrOnCastFail(label: LabelIDX, rt1: RefType, rt2: RefType)
  Return
  Call(func_idx: FuncIDX)
  CallRef(type_idx: TypeIDX)
  CallIndirect(type_idx: TypeIDX, table_idx: TableIDX)
  ReturnCall(func_idx: FuncIDX)
  ReturnCallRef(type_idx: TypeIDX)
  ReturnCallIndirect(type_idx: TypeIDX, table_idx: TableIDX)
  End
  Else
}

pub fn get_result_type(instruction: Instruction) -> Result(ResultType, Nil) {
  case instruction {
    End | Else -> Error(Nil)

    I64Const(_) ->
      Ok(ResultType(finger_tree.new(), finger_tree.from_list([I64ValType])))
    I32Const(_) | ArrayLen | I31GetS | I31GetU ->
      Ok(ResultType(finger_tree.new(), finger_tree.from_list([I32ValType])))
    F64Const(_) ->
      Ok(ResultType(finger_tree.new(), finger_tree.from_list([F64ValType])))
    F32Const(_) ->
      Ok(ResultType(finger_tree.new(), finger_tree.from_list([F32ValType])))
    I64Popcnt | I64Ctz | I64Clz | I64Extend32S | I64Extend16S | I64Extend8S ->
      Ok(ResultType(
        finger_tree.from_list([I64ValType]),
        finger_tree.from_list([I64ValType]),
      ))
    I32Popcnt
    | I32Ctz
    | I32Clz
    | I32Eqz
    | I32Extend16S
    | I32Extend8S
    | I32Load(_)
    | I32Load16S(_)
    | I32Load16U(_)
    | I32Load8S(_)
    | I32Load8U(_)
    | MemoryGrow ->
      Ok(ResultType(
        finger_tree.from_list([I32ValType]),
        finger_tree.from_list([I32ValType]),
      ))
    F64Nearest | F64Trunc | F64Floor | F64Ceil | F64Sqrt | F64Neg | F64Abs ->
      Ok(ResultType(
        finger_tree.from_list([F64ValType]),
        finger_tree.from_list([F64ValType]),
      ))
    F32Nearest | F32Trunc | F32Floor | F32Ceil | F32Sqrt | F32Neg | F32Abs ->
      Ok(ResultType(
        finger_tree.from_list([F32ValType]),
        finger_tree.from_list([F32ValType]),
      ))
    I64Rotr
    | I64Rotl
    | I64ShrS
    | I64ShrU
    | I64Shl
    | I64Xor
    | I64Or
    | I64And
    | I64RemS
    | I64RemU
    | I64DivS
    | I64DivU
    | I64Mul
    | I64Sub
    | I64Add ->
      Ok(ResultType(
        finger_tree.from_list([I64ValType, I64ValType]),
        finger_tree.from_list([I64ValType]),
      ))
    I32Rotr
    | I32Rotl
    | I32ShrS
    | I32ShrU
    | I32Shl
    | I32Xor
    | I32Or
    | I32And
    | I32RemS
    | I32RemU
    | I32DivS
    | I32DivU
    | I32Mul
    | I32Sub
    | I32Add
    | I32GeS
    | I32GeU
    | I32LeS
    | I32LeU
    | I32GtS
    | I32GtU
    | I32LtS
    | I32LtU
    | I32Ne
    | I32Eq ->
      Ok(ResultType(
        finger_tree.from_list([I32ValType, I32ValType]),
        finger_tree.from_list([I32ValType]),
      ))
    F64Copysign | F64Max | F64Min | F64Div | F64Mul | F64Sub | F64Add ->
      Ok(ResultType(
        finger_tree.from_list([F64ValType, F64ValType]),
        finger_tree.from_list([F64ValType]),
      ))
    F32Copysign | F32Max | F32Min | F32Div | F32Mul | F32Sub | F32Add ->
      Ok(ResultType(
        finger_tree.from_list([F32ValType, F32ValType]),
        finger_tree.from_list([F32ValType]),
      ))
    I64Eqz | I32WrapI64 ->
      Ok(ResultType(
        finger_tree.from_list([I64ValType]),
        finger_tree.from_list([I32ValType]),
      ))
    I64GeS
    | I64GeU
    | I64LeS
    | I64LeU
    | I64GtS
    | I64GtU
    | I64LtS
    | I64LtU
    | I64Ne
    | I64Eq ->
      Ok(ResultType(
        finger_tree.from_list([I64ValType, I64ValType]),
        finger_tree.from_list([I32ValType]),
      ))

    F64Ge | F64Le | F64Gt | F64Lt | F64Ne | F64Eq ->
      Ok(ResultType(
        finger_tree.from_list([F64ValType, F64ValType]),
        finger_tree.from_list([I32ValType]),
      ))
    F32Ge | F32Le | F32Gt | F32Lt | F32Ne | F32Eq ->
      Ok(ResultType(
        finger_tree.from_list([F32ValType, F32ValType]),
        finger_tree.from_list([I32ValType]),
      ))
    I64ExtendI32S
    | I64ExtendI32U
    | I64Load(_)
    | I64Load16S(_)
    | I64Load16U(_)
    | I64Load8S(_)
    | I64Load8U(_)
    | I64Load32S(_)
    | I64Load32U(_) ->
      Ok(ResultType(
        finger_tree.from_list([I32ValType]),
        finger_tree.from_list([I64ValType]),
      ))
    I64TruncF64S
    | I64TruncF64U
    | I64TruncSatF64S
    | I64TruncSatF64U
    | I64ReinterpretF64 ->
      Ok(ResultType(
        finger_tree.from_list([F64ValType]),
        finger_tree.from_list([I64ValType]),
      ))
    I64TruncF32S | I64TruncF32U | I64TruncSatF32S | I64TruncSatF32U ->
      Ok(ResultType(
        finger_tree.from_list([F32ValType]),
        finger_tree.from_list([I64ValType]),
      ))
    I32TruncF64S | I32TruncF64U | I32TruncSatF64S | I32TruncSatF64U ->
      Ok(ResultType(
        finger_tree.from_list([F64ValType]),
        finger_tree.from_list([I32ValType]),
      ))
    I32TruncF32S
    | I32TruncF32U
    | I32TruncSatF32S
    | I32TruncSatF32U
    | I32ReinterpretF32 ->
      Ok(ResultType(
        finger_tree.from_list([F32ValType]),
        finger_tree.from_list([I32ValType]),
      ))
    F32DemoteF64 ->
      Ok(ResultType(
        finger_tree.from_list([F32ValType]),
        finger_tree.from_list([F64ValType]),
      ))
    F64PromoteF32 ->
      Ok(ResultType(
        finger_tree.from_list([F64ValType]),
        finger_tree.from_list([F32ValType]),
      ))
    F64ConvertI64S | F64ConvertI64U | F64ReinterpretI64 ->
      Ok(ResultType(
        finger_tree.from_list([I64ValType]),
        finger_tree.from_list([F64ValType]),
      ))
    F64ConvertI32S | F64ConvertI32U | F64Load(_) ->
      Ok(ResultType(
        finger_tree.from_list([I32ValType]),
        finger_tree.from_list([F64ValType]),
      ))
    F32ConvertI64S | F32ConvertI64U ->
      Ok(ResultType(
        finger_tree.from_list([I64ValType]),
        finger_tree.from_list([F32ValType]),
      ))
    F32ConvertI32S | F32ConvertI32U | F32ReinterpretI32 | F32Load(_) ->
      Ok(ResultType(
        finger_tree.from_list([I32ValType]),
        finger_tree.from_list([F32ValType]),
      ))
    V128Const(_) ->
      Ok(ResultType(finger_tree.new(), finger_tree.from_list([V128ValType])))
    V128Not
    | I64x2Neg
    | I64x2Abs
    | I32x4Neg
    | I32x4Abs
    | I16x8Neg
    | I16x8Abs
    | I8x16Neg
    | I8x16Abs
    | I8x16Popcnt
    | F64x2Nearest
    | F64x2Trunc
    | F64x2Floor
    | F64x2Ceil
    | F64x2Sqrt
    | F64x2Neg
    | F64x2Abs
    | F32x4Nearest
    | F32x4Trunc
    | F32x4Floor
    | F32x4Ceil
    | F32x4Sqrt
    | F32x4Neg
    | F32x4Abs
    | I8x16NarrowI16x8S
    | I8x16NarrowI16x8U
    | I16x8NarrowI32x4S
    | I16x8NarrowI32x4U
    | I32x4TruncSatF32x4S
    | I32x4TruncSatF32x4U
    | I32x4TruncSatF64x2SZero
    | I32x4TruncSatF64x2UZero
    | F32x4ConvertI32x4S
    | F32x4ConvertI32x4U
    | F32x4DemoteF64x2Zero
    | F64x2ConvertLowI32x4S
    | F64x2ConvertLowI32x4U
    | F64x2PromoteLowF32x4 ->
      Ok(ResultType(
        finger_tree.from_list([V128ValType]),
        finger_tree.from_list([V128ValType]),
      ))
    V128Xor
    | V128AndNot
    | V128Or
    | V128And
    | I8x16Shuffle(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    | I8x16Swizzle
    | I8x16GeS
    | I8x16GeU
    | I8x16LeS
    | I8x16LeU
    | I8x16GtS
    | I8x16GtU
    | I8x16LtS
    | I8x16LtU
    | I8x16Ne
    | I8x16Eq
    | I16x8GeS
    | I16x8GeU
    | I16x8LeS
    | I16x8LeU
    | I16x8GtS
    | I16x8GtU
    | I16x8LtS
    | I16x8LtU
    | I16x8Ne
    | I16x8Eq
    | I32x4GeS
    | I32x4GeU
    | I32x4LeS
    | I32x4LeU
    | I32x4GtS
    | I32x4GtU
    | I32x4LtS
    | I32x4LtU
    | I32x4Ne
    | I32x4Eq
    | I64x2GeS
    | I64x2LeS
    | I64x2GtS
    | I64x2LtS
    | I64x2Ne
    | I64x2Eq
    | F64x2Ge
    | F64x2Le
    | F64x2Gt
    | F64x2Lt
    | F64x2Ne
    | F64x2Eq
    | F32x4Ge
    | F32x4Le
    | F32x4Gt
    | F32x4Lt
    | F32x4Ne
    | F32x4Eq
    | I16x8Q15mulrSatS
    | I32x4DotI16x8S
    | I16x8ExtendHighI8x16S
    | I16x8ExtendHighI8x16U
    | I16x8ExtendLowI8x16S
    | I16x8ExtendLowI8x16U
    | I32x4ExtendHighI16x8S
    | I32x4ExtendHighI16x8U
    | I32x4ExtendLowI16x8S
    | I32x4ExtendLowI16x8U
    | I64x2ExtendHighI32x4S
    | I64x2ExtendHighI32x4U
    | I64x2ExtendLowI32x4S
    | I64x2ExtendLowI32x4U
    | I64x2Sub
    | I64x2Add
    | I32x4Sub
    | I32x4Add
    | I16x8Sub
    | I16x8Add
    | I8x16Sub
    | I8x16Add
    | I8x16MaxS
    | I8x16MaxU
    | I8x16MinS
    | I8x16MinU
    | I16x8MaxS
    | I16x8MaxU
    | I16x8MinS
    | I16x8MinU
    | I32x4MaxS
    | I32x4MaxU
    | I32x4MinS
    | I32x4MinU
    | I8x16SubSatS
    | I8x16SubSatU
    | I8x16AddSatS
    | I8x16AddSatU
    | I16x8SubSatS
    | I16x8SubSatU
    | I16x8AddSatS
    | I16x8AddSatU
    | I16x8Mul
    | I32x4Mul
    | I8x16AvgrU
    | I16x8AvgrU
    | I16x8ExtmulHighI8x16S
    | I16x8ExtmulHighI8x16U
    | I16x8ExtmulLowI8x16S
    | I16x8ExtmulLowI8x16U
    | I32x4ExtmulHighI16x8S
    | I32x4ExtmulHighI16x8U
    | I32x4ExtmulLowI16x8S
    | I32x4ExtmulLowI16x8U
    | I64x2ExtmulHighI32x4S
    | I64x2ExtmulHighI32x4U
    | I64x2ExtmulLowI32x4S
    | I64x2ExtmulLowI32x4U
    | I16x8ExtaddPairwiseI8x16S
    | I16x8ExtaddPairwiseI8x16U
    | I32x4ExtaddPairwiseI16x8S
    | I32x4ExtaddPairwiseI16x8U
    | F64x2Pmax
    | F64x2Pmin
    | F64x2Max
    | F64x2Min
    | F64x2Div
    | F64x2Mul
    | F64x2Sub
    | F64x2Add
    | F32x4Pmax
    | F32x4Pmin
    | F32x4Max
    | F32x4Min
    | F32x4Div
    | F32x4Mul
    | F32x4Sub
    | F32x4Add
    | I64x2Mul ->
      Ok(ResultType(
        finger_tree.from_list([V128ValType, V128ValType]),
        finger_tree.from_list([V128ValType]),
      ))
    V128Bitselect ->
      Ok(ResultType(
        finger_tree.from_list([V128ValType, V128ValType, V128ValType]),
        finger_tree.from_list([V128ValType]),
      ))
    V128AnyTrue
    | I8x16ExtractLaneS(_)
    | I16x8ExtractLaneS(_)
    | I8x16ExtractLaneU(_)
    | I16x8ExtractLaneU(_)
    | I64x2AllTrue
    | I32x4AllTrue
    | I16x8AllTrue
    | I8x16AllTrue
    | I64x2Bitmask
    | I32x4Bitmask
    | I16x8Bitmask
    | I8x16Bitmask ->
      Ok(ResultType(
        finger_tree.from_list([V128ValType]),
        finger_tree.from_list([I32ValType]),
      ))
    I32x4Splat
    | I16x8Splat
    | I8x16Splat
    | V128Load(_)
    | V128Load8x8S(_)
    | V128Load8x8U(_)
    | V128Load16x4S(_)
    | V128Load16x4U(_)
    | V128Load32x2S(_)
    | V128Load32x2U(_)
    | V128Load32Zero(_)
    | V128Load64Zero(_)
    | V128Load64Splat(_)
    | V128Load32Splat(_)
    | V128Load16Splat(_)
    | V128Load8Splat(_) ->
      Ok(ResultType(
        finger_tree.from_list([I32ValType]),
        finger_tree.from_list([V128ValType]),
      ))
    I64x2Splat ->
      Ok(ResultType(
        finger_tree.from_list([I64ValType]),
        finger_tree.from_list([V128ValType]),
      ))
    F32x4Splat ->
      Ok(ResultType(
        finger_tree.from_list([F32ValType]),
        finger_tree.from_list([V128ValType]),
      ))
    F64x2Splat ->
      Ok(ResultType(
        finger_tree.from_list([F64ValType]),
        finger_tree.from_list([V128ValType]),
      ))
    I64x2ExtractLane(_) ->
      Ok(ResultType(
        finger_tree.from_list([V128ValType]),
        finger_tree.from_list([I64ValType]),
      ))
    I32x4ExtractLane(_) ->
      Ok(ResultType(
        finger_tree.from_list([V128ValType]),
        finger_tree.from_list([I32ValType]),
      ))
    F32x4ExtractLane(_) ->
      Ok(ResultType(
        finger_tree.from_list([V128ValType]),
        finger_tree.from_list([F32ValType]),
      ))
    F64x2ExtractLane(_) ->
      Ok(ResultType(
        finger_tree.from_list([V128ValType]),
        finger_tree.from_list([F64ValType]),
      ))
    I8x16ReplaceLane(_)
    | I16x8ReplaceLane(_)
    | I32x4ReplaceLane(_)
    | I64x2ShrS
    | I64x2ShrU
    | I64x2Shl
    | I32x4ShrS
    | I32x4ShrU
    | I32x4Shl
    | I16x8ShrS
    | I16x8ShrU
    | I16x8Shl
    | I8x16ShrS
    | I8x16ShrU
    | I8x16Shl ->
      Ok(ResultType(
        finger_tree.from_list([V128ValType, I32ValType]),
        finger_tree.from_list([V128ValType]),
      ))
    I64x2ReplaceLane(_) ->
      Ok(ResultType(
        finger_tree.from_list([V128ValType, I64ValType]),
        finger_tree.from_list([V128ValType]),
      ))
    F32x4ReplaceLane(_) ->
      Ok(ResultType(
        finger_tree.from_list([V128ValType, F32ValType]),
        finger_tree.from_list([V128ValType]),
      ))
    F64x2ReplaceLane(_) ->
      Ok(ResultType(
        finger_tree.from_list([V128ValType, F64ValType]),
        finger_tree.from_list([V128ValType]),
      ))
    Nop -> Ok(ResultType(finger_tree.new(), finger_tree.new()))
    RefFunc(_)
    | ArrayNewFixed(_, _)
    | RefNull(_)
    | RefIsNull
    | RefAsNonNull
    | RefEq
    | RefTest(_)
    | RefCast(_)
    | StructGet(_, _)
    | StructGetS(_, _)
    | StructGetU(_, _)
    | StructSet(_, _)
    | ArrayNew(_)
    | ArrayNewDefault(_)
    | ArrayNewData(_, _)
    | ArrayNewElem(_, _)
    | ArrayGet(_)
    | ArrayGetS(_)
    | ArrayGetU(_)
    | ArraySet(_)
    | ArrayFill(_)
    | ArrayCopy(_, _)
    | ArrayInitData(_, _)
    | ArrayInitElem(_, _)
    | Drop
    | Select
    | SelectT(_)
    | LocalGet(_)
    | LocalSet(_)
    | LocalTee(_)
    | GlobalGet(_)
    | GlobalSet(_)
    | TableGet(_)
    | TableSet(_)
    | TableSize(_)
    | TableGrow(_)
    | TableFill(_)
    | TableCopy(_, _)
    | TableInit(_, _)
    | ElemDrop(_)
    | Unreachable
    | Block(_, _)
    | Loop(_, _)
    | If(_, _, _)
    | Br(_)
    | BrIf(_)
    | BrTable(_, _)
    | BrOnNull(_)
    | BrOnNonNull(_)
    | BrOnCast(_, _, _)
    | BrOnCastFail(_, _, _)
    | Return
    | Call(_)
    | CallRef(_)
    | CallIndirect(_, _)
    | ReturnCall(_)
    | ReturnCallRef(_)
    | ReturnCallIndirect(_, _)
    | StructNew(_)
    | StructNewDefault(_) -> todo

    RefI31 ->
      Ok(ResultType(
        finger_tree.new(),
        finger_tree.from_list([RefTypeValType(I31RefType)]),
      ))
    AnyConvertExtern ->
      Ok(ResultType(
        finger_tree.from_list([RefTypeValType(ExternRefType)]),
        finger_tree.from_list([RefTypeValType(AnyRefType)]),
      ))
    ExternConvertAny ->
      Ok(ResultType(
        finger_tree.from_list([RefTypeValType(AnyRefType)]),
        finger_tree.from_list([RefTypeValType(ExternRefType)]),
      ))
    I64Store(_) | I64Store16(_) | I64Store8(_) | I64Store32(_) ->
      Ok(ResultType(
        finger_tree.from_list([I32ValType, I64ValType]),
        finger_tree.new(),
      ))
    I32Store(_) | I32Store16(_) | I32Store8(_) ->
      Ok(ResultType(
        finger_tree.from_list([I32ValType, I32ValType]),
        finger_tree.new(),
      ))
    F64Store(_) ->
      Ok(ResultType(
        finger_tree.from_list([I32ValType, F64ValType]),
        finger_tree.new(),
      ))
    F32Store(_) ->
      Ok(ResultType(
        finger_tree.from_list([I32ValType, F32ValType]),
        finger_tree.new(),
      ))
    V128Store(_)
    | V128Store8Lane(_, _)
    | V128Store16Lane(_, _)
    | V128Store32Lane(_, _)
    | V128Store64Lane(_, _) ->
      Ok(ResultType(
        finger_tree.from_list([I32ValType, V128ValType]),
        finger_tree.new(),
      ))
    V128Load8Lane(_, _)
    | V128Load16Lane(_, _)
    | V128Load32Lane(_, _)
    | V128Load64Lane(_, _) ->
      Ok(ResultType(
        finger_tree.from_list([I32ValType, V128ValType]),
        finger_tree.from_list([V128ValType]),
      ))
    MemorySize ->
      Ok(ResultType(finger_tree.new(), finger_tree.from_list([I32ValType])))
    MemoryFill | MemoryCopy | MemoryInit(_) ->
      Ok(ResultType(
        finger_tree.from_list([I32ValType, I32ValType, I32ValType]),
        finger_tree.new(),
      ))
    DataDrop(_) -> Ok(ResultType(finger_tree.new(), finger_tree.new()))
  }
}

pub type Expr {
  Expr(insts: FingerTree(Instruction))
}

pub fn def_type_expand(dt: DefType) {
  let DefType(RecType(st), idx) = dt
  use st <- result.try(st |> finger_tree.drop(idx))
  use #(st, _) <- result.map(st |> finger_tree.shift)
  st.ct
}

pub type Import {
  FuncImport(mod: String, name: String, type_idx: TypeIDX)
  TableImport(mod: String, name: String, type_idx: TableType)
  MemImport(mod: String, name: String, type_idx: MemType)
  GlobalImport(mod: String, name: String, type_idx: GlobalType)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#functions
pub type Func {
  Func(type_: U32, locals: FingerTree(Local), body: Expr)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#functions
pub type Local {
  Local(type_: ValType)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#tables
pub type Table {
  Table(type_: TableType, init: Expr)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#memories
pub type Mem {
  Mem(type_: MemType)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#globals
pub type Global {
  Global(type_: GlobalType, init: Expr)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#element-segments
pub type Elem {
  Elem(type_: RefType, init: FingerTree(Expr), mode: ElemMode)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#element-segments
pub type ElemMode {
  PassiveElemMode
  ActiveElemMode(table: U32, offset: Expr)
  DeclarativeElemMode
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#start-function
pub type Start {
  Start(func: U32)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#exports
pub type Export {
  FuncExport(name: String, func_idx: FuncIDX)
  TableExport(name: String, table_idx: TableIDX)
  MemExport(name: String, mem_idx: MemIDX)
  GlobalExport(name: String, global_idx: GlobalIDX)
}

pub fn export_is_func(export: Export) -> Bool {
  case export {
    FuncExport(_, _) -> True
    _ -> False
  }
}

/// Conventions: https://webassembly.github.io/gc/core/syntax/modules.html#exports
pub fn export_is_table(export: Export) -> Bool {
  case export {
    TableExport(_, _) -> True
    _ -> False
  }
}

pub fn export_is_mem(export: Export) -> Bool {
  case export {
    MemExport(_, _) -> True
    _ -> False
  }
}

pub fn export_is_global(export: Export) -> Bool {
  case export {
    GlobalExport(_, _) -> True
    _ -> False
  }
}

pub fn funcs(exports: FingerTree(Export)) -> FingerTree(Export) {
  exports |> finger_tree.filter(export_is_func)
}

pub fn tables(exports: FingerTree(Export)) -> FingerTree(Export) {
  exports |> finger_tree.filter(export_is_table)
}

pub fn mems(exports: FingerTree(Export)) -> FingerTree(Export) {
  exports |> finger_tree.filter(export_is_mem)
}

pub fn globals(exports: FingerTree(Export)) -> FingerTree(Export) {
  exports |> finger_tree.filter(export_is_global)
}

pub type Code {
  Code(size: U32, locals: FingerTree(FingerTree(Local)), body: Expr)
}

pub type DataMode {
  Active
  Passive
}

pub type Data {
  Data(
    mode: DataMode,
    mem: Option(MemIDX),
    offset: Option(Expr),
    init: BitArray,
  )
}

pub opaque type Module {
  Module(
    has_type: Bool,
    has_import: Bool,
    has_func: Bool,
    has_table: Bool,
    has_mem: Bool,
    has_global: Bool,
    has_export: Bool,
    has_start: Bool,
    has_elem: Bool,
    has_code: Bool,
    has_data: Bool,
    has_data_count: Bool,
    sections: FingerTree(Section),
  )
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#modules
pub type Section {
  CustomSection(name: String, bytes: BitArray)
  TypeSection(types: FingerTree(RecType))
  ImportSection(imports: FingerTree(Import))
  FuncSection(funcs: FingerTree(TypeIDX))
  TableSection(tables: FingerTree(Table))
  MemSection(mems: FingerTree(MemType))
  GlobalSection(globals: FingerTree(Global))
  ExportSection(exports: FingerTree(Export))
  StartSection(start: Option(FuncIDX))
  ElemSection(elems: FingerTree(Elem))
  CodeSection(codes: FingerTree(Code))
  DataSection(data: FingerTree(Data))
  DataCountSection(count: U32)
}

pub fn module_new() {
  Module(
    has_type: False,
    has_import: False,
    has_func: False,
    has_table: False,
    has_mem: False,
    has_global: False,
    has_export: False,
    has_start: False,
    has_elem: False,
    has_code: False,
    has_data: False,
    has_data_count: False,
    sections: finger_tree.new(),
  )
}

pub fn custom_section(mod: Module, name: String, bytes: BitArray) {
  Module(
    ..mod,
    sections: mod.sections |> finger_tree.push(CustomSection(name, bytes)),
  )
}

pub fn type_section(mod: Module, types: FingerTree(RecType)) {
  case mod {
    Module(has_type: True, ..) -> Error("Section already exists")
    mod ->
      Ok(
        Module(
          ..mod,
          sections: mod.sections |> finger_tree.push(TypeSection(types)),
        ),
      )
  }
}

pub fn import_section(mod: Module, imports: FingerTree(Import)) {
  case mod {
    Module(has_import: True, ..) -> Error("Section already exists")
    mod ->
      Ok(
        Module(
          ..mod,
          sections: mod.sections |> finger_tree.push(ImportSection(imports)),
        ),
      )
  }
}

pub fn func_section(mod: Module, funcs: FingerTree(TypeIDX)) {
  case mod {
    Module(has_func: True, ..) -> Error("Section already exists")
    mod ->
      Ok(
        Module(
          ..mod,
          sections: mod.sections |> finger_tree.push(FuncSection(funcs)),
        ),
      )
  }
}

pub fn table_section(mod: Module, tables: FingerTree(Table)) {
  case mod {
    Module(has_table: True, ..) -> Error("Section already exists")
    mod ->
      Ok(
        Module(
          ..mod,
          sections: mod.sections |> finger_tree.push(TableSection(tables)),
        ),
      )
  }
}

pub fn mem_section(mod: Module, mems: FingerTree(MemType)) {
  case mod {
    Module(has_mem: True, ..) -> Error("Section already exists")
    mod ->
      Ok(
        Module(
          ..mod,
          sections: mod.sections |> finger_tree.push(MemSection(mems)),
        ),
      )
  }
}

pub fn global_section(mod: Module, globals: FingerTree(Global)) {
  case mod {
    Module(has_global: True, ..) -> Error("Section already exists")
    mod ->
      Ok(
        Module(
          ..mod,
          sections: mod.sections |> finger_tree.push(GlobalSection(globals)),
        ),
      )
  }
}

pub fn export_section(mod: Module, exports: FingerTree(Export)) {
  case mod {
    Module(has_export: True, ..) -> Error("Section already exists")
    mod ->
      Ok(
        Module(
          ..mod,
          sections: mod.sections |> finger_tree.push(ExportSection(exports)),
        ),
      )
  }
}

pub fn start_section(mod: Module, start: Option(FuncIDX)) {
  case mod {
    Module(has_start: True, ..) -> Error("Section already exists")
    mod ->
      Ok(
        Module(
          ..mod,
          sections: mod.sections |> finger_tree.push(StartSection(start)),
        ),
      )
  }
}

pub fn elem_section(mod: Module, elems: FingerTree(Elem)) {
  case mod {
    Module(has_elem: True, ..) -> Error("Section already exists")
    mod ->
      Ok(
        Module(
          ..mod,
          sections: mod.sections |> finger_tree.push(ElemSection(elems)),
        ),
      )
  }
}

pub fn code_section(mod: Module, codes: FingerTree(Code)) {
  case mod {
    Module(has_code: True, ..) -> Error("Section already exists")
    mod ->
      Ok(
        Module(
          ..mod,
          sections: mod.sections |> finger_tree.push(CodeSection(codes)),
        ),
      )
  }
}

pub fn data_section(mod: Module, data: FingerTree(Data)) {
  case mod {
    Module(has_data: True, ..) -> Error("Section already exists")
    mod ->
      Ok(
        Module(
          ..mod,
          sections: mod.sections |> finger_tree.push(DataSection(data)),
        ),
      )
  }
}

pub fn data_count_section(mod: Module, count: U32) {
  case mod {
    Module(has_data_count: True, ..) -> Error("Section already exists")
    mod ->
      Ok(
        Module(
          ..mod,
          sections: mod.sections |> finger_tree.push(DataCountSection(count)),
        ),
      )
  }
}
