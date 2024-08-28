import gleam/bit_array
import gleam/bytes_builder.{type BytesBuilder}
import gleam/dynamic
import gleam/int
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import internal/binary/common
import internal/binary/values.{
  decode_i32, decode_i64, decode_s33, decode_u32, encode_i32, encode_i64,
  encode_s33, encode_u32,
}
import internal/finger_tree.{type FingerTree}
import internal/structure/numbers.{
  decode_f32, decode_f64, encode_f32, encode_f64, s33, u32, unwrap_s33,
  unwrap_u32, unwrap_v128, v128,
}
import internal/structure/types.{
  type ArrayType, type BlockType, type CompositeType, type DataIDX, type ElemIDX,
  type Expr, type FieldIDX, type FieldType, type FuncIDX, type FuncType,
  type GlobalIDX, type GlobalType, type HeapType, type Instruction,
  type LabelIDX, type LaneIDX16, type LaneIDX2, type LaneIDX4, type LaneIDX8,
  type Limits, type LocalIDX, type Locals, type MemArg, type MemIDX,
  type MemType, type Mut, type RecType, type RefType, type StorageType,
  type StructType, type SubType, type TableIDX, type TableType, type TypeIDX,
  type ValType, AnyConvertExtern, AnyHeapType, AnyRefType, ArrayCompositeType,
  ArrayCopy, ArrayFill, ArrayGet, ArrayGetS, ArrayGetU, ArrayHeapType,
  ArrayInitData, ArrayInitElem, ArrayLen, ArrayNew, ArrayNewData,
  ArrayNewDefault, ArrayNewElem, ArrayNewFixed, ArrayRefType, ArraySet,
  ArrayType, Block, BotValType, Br, BrIf, BrOnCast, BrOnCastFail, BrOnNonNull,
  BrOnNull, BrTable, Call, CallIndirect, CallRef, ConcreteHeapType, Const,
  DataDrop, DataIDX, Drop, ElemDrop, ElemIDX, EqHeapType, EqRefType, Expr,
  ExternConvertAny, ExternHeapType, ExternRefType, F32Abs, F32Add, F32Ceil,
  F32Const, F32ConvertI32S, F32ConvertI32U, F32ConvertI64S, F32ConvertI64U,
  F32Copysign, F32DemoteF64, F32Div, F32Eq, F32Floor, F32Ge, F32Gt, F32Le,
  F32Load, F32Lt, F32Max, F32Min, F32Mul, F32Ne, F32Nearest, F32Neg,
  F32ReinterpretI32, F32Sqrt, F32Store, F32Sub, F32Trunc, F32ValType, F32x4Abs,
  F32x4Add, F32x4Ceil, F32x4ConvertI32x4S, F32x4ConvertI32x4U,
  F32x4DemoteF64x2Zero, F32x4Div, F32x4Eq, F32x4ExtractLane, F32x4Floor, F32x4Ge,
  F32x4Gt, F32x4Le, F32x4Lt, F32x4Max, F32x4Min, F32x4Mul, F32x4Ne, F32x4Nearest,
  F32x4Neg, F32x4Pmax, F32x4Pmin, F32x4ReplaceLane, F32x4Splat, F32x4Sqrt,
  F32x4Sub, F32x4Trunc, F64Abs, F64Add, F64Ceil, F64Const, F64ConvertI32S,
  F64ConvertI32U, F64ConvertI64S, F64ConvertI64U, F64Copysign, F64Div, F64Eq,
  F64Floor, F64Ge, F64Gt, F64Le, F64Load, F64Lt, F64Max, F64Min, F64Mul, F64Ne,
  F64Nearest, F64Neg, F64PromoteF32, F64ReinterpretI64, F64Sqrt, F64Store,
  F64Sub, F64Trunc, F64ValType, F64x2Abs, F64x2Add, F64x2Ceil,
  F64x2ConvertLowI32x4S, F64x2ConvertLowI32x4U, F64x2Div, F64x2Eq,
  F64x2ExtractLane, F64x2Floor, F64x2Ge, F64x2Gt, F64x2Le, F64x2Lt, F64x2Max,
  F64x2Min, F64x2Mul, F64x2Ne, F64x2Nearest, F64x2Neg, F64x2Pmax, F64x2Pmin,
  F64x2PromoteLowF32x4, F64x2ReplaceLane, F64x2Splat, F64x2Sqrt, F64x2Sub,
  F64x2Trunc, FieldIDX, FieldType, FuncCompositeType, FuncHeapType, FuncIDX,
  FuncRefType, FuncType, FuncTypeBlockType, GlobalGet, GlobalIDX, GlobalSet,
  GlobalType, HeapTypeRefType, I16StorageType, I16x8Abs, I16x8Add, I16x8AddSatS,
  I16x8AddSatU, I16x8AllTrue, I16x8AvgrU, I16x8Bitmask, I16x8Eq,
  I16x8ExtaddPairwiseI8x16S, I16x8ExtaddPairwiseI8x16U, I16x8ExtendHighI8x16S,
  I16x8ExtendHighI8x16U, I16x8ExtendLowI8x16S, I16x8ExtendLowI8x16U,
  I16x8ExtmulHighI8x16S, I16x8ExtmulHighI8x16U, I16x8ExtmulLowI8x16S,
  I16x8ExtmulLowI8x16U, I16x8ExtractLaneS, I16x8ExtractLaneU, I16x8GeS, I16x8GeU,
  I16x8GtS, I16x8GtU, I16x8LeS, I16x8LeU, I16x8LtS, I16x8LtU, I16x8MaxS,
  I16x8MaxU, I16x8MinS, I16x8MinU, I16x8Mul, I16x8NarrowI32x4S,
  I16x8NarrowI32x4U, I16x8Ne, I16x8Neg, I16x8Q15mulrSatS, I16x8ReplaceLane,
  I16x8Shl, I16x8ShrS, I16x8ShrU, I16x8Splat, I16x8Sub, I16x8SubSatS,
  I16x8SubSatU, I31GetS, I31GetU, I31HeapType, I31RefType, I32Add, I32And,
  I32Clz, I32Const, I32Ctz, I32DivS, I32DivU, I32Eq, I32Eqz, I32Extend16S,
  I32Extend8S, I32GeS, I32GeU, I32GtS, I32GtU, I32LeS, I32LeU, I32Load,
  I32Load16S, I32Load16U, I32Load8S, I32Load8U, I32LtS, I32LtU, I32Mul, I32Ne,
  I32Or, I32Popcnt, I32ReinterpretF32, I32RemS, I32RemU, I32Rotl, I32Rotr,
  I32Shl, I32ShrS, I32ShrU, I32Store, I32Store16, I32Store8, I32Sub,
  I32TruncF32S, I32TruncF32U, I32TruncF64S, I32TruncF64U, I32TruncSatF32S,
  I32TruncSatF32U, I32TruncSatF64S, I32TruncSatF64U, I32ValType, I32WrapI64,
  I32Xor, I32x4Abs, I32x4Add, I32x4AllTrue, I32x4Bitmask, I32x4DotI16x8S,
  I32x4Eq, I32x4ExtaddPairwiseI16x8S, I32x4ExtaddPairwiseI16x8U,
  I32x4ExtendHighI16x8S, I32x4ExtendHighI16x8U, I32x4ExtendLowI16x8S,
  I32x4ExtendLowI16x8U, I32x4ExtmulHighI16x8S, I32x4ExtmulHighI16x8U,
  I32x4ExtmulLowI16x8S, I32x4ExtmulLowI16x8U, I32x4ExtractLane, I32x4GeS,
  I32x4GeU, I32x4GtS, I32x4GtU, I32x4LeS, I32x4LeU, I32x4LtS, I32x4LtU,
  I32x4MaxS, I32x4MaxU, I32x4MinS, I32x4MinU, I32x4Mul, I32x4Ne, I32x4Neg,
  I32x4ReplaceLane, I32x4Shl, I32x4ShrS, I32x4ShrU, I32x4Splat, I32x4Sub,
  I32x4TruncSatF32x4S, I32x4TruncSatF32x4U, I32x4TruncSatF64x2SZero,
  I32x4TruncSatF64x2UZero, I64Add, I64And, I64Clz, I64Const, I64Ctz, I64DivS,
  I64DivU, I64Eq, I64Eqz, I64Extend16S, I64Extend32S, I64Extend8S, I64ExtendI32S,
  I64ExtendI32U, I64GeS, I64GeU, I64GtS, I64GtU, I64LeS, I64LeU, I64Load,
  I64Load16S, I64Load16U, I64Load32S, I64Load32U, I64Load8S, I64Load8U, I64LtS,
  I64LtU, I64Mul, I64Ne, I64Or, I64Popcnt, I64ReinterpretF64, I64RemS, I64RemU,
  I64Rotl, I64Rotr, I64Shl, I64ShrS, I64ShrU, I64Store, I64Store16, I64Store32,
  I64Store8, I64Sub, I64TruncF32S, I64TruncF32U, I64TruncF64S, I64TruncF64U,
  I64TruncSatF32S, I64TruncSatF32U, I64TruncSatF64S, I64TruncSatF64U, I64ValType,
  I64Xor, I64x2Abs, I64x2Add, I64x2AllTrue, I64x2Bitmask, I64x2Eq,
  I64x2ExtendHighI32x4S, I64x2ExtendHighI32x4U, I64x2ExtendLowI32x4S,
  I64x2ExtendLowI32x4U, I64x2ExtmulHighI32x4S, I64x2ExtmulHighI32x4U,
  I64x2ExtmulLowI32x4S, I64x2ExtmulLowI32x4U, I64x2ExtractLane, I64x2GeS,
  I64x2GtS, I64x2LeS, I64x2LtS, I64x2Mul, I64x2Ne, I64x2Neg, I64x2ReplaceLane,
  I64x2Shl, I64x2ShrS, I64x2ShrU, I64x2Splat, I64x2Sub, I8StorageType, I8x16Abs,
  I8x16Add, I8x16AddSatS, I8x16AddSatU, I8x16AllTrue, I8x16AvgrU, I8x16Bitmask,
  I8x16Eq, I8x16ExtractLaneS, I8x16ExtractLaneU, I8x16GeS, I8x16GeU, I8x16GtS,
  I8x16GtU, I8x16LeS, I8x16LeU, I8x16LtS, I8x16LtU, I8x16MaxS, I8x16MaxU,
  I8x16MinS, I8x16MinU, I8x16NarrowI16x8S, I8x16NarrowI16x8U, I8x16Ne, I8x16Neg,
  I8x16Popcnt, I8x16ReplaceLane, I8x16Shl, I8x16ShrS, I8x16ShrU, I8x16Shuffle,
  I8x16Splat, I8x16Sub, I8x16SubSatS, I8x16SubSatU, I8x16Swizzle, If, LabelIDX,
  Limits, LocalGet, LocalIDX, LocalSet, LocalTee, Locals, Loop, MemArg, MemIDX,
  MemType, MemoryCopy, MemoryFill, MemoryGrow, MemoryInit, MemorySize,
  NoExternHeapType, NoExternRefType, NoFuncHeapType, NoFuncRefType, NoneHeapType,
  NoneRefType, Nop, RecType, RefAsNonNull, RefCast, RefEq, RefFunc, RefI31,
  RefIsNull, RefNull, RefTest, RefTypeValType, Return, ReturnCall,
  ReturnCallIndirect, ReturnCallRef, Select, SelectT, StructCompositeType,
  StructGet, StructGetS, StructGetU, StructHeapType, StructNew, StructNewDefault,
  StructRefType, StructSet, StructType, SubType, TableCopy, TableFill, TableGet,
  TableGrow, TableIDX, TableInit, TableSet, TableSize, TableType, TypeIDX,
  Unreachable, V128And, V128AndNot, V128AnyTrue, V128Bitselect, V128Const,
  V128Load, V128Load16Lane, V128Load16Splat, V128Load16x4S, V128Load16x4U,
  V128Load32Lane, V128Load32Splat, V128Load32Zero, V128Load32x2S, V128Load32x2U,
  V128Load64Lane, V128Load64Splat, V128Load64Zero, V128Load8Lane, V128Load8Splat,
  V128Load8x8S, V128Load8x8U, V128Not, V128Or, V128Store, V128Store16Lane,
  V128Store32Lane, V128Store64Lane, V128Store8Lane, V128ValType, V128Xor,
  ValTypeBlockType, ValTypeStorageType, Var, VoidBlockType, lane_16, lane_2,
  lane_4, lane_8,
}
import simplifile

pub fn decode_lane_16(bits: BitArray) {
  use #(val, rest) <- result.try(decode_u32(bits))
  use lane <- result.map(lane_16(val |> unwrap_u32))
  #(lane, rest)
}

pub fn decode_lane_8(bits: BitArray) {
  use #(val, rest) <- result.try(decode_u32(bits))
  use lane <- result.map(lane_8(val |> unwrap_u32))
  #(lane, rest)
}

pub fn decode_lane_4(bits: BitArray) {
  use #(val, rest) <- result.try(decode_u32(bits))
  use lane <- result.map(lane_4(val |> unwrap_u32))
  #(lane, rest)
}

pub fn decode_lane_2(bits: BitArray) {
  use #(val, rest) <- result.try(decode_u32(bits))
  use lane <- result.map(lane_2(val |> unwrap_u32))
  #(lane, rest)
}

pub fn encode_lane_16(builder: BytesBuilder, lane: LaneIDX16) {
  use lane <- result.map(lane |> types.unwrap_lane_16 |> u32)
  builder |> encode_u32(lane)
}

pub fn encode_lane_8(builder: BytesBuilder, lane: LaneIDX8) {
  use lane <- result.map(lane |> types.unwrap_lane_8 |> u32)
  builder |> encode_u32(lane)
}

pub fn encode_lane_4(builder: BytesBuilder, lane: LaneIDX4) {
  use lane <- result.map(lane |> types.unwrap_lane_4 |> u32)
  builder |> encode_u32(lane)
}

pub fn encode_lane_2(builder: BytesBuilder, lane: LaneIDX2) {
  use lane <- result.map(lane |> types.unwrap_lane_2 |> u32)
  builder |> encode_u32(lane)
}

pub fn encode_locals(builder: BytesBuilder, locals: Locals) {
  let Locals(count, type_) = locals
  builder
  |> encode_u32(count)
  |> encode_val_type(type_)
}

pub fn decode_heap_type(bits: BitArray) {
  case bits {
    <<0x73, rest:bits>> -> Ok(#(NoFuncHeapType, rest))
    <<0x72, rest:bits>> -> Ok(#(NoExternHeapType, rest))
    <<0x71, rest:bits>> -> Ok(#(NoneHeapType, rest))
    <<0x70, rest:bits>> -> Ok(#(FuncHeapType, rest))
    <<0x6F, rest:bits>> -> Ok(#(ExternHeapType, rest))
    <<0x6E, rest:bits>> -> Ok(#(AnyHeapType, rest))
    <<0x6D, rest:bits>> -> Ok(#(EqHeapType, rest))
    <<0x6C, rest:bits>> -> Ok(#(I31HeapType, rest))
    <<0x6B, rest:bits>> -> Ok(#(StructHeapType, rest))
    <<0x6A, rest:bits>> -> Ok(#(ArrayHeapType, rest))
    _ -> {
      use #(val, rest) <- result.try(decode_s33(bits))
      let val = val |> unwrap_s33
      use val <- result.map(u32(val))
      #(ConcreteHeapType(TypeIDX(val)), rest)
    }
  }
}

pub fn encode_heap_type(builder: BytesBuilder, heap_type: HeapType) {
  case heap_type {
    NoFuncHeapType -> Ok(builder |> bytes_builder.append(<<0x73>>))
    NoExternHeapType -> Ok(builder |> bytes_builder.append(<<0x72>>))
    NoneHeapType -> Ok(builder |> bytes_builder.append(<<0x71>>))
    FuncHeapType -> Ok(builder |> bytes_builder.append(<<0x70>>))
    ExternHeapType -> Ok(builder |> bytes_builder.append(<<0x6F>>))
    AnyHeapType -> Ok(builder |> bytes_builder.append(<<0x6E>>))
    EqHeapType -> Ok(builder |> bytes_builder.append(<<0x6D>>))
    I31HeapType -> Ok(builder |> bytes_builder.append(<<0x6C>>))
    StructHeapType -> Ok(builder |> bytes_builder.append(<<0x6B>>))
    ArrayHeapType -> Ok(builder |> bytes_builder.append(<<0x6A>>))
    ConcreteHeapType(TypeIDX(val)) -> {
      let val = val |> unwrap_u32
      use val <- result.map(s33(val))
      builder |> encode_s33(val)
    }
    _ -> Error("Invalid heap type")
  }
}

pub fn decode_ref_type(bits: BitArray) {
  case bits {
    <<0x73, rest:bits>> -> Ok(#(NoFuncRefType, rest))
    <<0x72, rest:bits>> -> Ok(#(NoExternRefType, rest))
    <<0x71, rest:bits>> -> Ok(#(NoneRefType, rest))
    <<0x70, rest:bits>> -> Ok(#(FuncRefType, rest))
    <<0x6F, rest:bits>> -> Ok(#(ExternRefType, rest))
    <<0x6E, rest:bits>> -> Ok(#(AnyRefType, rest))
    <<0x6D, rest:bits>> -> Ok(#(EqRefType, rest))
    <<0x6C, rest:bits>> -> Ok(#(I31RefType, rest))
    <<0x6B, rest:bits>> -> Ok(#(StructRefType, rest))
    <<0x6A, rest:bits>> -> Ok(#(ArrayRefType, rest))
    <<0x63, 0x73, rest:bits>> -> Ok(#(NoFuncRefType, rest))
    <<0x63, 0x72, rest:bits>> -> Ok(#(NoExternRefType, rest))
    <<0x63, 0x71, rest:bits>> -> Ok(#(NoneRefType, rest))
    <<0x63, 0x70, rest:bits>> -> Ok(#(FuncRefType, rest))
    <<0x63, 0x6F, rest:bits>> -> Ok(#(ExternRefType, rest))
    <<0x63, 0x6E, rest:bits>> -> Ok(#(AnyRefType, rest))
    <<0x63, 0x6D, rest:bits>> -> Ok(#(EqRefType, rest))
    <<0x63, 0x6C, rest:bits>> -> Ok(#(I31RefType, rest))
    <<0x63, 0x6B, rest:bits>> -> Ok(#(StructRefType, rest))
    <<0x63, 0x6A, rest:bits>> -> Ok(#(ArrayRefType, rest))
    <<0x63, rest:bits>> -> {
      use #(heap_type, rest) <- result.map(decode_heap_type(rest))
      case heap_type {
        NoFuncHeapType -> #(NoFuncRefType, rest)
        NoExternHeapType -> #(NoExternRefType, rest)
        NoneHeapType -> #(NoneRefType, rest)
        FuncHeapType -> #(FuncRefType, rest)
        ExternHeapType -> #(ExternRefType, rest)
        AnyHeapType -> #(AnyRefType, rest)
        EqHeapType -> #(EqRefType, rest)
        I31HeapType -> #(I31RefType, rest)
        StructHeapType -> #(StructRefType, rest)
        ArrayHeapType -> #(ArrayRefType, rest)
        _ -> #(HeapTypeRefType(heap_type, True), rest)
      }
    }
    <<0x64, rest:bits>> -> {
      use #(heap_type, rest) <- result.map(decode_heap_type(rest))
      #(HeapTypeRefType(heap_type, False), rest)
    }
    _ -> Error("Invalid reference type")
  }
}

pub fn encode_ref_type(builder: BytesBuilder, ref_type: RefType) {
  case ref_type {
    // (ref null NoFunc)
    NoFuncRefType -> Ok(builder |> bytes_builder.append(<<0x73>>))
    NoExternRefType -> Ok(builder |> bytes_builder.append(<<0x72>>))
    NoneRefType -> Ok(builder |> bytes_builder.append(<<0x71>>))
    FuncRefType -> Ok(builder |> bytes_builder.append(<<0x70>>))
    ExternRefType -> Ok(builder |> bytes_builder.append(<<0x6F>>))
    AnyRefType -> Ok(builder |> bytes_builder.append(<<0x6E>>))
    EqRefType -> Ok(builder |> bytes_builder.append(<<0x6D>>))
    I31RefType -> Ok(builder |> bytes_builder.append(<<0x6C>>))
    StructRefType -> Ok(builder |> bytes_builder.append(<<0x6B>>))
    ArrayRefType -> Ok(builder |> bytes_builder.append(<<0x6A>>))
    HeapTypeRefType(NoFuncHeapType, True) ->
      Ok(builder |> bytes_builder.append(<<0x73>>))
    HeapTypeRefType(NoExternHeapType, True) ->
      Ok(builder |> bytes_builder.append(<<0x72>>))
    HeapTypeRefType(NoneHeapType, True) ->
      Ok(builder |> bytes_builder.append(<<0x71>>))
    HeapTypeRefType(FuncHeapType, True) ->
      Ok(builder |> bytes_builder.append(<<0x70>>))
    HeapTypeRefType(ExternHeapType, True) ->
      Ok(builder |> bytes_builder.append(<<0x6F>>))
    HeapTypeRefType(AnyHeapType, True) ->
      Ok(builder |> bytes_builder.append(<<0x6E>>))
    HeapTypeRefType(EqHeapType, True) ->
      Ok(builder |> bytes_builder.append(<<0x6D>>))
    HeapTypeRefType(I31HeapType, True) ->
      Ok(builder |> bytes_builder.append(<<0x6C>>))
    HeapTypeRefType(StructHeapType, True) ->
      Ok(builder |> bytes_builder.append(<<0x6B>>))
    HeapTypeRefType(ArrayHeapType, True) ->
      Ok(builder |> bytes_builder.append(<<0x6A>>))
    HeapTypeRefType(ht, True) ->
      builder
      |> bytes_builder.append(<<0x63>>)
      |> encode_heap_type(ht)
    HeapTypeRefType(ht, False) ->
      builder
      |> bytes_builder.append(<<0x64>>)
      |> encode_heap_type(ht)
  }
}

pub fn decode_val_type(bits: BitArray) {
  case bits {
    <<0x7F, rest:bits>> -> Ok(#(I32ValType, rest))
    <<0x7E, rest:bits>> -> Ok(#(I64ValType, rest))
    <<0x7D, rest:bits>> -> Ok(#(F32ValType, rest))
    <<0x7C, rest:bits>> -> Ok(#(F64ValType, rest))
    <<0x7B, rest:bits>> -> Ok(#(V128ValType, rest))
    _ -> {
      use #(rt, rest) <- result.map(decode_ref_type(bits))
      #(RefTypeValType(rt), rest)
    }
  }
}

pub fn encode_val_type(builder: BytesBuilder, val_type: ValType) {
  case val_type {
    I32ValType -> Ok(builder |> bytes_builder.append(<<0x7F>>))
    I64ValType -> Ok(builder |> bytes_builder.append(<<0x7E>>))
    F32ValType -> Ok(builder |> bytes_builder.append(<<0x7D>>))
    F64ValType -> Ok(builder |> bytes_builder.append(<<0x7C>>))
    V128ValType -> Ok(builder |> bytes_builder.append(<<0x7B>>))
    RefTypeValType(rt) -> encode_ref_type(builder, rt)
    BotValType -> Error("Cannot encode BotValType")
  }
}

pub fn encode_result_type(
  builder: BytesBuilder,
  result_type: FingerTree(ValType),
) {
  builder |> common.encode_vec(result_type, encode_val_type)
}

pub fn decode_result_type(bits: BitArray) {
  common.decode_vec(bits, decode_val_type)
}

pub fn encode_func_type(builder: BytesBuilder, func_type: FuncType) {
  use builder <- result.try(
    builder |> common.encode_vec(func_type.parameters, encode_val_type),
  )
  builder |> common.encode_vec(func_type.results, encode_val_type)
}

pub fn decode_func_type(bits: BitArray) {
  use #(parameters, rest) <- result.try(common.decode_vec(bits, decode_val_type))
  use #(results, rest) <- result.map(common.decode_vec(rest, decode_val_type))
  #(FuncType(parameters, results), rest)
}

pub fn decode_array_type(bits: BitArray) {
  use #(ft, rest) <- result.map(decode_field_type(bits))
  #(ArrayType(ft), rest)
}

pub fn encode_array_type(builder: BytesBuilder, array_type: ArrayType) {
  builder |> encode_field_type(array_type.ft)
}

pub fn decode_field_type(bits: BitArray) {
  use #(st, rest) <- result.try(decode_storage_type(bits))
  use #(mut, rest) <- result.map(decode_mut(rest))
  #(FieldType(st, mut), rest)
}

pub fn encode_field_type(builder: BytesBuilder, field_type: FieldType) {
  use builder <- result.try(builder |> encode_storage_type(field_type.st))
  builder |> encode_mut(field_type.mut)
}

pub fn decode_storage_type(bits: BitArray) {
  case bits {
    <<0x78, rest:bits>> -> Ok(#(I8StorageType, rest))
    <<0x77, rest:bits>> -> Ok(#(I16StorageType, rest))
    _ -> {
      use #(vt, rest) <- result.map(decode_val_type(bits))
      #(ValTypeStorageType(vt), rest)
    }
  }
}

pub fn encode_storage_type(builder: BytesBuilder, storage_type: StorageType) {
  case storage_type {
    ValTypeStorageType(vt) -> encode_val_type(builder, vt)
    I8StorageType -> Ok(builder |> bytes_builder.append(<<0x78>>))
    I16StorageType -> Ok(builder |> bytes_builder.append(<<0x77>>))
  }
}

pub fn decode_mut(bits: BitArray) {
  case bits {
    <<0x00, rest:bits>> -> Ok(#(Const, rest))
    <<0x01, rest:bits>> -> Ok(#(Var, rest))
    _ -> Error("Invalid mutability")
  }
}

pub fn encode_mut(builder: BytesBuilder, mut: Mut) {
  case mut {
    Const -> Ok(builder |> bytes_builder.append(<<0x00>>))
    Var -> Ok(builder |> bytes_builder.append(<<0x01>>))
  }
}

pub fn decode_struct_type(bits: BitArray) {
  use #(fields, rest) <- result.map(common.decode_vec(bits, decode_field_type))
  #(StructType(fields), rest)
}

pub fn encode_struct_type(builder: BytesBuilder, struct_type: StructType) {
  builder |> common.encode_vec(struct_type.ft, encode_field_type)
}

pub fn decode_comp_type(bits: BitArray) {
  case bits {
    <<0x5E, rest:bits>> -> {
      use #(at, rest) <- result.map(decode_array_type(rest))
      #(ArrayCompositeType(at), rest)
    }
    <<0x5F, rest:bits>> -> {
      use #(st, rest) <- result.map(decode_struct_type(rest))
      #(StructCompositeType(st), rest)
    }
    <<0x60, rest:bits>> -> {
      use #(ft, rest) <- result.map(decode_func_type(rest))
      #(FuncCompositeType(ft), rest)
    }
    _ -> Error("Invalid composite type")
  }
}

pub fn encode_composite_type(builder: BytesBuilder, comp_type: CompositeType) {
  case comp_type {
    FuncCompositeType(ft) ->
      builder
      |> bytes_builder.append(<<0x60>>)
      |> encode_func_type(ft)
    StructCompositeType(st) ->
      builder
      |> bytes_builder.append(<<0x5F>>)
      |> encode_struct_type(st)
    ArrayCompositeType(at) ->
      builder
      |> bytes_builder.append(<<0x5E>>)
      |> encode_array_type(at)
  }
}

pub fn decode_rec_type(bits: BitArray) {
  case bits {
    <<0x4E, rest:bits>> -> {
      use #(st, rest) <- result.map(common.decode_vec(rest, decode_sub_type))
      #(RecType(st), rest)
    }
    _ -> {
      use #(st, rest) <- result.map(decode_sub_type(bits))
      #(RecType(finger_tree.from_list([st])), rest)
    }
  }
}

pub fn encode_rec_type(builder: BytesBuilder, rec_type: RecType) {
  case rec_type.st |> finger_tree.size {
    1 ->
      case rec_type.st |> finger_tree.shift {
        Ok(#(st, _)) -> encode_sub_type(builder, st)
        Error(_) -> Error("Invalid recursive type")
      }
    t if t > 1 ->
      builder
      |> bytes_builder.append(<<0x4E>>)
      |> common.encode_vec(rec_type.st, encode_sub_type)
    _ -> Error("Invalid recursive type")
  }
}

pub fn decode_sub_type(bits: BitArray) {
  case bits {
    <<0x50, rest:bits>> -> {
      use #(ty, rest) <- result.try(common.decode_vec(rest, decode_type_idx))
      use #(ct, rest) <- result.map(decode_comp_type(rest))
      #(SubType(False, ty, ct), rest)
    }
    <<0x4F, rest:bits>> -> {
      use #(ty, rest) <- result.try(common.decode_vec(rest, decode_type_idx))
      use #(ct, rest) <- result.map(decode_comp_type(rest))
      #(SubType(True, ty, ct), rest)
    }
    _ -> {
      use #(ct, rest) <- result.map(decode_comp_type(bits))
      #(SubType(True, finger_tree.new(), ct), rest)
    }
  }
}

pub fn encode_sub_type(builder: BytesBuilder, sub_type: SubType) {
  case sub_type.final, sub_type.t |> finger_tree.size {
    True, 0 -> builder |> encode_composite_type(sub_type.ct)
    True, _ -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x4F>>)
        |> common.encode_vec(sub_type.t, encode_type_idx),
      )
      builder |> encode_composite_type(sub_type.ct)
    }
    False, _ -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x50>>)
        |> common.encode_vec(sub_type.t, encode_type_idx),
      )
      builder |> encode_composite_type(sub_type.ct)
    }
  }
}

pub fn decode_func_idx(bits: BitArray) {
  use #(idx, rest) <- result.map(decode_u32(bits))
  #(FuncIDX(idx), rest)
}

pub fn encode_func_idx(builder: BytesBuilder, func_idx: FuncIDX) {
  Ok(builder |> encode_u32(func_idx.id))
}

pub fn decode_table_idx(bits: BitArray) {
  use #(idx, rest) <- result.map(decode_u32(bits))
  #(TableIDX(idx), rest)
}

pub fn encode_table_idx(builder: BytesBuilder, func_idx: TableIDX) {
  Ok(builder |> encode_u32(func_idx.id))
}

pub fn decode_field_idx(bits: BitArray) {
  use #(idx, rest) <- result.map(decode_u32(bits))
  #(FieldIDX(idx), rest)
}

pub fn encode_field_idx(builder: BytesBuilder, field_idx: FieldIDX) {
  Ok(builder |> encode_u32(field_idx.id))
}

pub fn decode_type_idx(bits: BitArray) {
  use #(idx, rest) <- result.map(decode_u32(bits))
  #(TypeIDX(idx), rest)
}

pub fn encode_type_idx(builder: BytesBuilder, type_idx: TypeIDX) {
  case type_idx {
    TypeIDX(id) -> Ok(builder |> encode_u32(id))
    _ -> Error("Invalid type index, found concrete index instead of numeric.")
  }
}

pub fn encode_mem_idx(builder: BytesBuilder, mem_idx: MemIDX) {
  Ok(builder |> encode_u32(mem_idx.id))
}

pub fn decode_mem_idx(bits: BitArray) {
  use #(idx, rest) <- result.map(decode_u32(bits))
  #(MemIDX(idx), rest)
}

pub fn decode_local_idx(bits: BitArray) {
  use #(idx, rest) <- result.map(decode_u32(bits))
  #(LocalIDX(idx), rest)
}

pub fn encode_local_idx(builder: BytesBuilder, local_idx: LocalIDX) {
  Ok(builder |> encode_u32(local_idx.id))
}

pub fn decode_global_idx(bits: BitArray) {
  use #(idx, rest) <- result.map(decode_u32(bits))
  #(GlobalIDX(idx), rest)
}

pub fn encode_global_idx(builder: BytesBuilder, global_idx: GlobalIDX) {
  Ok(builder |> encode_u32(global_idx.id))
}

pub fn decode_data_idx(bits: BitArray) {
  use #(idx, rest) <- result.map(decode_u32(bits))
  #(DataIDX(idx), rest)
}

pub fn encode_data_idx(builder: BytesBuilder, data_idx: DataIDX) {
  Ok(builder |> encode_u32(data_idx.id))
}

pub fn decode_elem_idx(bits: BitArray) {
  use #(idx, rest) <- result.map(decode_u32(bits))
  #(ElemIDX(idx), rest)
}

pub fn encode_elem_idx(builder: BytesBuilder, elem_idx: ElemIDX) {
  Ok(builder |> encode_u32(elem_idx.id))
}

pub fn decode_label_idx(bits: BitArray) {
  use #(idx, rest) <- result.map(decode_u32(bits))
  #(LabelIDX(idx), rest)
}

pub fn encode_label_idx(builder: BytesBuilder, type_idx: LabelIDX) {
  Ok(builder |> encode_u32(type_idx.id))
}

pub fn decode_limits(bits: BitArray) {
  case bits {
    <<0x00, rest:bits>> -> {
      use #(min, rest) <- result.map(decode_u32(rest))
      #(Limits(min, None), rest)
    }
    <<0x01, rest:bits>> -> {
      use #(min, rest) <- result.try(decode_u32(rest))
      use #(max, rest) <- result.map(decode_u32(rest))
      #(Limits(min, Some(max)), rest)
    }
    _ -> Error("Invalid limits")
  }
}

pub fn encode_limits(builder: BytesBuilder, limits: Limits) {
  case limits.max {
    Some(max) ->
      Ok(
        builder
        |> bytes_builder.append(<<0x01>>)
        |> encode_u32(limits.min)
        |> encode_u32(max),
      )
    None ->
      Ok(
        builder
        |> bytes_builder.append(<<0x00>>)
        |> encode_u32(limits.min),
      )
  }
}

pub fn decode_mem_type(bits: BitArray) {
  use #(limits, rest) <- result.map(decode_limits(bits))
  #(MemType(limits), rest)
}

pub fn encode_mem_type(builder: BytesBuilder, mem_type: MemType) {
  builder |> encode_limits(mem_type.limits)
}

pub fn decode_table_type(rest: BitArray) {
  use #(rt, rest) <- result.try(decode_ref_type(rest))
  use #(limits, rest) <- result.map(decode_limits(rest))
  #(TableType(rt, limits), rest)
}

pub fn encode_table_type(builder: BytesBuilder, table_type: TableType) {
  use builder <- result.try(builder |> encode_ref_type(table_type.t))
  builder |> encode_limits(table_type.limits)
}

pub fn decode_global_type(rest: BitArray) {
  use #(vt, rest) <- result.try(decode_val_type(rest))
  use #(mutability, rest) <- result.map(decode_mut(rest))
  #(GlobalType(vt, mutability), rest)
}

pub fn encode_global_type(builder: BytesBuilder, global_type: GlobalType) {
  use builder <- result.try(builder |> encode_val_type(global_type.vt))
  builder |> encode_mut(global_type.mut)
}

pub fn decode_block_type(bits: BitArray) {
  case bits {
    <<0x40, rest:bits>> -> Ok(#(VoidBlockType, rest))
    _ -> {
      case bits |> decode_val_type {
        Ok(#(vt, rest)) -> Ok(#(ValTypeBlockType(vt), rest))
        _ -> {
          use #(s33_value, rest) <- result.try(values.decode_s33(bits))
          use u32_value <- result.map(
            s33_value |> numbers.unwrap_s33 |> numbers.u32,
          )
          #(FuncTypeBlockType(TypeIDX(u32_value)), rest)
        }
      }
    }
  }
}

pub fn encode_block_type(builder: BytesBuilder, block_type: BlockType) {
  case block_type {
    VoidBlockType -> Ok(builder |> bytes_builder.append(<<0x40>>))
    ValTypeBlockType(vt) -> builder |> encode_val_type(vt)
    FuncTypeBlockType(TypeIDX(type_idx)) -> {
      use s33_value <- result.map(type_idx |> unwrap_u32 |> s33)
      builder |> encode_s33(s33_value)
    }
    _ ->
      panic as "Concrete block types cannot be encoded. Something went wrong."
  }
}

pub fn decode_cast_flags(bits: BitArray) {
  case bits {
    <<0x00, rest:bits>> -> Ok(#(#(False, False), rest))
    <<0x01, rest:bits>> -> Ok(#(#(True, False), rest))
    <<0x02, rest:bits>> -> Ok(#(#(False, True), rest))
    <<0x03, rest:bits>> -> Ok(#(#(True, True), rest))
    _ -> Error("Invalid cast flags")
  }
}

pub fn encode_cast_flags(builder: BytesBuilder, cast_flags: #(Bool, Bool)) {
  case cast_flags {
    #(False, False) -> Ok(builder |> bytes_builder.append(<<0x00>>))
    #(True, False) -> Ok(builder |> bytes_builder.append(<<0x01>>))
    #(False, True) -> Ok(builder |> bytes_builder.append(<<0x02>>))
    #(True, True) -> Ok(builder |> bytes_builder.append(<<0x03>>))
  }
}

pub fn decode_mem_arg(bits: BitArray) {
  use #(offset, rest) <- result.try(decode_u32(bits))
  use #(align, rest) <- result.map(decode_u32(rest))
  #(MemArg(offset, align), rest)
}

pub fn encode_mem_arg(builder: BytesBuilder, mem_arg: MemArg) {
  Ok(
    builder
    |> encode_u32(mem_arg.offset)
    |> encode_u32(mem_arg.align),
  )
}

pub fn encode_expression(
  builder: BytesBuilder,
  expression: Expr,
) -> Result(BytesBuilder, String) {
  todo
}

pub fn decode_expression(bits: BitArray) -> Result(#(Expr, BitArray), String) {
  todo
}

pub fn encode_instruction(
  builder: BytesBuilder,
  instruction: Instruction,
) -> Result(BytesBuilder, String) {
  case instruction {
    Unreachable -> Ok(builder |> bytes_builder.append(<<0x00>>))
    Nop -> Ok(builder |> bytes_builder.append(<<0x01>>))
    Br(LabelIDX(label_idx)) -> {
      builder
      |> bytes_builder.append(<<0x0C>>)
      |> encode_u32(label_idx)
      |> Ok
    }
    BrIf(LabelIDX(label_idx)) -> {
      builder
      |> bytes_builder.append(<<0x0D>>)
      |> encode_u32(label_idx)
      |> Ok
    }
    BrTable(labels, label_default) -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x0E>>)
        |> common.encode_vec(labels, encode_label_idx),
      )
      builder |> encode_label_idx(label_default)
    }
    Return -> Ok(builder |> bytes_builder.append(<<0x0F>>))
    Call(FuncIDX(function_idx)) -> {
      builder
      |> bytes_builder.append(<<0x10>>)
      |> encode_u32(function_idx)
      |> Ok
    }
    CallIndirect(table_idx, type_idx) -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x11>>)
        |> encode_type_idx(type_idx),
      )
      builder |> encode_table_idx(table_idx)
    }
    ReturnCall(func_idx) -> {
      builder
      |> bytes_builder.append(<<0x12>>)
      |> encode_func_idx(func_idx)
    }
    ReturnCallIndirect(table_idx, type_idx) -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x13>>)
        |> encode_type_idx(type_idx),
      )
      builder |> encode_table_idx(table_idx)
    }
    _ -> panic as "Invalid Instruction"
  }
}

pub fn decode_instruction(
  bits: BitArray,
) -> Result(#(Instruction, BitArray), String) {
  case bits {
    <<0x00, rest:bits>> -> Ok(#(Unreachable, rest))
    <<0x01, rest:bits>> -> Ok(#(Nop, rest))
    <<0x0C, rest:bits>> -> {
      use #(label_idx, rest) <- result.map(decode_u32(rest))
      #(Br(LabelIDX(label_idx)), rest)
    }
    <<0x0D, rest:bits>> -> {
      use #(label_idx, rest) <- result.map(decode_u32(rest))
      #(BrIf(LabelIDX(label_idx)), rest)
    }
    <<0x0E, rest:bits>> -> {
      use #(labels, rest) <- result.try(
        rest |> common.decode_vec(decode_label_idx),
      )
      use #(label_default, rest) <- result.map(decode_label_idx(rest))
      #(BrTable(labels, label_default), rest)
    }
    <<0x0F, rest:bits>> -> Ok(#(Return, rest))
    <<0x10, rest:bits>> -> {
      use #(idx, rest) <- result.map(decode_func_idx(rest))
      #(Call(idx), rest)
    }
    <<0x11, rest:bits>> -> {
      use #(type_idx, rest) <- result.try(decode_type_idx(rest))
      use #(table_idx, rest) <- result.map(decode_table_idx(rest))
      #(CallIndirect(table_idx, type_idx), rest)
    }
    <<0x12, rest:bits>> -> {
      use #(func_idx, rest) <- result.map(decode_func_idx(rest))
      #(ReturnCall(func_idx), rest)
    }
    <<0x13, rest:bits>> -> {
      use #(type_idx, rest) <- result.try(decode_type_idx(rest))
      use #(table_idx, rest) <- result.map(decode_table_idx(rest))
      #(ReturnCallIndirect(table_idx, type_idx), rest)
    }
    _ -> panic as "Invalid byte sequence"
  }
}
