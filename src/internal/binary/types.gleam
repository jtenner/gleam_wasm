import gleam/bit_array
import gleam/bytes_builder.{type BytesBuilder}
import gleam/io
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
  DataDrop, DataIDX, Drop, ElemDrop, ElemIDX, Else, End, EqHeapType, EqRefType,
  Expr, ExternConvertAny, ExternHeapType, ExternRefType, F32Abs, F32Add, F32Ceil,
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

pub fn decode_expression(bits: BitArray) {
  do_decode_expression(bits, finger_tree.new())
}

fn do_decode_expression(bits: BitArray, acc: FingerTree(Instruction)) {
  case decode_instruction(bits) {
    Ok(#(End, rest)) -> Ok(#(Expr(acc), rest))
    Ok(#(Else, _)) -> Error("Invalid Else nesting")
    Ok(#(If(block_type, _, _), rest)) -> {
      use #(if_, rest) <- result.map(do_decode_if(
        rest,
        block_type,
        finger_tree.new(),
      ))
      #(Expr(acc |> finger_tree.push(if_)), rest)
    }
    Ok(#(t1, rest)) -> do_decode_expression(rest, acc |> finger_tree.push(t1))
    _ -> Error("Invalid expression")
  }
}

fn do_decode_if(
  bits: BitArray,
  block_type: BlockType,
  t: FingerTree(Instruction),
) {
  case decode_instruction(bits) {
    Ok(#(End, rest)) -> Ok(#(If(block_type, Expr(t), None), rest))
    Ok(#(Else, rest)) -> do_decode_else(rest, block_type, t, finger_tree.new())
    Ok(#(t1, rest)) -> do_decode_if(rest, block_type, t |> finger_tree.push(t1))
    Error(e) -> Error(e)
  }
}

fn do_decode_else(
  bits: BitArray,
  block_type: BlockType,
  t: FingerTree(Instruction),
  e: FingerTree(Instruction),
) {
  case decode_instruction(bits) {
    Ok(#(End, rest)) -> Ok(#(If(block_type, Expr(t), Some(Expr(e))), rest))
    Ok(#(Else, _)) -> Error("Invalid Else nesting")
    Ok(#(t1, rest)) ->
      do_decode_else(rest, block_type, t, e |> finger_tree.push(t1))
    Error(e) -> Error(e)
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

fn decode_instruction(bits: BitArray) {
  case bits {
    <<0x00, rest:bits>> -> Ok(#(Unreachable, rest))
    <<0x01, rest:bits>> -> Ok(#(Nop, rest))
    <<0x02, rest:bits>> -> {
      use #(bt, rest) <- result.try(decode_block_type(rest))
      use #(expr, rest) <- result.map(decode_expression(rest))
      #(Block(bt, expr), rest)
    }
    <<0x03, rest:bits>> -> {
      use #(bt, rest) <- result.try(decode_block_type(rest))
      use #(expr, rest) <- result.map(decode_expression(rest))
      #(Loop(bt, expr), rest)
    }
    <<0x04, rest:bits>> -> {
      use #(bt, rest) <- result.try(decode_block_type(rest))
      use #(if_, rest) <- result.map(do_decode_if(rest, bt, finger_tree.new()))
      #(if_, rest)
    }
    <<0x05, rest:bits>> -> Ok(#(Else, rest))
    <<0x0B, rest:bits>> -> Ok(#(End, rest))
    <<0x0C, rest:bits>> -> {
      use #(idx, rest) <- result.map(decode_label_idx(rest))
      #(Br(idx), rest)
    }
    <<0x0D, rest:bits>> -> {
      use #(idx, rest) <- result.map(decode_label_idx(rest))
      #(BrIf(idx), rest)
    }
    <<0x0E, rest:bits>> -> {
      use #(label_idxs, rest) <- result.try(common.decode_vec(
        rest,
        decode_label_idx,
      ))
      use #(default_label_idx, rest) <- result.map(decode_label_idx(rest))
      #(BrTable(label_idxs, default_label_idx), rest)
    }
    <<0x0F, rest:bits>> -> Ok(#(Return, rest))
    <<0x10, rest:bits>> -> {
      use #(func_idx, rest) <- result.map(decode_func_idx(rest))
      #(Call(func_idx), rest)
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
    <<0x14, rest:bits>> -> {
      use #(type_idx, rest) <- result.map(decode_type_idx(rest))
      #(CallRef(type_idx), rest)
    }
    <<0x15, rest:bits>> -> {
      use #(type_idx, rest) <- result.map(decode_type_idx(rest))
      #(ReturnCallRef(type_idx), rest)
    }
    <<0xd5, rest:bits>> -> {
      use #(label_idx, rest) <- result.map(decode_label_idx(rest))
      #(BrOnNull(label_idx), rest)
    }
    <<0xd6, rest:bits>> -> {
      use #(label_idx, rest) <- result.map(decode_label_idx(rest))
      #(BrOnNonNull(label_idx), rest)
    }
    <<0xd0, rest:bits>> -> {
      use #(ht, rest) <- result.map(decode_heap_type(rest))
      #(RefNull(ht), rest)
    }
    <<0xd1, rest:bits>> -> Ok(#(RefIsNull, rest))
    <<0xd2, rest:bits>> -> {
      use #(idx, rest) <- result.map(decode_func_idx(rest))
      #(RefFunc(idx), rest)
    }
    <<0xd3, rest:bits>> -> Ok(#(RefEq, rest))
    <<0xd4, rest:bits>> -> Ok(#(RefAsNonNull, rest))
    <<0xfb, rest:bits>> -> {
      use #(op, rest) <- result.try(values.decode_u32(rest))
      case op |> unwrap_u32 {
        0 -> {
          use #(idx, rest) <- result.map(decode_type_idx(rest))
          #(StructNew(idx), rest)
        }
        1 -> {
          use #(idx, rest) <- result.map(decode_type_idx(rest))
          #(StructNewDefault(idx), rest)
        }
        2 -> {
          use #(type_idx, rest) <- result.try(decode_type_idx(rest))
          use #(field_idx, rest) <- result.map(decode_field_idx(rest))
          #(StructGet(type_idx, field_idx), rest)
        }
        3 -> {
          use #(type_idx, rest) <- result.try(decode_type_idx(rest))
          use #(field_idx, rest) <- result.map(decode_field_idx(rest))
          #(StructGetS(type_idx, field_idx), rest)
        }
        4 -> {
          use #(type_idx, rest) <- result.try(decode_type_idx(rest))
          use #(field_idx, rest) <- result.map(decode_field_idx(rest))
          #(StructGetU(type_idx, field_idx), rest)
        }
        5 -> {
          use #(type_idx, rest) <- result.try(decode_type_idx(rest))
          use #(field_idx, rest) <- result.map(decode_field_idx(rest))
          #(StructSet(type_idx, field_idx), rest)
        }
        6 -> {
          use #(type_idx, rest) <- result.map(decode_type_idx(rest))
          #(ArrayNew(type_idx), rest)
        }
        7 -> {
          use #(type_idx, rest) <- result.map(decode_type_idx(rest))
          #(ArrayNewDefault(type_idx), rest)
        }
        8 -> {
          use #(type_idx, rest) <- result.try(decode_type_idx(rest))
          use #(n, rest) <- result.map(decode_u32(rest))
          #(ArrayNewFixed(type_idx, n), rest)
        }
        9 -> {
          use #(type_idx, rest) <- result.try(decode_type_idx(rest))
          use #(data_idx, rest) <- result.map(decode_data_idx(rest))
          #(ArrayNewData(type_idx, data_idx), rest)
        }
        10 -> {
          use #(type_idx, rest) <- result.try(decode_type_idx(rest))
          use #(elem_idx, rest) <- result.map(decode_elem_idx(rest))
          #(ArrayNewElem(type_idx, elem_idx), rest)
        }
        11 -> {
          use #(idx, rest) <- result.map(decode_type_idx(rest))
          #(ArrayGet(idx), rest)
        }
        12 -> {
          use #(idx, rest) <- result.map(decode_type_idx(rest))
          #(ArrayGetS(idx), rest)
        }
        13 -> {
          use #(idx, rest) <- result.map(decode_type_idx(rest))
          #(ArrayGetU(idx), rest)
        }
        14 -> {
          use #(idx, rest) <- result.map(decode_type_idx(rest))
          #(ArraySet(idx), rest)
        }
        15 -> Ok(#(ArrayLen, rest))
        16 -> {
          use #(idx, rest) <- result.map(decode_type_idx(rest))
          #(ArrayFill(idx), rest)
        }
        17 -> {
          use #(id, rest) <- result.try(decode_type_idx(rest))
          use #(id2, rest) <- result.map(decode_type_idx(rest))
          #(ArrayCopy(id, id2), rest)
        }
        18 -> {
          use #(id, rest) <- result.try(decode_type_idx(rest))
          use #(id2, rest) <- result.map(decode_data_idx(rest))
          #(ArrayInitData(id, id2), rest)
        }
        19 -> {
          use #(id, rest) <- result.try(decode_type_idx(rest))
          use #(id2, rest) <- result.map(decode_elem_idx(rest))
          #(ArrayInitElem(id, id2), rest)
        }
        20 -> {
          use #(ht, rest) <- result.map(decode_heap_type(rest))
          #(RefTest(HeapTypeRefType(ht, False)), rest)
        }
        21 -> {
          use #(ht, rest) <- result.map(decode_heap_type(rest))
          case ht {
            NoFuncHeapType -> #(RefTest(NoFuncRefType), rest)
            NoExternHeapType -> #(RefTest(NoExternRefType), rest)
            NoneHeapType -> #(RefTest(NoneRefType), rest)
            FuncHeapType -> #(RefTest(FuncRefType), rest)
            ExternHeapType -> #(RefTest(ExternRefType), rest)
            AnyHeapType -> #(RefTest(AnyRefType), rest)
            EqHeapType -> #(RefTest(EqRefType), rest)
            I31HeapType -> #(RefTest(I31RefType), rest)
            StructHeapType -> #(RefTest(StructRefType), rest)
            ArrayHeapType -> #(RefTest(ArrayRefType), rest)
            _ -> #(RefTest(HeapTypeRefType(ht, True)), rest)
          }
        }
        22 -> {
          use #(ht, rest) <- result.map(decode_heap_type(rest))
          #(RefCast(HeapTypeRefType(ht, False)), rest)
        }
        23 -> {
          use #(ht, rest) <- result.map(decode_heap_type(rest))
          #(RefCast(HeapTypeRefType(ht, False)), rest)
        }
        24 -> {
          use #(#(null_1, null_2), rest) <- result.try(decode_cast_flags(rest))
          use #(label_idx, rest) <- result.try(decode_label_idx(rest))
          use #(ht1, rest) <- result.try(decode_heap_type(rest))
          use #(ht2, rest) <- result.map(decode_heap_type(rest))
          #(
            BrOnCast(
              label_idx,
              HeapTypeRefType(ht1, null_1),
              HeapTypeRefType(ht2, null_2),
            ),
            rest,
          )
        }
        25 -> {
          use #(#(null_1, null_2), rest) <- result.try(decode_cast_flags(rest))
          use #(label_idx, rest) <- result.try(decode_label_idx(rest))
          use #(ht1, rest) <- result.try(decode_heap_type(rest))
          use #(ht2, rest) <- result.map(decode_heap_type(rest))
          #(
            BrOnCastFail(
              label_idx,
              HeapTypeRefType(ht1, null_1),
              HeapTypeRefType(ht2, null_2),
            ),
            rest,
          )
        }
        26 -> Ok(#(AnyConvertExtern, rest))
        27 -> Ok(#(ExternConvertAny, rest))
        28 -> Ok(#(RefI31, rest))
        29 -> Ok(#(I31GetS, rest))
        30 -> Ok(#(I31GetU, rest))
        _ -> Error("Invalid ref instruction")
      }
    }
    <<0xfc, rest:bits>> -> {
      use #(op, rest) <- result.try(values.decode_u32(rest))
      case op |> unwrap_u32 {
        0 -> Ok(#(I32TruncSatF32S, rest))
        1 -> Ok(#(I32TruncSatF32U, rest))
        2 -> Ok(#(I32TruncSatF64S, rest))
        3 -> Ok(#(I32TruncSatF64U, rest))
        4 -> Ok(#(I64TruncSatF32S, rest))
        5 -> Ok(#(I64TruncSatF32U, rest))
        6 -> Ok(#(I64TruncSatF64S, rest))
        7 -> Ok(#(I64TruncSatF64U, rest))
        8 -> {
          use #(idx, rest) <- result.try(decode_data_idx(rest))
          case rest {
            <<0x00, rest:bits>> -> Ok(#(MemoryInit(idx), rest))
            _ -> Error("Invalid memory init instruction")
          }
        }
        9 -> {
          use #(idx, rest) <- result.map(decode_data_idx(rest))
          #(DataDrop(idx), rest)
        }
        10 -> {
          case rest {
            <<0x00, 0x00, rest:bits>> -> Ok(#(MemoryCopy, rest))
            _ -> Error("Invalid memory copy instruction")
          }
        }
        11 -> {
          case rest {
            <<0x00, rest:bits>> -> Ok(#(MemoryFill, rest))
            _ -> Error("Invalid memory fill instruction")
          }
        }
        12 -> {
          use #(idx, rest) <- result.try(decode_elem_idx(rest))
          use #(idx2, rest) <- result.map(decode_table_idx(rest))
          #(TableInit(idx, idx2), rest)
        }
        13 -> {
          use #(idx, rest) <- result.map(decode_elem_idx(rest))
          #(ElemDrop(idx), rest)
        }
        14 -> {
          use #(idx, rest) <- result.try(decode_table_idx(rest))
          use #(idx2, rest) <- result.map(decode_table_idx(rest))
          #(TableCopy(idx, idx2), rest)
        }
        15 -> {
          use #(idx, rest) <- result.map(decode_table_idx(rest))
          #(TableGrow(idx), rest)
        }
        16 -> {
          use #(idx, rest) <- result.map(decode_table_idx(rest))
          #(TableSize(idx), rest)
        }
        17 -> {
          use #(idx, rest) <- result.map(decode_table_idx(rest))
          #(TableFill(idx), rest)
        }
        _ -> Error("Invalid table instruction")
      }
    }
    <<0xfd, rest:bits>> -> {
      use #(op, rest) <- result.try(values.decode_u32(rest))
      case op |> unwrap_u32 {
        0 -> {
          use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
          #(V128Load(mem_arg), rest)
        }
        1 -> {
          use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
          #(V128Load8x8S(mem_arg), rest)
        }
        2 -> {
          use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
          #(V128Load8x8U(mem_arg), rest)
        }
        3 -> {
          use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
          #(V128Load16x4S(mem_arg), rest)
        }
        4 -> {
          use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
          #(V128Load16x4U(mem_arg), rest)
        }
        5 -> {
          use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
          #(V128Load32x2S(mem_arg), rest)
        }
        6 -> {
          use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
          #(V128Load32x2U(mem_arg), rest)
        }
        7 -> {
          use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
          #(V128Load8Splat(mem_arg), rest)
        }
        8 -> {
          use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
          #(V128Load16Splat(mem_arg), rest)
        }
        9 -> {
          use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
          #(V128Load32Splat(mem_arg), rest)
        }
        10 -> {
          use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
          #(V128Load64Splat(mem_arg), rest)
        }
        92 -> {
          use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
          #(V128Load32Zero(mem_arg), rest)
        }
        93 -> {
          use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
          #(V128Load64Zero(mem_arg), rest)
        }
        11 -> {
          use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
          #(V128Store(mem_arg), rest)
        }
        84 -> {
          use #(mem_arg, rest) <- result.try(decode_mem_arg(rest))
          use #(lane, rest) <- result.map(decode_lane_16(rest))
          #(V128Load8Lane(mem_arg, lane), rest)
        }
        85 -> {
          use #(mem_arg, rest) <- result.try(decode_mem_arg(rest))
          use #(lane, rest) <- result.map(decode_lane_8(rest))
          #(V128Load16Lane(mem_arg, lane), rest)
        }
        86 -> {
          use #(mem_arg, rest) <- result.try(decode_mem_arg(rest))
          use #(lane, rest) <- result.map(decode_lane_4(rest))
          #(V128Load32Lane(mem_arg, lane), rest)
        }
        87 -> {
          use #(mem_arg, rest) <- result.try(decode_mem_arg(rest))
          use #(lane, rest) <- result.map(decode_lane_2(rest))
          #(V128Load64Lane(mem_arg, lane), rest)
        }
        88 -> {
          use #(mem_arg, rest) <- result.try(decode_mem_arg(rest))
          use #(lane, rest) <- result.map(decode_lane_16(rest))
          #(V128Store8Lane(mem_arg, lane), rest)
        }
        89 -> {
          use #(mem_arg, rest) <- result.try(decode_mem_arg(rest))
          use #(lane, rest) <- result.map(decode_lane_8(rest))
          #(V128Store16Lane(mem_arg, lane), rest)
        }
        90 -> {
          use #(mem_arg, rest) <- result.try(decode_mem_arg(rest))
          use #(lane, rest) <- result.map(decode_lane_4(rest))
          #(V128Store32Lane(mem_arg, lane), rest)
        }
        91 -> {
          use #(mem_arg, rest) <- result.try(decode_mem_arg(rest))
          use #(lane, rest) <- result.map(decode_lane_2(rest))
          #(V128Store64Lane(mem_arg, lane), rest)
        }
        12 -> {
          case rest {
            <<bits:128-bits, rest:bits>> -> {
              use val <- result.map(v128(bits))
              #(V128Const(val), rest)
            }
            _ -> Error("Invalid v128 instruction")
          }
        }
        13 -> {
          use #(i0, rest) <- result.try(decode_lane_16(rest))
          use #(i1, rest) <- result.try(decode_lane_16(rest))
          use #(i2, rest) <- result.try(decode_lane_16(rest))
          use #(i3, rest) <- result.try(decode_lane_16(rest))
          use #(i4, rest) <- result.try(decode_lane_16(rest))
          use #(i5, rest) <- result.try(decode_lane_16(rest))
          use #(i6, rest) <- result.try(decode_lane_16(rest))
          use #(i7, rest) <- result.try(decode_lane_16(rest))
          use #(i8, rest) <- result.try(decode_lane_16(rest))
          use #(i9, rest) <- result.try(decode_lane_16(rest))
          use #(i10, rest) <- result.try(decode_lane_16(rest))
          use #(i11, rest) <- result.try(decode_lane_16(rest))
          use #(i12, rest) <- result.try(decode_lane_16(rest))
          use #(i13, rest) <- result.try(decode_lane_16(rest))
          use #(i14, rest) <- result.try(decode_lane_16(rest))
          use #(i15, rest) <- result.map(decode_lane_16(rest))
          #(
            I8x16Shuffle(
              i0,
              i1,
              i2,
              i3,
              i4,
              i5,
              i6,
              i7,
              i8,
              i9,
              i10,
              i11,
              i12,
              i13,
              i14,
              i15,
            ),
            rest,
          )
        }
        21 -> {
          use #(idx, rest) <- result.map(decode_lane_16(rest))
          #(I8x16ExtractLaneS(idx), rest)
        }
        22 -> {
          use #(idx, rest) <- result.map(decode_lane_16(rest))
          #(I8x16ExtractLaneU(idx), rest)
        }
        23 -> {
          use #(idx, rest) <- result.map(decode_lane_16(rest))
          #(I8x16ReplaceLane(idx), rest)
        }
        24 -> {
          use #(idx, rest) <- result.map(decode_lane_8(rest))
          #(I16x8ExtractLaneS(idx), rest)
        }
        25 -> {
          use #(idx, rest) <- result.map(decode_lane_8(rest))
          #(I16x8ExtractLaneU(idx), rest)
        }
        26 -> {
          use #(idx, rest) <- result.map(decode_lane_8(rest))
          #(I16x8ReplaceLane(idx), rest)
        }
        27 -> {
          use #(idx, rest) <- result.map(decode_lane_4(rest))
          #(I32x4ExtractLane(idx), rest)
        }
        28 -> {
          use #(idx, rest) <- result.map(decode_lane_4(rest))
          #(I32x4ReplaceLane(idx), rest)
        }
        29 -> {
          use #(idx, rest) <- result.map(decode_lane_2(rest))
          #(I64x2ExtractLane(idx), rest)
        }
        30 -> {
          use #(idx, rest) <- result.map(decode_lane_2(rest))
          #(I64x2ReplaceLane(idx), rest)
        }
        31 -> {
          use #(idx, rest) <- result.map(decode_lane_4(rest))
          #(F32x4ExtractLane(idx), rest)
        }
        32 -> {
          use #(idx, rest) <- result.map(decode_lane_4(rest))
          #(F32x4ReplaceLane(idx), rest)
        }
        33 -> {
          use #(idx, rest) <- result.map(decode_lane_2(rest))
          #(F64x2ExtractLane(idx), rest)
        }
        34 -> {
          use #(idx, rest) <- result.map(decode_lane_2(rest))
          #(F64x2ReplaceLane(idx), rest)
        }
        14 -> Ok(#(I8x16Swizzle, rest))
        15 -> Ok(#(I8x16Splat, rest))
        16 -> Ok(#(I16x8Splat, rest))
        17 -> Ok(#(I32x4Splat, rest))
        18 -> Ok(#(I64x2Splat, rest))
        19 -> Ok(#(F32x4Splat, rest))
        20 -> Ok(#(F64x2Splat, rest))
        35 -> Ok(#(I8x16Eq, rest))
        36 -> Ok(#(I8x16Ne, rest))
        37 -> Ok(#(I8x16LtS, rest))
        38 -> Ok(#(I8x16LtU, rest))
        39 -> Ok(#(I8x16GtS, rest))
        40 -> Ok(#(I8x16GtU, rest))
        41 -> Ok(#(I8x16LeS, rest))
        42 -> Ok(#(I8x16LeU, rest))
        43 -> Ok(#(I8x16GeS, rest))
        44 -> Ok(#(I8x16GeU, rest))
        45 -> Ok(#(I16x8Eq, rest))
        46 -> Ok(#(I16x8Ne, rest))
        47 -> Ok(#(I16x8LtS, rest))
        48 -> Ok(#(I16x8LtU, rest))
        49 -> Ok(#(I16x8GtS, rest))
        50 -> Ok(#(I16x8GtU, rest))
        51 -> Ok(#(I16x8LeS, rest))
        52 -> Ok(#(I16x8LeU, rest))
        53 -> Ok(#(I16x8GeS, rest))
        54 -> Ok(#(I16x8GeU, rest))
        55 -> Ok(#(I32x4Eq, rest))
        56 -> Ok(#(I32x4Ne, rest))
        57 -> Ok(#(I32x4LtS, rest))
        58 -> Ok(#(I32x4LtU, rest))
        59 -> Ok(#(I32x4GtS, rest))
        60 -> Ok(#(I32x4GtU, rest))
        61 -> Ok(#(I32x4LeS, rest))
        62 -> Ok(#(I32x4LeU, rest))
        63 -> Ok(#(I32x4GeS, rest))
        64 -> Ok(#(I32x4GeU, rest))
        214 -> Ok(#(I64x2Eq, rest))
        215 -> Ok(#(I64x2Ne, rest))
        216 -> Ok(#(I64x2LtS, rest))
        217 -> Ok(#(I64x2GtS, rest))
        218 -> Ok(#(I64x2LeS, rest))
        219 -> Ok(#(I64x2GeS, rest))
        65 -> Ok(#(F32x4Eq, rest))
        66 -> Ok(#(F32x4Ne, rest))
        67 -> Ok(#(F32x4Lt, rest))
        68 -> Ok(#(F32x4Gt, rest))
        69 -> Ok(#(F32x4Le, rest))
        70 -> Ok(#(F32x4Ge, rest))
        71 -> Ok(#(F64x2Eq, rest))
        72 -> Ok(#(F64x2Ne, rest))
        73 -> Ok(#(F64x2Lt, rest))
        74 -> Ok(#(F64x2Gt, rest))
        75 -> Ok(#(F64x2Le, rest))
        76 -> Ok(#(F64x2Ge, rest))
        77 -> Ok(#(V128Not, rest))
        78 -> Ok(#(V128And, rest))
        79 -> Ok(#(V128AndNot, rest))
        80 -> Ok(#(V128Or, rest))
        81 -> Ok(#(V128Xor, rest))
        82 -> Ok(#(V128Bitselect, rest))
        83 -> Ok(#(V128AnyTrue, rest))
        96 -> Ok(#(I8x16Abs, rest))
        97 -> Ok(#(I8x16Neg, rest))
        98 -> Ok(#(I8x16Popcnt, rest))
        99 -> Ok(#(I8x16AllTrue, rest))
        100 -> Ok(#(I8x16Bitmask, rest))
        101 -> Ok(#(I8x16NarrowI16x8S, rest))
        102 -> Ok(#(I8x16NarrowI16x8U, rest))
        107 -> Ok(#(I8x16Shl, rest))
        108 -> Ok(#(I8x16ShrS, rest))
        109 -> Ok(#(I8x16ShrU, rest))
        110 -> Ok(#(I8x16Add, rest))
        111 -> Ok(#(I8x16AddSatS, rest))
        112 -> Ok(#(I8x16AddSatU, rest))
        113 -> Ok(#(I8x16Sub, rest))
        114 -> Ok(#(I8x16SubSatS, rest))
        115 -> Ok(#(I8x16SubSatU, rest))
        118 -> Ok(#(I8x16MinS, rest))
        119 -> Ok(#(I8x16MinU, rest))
        120 -> Ok(#(I8x16MaxS, rest))
        121 -> Ok(#(I8x16MaxU, rest))
        123 -> Ok(#(I8x16AvgrU, rest))
        124 -> Ok(#(I16x8ExtaddPairwiseI8x16S, rest))
        125 -> Ok(#(I16x8ExtaddPairwiseI8x16U, rest))
        128 -> Ok(#(I16x8Abs, rest))
        129 -> Ok(#(I16x8Neg, rest))
        130 -> Ok(#(I16x8Q15mulrSatS, rest))
        131 -> Ok(#(I16x8AllTrue, rest))
        132 -> Ok(#(I16x8Bitmask, rest))
        133 -> Ok(#(I16x8NarrowI32x4S, rest))
        134 -> Ok(#(I16x8NarrowI32x4U, rest))
        135 -> Ok(#(I16x8ExtendLowI8x16S, rest))
        136 -> Ok(#(I16x8ExtendHighI8x16S, rest))
        137 -> Ok(#(I16x8ExtendLowI8x16U, rest))
        138 -> Ok(#(I16x8ExtendHighI8x16U, rest))
        139 -> Ok(#(I16x8Shl, rest))
        140 -> Ok(#(I16x8ShrS, rest))
        141 -> Ok(#(I16x8ShrU, rest))
        142 -> Ok(#(I16x8Add, rest))
        143 -> Ok(#(I16x8AddSatS, rest))
        144 -> Ok(#(I16x8AddSatU, rest))
        145 -> Ok(#(I16x8Sub, rest))
        146 -> Ok(#(I16x8SubSatS, rest))
        147 -> Ok(#(I16x8SubSatU, rest))
        149 -> Ok(#(I16x8Mul, rest))
        150 -> Ok(#(I16x8MinS, rest))
        151 -> Ok(#(I16x8MinU, rest))
        152 -> Ok(#(I16x8MaxS, rest))
        153 -> Ok(#(I16x8MaxU, rest))
        155 -> Ok(#(I16x8AvgrU, rest))
        156 -> Ok(#(I16x8ExtmulLowI8x16S, rest))
        157 -> Ok(#(I16x8ExtmulHighI8x16S, rest))
        158 -> Ok(#(I16x8ExtmulLowI8x16U, rest))
        159 -> Ok(#(I16x8ExtmulHighI8x16U, rest))
        126 -> Ok(#(I32x4ExtaddPairwiseI16x8S, rest))
        127 -> Ok(#(I32x4ExtaddPairwiseI16x8U, rest))
        160 -> Ok(#(I32x4Abs, rest))
        161 -> Ok(#(I32x4Neg, rest))
        163 -> Ok(#(I32x4AllTrue, rest))
        164 -> Ok(#(I32x4Bitmask, rest))
        167 -> Ok(#(I32x4ExtendLowI16x8S, rest))
        168 -> Ok(#(I32x4ExtendHighI16x8S, rest))
        169 -> Ok(#(I32x4ExtendLowI16x8U, rest))
        170 -> Ok(#(I32x4ExtendHighI16x8U, rest))
        171 -> Ok(#(I32x4Shl, rest))
        172 -> Ok(#(I32x4ShrS, rest))
        173 -> Ok(#(I32x4ShrU, rest))
        174 -> Ok(#(I32x4Add, rest))
        177 -> Ok(#(I32x4Sub, rest))
        181 -> Ok(#(I32x4Mul, rest))
        182 -> Ok(#(I32x4MinS, rest))
        183 -> Ok(#(I32x4MinU, rest))
        184 -> Ok(#(I32x4MaxS, rest))
        185 -> Ok(#(I32x4MaxU, rest))
        187 -> Ok(#(I32x4DotI16x8S, rest))
        188 -> Ok(#(I32x4ExtmulLowI16x8S, rest))
        189 -> Ok(#(I32x4ExtmulHighI16x8S, rest))
        190 -> Ok(#(I32x4ExtmulLowI16x8U, rest))
        191 -> Ok(#(I32x4ExtmulHighI16x8U, rest))
        192 -> Ok(#(I64x2Abs, rest))
        193 -> Ok(#(I64x2Neg, rest))
        195 -> Ok(#(I64x2AllTrue, rest))
        196 -> Ok(#(I64x2Bitmask, rest))
        199 -> Ok(#(I64x2ExtendLowI32x4S, rest))
        200 -> Ok(#(I64x2ExtendHighI32x4S, rest))
        201 -> Ok(#(I64x2ExtendLowI32x4U, rest))
        202 -> Ok(#(I64x2ExtendHighI32x4U, rest))
        203 -> Ok(#(I64x2Shl, rest))
        204 -> Ok(#(I64x2ShrS, rest))
        205 -> Ok(#(I64x2ShrU, rest))
        206 -> Ok(#(I64x2Add, rest))
        209 -> Ok(#(I64x2Sub, rest))
        213 -> Ok(#(I64x2Mul, rest))
        220 -> Ok(#(I64x2ExtmulLowI32x4S, rest))
        221 -> Ok(#(I64x2ExtmulHighI32x4S, rest))
        222 -> Ok(#(I64x2ExtmulLowI32x4U, rest))
        223 -> Ok(#(I64x2ExtmulHighI32x4U, rest))
        103 -> Ok(#(F32x4Ceil, rest))
        104 -> Ok(#(F32x4Floor, rest))
        105 -> Ok(#(F32x4Trunc, rest))
        106 -> Ok(#(F32x4Nearest, rest))
        224 -> Ok(#(F32x4Abs, rest))
        225 -> Ok(#(F32x4Neg, rest))
        227 -> Ok(#(F32x4Sqrt, rest))
        228 -> Ok(#(F32x4Add, rest))
        229 -> Ok(#(F32x4Sub, rest))
        230 -> Ok(#(F32x4Mul, rest))
        231 -> Ok(#(F32x4Div, rest))
        232 -> Ok(#(F32x4Min, rest))
        233 -> Ok(#(F32x4Max, rest))
        234 -> Ok(#(F32x4Pmin, rest))
        235 -> Ok(#(F32x4Pmax, rest))
        116 -> Ok(#(F64x2Ceil, rest))
        117 -> Ok(#(F64x2Floor, rest))
        122 -> Ok(#(F64x2Trunc, rest))
        148 -> Ok(#(F64x2Nearest, rest))
        236 -> Ok(#(F64x2Abs, rest))
        237 -> Ok(#(F64x2Neg, rest))
        239 -> Ok(#(F64x2Sqrt, rest))
        240 -> Ok(#(F64x2Add, rest))
        241 -> Ok(#(F64x2Sub, rest))
        242 -> Ok(#(F64x2Mul, rest))
        243 -> Ok(#(F64x2Div, rest))
        244 -> Ok(#(F64x2Min, rest))
        245 -> Ok(#(F64x2Max, rest))
        246 -> Ok(#(F64x2Pmin, rest))
        247 -> Ok(#(F64x2Pmax, rest))
        248 -> Ok(#(I32x4TruncSatF32x4S, rest))
        249 -> Ok(#(I32x4TruncSatF32x4U, rest))
        250 -> Ok(#(F32x4ConvertI32x4S, rest))
        251 -> Ok(#(F32x4ConvertI32x4U, rest))
        252 -> Ok(#(I32x4TruncSatF64x2SZero, rest))
        253 -> Ok(#(I32x4TruncSatF64x2UZero, rest))
        254 -> Ok(#(F64x2ConvertLowI32x4S, rest))
        255 -> Ok(#(F64x2ConvertLowI32x4U, rest))
        94 -> Ok(#(F32x4DemoteF64x2Zero, rest))
        95 -> Ok(#(F64x2PromoteLowF32x4, rest))
        _ -> Error("Invalid instruction")
      }
    }
    <<0x1A, rest:bits>> -> Ok(#(Drop, rest))
    <<0x1B, rest:bits>> -> Ok(#(Select, rest))
    <<0x1C, rest:bits>> -> {
      use #(idxs, rest) <- result.map(common.decode_vec(rest, decode_val_type))
      #(SelectT(idxs), rest)
    }
    <<0x20, rest:bits>> -> {
      use #(idx, rest) <- result.map(decode_local_idx(rest))
      #(LocalGet(idx), rest)
    }
    <<0x21, rest:bits>> -> {
      use #(idx, rest) <- result.map(decode_local_idx(rest))
      #(LocalSet(idx), rest)
    }
    <<0x22, rest:bits>> -> {
      use #(idx, rest) <- result.map(decode_local_idx(rest))
      #(LocalTee(idx), rest)
    }
    <<0x23, rest:bits>> -> {
      use #(idx, rest) <- result.map(decode_global_idx(rest))
      #(GlobalGet(idx), rest)
    }
    <<0x24, rest:bits>> -> {
      use #(idx, rest) <- result.map(decode_global_idx(rest))
      #(GlobalSet(idx), rest)
    }
    <<0x25, rest:bits>> -> {
      use #(idx, rest) <- result.map(decode_table_idx(rest))
      #(TableGet(idx), rest)
    }
    <<0x26, rest:bits>> -> {
      use #(idx, rest) <- result.map(decode_table_idx(rest))
      #(TableSet(idx), rest)
    }
    <<0x28, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I32Load(mem_arg), rest)
    }
    <<0x29, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I64Load(mem_arg), rest)
    }
    <<0x2A, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(F32Load(mem_arg), rest)
    }
    <<0x2B, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(F64Load(mem_arg), rest)
    }
    <<0x2C, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I32Load8S(mem_arg), rest)
    }
    <<0x2D, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I32Load8U(mem_arg), rest)
    }
    <<0x2E, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I32Load16S(mem_arg), rest)
    }
    <<0x2F, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I32Load16U(mem_arg), rest)
    }
    <<0x30, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I64Load8S(mem_arg), rest)
    }
    <<0x31, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I64Load8U(mem_arg), rest)
    }
    <<0x32, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I64Load16S(mem_arg), rest)
    }
    <<0x33, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I64Load16U(mem_arg), rest)
    }
    <<0x34, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I64Load32U(mem_arg), rest)
    }
    <<0x35, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I64Load32S(mem_arg), rest)
    }
    <<0x36, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I32Store(mem_arg), rest)
    }
    <<0x37, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I64Store(mem_arg), rest)
    }
    <<0x38, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(F32Store(mem_arg), rest)
    }
    <<0x39, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(F64Store(mem_arg), rest)
    }
    <<0x3A, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I32Store8(mem_arg), rest)
    }
    <<0x3B, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I32Store16(mem_arg), rest)
    }
    <<0x3C, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I64Store8(mem_arg), rest)
    }
    <<0x3D, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I64Store16(mem_arg), rest)
    }
    <<0x3E, rest:bits>> -> {
      use #(mem_arg, rest) <- result.map(decode_mem_arg(rest))
      #(I64Store32(mem_arg), rest)
    }
    <<0x3F, 0x00, rest:bits>> -> Ok(#(MemorySize, rest))
    <<0x40, 0x00, rest:bits>> -> Ok(#(MemoryGrow, rest))
    <<0x41, rest:bits>> -> {
      use #(i32_value, rest) <- result.map(decode_i32(rest))
      #(I32Const(i32_value), rest)
    }
    <<0x42, rest:bits>> -> {
      use #(i64_value, rest) <- result.map(decode_i64(rest))
      #(I64Const(i64_value), rest)
    }
    <<0x43, rest:bits>> -> {
      use #(f32_value, rest) <- result.map(decode_f32(rest))
      #(F32Const(f32_value), rest)
    }
    <<0x44, rest:bits>> -> {
      use #(f64_value, rest) <- result.map(decode_f64(rest))
      #(F64Const(f64_value), rest)
    }
    <<0x45, rest:bits>> -> Ok(#(I32Eqz, rest))
    <<0x46, rest:bits>> -> Ok(#(I32Eq, rest))
    <<0x47, rest:bits>> -> Ok(#(I32Ne, rest))
    <<0x48, rest:bits>> -> Ok(#(I32LtS, rest))
    <<0x49, rest:bits>> -> Ok(#(I32LtU, rest))
    <<0x4A, rest:bits>> -> Ok(#(I32GtS, rest))
    <<0x4B, rest:bits>> -> Ok(#(I32GtU, rest))
    <<0x4C, rest:bits>> -> Ok(#(I32LeS, rest))
    <<0x4D, rest:bits>> -> Ok(#(I32LeU, rest))
    <<0x4E, rest:bits>> -> Ok(#(I32GeS, rest))
    <<0x4F, rest:bits>> -> Ok(#(I32GeU, rest))
    <<0x50, rest:bits>> -> Ok(#(I64Eqz, rest))
    <<0x51, rest:bits>> -> Ok(#(I64Eq, rest))
    <<0x52, rest:bits>> -> Ok(#(I64Ne, rest))
    <<0x53, rest:bits>> -> Ok(#(I64LtS, rest))
    <<0x54, rest:bits>> -> Ok(#(I64LtU, rest))
    <<0x55, rest:bits>> -> Ok(#(I64GtS, rest))
    <<0x56, rest:bits>> -> Ok(#(I64GtU, rest))
    <<0x57, rest:bits>> -> Ok(#(I64LeS, rest))
    <<0x58, rest:bits>> -> Ok(#(I64LeU, rest))
    <<0x59, rest:bits>> -> Ok(#(I64GeS, rest))
    <<0x5A, rest:bits>> -> Ok(#(I64GeU, rest))
    <<0x5B, rest:bits>> -> Ok(#(F32Eq, rest))
    <<0x5C, rest:bits>> -> Ok(#(F32Ne, rest))
    <<0x5D, rest:bits>> -> Ok(#(F32Lt, rest))
    <<0x5E, rest:bits>> -> Ok(#(F32Gt, rest))
    <<0x5F, rest:bits>> -> Ok(#(F32Le, rest))
    <<0x60, rest:bits>> -> Ok(#(F32Ge, rest))
    <<0x61, rest:bits>> -> Ok(#(F64Eq, rest))
    <<0x62, rest:bits>> -> Ok(#(F64Ne, rest))
    <<0x63, rest:bits>> -> Ok(#(F64Lt, rest))
    <<0x64, rest:bits>> -> Ok(#(F64Gt, rest))
    <<0x65, rest:bits>> -> Ok(#(F64Le, rest))
    <<0x66, rest:bits>> -> Ok(#(F64Ge, rest))
    <<0x67, rest:bits>> -> Ok(#(I32Clz, rest))
    <<0x68, rest:bits>> -> Ok(#(I32Ctz, rest))
    <<0x69, rest:bits>> -> Ok(#(I32Popcnt, rest))
    <<0x6A, rest:bits>> -> Ok(#(I32Add, rest))
    <<0x6B, rest:bits>> -> Ok(#(I32Sub, rest))
    <<0x6C, rest:bits>> -> Ok(#(I32Mul, rest))
    <<0x6D, rest:bits>> -> Ok(#(I32DivS, rest))
    <<0x6E, rest:bits>> -> Ok(#(I32DivU, rest))
    <<0x6F, rest:bits>> -> Ok(#(I32RemS, rest))
    <<0x70, rest:bits>> -> Ok(#(I32RemU, rest))
    <<0x71, rest:bits>> -> Ok(#(I32And, rest))
    <<0x72, rest:bits>> -> Ok(#(I32Or, rest))
    <<0x73, rest:bits>> -> Ok(#(I32Xor, rest))
    <<0x74, rest:bits>> -> Ok(#(I32Shl, rest))
    <<0x75, rest:bits>> -> Ok(#(I32ShrS, rest))
    <<0x76, rest:bits>> -> Ok(#(I32ShrU, rest))
    <<0x77, rest:bits>> -> Ok(#(I32Rotl, rest))
    <<0x78, rest:bits>> -> Ok(#(I32Rotr, rest))
    <<0x79, rest:bits>> -> Ok(#(I64Clz, rest))
    <<0x7A, rest:bits>> -> Ok(#(I64Ctz, rest))
    <<0x7B, rest:bits>> -> Ok(#(I64Popcnt, rest))
    <<0x7C, rest:bits>> -> Ok(#(I64Add, rest))
    <<0x7D, rest:bits>> -> Ok(#(I64Sub, rest))
    <<0x7E, rest:bits>> -> Ok(#(I64Mul, rest))
    <<0x7F, rest:bits>> -> Ok(#(I64DivS, rest))
    <<0x80, rest:bits>> -> Ok(#(I64DivU, rest))
    <<0x81, rest:bits>> -> Ok(#(I64RemS, rest))
    <<0x82, rest:bits>> -> Ok(#(I64RemU, rest))
    <<0x83, rest:bits>> -> Ok(#(I64And, rest))
    <<0x84, rest:bits>> -> Ok(#(I64Or, rest))
    <<0x85, rest:bits>> -> Ok(#(I64Xor, rest))
    <<0x86, rest:bits>> -> Ok(#(I64Shl, rest))
    <<0x87, rest:bits>> -> Ok(#(I64ShrS, rest))
    <<0x88, rest:bits>> -> Ok(#(I64ShrU, rest))
    <<0x89, rest:bits>> -> Ok(#(I64Rotl, rest))
    <<0x8A, rest:bits>> -> Ok(#(I64Rotr, rest))
    <<0x8B, rest:bits>> -> Ok(#(F32Abs, rest))
    <<0x8C, rest:bits>> -> Ok(#(F32Neg, rest))
    <<0x8D, rest:bits>> -> Ok(#(F32Ceil, rest))
    <<0x8E, rest:bits>> -> Ok(#(F32Floor, rest))
    <<0x8F, rest:bits>> -> Ok(#(F32Trunc, rest))
    <<0x90, rest:bits>> -> Ok(#(F32Nearest, rest))
    <<0x91, rest:bits>> -> Ok(#(F32Sqrt, rest))
    <<0x92, rest:bits>> -> Ok(#(F32Add, rest))
    <<0x93, rest:bits>> -> Ok(#(F32Sub, rest))
    <<0x94, rest:bits>> -> Ok(#(F32Mul, rest))
    <<0x95, rest:bits>> -> Ok(#(F32Div, rest))
    <<0x96, rest:bits>> -> Ok(#(F32Min, rest))
    <<0x97, rest:bits>> -> Ok(#(F32Max, rest))
    <<0x98, rest:bits>> -> Ok(#(F32Copysign, rest))
    <<0x99, rest:bits>> -> Ok(#(F64Abs, rest))
    <<0x9A, rest:bits>> -> Ok(#(F64Neg, rest))
    <<0x9B, rest:bits>> -> Ok(#(F64Ceil, rest))
    <<0x9C, rest:bits>> -> Ok(#(F64Floor, rest))
    <<0x9D, rest:bits>> -> Ok(#(F64Trunc, rest))
    <<0x9E, rest:bits>> -> Ok(#(F64Nearest, rest))
    <<0x9F, rest:bits>> -> Ok(#(F64Sqrt, rest))
    <<0xA0, rest:bits>> -> Ok(#(F64Add, rest))
    <<0xA1, rest:bits>> -> Ok(#(F64Sub, rest))
    <<0xA2, rest:bits>> -> Ok(#(F64Mul, rest))
    <<0xA3, rest:bits>> -> Ok(#(F64Div, rest))
    <<0xA4, rest:bits>> -> Ok(#(F64Min, rest))
    <<0xA5, rest:bits>> -> Ok(#(F64Max, rest))
    <<0xA6, rest:bits>> -> Ok(#(F64Copysign, rest))
    <<0xA7, rest:bits>> -> Ok(#(I32WrapI64, rest))
    <<0xA8, rest:bits>> -> Ok(#(I32TruncF32S, rest))
    <<0xA9, rest:bits>> -> Ok(#(I32TruncF32U, rest))
    <<0xAA, rest:bits>> -> Ok(#(I32TruncF64S, rest))
    <<0xAB, rest:bits>> -> Ok(#(I32TruncF64U, rest))
    <<0xAC, rest:bits>> -> Ok(#(I64ExtendI32S, rest))
    <<0xAD, rest:bits>> -> Ok(#(I64ExtendI32U, rest))
    <<0xAE, rest:bits>> -> Ok(#(I64TruncF32S, rest))
    <<0xAF, rest:bits>> -> Ok(#(I64TruncF32U, rest))
    <<0xB0, rest:bits>> -> Ok(#(I64TruncF64S, rest))
    <<0xB1, rest:bits>> -> Ok(#(I64TruncF64U, rest))
    <<0xB2, rest:bits>> -> Ok(#(F32ConvertI32S, rest))
    <<0xB3, rest:bits>> -> Ok(#(F32ConvertI32U, rest))
    <<0xB4, rest:bits>> -> Ok(#(F32ConvertI64S, rest))
    <<0xB5, rest:bits>> -> Ok(#(F32ConvertI64U, rest))
    <<0xB6, rest:bits>> -> Ok(#(F32DemoteF64, rest))
    <<0xB7, rest:bits>> -> Ok(#(F64ConvertI32S, rest))
    <<0xB8, rest:bits>> -> Ok(#(F64ConvertI32U, rest))
    <<0xB9, rest:bits>> -> Ok(#(F64ConvertI64S, rest))
    <<0xBA, rest:bits>> -> Ok(#(F64ConvertI64U, rest))
    <<0xBB, rest:bits>> -> Ok(#(F64PromoteF32, rest))
    <<0xBC, rest:bits>> -> Ok(#(I32ReinterpretF32, rest))
    <<0xBD, rest:bits>> -> Ok(#(I64ReinterpretF64, rest))
    <<0xBE, rest:bits>> -> Ok(#(F32ReinterpretI32, rest))
    <<0xBF, rest:bits>> -> Ok(#(F64ReinterpretI64, rest))
    <<0xC0, rest:bits>> -> Ok(#(I32Extend8S, rest))
    <<0xC1, rest:bits>> -> Ok(#(I32Extend16S, rest))
    <<0xC2, rest:bits>> -> Ok(#(I64Extend8S, rest))
    <<0xC3, rest:bits>> -> Ok(#(I64Extend16S, rest))
    <<0xC4, rest:bits>> -> Ok(#(I64ExtendI32S, rest))
    _ -> Error("Invalid instruction")
  }
}

pub fn encode_expression(
  builder: BytesBuilder,
  expr: Expr,
) -> Result(BytesBuilder, String) {
  do_encode_expression(builder, expr.insts)
}

fn do_encode_expression(
  builder: BytesBuilder,
  expr: FingerTree(Instruction),
) -> Result(BytesBuilder, String) {
  case finger_tree.shift(expr) {
    Ok(#(inst, rest)) -> {
      use builder <- result.try(encode_instruction(builder, inst))
      do_encode_expression(builder, rest)
    }
    Error(_) -> Ok(builder |> bytes_builder.append(<<0x0B>>))
  }
}

fn encode_instruction(
  builder: BytesBuilder,
  inst: Instruction,
) -> Result(BytesBuilder, String) {
  case inst {
    End -> Ok(builder |> bytes_builder.append(<<0x0B>>))
    Unreachable -> Ok(builder |> bytes_builder.append(<<0x00>>))
    Nop -> Ok(builder |> bytes_builder.append(<<0x01>>))
    Block(bt, expr) -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x02>>)
        |> encode_block_type(bt),
      )
      builder |> encode_expression(expr)
    }
    Loop(bt, expr) -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x03>>)
        |> encode_block_type(bt),
      )
      builder |> encode_expression(expr)
    }
    If(bt, expr, else_expr) -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x04>>)
        |> encode_block_type(bt),
      )
      use builder <- result.try(builder |> encode_expression(expr))
      case else_expr {
        Some(else_expr) -> builder |> encode_expression(else_expr)
        None -> Ok(builder)
      }
    }
    Else -> Ok(builder |> bytes_builder.append(<<0x05>>))
    Br(idx) ->
      builder
      |> bytes_builder.append(<<0x0C>>)
      |> encode_label_idx(idx)
    BrIf(idx) ->
      builder
      |> bytes_builder.append(<<0x0D>>)
      |> encode_label_idx(idx)
    BrTable(label_idxs, default_label_idx) -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x0E>>)
        |> common.encode_vec(label_idxs, encode_label_idx),
      )
      builder |> encode_label_idx(default_label_idx)
    }
    Return -> Ok(builder |> bytes_builder.append(<<0x0F>>))
    Call(func_idx) ->
      builder
      |> bytes_builder.append(<<0x10>>)
      |> encode_func_idx(func_idx)
    CallIndirect(table_idx, type_idx) -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x11>>)
        |> encode_type_idx(type_idx),
      )
      builder |> encode_table_idx(table_idx)
    }
    ReturnCall(func_idx) ->
      builder
      |> bytes_builder.append(<<0x12>>)
      |> encode_func_idx(func_idx)
    ReturnCallIndirect(table_idx, type_idx) -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x13>>)
        |> encode_table_idx(table_idx),
      )
      builder |> encode_type_idx(type_idx)
    }
    CallRef(type_idx) ->
      builder
      |> bytes_builder.append(<<0x14>>)
      |> encode_type_idx(type_idx)
    ReturnCallRef(type_idx) ->
      builder
      |> bytes_builder.append(<<0x15>>)
      |> encode_type_idx(type_idx)
    BrOnNull(label_idx) ->
      builder
      |> bytes_builder.append(<<0xd5>>)
      |> encode_label_idx(label_idx)
    BrOnNonNull(idx) ->
      builder
      |> bytes_builder.append(<<0xd6>>)
      |> encode_label_idx(idx)
    RefNull(ht) ->
      builder
      |> bytes_builder.append(<<0xd0>>)
      |> encode_heap_type(ht)
    RefIsNull -> Ok(builder |> bytes_builder.append(<<0xd1>>))
    RefFunc(idx) ->
      builder
      |> bytes_builder.append(<<0xd2>>)
      |> encode_func_idx(idx)

    RefEq -> Ok(builder |> bytes_builder.append(<<0xd3>>))
    RefAsNonNull -> Ok(builder |> bytes_builder.append(<<0xd4>>))
    StructNew(idx) -> {
      use op <- result.try(u32(0))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
      |> encode_type_idx(idx)
    }
    StructNewDefault(idx) -> {
      use op <- result.try(u32(1))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
      |> encode_type_idx(idx)
    }
    StructGet(type_idx, field_idx) -> {
      use op <- result.try(u32(2))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfb>>)
        |> encode_u32(op)
        |> encode_type_idx(type_idx),
      )
      builder |> encode_field_idx(field_idx)
    }
    StructGetS(type_idx, field_idx) -> {
      use op <- result.try(u32(3))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfb>>)
        |> encode_u32(op)
        |> encode_type_idx(type_idx),
      )
      builder |> encode_field_idx(field_idx)
    }
    StructGetU(type_idx, field_idx) -> {
      use op <- result.try(u32(4))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfb>>)
        |> encode_u32(op)
        |> encode_type_idx(type_idx),
      )
      builder |> encode_field_idx(field_idx)
    }
    StructSet(type_idx, field_idx) -> {
      use op <- result.try(u32(5))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfb>>)
        |> encode_u32(op)
        |> encode_type_idx(type_idx),
      )
      builder |> encode_field_idx(field_idx)
    }
    ArrayNew(type_idx) -> {
      use op <- result.try(u32(6))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
      |> encode_type_idx(type_idx)
    }
    ArrayNewDefault(type_idx) -> {
      use op <- result.try(u32(7))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
      |> encode_type_idx(type_idx)
    }
    ArrayNewFixed(type_idx, n) -> {
      use op <- result.try(u32(8))
      use builder <- result.map(
        builder
        |> bytes_builder.append(<<0xfb>>)
        |> encode_u32(op)
        |> encode_type_idx(type_idx),
      )
      builder |> encode_u32(n)
    }
    ArrayNewData(type_idx, data_idx) -> {
      use op <- result.try(u32(9))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfb>>)
        |> encode_u32(op)
        |> encode_type_idx(type_idx),
      )
      builder |> encode_data_idx(data_idx)
    }
    ArrayNewElem(type_idx, elem_idx) -> {
      use op <- result.try(u32(10))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfb>>)
        |> encode_u32(op)
        |> encode_type_idx(type_idx),
      )
      builder |> encode_elem_idx(elem_idx)
    }
    ArrayGet(idx) -> {
      use op <- result.try(u32(11))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
      |> encode_type_idx(idx)
    }
    ArrayGetS(idx) -> {
      use op <- result.try(u32(12))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
      |> encode_type_idx(idx)
    }
    ArrayGetU(idx) -> {
      use op <- result.try(u32(13))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
      |> encode_type_idx(idx)
    }
    ArraySet(idx) -> {
      use op <- result.try(u32(14))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
      |> encode_type_idx(idx)
    }
    ArrayLen -> {
      use op <- result.map(u32(15))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
    }
    ArrayFill(idx) -> {
      use op <- result.try(u32(16))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
      |> encode_type_idx(idx)
    }
    ArrayCopy(id, id2) -> {
      use op <- result.try(u32(17))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfb>>)
        |> encode_u32(op)
        |> encode_type_idx(id),
      )
      builder |> encode_type_idx(id2)
    }
    ArrayInitData(id, id2) -> {
      use op <- result.try(u32(18))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfb>>)
        |> encode_u32(op)
        |> encode_type_idx(id),
      )
      builder |> encode_data_idx(id2)
    }
    ArrayInitElem(id, id2) -> {
      use op <- result.try(u32(19))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfb>>)
        |> encode_u32(op)
        |> encode_type_idx(id),
      )
      builder |> encode_elem_idx(id2)
    }
    RefTest(rt) -> {
      let op = case rt |> types.ref_type_is_nullable {
        True -> 21
        False -> 20
      }
      use op <- result.try(u32(op))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
      |> encode_heap_type(rt |> types.ref_type_unwrap_heap_type)
    }
    RefCast(rt) -> {
      let op = case rt |> types.ref_type_is_nullable {
        True -> 23
        False -> 22
      }
      use op <- result.try(u32(op))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
      |> encode_heap_type(rt |> types.ref_type_unwrap_heap_type)
    }
    BrOnCast(label_idx, rt1, rt2) -> {
      use op <- result.try(u32(24))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfb>>)
        |> encode_u32(op)
        |> encode_cast_flags(#(
          rt1 |> types.ref_type_is_nullable,
          rt2 |> types.ref_type_is_nullable,
        )),
      )
      use builder <- result.try(
        builder
        |> encode_label_idx(label_idx),
      )
      use builder <- result.try(
        builder
        |> encode_heap_type(rt1 |> types.ref_type_unwrap_heap_type),
      )
      builder |> encode_heap_type(rt2 |> types.ref_type_unwrap_heap_type)
    }
    BrOnCastFail(label_idx, rt1, rt2) -> {
      use op <- result.try(u32(25))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfb>>)
        |> encode_u32(op)
        |> encode_cast_flags(#(
          rt1 |> types.ref_type_is_nullable,
          rt2 |> types.ref_type_is_nullable,
        )),
      )
      use builder <- result.try(
        builder
        |> encode_label_idx(label_idx),
      )
      use builder <- result.try(
        builder
        |> encode_heap_type(rt1 |> types.ref_type_unwrap_heap_type),
      )
      builder |> encode_heap_type(rt2 |> types.ref_type_unwrap_heap_type)
    }
    AnyConvertExtern -> {
      use op <- result.map(u32(26))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
    }
    ExternConvertAny -> {
      use op <- result.map(u32(27))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
    }
    RefI31 -> {
      use op <- result.map(u32(28))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
    }
    I31GetS -> {
      use op <- result.map(u32(29))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
    }
    I31GetU -> {
      use op <- result.map(u32(30))
      builder
      |> bytes_builder.append(<<0xfb>>)
      |> encode_u32(op)
    }
    I32TruncSatF32S -> {
      use op <- result.map(u32(0))
      builder
      |> bytes_builder.append(<<0xfc>>)
      |> encode_u32(op)
    }
    I32TruncSatF32U -> {
      use op <- result.map(u32(1))
      builder
      |> bytes_builder.append(<<0xfc>>)
      |> encode_u32(op)
    }
    I32TruncSatF64S -> {
      use op <- result.map(u32(2))
      builder
      |> bytes_builder.append(<<0xfc>>)
      |> encode_u32(op)
    }
    I32TruncSatF64U -> {
      use op <- result.map(u32(3))
      builder
      |> bytes_builder.append(<<0xfc>>)
      |> encode_u32(op)
    }
    I64TruncSatF32S -> {
      use op <- result.map(u32(4))
      builder
      |> bytes_builder.append(<<0xfc>>)
      |> encode_u32(op)
    }
    I64TruncSatF32U -> {
      use op <- result.map(u32(5))
      builder
      |> bytes_builder.append(<<0xfc>>)
      |> encode_u32(op)
    }
    I64TruncSatF64S -> {
      use op <- result.map(u32(6))
      builder
      |> bytes_builder.append(<<0xfc>>)
      |> encode_u32(op)
    }
    I64TruncSatF64U -> {
      use op <- result.map(u32(7))
      builder
      |> bytes_builder.append(<<0xfc>>)
      |> encode_u32(op)
    }
    V128Load(mem_arg) -> {
      use op <- result.try(u32(0))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_mem_arg(mem_arg)
    }
    V128Load8x8S(mem_arg) -> {
      use op <- result.try(u32(1))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_mem_arg(mem_arg)
    }
    V128Load8x8U(mem_arg) -> {
      use op <- result.try(u32(2))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_mem_arg(mem_arg)
    }
    V128Load16x4S(mem_arg) -> {
      use op <- result.try(u32(3))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_mem_arg(mem_arg)
    }
    V128Load16x4U(mem_arg) -> {
      use op <- result.try(u32(4))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_mem_arg(mem_arg)
    }
    V128Load32x2S(mem_arg) -> {
      use op <- result.try(u32(5))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_mem_arg(mem_arg)
    }
    V128Load32x2U(mem_arg) -> {
      use op <- result.try(u32(6))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_mem_arg(mem_arg)
    }
    V128Load8Splat(mem_arg) -> {
      use op <- result.try(u32(7))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_mem_arg(mem_arg)
    }
    V128Load16Splat(mem_arg) -> {
      use op <- result.try(u32(8))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_mem_arg(mem_arg)
    }
    V128Load32Splat(mem_arg) -> {
      use op <- result.try(u32(9))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_mem_arg(mem_arg)
    }
    V128Load64Splat(mem_arg) -> {
      use op <- result.try(u32(10))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_mem_arg(mem_arg)
    }
    V128Load32Zero(mem_arg) -> {
      use op <- result.try(u32(92))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_mem_arg(mem_arg)
    }
    V128Load64Zero(mem_arg) -> {
      use op <- result.try(u32(93))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_mem_arg(mem_arg)
    }
    V128Store(mem_arg) -> {
      use op <- result.try(u32(11))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_mem_arg(mem_arg)
    }
    V128Load8Lane(mem_arg, lane) -> {
      use op <- result.try(u32(84))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfd>>)
        |> encode_u32(op)
        |> encode_mem_arg(mem_arg),
      )
      builder |> encode_lane_16(lane)
    }
    V128Load16Lane(mem_arg, lane) -> {
      use op <- result.try(u32(85))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfd>>)
        |> encode_u32(op)
        |> encode_mem_arg(mem_arg),
      )
      builder |> encode_lane_8(lane)
    }
    V128Load32Lane(mem_arg, lane) -> {
      use op <- result.try(u32(86))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfd>>)
        |> encode_u32(op)
        |> encode_mem_arg(mem_arg),
      )
      builder |> encode_lane_4(lane)
    }
    V128Load64Lane(mem_arg, lane) -> {
      use op <- result.try(u32(87))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfd>>)
        |> encode_u32(op)
        |> encode_mem_arg(mem_arg),
      )
      builder |> encode_lane_2(lane)
    }
    V128Store8Lane(mem_arg, lane) -> {
      use op <- result.try(u32(88))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfd>>)
        |> encode_u32(op)
        |> encode_mem_arg(mem_arg),
      )
      builder |> encode_lane_16(lane)
    }
    V128Store16Lane(mem_arg, lane) -> {
      use op <- result.try(u32(89))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfd>>)
        |> encode_u32(op)
        |> encode_mem_arg(mem_arg),
      )
      builder |> encode_lane_8(lane)
    }
    V128Store32Lane(mem_arg, lane) -> {
      use op <- result.try(u32(90))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfd>>)
        |> encode_u32(op)
        |> encode_mem_arg(mem_arg),
      )
      builder |> encode_lane_4(lane)
    }
    V128Store64Lane(mem_arg, lane) -> {
      use op <- result.try(u32(91))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfd>>)
        |> encode_u32(op)
        |> encode_mem_arg(mem_arg),
      )
      builder |> encode_lane_2(lane)
    }
    V128Const(val) -> {
      use op <- result.map(u32(12))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> bytes_builder.append(val |> unwrap_v128)
    }
    I8x16Shuffle(
      i0,
      i1,
      i2,
      i3,
      i4,
      i5,
      i6,
      i7,
      i8,
      i9,
      i10,
      i11,
      i12,
      i13,
      i14,
      i15,
    ) -> {
      use op <- result.try(u32(13))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfd>>)
        |> encode_u32(op)
        |> encode_lane_16(i0),
      )
      use builder <- result.try(builder |> encode_lane_16(i1))
      use builder <- result.try(builder |> encode_lane_16(i2))
      use builder <- result.try(builder |> encode_lane_16(i3))
      use builder <- result.try(builder |> encode_lane_16(i4))
      use builder <- result.try(builder |> encode_lane_16(i5))
      use builder <- result.try(builder |> encode_lane_16(i6))
      use builder <- result.try(builder |> encode_lane_16(i7))
      use builder <- result.try(builder |> encode_lane_16(i8))
      use builder <- result.try(builder |> encode_lane_16(i9))
      use builder <- result.try(builder |> encode_lane_16(i10))
      use builder <- result.try(builder |> encode_lane_16(i11))
      use builder <- result.try(builder |> encode_lane_16(i12))
      use builder <- result.try(builder |> encode_lane_16(i13))
      use builder <- result.try(builder |> encode_lane_16(i14))
      builder |> encode_lane_16(i15)
    }
    I8x16ExtractLaneS(idx) -> {
      use op <- result.try(u32(21))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_lane_16(idx)
    }
    I8x16ExtractLaneU(idx) -> {
      use op <- result.try(u32(22))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_lane_16(idx)
    }
    I8x16ReplaceLane(idx) -> {
      use op <- result.try(u32(23))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_lane_16(idx)
    }
    I16x8ExtractLaneS(idx) -> {
      use op <- result.try(u32(24))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_lane_8(idx)
    }
    I16x8ExtractLaneU(idx) -> {
      use op <- result.try(u32(25))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_lane_8(idx)
    }
    I16x8ReplaceLane(idx) -> {
      use op <- result.try(u32(26))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_lane_8(idx)
    }
    I32x4ExtractLane(idx) -> {
      use op <- result.try(u32(27))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_lane_4(idx)
    }
    I32x4ReplaceLane(idx) -> {
      use op <- result.try(u32(28))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_lane_4(idx)
    }
    I64x2ExtractLane(idx) -> {
      use op <- result.try(u32(29))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_lane_2(idx)
    }
    I64x2ReplaceLane(idx) -> {
      use op <- result.try(u32(30))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_lane_2(idx)
    }
    F32x4ExtractLane(idx) -> {
      use op <- result.try(u32(31))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_lane_4(idx)
    }
    F32x4ReplaceLane(idx) -> {
      use op <- result.try(u32(32))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_lane_4(idx)
    }
    F64x2ExtractLane(idx) -> {
      use op <- result.try(u32(33))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_lane_2(idx)
    }
    F64x2ReplaceLane(idx) -> {
      use op <- result.try(u32(34))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
      |> encode_lane_2(idx)
    }
    I8x16Swizzle -> {
      use op <- result.map(u32(14))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16Splat -> {
      use op <- result.map(u32(15))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8Splat -> {
      use op <- result.map(u32(16))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4Splat -> {
      use op <- result.map(u32(17))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2Splat -> {
      use op <- result.map(u32(18))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Splat -> {
      use op <- result.map(u32(19))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Splat -> {
      use op <- result.map(u32(20))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16Eq -> {
      use op <- result.map(u32(35))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16Ne -> {
      use op <- result.map(u32(36))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16LtS -> {
      use op <- result.map(u32(37))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16LtU -> {
      use op <- result.map(u32(38))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16GtS -> {
      use op <- result.map(u32(39))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16GtU -> {
      use op <- result.map(u32(40))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16LeS -> {
      use op <- result.map(u32(41))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16LeU -> {
      use op <- result.map(u32(42))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16GeS -> {
      use op <- result.map(u32(43))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16GeU -> {
      use op <- result.map(u32(44))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8Eq -> {
      use op <- result.map(u32(45))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8Ne -> {
      use op <- result.map(u32(46))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8LtS -> {
      use op <- result.map(u32(47))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8LtU -> {
      use op <- result.map(u32(48))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8GtS -> {
      use op <- result.map(u32(49))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8GtU -> {
      use op <- result.map(u32(50))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8LeS -> {
      use op <- result.map(u32(51))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8LeU -> {
      use op <- result.map(u32(52))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8GeS -> {
      use op <- result.map(u32(53))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8GeU -> {
      use op <- result.map(u32(54))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4Eq -> {
      use op <- result.map(u32(55))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4Ne -> {
      use op <- result.map(u32(56))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4LtS -> {
      use op <- result.map(u32(57))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4LtU -> {
      use op <- result.map(u32(58))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4GtS -> {
      use op <- result.map(u32(59))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4GtU -> {
      use op <- result.map(u32(60))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4LeS -> {
      use op <- result.map(u32(61))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4LeU -> {
      use op <- result.map(u32(62))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4GeS -> {
      use op <- result.map(u32(63))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4GeU -> {
      use op <- result.map(u32(64))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2Eq -> {
      use op <- result.map(u32(214))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2Ne -> {
      use op <- result.map(u32(215))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2LtS -> {
      use op <- result.map(u32(216))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2GtS -> {
      use op <- result.map(u32(217))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2LeS -> {
      use op <- result.map(u32(218))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2GeS -> {
      use op <- result.map(u32(219))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Eq -> {
      use op <- result.map(u32(65))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Ne -> {
      use op <- result.map(u32(66))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Lt -> {
      use op <- result.map(u32(67))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Gt -> {
      use op <- result.map(u32(68))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Le -> {
      use op <- result.map(u32(69))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Ge -> {
      use op <- result.map(u32(70))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Eq -> {
      use op <- result.map(u32(71))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Ne -> {
      use op <- result.map(u32(72))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Lt -> {
      use op <- result.map(u32(73))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Gt -> {
      use op <- result.map(u32(74))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Le -> {
      use op <- result.map(u32(75))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Ge -> {
      use op <- result.map(u32(76))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    V128Not -> {
      use op <- result.map(u32(77))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    V128And -> {
      use op <- result.map(u32(78))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    V128AndNot -> {
      use op <- result.map(u32(79))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    V128Or -> {
      use op <- result.map(u32(80))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    V128Xor -> {
      use op <- result.map(u32(81))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    V128Bitselect -> {
      use op <- result.map(u32(82))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    V128AnyTrue -> {
      use op <- result.map(u32(83))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16Abs -> {
      use op <- result.map(u32(96))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16Neg -> {
      use op <- result.map(u32(97))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16Popcnt -> {
      use op <- result.map(u32(98))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16AllTrue -> {
      use op <- result.map(u32(99))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16Bitmask -> {
      use op <- result.map(u32(100))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16NarrowI16x8S -> {
      use op <- result.map(u32(101))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16NarrowI16x8U -> {
      use op <- result.map(u32(102))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16Shl -> {
      use op <- result.map(u32(107))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16ShrS -> {
      use op <- result.map(u32(108))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16ShrU -> {
      use op <- result.map(u32(109))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16Add -> {
      use op <- result.map(u32(110))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16AddSatS -> {
      use op <- result.map(u32(111))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16AddSatU -> {
      use op <- result.map(u32(112))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16Sub -> {
      use op <- result.map(u32(113))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16SubSatS -> {
      use op <- result.map(u32(114))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16SubSatU -> {
      use op <- result.map(u32(115))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16MinS -> {
      use op <- result.map(u32(118))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16MinU -> {
      use op <- result.map(u32(119))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16MaxS -> {
      use op <- result.map(u32(120))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16MaxU -> {
      use op <- result.map(u32(121))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I8x16AvgrU -> {
      use op <- result.map(u32(123))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8ExtaddPairwiseI8x16S -> {
      use op <- result.map(u32(124))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8ExtaddPairwiseI8x16U -> {
      use op <- result.map(u32(125))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8Abs -> {
      use op <- result.map(u32(128))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8Neg -> {
      use op <- result.map(u32(129))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8Q15mulrSatS -> {
      use op <- result.map(u32(130))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8AllTrue -> {
      use op <- result.map(u32(131))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8Bitmask -> {
      use op <- result.map(u32(132))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8NarrowI32x4S -> {
      use op <- result.map(u32(133))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8NarrowI32x4U -> {
      use op <- result.map(u32(134))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8ExtendLowI8x16S -> {
      use op <- result.map(u32(135))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8ExtendHighI8x16S -> {
      use op <- result.map(u32(136))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8ExtendLowI8x16U -> {
      use op <- result.map(u32(137))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8ExtendHighI8x16U -> {
      use op <- result.map(u32(138))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8Shl -> {
      use op <- result.map(u32(139))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8ShrS -> {
      use op <- result.map(u32(140))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8ShrU -> {
      use op <- result.map(u32(141))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8Add -> {
      use op <- result.map(u32(142))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8AddSatS -> {
      use op <- result.map(u32(143))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8AddSatU -> {
      use op <- result.map(u32(144))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8Sub -> {
      use op <- result.map(u32(145))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8SubSatS -> {
      use op <- result.map(u32(146))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8SubSatU -> {
      use op <- result.map(u32(147))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8Mul -> {
      use op <- result.map(u32(149))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8MinS -> {
      use op <- result.map(u32(150))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8MinU -> {
      use op <- result.map(u32(151))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8MaxS -> {
      use op <- result.map(u32(152))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8MaxU -> {
      use op <- result.map(u32(153))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8AvgrU -> {
      use op <- result.map(u32(155))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8ExtmulLowI8x16S -> {
      use op <- result.map(u32(156))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8ExtmulHighI8x16S -> {
      use op <- result.map(u32(157))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8ExtmulLowI8x16U -> {
      use op <- result.map(u32(158))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I16x8ExtmulHighI8x16U -> {
      use op <- result.map(u32(159))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4ExtaddPairwiseI16x8S -> {
      use op <- result.map(u32(126))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4ExtaddPairwiseI16x8U -> {
      use op <- result.map(u32(127))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4Abs -> {
      use op <- result.map(u32(160))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4Neg -> {
      use op <- result.map(u32(161))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4AllTrue -> {
      use op <- result.map(u32(163))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4Bitmask -> {
      use op <- result.map(u32(164))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4ExtendLowI16x8S -> {
      use op <- result.map(u32(167))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4ExtendHighI16x8S -> {
      use op <- result.map(u32(168))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4ExtendLowI16x8U -> {
      use op <- result.map(u32(169))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4ExtendHighI16x8U -> {
      use op <- result.map(u32(170))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4Shl -> {
      use op <- result.map(u32(171))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4ShrS -> {
      use op <- result.map(u32(172))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4ShrU -> {
      use op <- result.map(u32(173))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4Add -> {
      use op <- result.map(u32(174))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4Sub -> {
      use op <- result.map(u32(177))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4Mul -> {
      use op <- result.map(u32(181))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4MinS -> {
      use op <- result.map(u32(182))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4MinU -> {
      use op <- result.map(u32(183))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4MaxS -> {
      use op <- result.map(u32(184))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4MaxU -> {
      use op <- result.map(u32(185))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4DotI16x8S -> {
      use op <- result.map(u32(187))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4ExtmulLowI16x8S -> {
      use op <- result.map(u32(188))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4ExtmulHighI16x8S -> {
      use op <- result.map(u32(189))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4ExtmulLowI16x8U -> {
      use op <- result.map(u32(190))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4ExtmulHighI16x8U -> {
      use op <- result.map(u32(191))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2Abs -> {
      use op <- result.map(u32(192))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2Neg -> {
      use op <- result.map(u32(193))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2AllTrue -> {
      use op <- result.map(u32(195))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2Bitmask -> {
      use op <- result.map(u32(196))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2ExtendLowI32x4S -> {
      use op <- result.map(u32(199))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2ExtendHighI32x4S -> {
      use op <- result.map(u32(200))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2ExtendLowI32x4U -> {
      use op <- result.map(u32(201))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2ExtendHighI32x4U -> {
      use op <- result.map(u32(202))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2Shl -> {
      use op <- result.map(u32(203))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2ShrS -> {
      use op <- result.map(u32(204))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2ShrU -> {
      use op <- result.map(u32(205))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2Add -> {
      use op <- result.map(u32(206))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2Sub -> {
      use op <- result.map(u32(209))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2Mul -> {
      use op <- result.map(u32(213))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2ExtmulLowI32x4S -> {
      use op <- result.map(u32(220))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2ExtmulHighI32x4S -> {
      use op <- result.map(u32(221))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2ExtmulLowI32x4U -> {
      use op <- result.map(u32(222))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I64x2ExtmulHighI32x4U -> {
      use op <- result.map(u32(223))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Ceil -> {
      use op <- result.map(u32(103))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Floor -> {
      use op <- result.map(u32(104))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Trunc -> {
      use op <- result.map(u32(105))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Nearest -> {
      use op <- result.map(u32(106))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Abs -> {
      use op <- result.map(u32(224))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Neg -> {
      use op <- result.map(u32(225))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Sqrt -> {
      use op <- result.map(u32(227))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Add -> {
      use op <- result.map(u32(228))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Sub -> {
      use op <- result.map(u32(229))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Mul -> {
      use op <- result.map(u32(230))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Div -> {
      use op <- result.map(u32(231))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Min -> {
      use op <- result.map(u32(232))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Max -> {
      use op <- result.map(u32(233))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Pmin -> {
      use op <- result.map(u32(234))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4Pmax -> {
      use op <- result.map(u32(235))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Ceil -> {
      use op <- result.map(u32(116))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Floor -> {
      use op <- result.map(u32(117))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Trunc -> {
      use op <- result.map(u32(122))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Nearest -> {
      use op <- result.map(u32(148))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Abs -> {
      use op <- result.map(u32(236))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Neg -> {
      use op <- result.map(u32(237))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Sqrt -> {
      use op <- result.map(u32(239))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Add -> {
      use op <- result.map(u32(240))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Sub -> {
      use op <- result.map(u32(241))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Mul -> {
      use op <- result.map(u32(242))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Div -> {
      use op <- result.map(u32(243))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Min -> {
      use op <- result.map(u32(244))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Max -> {
      use op <- result.map(u32(245))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Pmin -> {
      use op <- result.map(u32(246))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2Pmax -> {
      use op <- result.map(u32(247))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4TruncSatF32x4S -> {
      use op <- result.map(u32(248))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4TruncSatF32x4U -> {
      use op <- result.map(u32(249))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4ConvertI32x4S -> {
      use op <- result.map(u32(250))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4ConvertI32x4U -> {
      use op <- result.map(u32(251))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4TruncSatF64x2SZero -> {
      use op <- result.map(u32(252))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    I32x4TruncSatF64x2UZero -> {
      use op <- result.map(u32(253))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2ConvertLowI32x4S -> {
      use op <- result.map(u32(254))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2ConvertLowI32x4U -> {
      use op <- result.map(u32(255))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F32x4DemoteF64x2Zero -> {
      use op <- result.map(u32(94))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }
    F64x2PromoteLowF32x4 -> {
      use op <- result.map(u32(95))
      builder
      |> bytes_builder.append(<<0xfd>>)
      |> encode_u32(op)
    }

    Drop -> Ok(builder |> bytes_builder.append(<<0x1A>>))
    Select -> Ok(builder |> bytes_builder.append(<<0x1B>>))
    SelectT(idxs) ->
      builder
      |> bytes_builder.append(<<0x1C>>)
      |> common.encode_vec(idxs, encode_val_type)
    LocalGet(idx) ->
      builder
      |> bytes_builder.append(<<0x20>>)
      |> encode_local_idx(idx)
    LocalSet(idx) ->
      builder
      |> bytes_builder.append(<<0x21>>)
      |> encode_local_idx(idx)
    LocalTee(idx) ->
      builder
      |> bytes_builder.append(<<0x22>>)
      |> encode_local_idx(idx)
    GlobalGet(idx) ->
      builder
      |> bytes_builder.append(<<0x23>>)
      |> encode_global_idx(idx)
    GlobalSet(idx) ->
      builder
      |> bytes_builder.append(<<0x24>>)
      |> encode_global_idx(idx)
    TableGet(idx) ->
      builder
      |> bytes_builder.append(<<0x25>>)
      |> encode_table_idx(idx)
    TableSet(idx) ->
      builder
      |> bytes_builder.append(<<0x26>>)
      |> encode_table_idx(idx)
    MemoryInit(idx) -> {
      use op <- result.try(u32(8))
      use builder <- result.map(
        builder
        |> bytes_builder.append(<<0xfc>>)
        |> encode_u32(op)
        |> encode_data_idx(idx),
      )
      builder |> bytes_builder.append(<<0x00>>)
    }
    DataDrop(idx) -> {
      use op <- result.try(u32(9))
      builder
      |> bytes_builder.append(<<0xfc>>)
      |> encode_u32(op)
      |> encode_data_idx(idx)
    }
    MemoryCopy -> {
      use op <- result.map(u32(10))
      builder
      |> bytes_builder.append(<<0xfc>>)
      |> encode_u32(op)
      |> bytes_builder.append(<<0x00, 0x00>>)
    }
    MemoryFill -> {
      use op <- result.map(u32(11))
      builder
      |> bytes_builder.append(<<0xfc>>)
      |> encode_u32(op)
      |> bytes_builder.append(<<0x00>>)
    }
    TableInit(idx, idx2) -> {
      use op <- result.try(u32(12))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfc>>)
        |> encode_u32(op)
        |> encode_elem_idx(idx),
      )
      builder |> encode_table_idx(idx2)
    }
    ElemDrop(idx) -> {
      use op <- result.try(u32(13))
      builder
      |> bytes_builder.append(<<0xfc>>)
      |> encode_u32(op)
      |> encode_elem_idx(idx)
    }
    TableCopy(idx, idx2) -> {
      use op <- result.try(u32(14))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0xfc>>)
        |> encode_u32(op)
        |> encode_table_idx(idx),
      )
      builder |> encode_table_idx(idx2)
    }
    TableGrow(idx) -> {
      use op <- result.try(u32(15))
      builder
      |> bytes_builder.append(<<0xfc>>)
      |> encode_u32(op)
      |> encode_table_idx(idx)
    }
    TableSize(idx) -> {
      use op <- result.try(u32(16))
      builder
      |> bytes_builder.append(<<0xfc>>)
      |> encode_u32(op)
      |> encode_table_idx(idx)
    }
    TableFill(idx) -> {
      use op <- result.try(u32(17))
      builder
      |> bytes_builder.append(<<0xfc>>)
      |> encode_u32(op)
      |> encode_table_idx(idx)
    }
    I32Load(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x28>>)
      |> encode_mem_arg(mem_arg)
    I64Load(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x29>>)
      |> encode_mem_arg(mem_arg)
    F32Load(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x2A>>)
      |> encode_mem_arg(mem_arg)
    F64Load(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x2B>>)
      |> encode_mem_arg(mem_arg)
    I32Load8S(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x2C>>)
      |> encode_mem_arg(mem_arg)
    I32Load8U(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x2D>>)
      |> encode_mem_arg(mem_arg)
    I32Load16S(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x2E>>)
      |> encode_mem_arg(mem_arg)
    I32Load16U(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x2F>>)
      |> encode_mem_arg(mem_arg)
    I64Load8S(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x30>>)
      |> encode_mem_arg(mem_arg)
    I64Load8U(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x31>>)
      |> encode_mem_arg(mem_arg)
    I64Load16S(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x32>>)
      |> encode_mem_arg(mem_arg)
    I64Load16U(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x33>>)
      |> encode_mem_arg(mem_arg)
    I64Load32U(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x34>>)
      |> encode_mem_arg(mem_arg)
    I64Load32S(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x35>>)
      |> encode_mem_arg(mem_arg)
    I32Store(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x36>>)
      |> encode_mem_arg(mem_arg)
    I64Store(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x37>>)
      |> encode_mem_arg(mem_arg)
    F32Store(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x38>>)
      |> encode_mem_arg(mem_arg)
    F64Store(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x39>>)
      |> encode_mem_arg(mem_arg)
    I32Store8(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x3A>>)
      |> encode_mem_arg(mem_arg)
    I32Store16(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x3B>>)
      |> encode_mem_arg(mem_arg)
    I64Store8(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x3C>>)
      |> encode_mem_arg(mem_arg)
    I64Store16(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x3D>>)
      |> encode_mem_arg(mem_arg)
    I64Store32(mem_arg) ->
      builder
      |> bytes_builder.append(<<0x3E>>)
      |> encode_mem_arg(mem_arg)
    MemorySize -> Ok(builder |> bytes_builder.append(<<0x3F, 0x00>>))
    MemoryGrow -> Ok(builder |> bytes_builder.append(<<0x40, 0x00>>))
    I32Const(i32_value) ->
      Ok(
        builder
        |> bytes_builder.append(<<0x41>>)
        |> encode_i32(i32_value),
      )
    I64Const(i64_value) ->
      Ok(
        builder
        |> bytes_builder.append(<<0x42>>)
        |> encode_i64(i64_value),
      )
    F32Const(f32_value) ->
      Ok(
        builder
        |> bytes_builder.append(<<0x43>>)
        |> encode_f32(f32_value),
      )
    F64Const(f64_value) ->
      Ok(
        builder
        |> bytes_builder.append(<<0x44>>)
        |> encode_f64(f64_value),
      )
    I32Eqz -> Ok(builder |> bytes_builder.append(<<0x45>>))
    I32Eq -> Ok(builder |> bytes_builder.append(<<0x46>>))
    I32Ne -> Ok(builder |> bytes_builder.append(<<0x47>>))
    I32LtS -> Ok(builder |> bytes_builder.append(<<0x48>>))
    I32LtU -> Ok(builder |> bytes_builder.append(<<0x49>>))
    I32GtS -> Ok(builder |> bytes_builder.append(<<0x4A>>))
    I32GtU -> Ok(builder |> bytes_builder.append(<<0x4B>>))
    I32LeS -> Ok(builder |> bytes_builder.append(<<0x4C>>))
    I32LeU -> Ok(builder |> bytes_builder.append(<<0x4D>>))
    I32GeS -> Ok(builder |> bytes_builder.append(<<0x4E>>))
    I32GeU -> Ok(builder |> bytes_builder.append(<<0x4F>>))
    I64Eqz -> Ok(builder |> bytes_builder.append(<<0x50>>))
    I64Eq -> Ok(builder |> bytes_builder.append(<<0x51>>))
    I64Ne -> Ok(builder |> bytes_builder.append(<<0x52>>))
    I64LtS -> Ok(builder |> bytes_builder.append(<<0x53>>))
    I64LtU -> Ok(builder |> bytes_builder.append(<<0x54>>))
    I64GtS -> Ok(builder |> bytes_builder.append(<<0x55>>))
    I64GtU -> Ok(builder |> bytes_builder.append(<<0x56>>))
    I64LeS -> Ok(builder |> bytes_builder.append(<<0x57>>))
    I64LeU -> Ok(builder |> bytes_builder.append(<<0x58>>))
    I64GeS -> Ok(builder |> bytes_builder.append(<<0x59>>))
    I64GeU -> Ok(builder |> bytes_builder.append(<<0x5A>>))
    F32Eq -> Ok(builder |> bytes_builder.append(<<0x5B>>))
    F32Ne -> Ok(builder |> bytes_builder.append(<<0x5C>>))
    F32Lt -> Ok(builder |> bytes_builder.append(<<0x5D>>))
    F32Gt -> Ok(builder |> bytes_builder.append(<<0x5E>>))
    F32Le -> Ok(builder |> bytes_builder.append(<<0x5F>>))
    F32Ge -> Ok(builder |> bytes_builder.append(<<0x60>>))
    F64Eq -> Ok(builder |> bytes_builder.append(<<0x61>>))
    F64Ne -> Ok(builder |> bytes_builder.append(<<0x62>>))
    F64Lt -> Ok(builder |> bytes_builder.append(<<0x63>>))
    F64Gt -> Ok(builder |> bytes_builder.append(<<0x64>>))
    F64Le -> Ok(builder |> bytes_builder.append(<<0x65>>))
    F64Ge -> Ok(builder |> bytes_builder.append(<<0x66>>))
    I32Clz -> Ok(builder |> bytes_builder.append(<<0x67>>))
    I32Ctz -> Ok(builder |> bytes_builder.append(<<0x68>>))
    I32Popcnt -> Ok(builder |> bytes_builder.append(<<0x69>>))
    I32Add -> Ok(builder |> bytes_builder.append(<<0x6A>>))
    I32Sub -> Ok(builder |> bytes_builder.append(<<0x6B>>))
    I32Mul -> Ok(builder |> bytes_builder.append(<<0x6C>>))
    I32DivS -> Ok(builder |> bytes_builder.append(<<0x6D>>))
    I32DivU -> Ok(builder |> bytes_builder.append(<<0x6E>>))
    I32RemS -> Ok(builder |> bytes_builder.append(<<0x6F>>))
    I32RemU -> Ok(builder |> bytes_builder.append(<<0x70>>))
    I32And -> Ok(builder |> bytes_builder.append(<<0x71>>))
    I32Or -> Ok(builder |> bytes_builder.append(<<0x72>>))
    I32Xor -> Ok(builder |> bytes_builder.append(<<0x73>>))
    I32Shl -> Ok(builder |> bytes_builder.append(<<0x74>>))
    I32ShrS -> Ok(builder |> bytes_builder.append(<<0x75>>))
    I32ShrU -> Ok(builder |> bytes_builder.append(<<0x76>>))
    I32Rotl -> Ok(builder |> bytes_builder.append(<<0x77>>))
    I32Rotr -> Ok(builder |> bytes_builder.append(<<0x78>>))
    I64Clz -> Ok(builder |> bytes_builder.append(<<0x79>>))
    I64Ctz -> Ok(builder |> bytes_builder.append(<<0x7A>>))
    I64Popcnt -> Ok(builder |> bytes_builder.append(<<0x7B>>))
    I64Add -> Ok(builder |> bytes_builder.append(<<0x7C>>))
    I64Sub -> Ok(builder |> bytes_builder.append(<<0x7D>>))
    I64Mul -> Ok(builder |> bytes_builder.append(<<0x7E>>))
    I64DivS -> Ok(builder |> bytes_builder.append(<<0x7F>>))
    I64DivU -> Ok(builder |> bytes_builder.append(<<0x80>>))
    I64RemS -> Ok(builder |> bytes_builder.append(<<0x81>>))
    I64RemU -> Ok(builder |> bytes_builder.append(<<0x82>>))
    I64And -> Ok(builder |> bytes_builder.append(<<0x83>>))
    I64Or -> Ok(builder |> bytes_builder.append(<<0x84>>))
    I64Xor -> Ok(builder |> bytes_builder.append(<<0x85>>))
    I64Shl -> Ok(builder |> bytes_builder.append(<<0x86>>))
    I64ShrS -> Ok(builder |> bytes_builder.append(<<0x87>>))
    I64ShrU -> Ok(builder |> bytes_builder.append(<<0x88>>))
    I64Rotl -> Ok(builder |> bytes_builder.append(<<0x89>>))
    I64Rotr -> Ok(builder |> bytes_builder.append(<<0x8A>>))
    F32Abs -> Ok(builder |> bytes_builder.append(<<0x8B>>))
    F32Neg -> Ok(builder |> bytes_builder.append(<<0x8C>>))
    F32Ceil -> Ok(builder |> bytes_builder.append(<<0x8D>>))
    F32Floor -> Ok(builder |> bytes_builder.append(<<0x8E>>))
    F32Trunc -> Ok(builder |> bytes_builder.append(<<0x8F>>))
    F32Nearest -> Ok(builder |> bytes_builder.append(<<0x90>>))
    F32Sqrt -> Ok(builder |> bytes_builder.append(<<0x91>>))
    F32Add -> Ok(builder |> bytes_builder.append(<<0x92>>))
    F32Sub -> Ok(builder |> bytes_builder.append(<<0x93>>))
    F32Mul -> Ok(builder |> bytes_builder.append(<<0x94>>))
    F32Div -> Ok(builder |> bytes_builder.append(<<0x95>>))
    F32Min -> Ok(builder |> bytes_builder.append(<<0x96>>))
    F32Max -> Ok(builder |> bytes_builder.append(<<0x97>>))
    F32Copysign -> Ok(builder |> bytes_builder.append(<<0x98>>))
    F64Abs -> Ok(builder |> bytes_builder.append(<<0x99>>))
    F64Neg -> Ok(builder |> bytes_builder.append(<<0x9A>>))
    F64Ceil -> Ok(builder |> bytes_builder.append(<<0x9B>>))
    F64Floor -> Ok(builder |> bytes_builder.append(<<0x9C>>))
    F64Trunc -> Ok(builder |> bytes_builder.append(<<0x9D>>))
    F64Nearest -> Ok(builder |> bytes_builder.append(<<0x9E>>))
    F64Sqrt -> Ok(builder |> bytes_builder.append(<<0x9F>>))
    F64Add -> Ok(builder |> bytes_builder.append(<<0xA0>>))
    F64Sub -> Ok(builder |> bytes_builder.append(<<0xA1>>))
    F64Mul -> Ok(builder |> bytes_builder.append(<<0xA2>>))
    F64Div -> Ok(builder |> bytes_builder.append(<<0xA3>>))
    F64Min -> Ok(builder |> bytes_builder.append(<<0xA4>>))
    F64Max -> Ok(builder |> bytes_builder.append(<<0xA5>>))
    F64Copysign -> Ok(builder |> bytes_builder.append(<<0xA6>>))
    I32WrapI64 -> Ok(builder |> bytes_builder.append(<<0xA7>>))
    I32TruncF32S -> Ok(builder |> bytes_builder.append(<<0xA8>>))
    I32TruncF32U -> Ok(builder |> bytes_builder.append(<<0xA9>>))
    I32TruncF64S -> Ok(builder |> bytes_builder.append(<<0xAA>>))
    I32TruncF64U -> Ok(builder |> bytes_builder.append(<<0xAB>>))
    I64Extend32S -> Ok(builder |> bytes_builder.append(<<0xC4>>))
    I64ExtendI32S -> Ok(builder |> bytes_builder.append(<<0xAC>>))
    I64ExtendI32U -> Ok(builder |> bytes_builder.append(<<0xAD>>))
    I64TruncF32S -> Ok(builder |> bytes_builder.append(<<0xAE>>))
    I64TruncF32U -> Ok(builder |> bytes_builder.append(<<0xAF>>))
    I64TruncF64S -> Ok(builder |> bytes_builder.append(<<0xB0>>))
    I64TruncF64U -> Ok(builder |> bytes_builder.append(<<0xB1>>))
    F32ConvertI32S -> Ok(builder |> bytes_builder.append(<<0xB2>>))
    F32ConvertI32U -> Ok(builder |> bytes_builder.append(<<0xB3>>))
    F32ConvertI64S -> Ok(builder |> bytes_builder.append(<<0xB4>>))
    F32ConvertI64U -> Ok(builder |> bytes_builder.append(<<0xB5>>))
    F32DemoteF64 -> Ok(builder |> bytes_builder.append(<<0xB6>>))
    F64ConvertI32S -> Ok(builder |> bytes_builder.append(<<0xB7>>))
    F64ConvertI32U -> Ok(builder |> bytes_builder.append(<<0xB8>>))
    F64ConvertI64S -> Ok(builder |> bytes_builder.append(<<0xB9>>))
    F64ConvertI64U -> Ok(builder |> bytes_builder.append(<<0xBA>>))
    F64PromoteF32 -> Ok(builder |> bytes_builder.append(<<0xBB>>))
    I32ReinterpretF32 -> Ok(builder |> bytes_builder.append(<<0xBC>>))
    I64ReinterpretF64 -> Ok(builder |> bytes_builder.append(<<0xBD>>))
    F32ReinterpretI32 -> Ok(builder |> bytes_builder.append(<<0xBE>>))
    F64ReinterpretI64 -> Ok(builder |> bytes_builder.append(<<0xBF>>))
    I32Extend8S -> Ok(builder |> bytes_builder.append(<<0xC0>>))
    I32Extend16S -> Ok(builder |> bytes_builder.append(<<0xC1>>))
    I64Extend8S -> Ok(builder |> bytes_builder.append(<<0xC2>>))
    I64Extend16S -> Ok(builder |> bytes_builder.append(<<0xC3>>))
  }
}
