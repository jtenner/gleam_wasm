import gleam/bytes_builder.{type BytesBuilder}
import gleam/option.{type Option, None, Some}
import gleam/result
import internal/binary/common
import internal/binary/values.{
  decode_i32, decode_i64, decode_s33, decode_u32, encode_s33, encode_u32,
}
import internal/finger_tree.{type FingerTree, size}
import internal/structure/numbers.{
  decode_f32, decode_f64, s33, u32, unwrap_s33, unwrap_u32, v128,
}
import internal/structure/types.{
  type ArrayType, type BlockType, type CompositeType, type DataIDX, type ElemIDX,
  type FieldIDX, type FieldType, type FuncIDX, type FuncType, type GlobalIDX,
  type GlobalType, type HeapType, type Instruction, type LabelIDX,
  type LaneIDX16, type LaneIDX2, type LaneIDX4, type LaneIDX8, type Limits,
  type LocalIDX, type MemArg, type MemType, type Mut, type RecType, type RefType,
  type ResultType, type StorageType, type StructType, type SubType,
  type TableIDX, type TableType, type TypeIDX, type ValType, AnyConvertExtern,
  AnyHeapType, AnyRefType, ArrayCompositeType, ArrayCopy, ArrayFill, ArrayGet,
  ArrayGetS, ArrayGetU, ArrayHeapType, ArrayInitData, ArrayInitElem, ArrayLen,
  ArrayNew, ArrayNewData, ArrayNewDefault, ArrayNewElem, ArrayNewFixed,
  ArrayRefType, ArraySet, ArrayType, Block, Br, BrIf, BrOnCast, BrOnCastFail,
  BrOnNonNull, BrOnNull, BrTable, Call, CallIndirect, CallRef, ConcreteHeapType,
  Const, DataDrop, DataIDX, Drop, ElemDrop, ElemIDX, Else, End, EqHeapType,
  EqRefType, ExternConvertAny, ExternHeapType, ExternRefType, F32Abs, F32Add,
  F32Ceil, F32Const, F32ConvertI32S, F32ConvertI32U, F32ConvertI64S,
  F32ConvertI64U, F32Copysign, F32DemoteF64, F32Div, F32Eq, F32Floor, F32Ge,
  F32Gt, F32Le, F32Load, F32Lt, F32Max, F32Min, F32Mul, F32Ne, F32Nearest,
  F32Neg, F32ReinterpretI32, F32ReinterpretI64, F32Sqrt, F32Store, F32Sub,
  F32Trunc, F32ValType, F32x4Abs, F32x4Add, F32x4Ceil, F32x4ConvertI32x4S,
  F32x4ConvertI32x4U, F32x4DemoteF64x2Zero, F32x4Div, F32x4Eq, F32x4ExtractLane,
  F32x4Floor, F32x4Ge, F32x4Gt, F32x4Le, F32x4Lt, F32x4Max, F32x4Min, F32x4Mul,
  F32x4Ne, F32x4Nearest, F32x4Neg, F32x4Pmax, F32x4Pmin, F32x4ReplaceLane,
  F32x4Splat, F32x4Sqrt, F32x4Sub, F32x4Trunc, F64Abs, F64Add, F64Ceil, F64Const,
  F64ConvertI32S, F64ConvertI32U, F64ConvertI64S, F64ConvertI64U, F64Copysign,
  F64Div, F64Eq, F64Floor, F64Ge, F64Gt, F64Le, F64Load, F64Lt, F64Max, F64Min,
  F64Mul, F64Ne, F64Nearest, F64Neg, F64PromoteF32, F64ReinterpretI32,
  F64ReinterpretI64, F64Sqrt, F64Store, F64Sub, F64Trunc, F64ValType, F64x2Abs,
  F64x2Add, F64x2Ceil, F64x2ConvertLowI32x4S, F64x2ConvertLowI32x4U, F64x2Div,
  F64x2Eq, F64x2ExtractLane, F64x2Floor, F64x2Ge, F64x2Gt, F64x2Le, F64x2Lt,
  F64x2Max, F64x2Min, F64x2Mul, F64x2Ne, F64x2Nearest, F64x2Neg, F64x2Pmax,
  F64x2Pmin, F64x2PromoteLowF32x4, F64x2ReplaceLane, F64x2Splat, F64x2Sqrt,
  F64x2Sub, F64x2Trunc, FieldIDX, FieldType, FuncCompositeType, FuncHeapType,
  FuncIDX, FuncRefType, FuncType, FuncTypeBlockType, GlobalGet, GlobalIDX,
  GlobalSet, GlobalType, HeapTypeRefType, I16StorageType, I16x8Abs, I16x8Add,
  I16x8AddSatS, I16x8AddSatU, I16x8AllTrue, I16x8AvgrU, I16x8Bitmask, I16x8Eq,
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
  I32Or, I32Popcnt, I32ReinterpretF32, I32ReinterpretF64, I32RemS, I32RemU,
  I32Rotl, I32Rotr, I32Shl, I32ShrS, I32ShrU, I32Store, I32Store16, I32Store8,
  I32Sub, I32TruncF32S, I32TruncF32U, I32TruncF64S, I32TruncF64U,
  I32TruncSatF32S, I32TruncSatF32U, I32TruncSatF64S, I32TruncSatF64U, I32ValType,
  I32WrapI64, I32Xor, I32x4Abs, I32x4Add, I32x4AllTrue, I32x4Bitmask,
  I32x4DotI8x16S, I32x4Eq, I32x4ExtaddPairwiseI16x8S, I32x4ExtaddPairwiseI16x8U,
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
  I64LtU, I64Mul, I64Ne, I64Or, I64Popcnt, I64ReinterpretF32, I64ReinterpretF64,
  I64RemS, I64RemU, I64Rotl, I64Rotr, I64Shl, I64ShrS, I64ShrU, I64Store,
  I64Store16, I64Store32, I64Store8, I64Sub, I64TruncF32S, I64TruncF32U,
  I64TruncF64S, I64TruncF64U, I64TruncSatF32S, I64TruncSatF32U, I64TruncSatF64S,
  I64TruncSatF64U, I64ValType, I64Xor, I64x2Abs, I64x2Add, I64x2AllTrue,
  I64x2Bitmask, I64x2Eq, I64x2ExtendHighI32x4S, I64x2ExtendHighI32x4U,
  I64x2ExtendLowI32x4S, I64x2ExtendLowI32x4U, I64x2ExtractLane, I64x2GeS,
  I64x2GtS, I64x2LeS, I64x2LtS, I64x2Ne, I64x2Neg, I64x2ReplaceLane, I64x2Shl,
  I64x2ShrS, I64x2ShrU, I64x2Splat, I64x2Sub, I64x4ExtmulHighI32x4S,
  I64x4ExtmulHighI32x4U, I64x4ExtmulLowI32x4S, I64x4ExtmulLowI32x4U,
  I8StorageType, I8x16Abs, I8x16Add, I8x16AddSatS, I8x16AddSatU, I8x16AllTrue,
  I8x16AvgrU, I8x16Bitmask, I8x16Eq, I8x16ExtractLaneS, I8x16ExtractLaneU,
  I8x16GeS, I8x16GeU, I8x16GtS, I8x16GtU, I8x16LeS, I8x16LeU, I8x16LtS, I8x16LtU,
  I8x16MaxS, I8x16MaxU, I8x16MinS, I8x16MinU, I8x16Mul, I8x16NarrowI16x8S,
  I8x16NarrowI16x8U, I8x16Ne, I8x16Neg, I8x16Popcnt, I8x16ReplaceLane, I8x16Shl,
  I8x16ShrS, I8x16ShrU, I8x16Shuffle, I8x16Splat, I8x16Sub, I8x16SubSatS,
  I8x16SubSatU, I8x16Swizzle, If, LabelIDX, Limits, LocalGet, LocalIDX, LocalSet,
  LocalTee, Loop, MemArg, MemType, MemoryCopy, MemoryFill, MemoryGrow,
  MemoryInit, MemorySize, NoExternHeapType, NoExternRefType, NoFuncHeapType,
  NoFuncRefType, NoneHeapType, NoneRefType, Nop, RecType, RefAsNonNull, RefCast,
  RefEq, RefFunc, RefI31, RefIsNull, RefNull, RefTest, RefTypeValType,
  ResultType, Return, ReturnCall, ReturnCallIndirect, ReturnCallRef, Select,
  SelectT, StructCompositeType, StructGet, StructGetS, StructGetU,
  StructHeapType, StructNew, StructNewDefault, StructRefType, StructSet,
  StructType, SubType, TableCopy, TableFill, TableGet, TableGrow, TableIDX,
  TableInit, TableSet, TableSize, TableType, TypeIDX, Unreachable, V128And,
  V128Andor, V128AnyTrue, V128Bitselect, V128Const, V128Load, V128Load16Lane,
  V128Load16Splat, V128Load16x4S, V128Load16x4U, V128Load32Lane, V128Load32Splat,
  V128Load32Zero, V128Load32x2S, V128Load32x2U, V128Load64Lane, V128Load64Splat,
  V128Load64Zero, V128Load8Lane, V128Load8Splat, V128Load8x8S, V128Load8x8U,
  V128Not, V128Or, V128Store, V128Store16Lane, V128Store32Lane, V128Store64Lane,
  V128Store8Lane, V128ValType, V128Xor, ValTypeBlockType, ValTypeStorageType,
  Var, VoidBlockType, lane_16, lane_2, lane_4, lane_8,
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
    <<0x64, rest:bits>> -> {
      use #(heap_type, rest) <- result.map(decode_heap_type(rest))
      #(HeapTypeRefType(heap_type, False), rest)
    }
    <<0x63, rest:bits>> -> {
      use #(heap_type, rest) <- result.map(decode_heap_type(rest))
      #(HeapTypeRefType(heap_type, True), rest)
    }
    _ -> Error("Invalid reference type")
  }
}

pub fn encode_ref_type(builder: BytesBuilder, ref_type: RefType) {
  case ref_type {
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
    HeapTypeRefType(ht, null) -> {
      case null {
        False -> builder |> bytes_builder.append(<<0x64>>)
        True -> builder |> bytes_builder.append(<<0x63>>)
      }
      |> encode_heap_type(ht)
    }
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
  }
}

pub fn decode_result_type(bits: BitArray) {
  use #(parameters, rest) <- result.try(common.decode_vec(bits, decode_val_type))
  use #(results, rest) <- result.map(common.decode_vec(rest, decode_val_type))
  #(ResultType(parameters, results), rest)
}

pub fn encode_func_type(builder: BytesBuilder, func_type: FuncType) {
  builder |> encode_result_type(func_type.rt)
}

pub fn encode_result_type(builder: BytesBuilder, result_type: ResultType) {
  use builder <- result.try(
    builder |> common.encode_vec(result_type.parameters, encode_val_type),
  )
  builder |> common.encode_vec(result_type.result, encode_val_type)
}

pub fn decode_func_type(bits: BitArray) {
  use #(rt, rest) <- result.map(decode_result_type(bits))
  #(FuncType(rt), rest)
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

pub fn encode_comp_type(builder: BytesBuilder, comp_type: CompositeType) {
  case comp_type {
    FuncCompositeType(ft) ->
      builder
      |> bytes_builder.append(<<0x5E>>)
      |> encode_func_type(ft)
    StructCompositeType(st) ->
      builder
      |> bytes_builder.append(<<0x5F>>)
      |> encode_struct_type(st)
    ArrayCompositeType(at) ->
      builder
      |> bytes_builder.append(<<0x60>>)
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
    1 -> {
      let assert Ok(#(st, _)) = rec_type.st |> finger_tree.shift
      builder |> encode_sub_type(st)
    }
    t if t > 1 -> builder |> common.encode_vec(rec_type.st, encode_sub_type)
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
      #(SubType(False, finger_tree.new(), ct), rest)
    }
  }
}

pub fn encode_sub_type(builder: BytesBuilder, sub_type: SubType) {
  case sub_type.t |> finger_tree.size {
    0 -> builder |> encode_comp_type(sub_type.ct)
    _ ->
      case sub_type.final {
        True -> {
          let builder = builder |> bytes_builder.append(<<0x50>>)
          use builder <- result.try(
            builder |> common.encode_vec(sub_type.t, encode_type_idx),
          )
          builder |> encode_comp_type(sub_type.ct)
        }
        False -> {
          let builder = builder |> bytes_builder.append(<<0x4F>>)
          use builder <- result.try(
            builder |> common.encode_vec(sub_type.t, encode_type_idx),
          )
          builder |> encode_comp_type(sub_type.ct)
        }
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
  Ok(builder |> encode_u32(type_idx.id))
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
  let builder = builder |> encode_u32(limits.min)
  case limits.max {
    Some(max) -> Ok(builder |> encode_u32(max))
    None -> Ok(builder)
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
  }
}

pub fn decode_expression(bits: BitArray) {
  do_decode_expression(bits, finger_tree.new())
}

fn do_decode_expression(bits: BitArray, acc: FingerTree(Instruction)) {
  case decode_instruction(bits) {
    Ok(#(End, rest)) -> Ok(#(acc, rest))
    Ok(#(Else, rest)) -> Error("Invalid Else nesting")
    Ok(#(If(block_type, _, _), rest)) -> {
      use #(if_, rest) <- result.map(do_decode_if(
        bits,
        block_type,
        finger_tree.new(),
      ))
      #(acc |> finger_tree.push(if_), rest)
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
    Ok(#(End, rest)) -> Ok(#(If(block_type, t, None), rest))
    Ok(#(Else, rest)) -> do_decode_else(bits, block_type, t, finger_tree.new())
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
    Ok(#(End, rest)) -> Ok(#(If(block_type, t, Some(e)), rest))
    Ok(#(Else, rest)) -> Error("Invalid Else nesting")
    Ok(#(t1, rest)) ->
      do_decode_else(rest, block_type, t, e |> finger_tree.push(t1))
    Error(e) -> Error(e)
  }
}

pub fn decode_cast_flags(bits: BitArray) {
  case bits {
    <<0x00, rest:bits>> -> Ok(#(#(False, False), rest))
    <<0x01, rest:bits>> -> Ok(#(#(False, True), rest))
    <<0x02, rest:bits>> -> Ok(#(#(True, False), rest))
    <<0x03, rest:bits>> -> Ok(#(#(True, True), rest))
    _ -> Error("Invalid cast flags")
  }
}

pub fn encode_cast_flags(builder: BytesBuilder, cast_flags: #(Bool, Bool)) {
  case cast_flags {
    #(False, False) -> Ok(builder |> bytes_builder.append(<<0x00>>))
    #(False, True) -> Ok(builder |> bytes_builder.append(<<0x01>>))
    #(True, False) -> Ok(builder |> bytes_builder.append(<<0x02>>))
    #(True, True) -> Ok(builder |> bytes_builder.append(<<0x03>>))
  }
}

pub fn decode_mem_arg(bits: BitArray) {
  use #(offset, rest) <- result.try(decode_u32(bits))
  use #(align, rest) <- result.map(decode_u32(rest))
  #(MemArg(offset, align), rest)
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
      #(CallIndirect(type_idx, table_idx), rest)
    }
    <<0x12, rest:bits>> -> {
      use #(func_idx, rest) <- result.map(decode_func_idx(rest))
      #(ReturnCall(func_idx), rest)
    }
    <<0x13, rest:bits>> -> {
      use #(type_idx, rest) <- result.try(decode_type_idx(rest))
      use #(table_idx, rest) <- result.map(decode_table_idx(rest))
      #(ReturnCallIndirect(type_idx, table_idx), rest)
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
          #(RefTest(HeapTypeRefType(ht, True)), rest)
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
        _ -> Error("Invalid instruction")
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
        _ -> Error("Invalid instruction")
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
            _ -> Error("Invalid instruction")
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
    <<0xFC, rest:bits>> -> {
      use #(op, rest) <- result.try(values.decode_u32(rest))
      case op |> unwrap_u32 {
        8 -> {
          use #(idx, rest) <- result.try(decode_data_idx(rest))
          case rest {
            <<0x00, rest:bits>> -> Ok(#(MemoryInit(idx), rest))
            _ -> Error("Invalid instruction")
          }
        }
        9 -> {
          use #(idx, rest) <- result.map(decode_data_idx(rest))
          #(DataDrop(idx), rest)
        }
        10 -> {
          case rest {
            <<0x00, 0x00, rest:bits>> -> Ok(#(MemoryCopy, rest))
            _ -> Error("Invalid instruction")
          }
        }
        11 -> {
          case rest {
            <<0x00, rest:bits>> -> Ok(#(MemoryFill, rest))
            _ -> Error("Invalid instruction")
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
        _ -> Error("Invalid instruction")
      }
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
