import gleam/bytes_builder.{type BytesBuilder}
import gleam/result
import internal/binary/common
import internal/binary/values.{decode_s33, decode_u32, encode_s33, encode_u32}
import internal/finger_tree.{type FingerTree, size}
import internal/structure/numbers.{s33, u32, unwrap_s33, unwrap_u32}
import internal/structure/types.{
  type HeapType, type RefType, type ValType, type VecType, AnyHeapType,
  AnyRefType, ArrayHeapType, ArrayRefType, ConcreteHeapType, EqHeapType,
  EqRefType, ExternHeapType, ExternRefType, F32ValType, F64ValType, FuncHeapType,
  FuncRefType, HeapTypeRefType, I31HeapType, I31RefType, I32ValType, I64ValType,
  NoExternHeapType, NoExternRefType, NoFuncHeapType, NoFuncRefType, NoneHeapType,
  NoneRefType, RefTypeValType, StructHeapType, StructRefType, TypeIDX,
  V128ValType,
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

pub fn encode_result_type(
  builder: BytesBuilder,
  result_type: FingerTree(ValType),
) {
  builder |> common.encode_vec(result_type, encode_val_type)
}

pub fn decode_result_type(bits: BitArray) {
  common.decode_vec(bits, decode_val_type)
}
