import gleam/bit_array
import gleam/bytes_builder
import gleam/dynamic
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/result
import gleeunit/should
import internal/binary/types
import internal/finger_tree
import internal/structure/numbers
import internal/structure/types as structure_types
import pprint

fn round_trip(
  expected_inst: structure_types.Instruction,
  expected_bits: BitArray,
) {
  let name = dynamic.from(expected_inst) |> dynamic.classify

  let actual_encoded =
    bytes_builder.new()
    |> types.encode_instruction(expected_inst)
    |> result.map(bytes_builder.to_bit_array)

  let actual_decoded =
    expected_bits
    |> types.decode_instruction

  case actual_encoded, actual_decoded {
    Ok(actual_bits), Ok(#(actual_inst, <<>>)) ->
      case actual_bits == expected_bits, actual_inst == expected_inst {
        False, _ -> {
          let actual_bits = actual_bits |> bit_array.inspect
          let expected_bits = expected_bits |> bit_array.inspect
          let fail_text =
            "Instruction "
            <> name
            <> " did not match. Expected:\n"
            <> expected_bits
            <> "\nActual:\n"
            <> actual_bits

          panic as fail_text
        }
        _, False -> {
          let actual_inst = actual_inst |> pprint.format
          let expected_inst = expected_inst |> pprint.format
          let fail_text =
            "Instruction "
            <> name
            <> " did not decode correctly. Expected:\n"
            <> expected_inst
            <> "\nActual:\n"
            <> actual_inst
          panic as fail_text
        }
        _, _ -> Nil
      }
    _, Ok(#(_, rest)) -> {
      let fail_text =
        "Instruction "
        <> name
        <> " did not decode all the bytes. The following bytes were left over:\n"
        <> { rest |> bit_array.inspect }
      panic as fail_text
    }
    Error(err), _ -> {
      let fail_text =
        "Instruction " <> name <> " failed to encode with error: " <> err

      panic as fail_text
    }
    _, Error(err) -> {
      let fail_text =
        "Instruction " <> name <> " failed to decode with error: " <> err
      panic as fail_text
    }
  }
}

pub fn unreachable_test() {
  round_trip(structure_types.Unreachable, <<0x00>>)
}

pub fn nop_test() {
  round_trip(structure_types.Nop, <<0x01>>)
}

pub fn br_test() {
  let assert Ok(label_idx) = numbers.u32(42)
  let label_idx = structure_types.LabelIDX(label_idx)

  round_trip(structure_types.Br(label_idx), <<0x0C, 42>>)
}

pub fn br_if_test() {
  let assert Ok(label_idx) = numbers.u32(42)
  let label_idx = structure_types.LabelIDX(label_idx)

  round_trip(structure_types.BrIf(label_idx), <<0x0D, 42>>)
}

pub fn br_table_test() {
  let assert Ok(label_default) = numbers.u32(42)
  let assert Ok(label_a) = numbers.u32(1)
  let assert Ok(label_b) = numbers.u32(2)
  let assert Ok(label_c) = numbers.u32(3)

  let label_default = structure_types.LabelIDX(label_default)
  let label_a = structure_types.LabelIDX(label_a)
  let label_b = structure_types.LabelIDX(label_b)
  let label_c = structure_types.LabelIDX(label_c)

  let labels =
    [label_a, label_b, label_c]
    |> finger_tree.from_list

  round_trip(structure_types.BrTable(labels, label_default), <<
    0x0E, 3, 1, 2, 3, 42,
  >>)
}

pub fn return_test() {
  round_trip(structure_types.Return, <<0x0F>>)
}

pub fn call_test() {
  let assert Ok(function_idx) = numbers.u32(42)
  let function_idx = structure_types.FuncIDX(function_idx)

  round_trip(structure_types.Call(function_idx), <<0x10, 42>>)
}

pub fn call_indirect_test() {
  let assert Ok(type_idx) = numbers.u32(42)
  let type_idx = structure_types.TypeIDX(type_idx)
  let assert Ok(table_idx) = numbers.u32(1)
  let table_idx = structure_types.TableIDX(table_idx)

  round_trip(structure_types.CallIndirect(table_idx, type_idx), <<0x11, 42, 1>>)
}

pub fn return_call_test() {
  let assert Ok(func_idx) = numbers.u32(42)
  let func_idx = structure_types.FuncIDX(func_idx)

  round_trip(structure_types.ReturnCall(func_idx), <<0x12, 42>>)
}

pub fn return_call_indirect_test() {
  let assert Ok(type_idx) = numbers.u32(42)
  let type_idx = structure_types.TypeIDX(type_idx)
  let assert Ok(table_idx) = numbers.u32(1)
  let table_idx = structure_types.TableIDX(table_idx)

  round_trip(structure_types.ReturnCallIndirect(table_idx, type_idx), <<
    0x13, 42, 1,
  >>)
}

pub fn call_ref_test() {
  let assert Ok(type_idx) = numbers.u32(42)
  let type_idx = structure_types.TypeIDX(type_idx)

  round_trip(structure_types.CallRef(type_idx), <<0x14, 42>>)
}

pub fn return_call_ref_test() {
  let assert Ok(type_idx) = numbers.u32(42)
  let type_idx = structure_types.TypeIDX(type_idx)

  round_trip(structure_types.ReturnCallRef(type_idx), <<0x15, 42>>)
}

pub fn block_test() {
  let assert Ok(local_idx1) = numbers.u32(0)
  let local_idx1 = structure_types.LocalIDX(local_idx1)
  let assert Ok(local_idx2) = numbers.u32(1)
  let local_idx2 = structure_types.LocalIDX(local_idx2)
  let local_get1 = structure_types.LocalGet(local_idx1)
  let local_get2 = structure_types.LocalGet(local_idx2)
  let i32_add = structure_types.I32Add
  let drop = structure_types.Drop

  let block_body =
    structure_types.Expr(
      finger_tree.from_list([local_get1, local_get2, i32_add, drop]),
    )

  round_trip(structure_types.Block(structure_types.VoidBlockType, block_body), <<
    0x02, 0x40, 0x20, 0, 0x20, 1, 0x6A, 0x1A, 0x0B,
  >>)
}

// https://webassembly.github.io/gc/core/binary/instructions.html#control-instructions
pub fn loop_test() {
  let assert Ok(local_idx1) = numbers.u32(0)
  let local_idx1 = structure_types.LocalIDX(local_idx1)
  let assert Ok(local_idx2) = numbers.u32(1)
  let local_idx2 = structure_types.LocalIDX(local_idx2)
  let local_get1 = structure_types.LocalGet(local_idx1)
  let local_get2 = structure_types.LocalGet(local_idx2)
  let i32_add = structure_types.I32Add
  let drop = structure_types.Drop

  let block_body =
    structure_types.Expr(
      finger_tree.from_list([local_get1, local_get2, i32_add, drop]),
    )

  round_trip(structure_types.Loop(structure_types.VoidBlockType, block_body), <<
    0x03, 0x40, 0x20, 0, 0x20, 1, 0x6A, 0x1A, 0x0B,
  >>)
}

pub fn if_test() {
  let assert Ok(local_idx1) = numbers.u32(0)
  let local_idx1 = structure_types.LocalIDX(local_idx1)
  let assert Ok(local_idx2) = numbers.u32(1)
  let local_idx2 = structure_types.LocalIDX(local_idx2)
  let local_get1 = structure_types.LocalGet(local_idx1)
  let local_get2 = structure_types.LocalGet(local_idx2)
  let i32_add = structure_types.I32Add
  let drop = structure_types.Drop

  let block_body =
    finger_tree.from_list([local_get1, local_get2, i32_add, drop])

  round_trip(
    structure_types.If(structure_types.VoidBlockType, block_body, None),
    <<0x04, 0x40, 0x20, 0, 0x20, 1, 0x6A, 0x1A, 0x0B>>,
  )
}

pub fn if_else_test() {
  let assert Ok(local_idx1) = numbers.u32(0)
  let local_idx1 = structure_types.LocalIDX(local_idx1)
  let local_get1 = structure_types.LocalGet(local_idx1)
  let drop = structure_types.Drop

  let block_body = finger_tree.from_list([local_get1, drop])

  round_trip(
    structure_types.If(
      structure_types.VoidBlockType,
      block_body,
      Some(block_body),
    ),
    <<0x04, 0x40, 0x20, 0, 0x1A, 0x05, 0x20, 0, 0x1A, 0x0B>>,
  )
}

pub fn br_on_null_test() {
  let assert Ok(label_idx) = numbers.u32(42)
  let label_idx = structure_types.LabelIDX(label_idx)

  round_trip(structure_types.BrOnNull(label_idx), <<0xD5, 42>>)
}

pub fn br_on_non_null_test() {
  let assert Ok(label_idx) = numbers.u32(42)
  let label_idx = structure_types.LabelIDX(label_idx)

  round_trip(structure_types.BrOnNonNull(label_idx), <<0xD6, 42>>)
}

pub fn i32_eqz_test() {
  round_trip(structure_types.I32Eqz, <<0x45>>)
}

pub fn i32_eq_test() {
  round_trip(structure_types.I32Eq, <<0x46>>)
}

pub fn i32_ne_test() {
  round_trip(structure_types.I32Ne, <<0x47>>)
}

pub fn i32_lt_s_test() {
  round_trip(structure_types.I32LtS, <<0x48>>)
}

pub fn i32_lt_u_test() {
  round_trip(structure_types.I32LtU, <<0x49>>)
}

pub fn i32_gt_s_test() {
  round_trip(structure_types.I32GtS, <<0x4A>>)
}

pub fn i32_gt_u_test() {
  round_trip(structure_types.I32GtU, <<0x4B>>)
}

pub fn i32_le_s_test() {
  round_trip(structure_types.I32LeS, <<0x4C>>)
}

pub fn i32_le_u_test() {
  round_trip(structure_types.I32LeU, <<0x4D>>)
}

pub fn i32_ge_s_test() {
  round_trip(structure_types.I32GeS, <<0x4E>>)
}

pub fn i32_ge_u_test() {
  round_trip(structure_types.I32GeU, <<0x4F>>)
}

pub fn i64_eqz_test() {
  round_trip(structure_types.I64Eqz, <<0x50>>)
}

pub fn i64_eq_test() {
  round_trip(structure_types.I64Eq, <<0x51>>)
}

pub fn i64_ne_test() {
  round_trip(structure_types.I64Ne, <<0x52>>)
}

pub fn i64_lt_s_test() {
  round_trip(structure_types.I64LtS, <<0x53>>)
}

pub fn i64_lt_u_test() {
  round_trip(structure_types.I64LtU, <<0x54>>)
}

pub fn i64_gt_s_test() {
  round_trip(structure_types.I64GtS, <<0x55>>)
}

pub fn i64_gt_u_test() {
  round_trip(structure_types.I64GtU, <<0x56>>)
}

pub fn i64_le_s_test() {
  round_trip(structure_types.I64LeS, <<0x57>>)
}

pub fn i64_le_u_test() {
  round_trip(structure_types.I64LeU, <<0x58>>)
}

pub fn i64_ge_s_test() {
  round_trip(structure_types.I64GeS, <<0x59>>)
}

pub fn i64_ge_u_test() {
  round_trip(structure_types.I64GeU, <<0x5A>>)
}

pub fn f32_eq_test() {
  round_trip(structure_types.F32Eq, <<0x5B>>)
}

pub fn f32_ne_test() {
  round_trip(structure_types.F32Ne, <<0x5C>>)
}

pub fn f32_lt_test() {
  round_trip(structure_types.F32Lt, <<0x5D>>)
}

pub fn f32_gt_test() {
  round_trip(structure_types.F32Gt, <<0x5E>>)
}

pub fn f32_le_test() {
  round_trip(structure_types.F32Le, <<0x5F>>)
}

pub fn f32_ge_test() {
  round_trip(structure_types.F32Ge, <<0x60>>)
}

pub fn f64_eq_test() {
  round_trip(structure_types.F64Eq, <<0x61>>)
}

pub fn f64_ne_test() {
  round_trip(structure_types.F64Ne, <<0x62>>)
}

pub fn f64_lt_test() {
  round_trip(structure_types.F64Lt, <<0x63>>)
}

pub fn f64_gt_test() {
  round_trip(structure_types.F64Gt, <<0x64>>)
}

pub fn f64_le_test() {
  round_trip(structure_types.F64Le, <<0x65>>)
}

pub fn f64_ge_test() {
  round_trip(structure_types.F64Ge, <<0x66>>)
}

pub fn i32_clz_test() {
  round_trip(structure_types.I32Clz, <<0x67>>)
}

pub fn i32_ctz_test() {
  round_trip(structure_types.I32Ctz, <<0x68>>)
}

pub fn i32_popcnt_test() {
  round_trip(structure_types.I32Popcnt, <<0x69>>)
}

pub fn i32_add_test() {
  round_trip(structure_types.I32Add, <<0x6A>>)
}

pub fn i32_sub_test() {
  round_trip(structure_types.I32Sub, <<0x6B>>)
}

pub fn i32_mul_test() {
  round_trip(structure_types.I32Mul, <<0x6C>>)
}

pub fn i32_div_s_test() {
  round_trip(structure_types.I32DivS, <<0x6D>>)
}

pub fn i32_div_u_test() {
  round_trip(structure_types.I32DivU, <<0x6E>>)
}

pub fn i32_rem_s_test() {
  round_trip(structure_types.I32RemS, <<0x6F>>)
}

pub fn i32_rem_u_test() {
  round_trip(structure_types.I32RemU, <<0x70>>)
}

pub fn i32_and_test() {
  round_trip(structure_types.I32And, <<0x71>>)
}

pub fn i32_or_test() {
  round_trip(structure_types.I32Or, <<0x72>>)
}

pub fn i32_xor_test() {
  round_trip(structure_types.I32Xor, <<0x73>>)
}

pub fn i32_shl_test() {
  round_trip(structure_types.I32Shl, <<0x74>>)
}

pub fn i32_shr_s_test() {
  round_trip(structure_types.I32ShrS, <<0x75>>)
}

pub fn i32_shr_u_test() {
  round_trip(structure_types.I32ShrU, <<0x76>>)
}

pub fn i32_rotl_test() {
  round_trip(structure_types.I32Rotl, <<0x77>>)
}

pub fn i32_rotr_test() {
  round_trip(structure_types.I32Rotr, <<0x78>>)
}

pub fn i64_clz_test() {
  round_trip(structure_types.I64Clz, <<0x79>>)
}

pub fn i64_ctz_test() {
  round_trip(structure_types.I64Ctz, <<0x7A>>)
}

pub fn i64_popcnt_test() {
  round_trip(structure_types.I64Popcnt, <<0x7B>>)
}

pub fn i64_add_test() {
  round_trip(structure_types.I64Add, <<0x7C>>)
}

pub fn i64_sub_test() {
  round_trip(structure_types.I64Sub, <<0x7D>>)
}

pub fn i64_mul_test() {
  round_trip(structure_types.I64Mul, <<0x7E>>)
}

pub fn i64_div_s_test() {
  round_trip(structure_types.I64DivS, <<0x7F>>)
}

pub fn i64_div_u_test() {
  round_trip(structure_types.I64DivU, <<0x80>>)
}

pub fn i64_rem_s_test() {
  round_trip(structure_types.I64RemS, <<0x81>>)
}

pub fn i64_rem_u_test() {
  round_trip(structure_types.I64RemU, <<0x82>>)
}

pub fn i64_and_test() {
  round_trip(structure_types.I64And, <<0x83>>)
}

pub fn i64_or_test() {
  round_trip(structure_types.I64Or, <<0x84>>)
}

pub fn i64_xor_test() {
  round_trip(structure_types.I64Xor, <<0x85>>)
}

pub fn i64_shl_test() {
  round_trip(structure_types.I64Shl, <<0x86>>)
}

pub fn i64_shr_s_test() {
  round_trip(structure_types.I64ShrS, <<0x87>>)
}

pub fn i64_shr_u_test() {
  round_trip(structure_types.I64ShrU, <<0x88>>)
}

pub fn i64_rotl_test() {
  round_trip(structure_types.I64Rotl, <<0x89>>)
}

pub fn i64_rotr_test() {
  round_trip(structure_types.I64Rotr, <<0x8A>>)
}

pub fn f32_abs_test() {
  round_trip(structure_types.F32Abs, <<0x8B>>)
}

pub fn f32_neg_test() {
  round_trip(structure_types.F32Neg, <<0x8C>>)
}

pub fn f32_ceil_test() {
  round_trip(structure_types.F32Ceil, <<0x8D>>)
}

pub fn f32_floor_test() {
  round_trip(structure_types.F32Floor, <<0x8E>>)
}

pub fn f32_trunc_test() {
  round_trip(structure_types.F32Trunc, <<0x8F>>)
}

pub fn f32_nearest_test() {
  round_trip(structure_types.F32Nearest, <<0x90>>)
}

pub fn f32_sqrt_test() {
  round_trip(structure_types.F32Sqrt, <<0x91>>)
}

pub fn f32_add_test() {
  round_trip(structure_types.F32Add, <<0x92>>)
}

pub fn f32_sub_test() {
  round_trip(structure_types.F32Sub, <<0x93>>)
}

pub fn f32_mul_test() {
  round_trip(structure_types.F32Mul, <<0x94>>)
}

pub fn f32_div_test() {
  round_trip(structure_types.F32Div, <<0x95>>)
}

pub fn f32_min_test() {
  round_trip(structure_types.F32Min, <<0x96>>)
}

pub fn f32_max_test() {
  round_trip(structure_types.F32Max, <<0x97>>)
}

pub fn f32_copysign_test() {
  round_trip(structure_types.F32Copysign, <<0x98>>)
}

pub fn f64_abs_test() {
  round_trip(structure_types.F64Abs, <<0x99>>)
}

pub fn f64_neg_test() {
  round_trip(structure_types.F64Neg, <<0x9A>>)
}

pub fn f64_ceil_test() {
  round_trip(structure_types.F64Ceil, <<0x9B>>)
}

pub fn f64_floor_test() {
  round_trip(structure_types.F64Floor, <<0x9C>>)
}

pub fn f64_trunc_test() {
  round_trip(structure_types.F64Trunc, <<0x9D>>)
}

pub fn f64_nearest_test() {
  round_trip(structure_types.F64Nearest, <<0x9E>>)
}

pub fn f64_sqrt_test() {
  round_trip(structure_types.F64Sqrt, <<0x9F>>)
}

pub fn f64_add_test() {
  round_trip(structure_types.F64Add, <<0xA0>>)
}

pub fn f64_sub_test() {
  round_trip(structure_types.F64Sub, <<0xA1>>)
}

pub fn f64_mul_test() {
  round_trip(structure_types.F64Mul, <<0xA2>>)
}

pub fn f64_div_test() {
  round_trip(structure_types.F64Div, <<0xA3>>)
}

pub fn f64_min_test() {
  round_trip(structure_types.F64Min, <<0xA4>>)
}

pub fn f64_max_test() {
  round_trip(structure_types.F64Max, <<0xA5>>)
}

pub fn f64_copysign_test() {
  round_trip(structure_types.F64Copysign, <<0xA6>>)
}

pub fn i32_wrap_i64_test() {
  round_trip(structure_types.I32WrapI64, <<0xA7>>)
}

pub fn i32_trunc_f32s_test() {
  round_trip(structure_types.I32TruncF32S, <<0xA8>>)
}

pub fn i32_trunc_f32u_test() {
  round_trip(structure_types.I32TruncF32U, <<0xA9>>)
}

pub fn i32_trunc_f64s_test() {
  round_trip(structure_types.I32TruncF64S, <<0xAA>>)
}

pub fn i32_trunc_f64u_test() {
  round_trip(structure_types.I32TruncF64U, <<0xAB>>)
}

pub fn i64_extend_i32s_test() {
  round_trip(structure_types.I64ExtendI32S, <<0xAC>>)
}

pub fn i64_extend_i32u_test() {
  round_trip(structure_types.I64ExtendI32U, <<0xAD>>)
}

pub fn i64_trunc_f32s_test() {
  round_trip(structure_types.I64TruncF32S, <<0xAE>>)
}

pub fn i64_trunc_f32u_test() {
  round_trip(structure_types.I64TruncF32U, <<0xAF>>)
}

pub fn i64_trunc_f64s_test() {
  round_trip(structure_types.I64TruncF64S, <<0xB0>>)
}

pub fn i64_trunc_f64u_test() {
  round_trip(structure_types.I64TruncF64U, <<0xB1>>)
}

pub fn f32_convert_i32s_test() {
  round_trip(structure_types.F32ConvertI32S, <<0xB2>>)
}

pub fn f32_convert_i32u_test() {
  round_trip(structure_types.F32ConvertI32U, <<0xB3>>)
}

pub fn f32_convert_i64s_test() {
  round_trip(structure_types.F32ConvertI64S, <<0xB4>>)
}

pub fn f32_convert_i64u_test() {
  round_trip(structure_types.F32ConvertI64U, <<0xB5>>)
}

pub fn f32_demote_f64_test() {
  round_trip(structure_types.F32DemoteF64, <<0xB6>>)
}

pub fn f64_convert_i32s_test() {
  round_trip(structure_types.F64ConvertI32S, <<0xB7>>)
}

pub fn f64_convert_i32u_test() {
  round_trip(structure_types.F64ConvertI32U, <<0xB8>>)
}

pub fn f64_convert_i64s_test() {
  round_trip(structure_types.F64ConvertI64S, <<0xB9>>)
}

pub fn f64_convert_i64u_test() {
  round_trip(structure_types.F64ConvertI64U, <<0xBA>>)
}

pub fn f64_promote_f32_test() {
  round_trip(structure_types.F64PromoteF32, <<0xBB>>)
}

pub fn i32_reinterpret_f32_test() {
  round_trip(structure_types.I32ReinterpretF32, <<0xBC>>)
}

pub fn i64_reinterpret_f64_test() {
  round_trip(structure_types.I64ReinterpretF64, <<0xBD>>)
}

pub fn f32_reinterpret_i32_test() {
  round_trip(structure_types.F32ReinterpretI32, <<0xBE>>)
}

pub fn f64_reinterpret_i64_test() {
  round_trip(structure_types.F64ReinterpretI64, <<0xBF>>)
}

pub fn i32_extend8s_test() {
  round_trip(structure_types.I32Extend8S, <<0xC0>>)
}

pub fn i32_extend16s_test() {
  round_trip(structure_types.I32Extend16S, <<0xC1>>)
}

pub fn i64_extend8s_test() {
  round_trip(structure_types.I64Extend8S, <<0xC2>>)
}

pub fn i64_extend16s_test() {
  round_trip(structure_types.I64Extend16S, <<0xC3>>)
}

pub fn i64_extend32s_test() {
  round_trip(structure_types.I64Extend32S, <<0xC4>>)
}

pub fn br_on_cast_test() {
  let assert Ok(label_idx) = numbers.u32(42)
  let label_idx = structure_types.LabelIDX(label_idx)
  let ht1 = structure_types.HeapTypeRefType(structure_types.I31HeapType, False)
  let ht2 = structure_types.HeapTypeRefType(structure_types.FuncHeapType, True)

  round_trip(structure_types.BrOnCast(label_idx, ht1, ht2), <<
    0xFB, 24, 0x02, 42, 0x6C, 0x70,
  >>)
}

pub fn br_on_cast_fail_test() {
  let assert Ok(label_idx) = numbers.u32(42)
  let label_idx = structure_types.LabelIDX(label_idx)
  let ht1 = structure_types.HeapTypeRefType(structure_types.I31HeapType, False)
  let ht2 = structure_types.HeapTypeRefType(structure_types.FuncHeapType, True)

  round_trip(structure_types.BrOnCastFail(label_idx, ht1, ht2), <<
    0xFB, 25, 0x02, 42, 0x6C, 0x70,
  >>)
}
