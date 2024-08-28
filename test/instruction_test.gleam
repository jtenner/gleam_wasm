import gleam/bytes_builder
import gleeunit/should
import internal/binary/types
import internal/finger_tree
import internal/structure/numbers
import internal/structure/types as structure_types

fn round_trip(inst: structure_types.Instruction, bits: BitArray) {
  bytes_builder.new()
  |> types.encode_instruction(inst)
  |> should.be_ok
  |> bytes_builder.to_bit_array
  |> should.equal(bits)

  bits
  |> types.decode_instruction
  |> should.be_ok
  |> should.equal(#(inst, <<>>))
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
  let assert Ok(type_idx) = numbers.u32(1)
  let type_idx = structure_types.TypeIDX(type_idx)

  round_trip(structure_types.CallIndirect(type_idx, table_idx), <<0x11, 42, 1>>)
}
