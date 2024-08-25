import gleam/io
import gleam/pair
import gleeunit/should
import internal/binary/common
import internal/binary/types
import internal/finger_tree
import internal/structure/numbers
import internal/structure/types as structure_types

fn should_equal_helper(val: Result(u, String), expected: u) {
  case val |> should.be_ok == expected {
    True -> Nil
    _ -> {
      io.debug(#("actual", val, "expected", expected))
      panic as "Values were not equal"
    }
  }
}

pub fn decode_custom_section_test() {
  common.decode_string(<<11, 70, 111, 111, 32, 66, 97, 114, 32, 66, 97, 122>>)
  |> should_equal_helper(#("Foo Bar Baz", <<>>))
}

pub fn encode_val_type_test() {
  <<0x7F>>
  |> types.decode_val_type
  |> should_equal_helper(#(structure_types.I32ValType, <<>>))
  <<0x7E>>
  |> types.decode_val_type
  |> should_equal_helper(#(structure_types.I64ValType, <<>>))
  <<0x7D>>
  |> types.decode_val_type
  |> should_equal_helper(#(structure_types.F32ValType, <<>>))
  <<0x7C>>
  |> types.decode_val_type
  |> should_equal_helper(#(structure_types.F64ValType, <<>>))
  <<0x7B>>
  |> types.decode_val_type
  |> should_equal_helper(#(structure_types.V128ValType, <<>>))
}

pub fn decode_abstract_heap_type_test() {
  <<0x73>>
  |> types.decode_heap_type
  |> should_equal_helper(#(structure_types.NoFuncHeapType, <<>>))
  <<0x72>>
  |> types.decode_heap_type
  |> should_equal_helper(#(structure_types.NoExternHeapType, <<>>))
  <<0x71>>
  |> types.decode_heap_type
  |> should_equal_helper(#(structure_types.NoneHeapType, <<>>))
  <<0x70>>
  |> types.decode_heap_type
  |> should_equal_helper(#(structure_types.FuncHeapType, <<>>))
  <<0x6F>>
  |> types.decode_heap_type
  |> should_equal_helper(#(structure_types.ExternHeapType, <<>>))
  // any, eq, i31
  <<0x6E>>
  |> types.decode_heap_type
  |> should_equal_helper(#(structure_types.AnyHeapType, <<>>))
  <<0x6D>>
  |> types.decode_heap_type
  |> should_equal_helper(#(structure_types.EqHeapType, <<>>))
  <<0x6C>>
  |> types.decode_heap_type
  |> should_equal_helper(#(structure_types.I31HeapType, <<>>))
  // struct, array
  <<0x6B>>
  |> types.decode_heap_type
  |> should_equal_helper(#(structure_types.StructHeapType, <<>>))
  <<0x6A>>
  |> types.decode_heap_type
  |> should_equal_helper(#(structure_types.ArrayHeapType, <<>>))
}

pub fn decode_concrete_heap_type_test() {
  let assert Ok(u32_value) = numbers.u32(10)
  <<0x0A>>
  |> types.decode_heap_type
  |> should_equal_helper(
    #(
      structure_types.ConcreteHeapType(structure_types.TypeIDX(u32_value)),
      <<>>,
    ),
  )
}

pub fn decode_ref_type_test() {
  <<0x73>>
  |> types.decode_ref_type
  |> should_equal_helper(#(structure_types.NoFuncRefType, <<>>))
  <<0x72>>
  |> types.decode_ref_type
  |> should_equal_helper(#(structure_types.NoExternRefType, <<>>))
  <<0x71>>
  |> types.decode_ref_type
  |> should_equal_helper(#(structure_types.NoneRefType, <<>>))
  <<0x70>>
  |> types.decode_ref_type
  |> should_equal_helper(#(structure_types.FuncRefType, <<>>))
  <<0x6F>>
  |> types.decode_ref_type
  |> should_equal_helper(#(structure_types.ExternRefType, <<>>))
  // any, eq, i31
  <<0x6E>>
  |> types.decode_ref_type
  |> should_equal_helper(#(structure_types.AnyRefType, <<>>))
  <<0x6D>>
  |> types.decode_ref_type
  |> should_equal_helper(#(structure_types.EqRefType, <<>>))
  <<0x6C>>
  |> types.decode_ref_type
  |> should_equal_helper(#(structure_types.I31RefType, <<>>))
  // struct, array
  <<0x6B>>
  |> types.decode_ref_type
  |> should_equal_helper(#(structure_types.StructRefType, <<>>))
  <<0x6A>>
  |> types.decode_ref_type
  |> should_equal_helper(#(structure_types.ArrayRefType, <<>>))
}

pub fn decode_heap_type_ref_type_test() {
  let assert Ok(u32_value) = numbers.u32(10)

  <<0x6B>>
  |> types.decode_ref_type
  |> should_equal_helper(#(structure_types.StructRefType, <<>>))

  <<0x63, 0x6B>>
  |> types.decode_ref_type
  |> should_equal_helper(#(structure_types.StructRefType, <<>>))

  <<0x64, 10>>
  |> types.decode_ref_type
  |> should_equal_helper(
    #(
      structure_types.HeapTypeRefType(
        structure_types.ConcreteHeapType(structure_types.TypeIDX(u32_value)),
        False,
      ),
      <<>>,
    ),
  )
  <<0x63, 10>>
  |> types.decode_ref_type
  |> should_equal_helper(
    #(
      structure_types.HeapTypeRefType(
        structure_types.ConcreteHeapType(structure_types.TypeIDX(u32_value)),
        True,
      ),
      <<>>,
    ),
  )
}

pub fn decode_result_type_test() {
  <<0>>
  |> types.decode_result_type
  |> should.be_ok
  |> pair.map_first(finger_tree.to_list)
  |> should.equal(#([], <<>>))

  <<4, 0x7F, 0x7E, 0x7D, 0x7C>>
  |> types.decode_result_type
  |> should.be_ok
  |> pair.map_first(finger_tree.to_list)
  |> should.equal(
    #(
      [
        structure_types.I32ValType,
        structure_types.I64ValType,
        structure_types.F32ValType,
        structure_types.F64ValType,
      ],
      <<>>,
    ),
  )
}

pub fn decode_array_type_test() {
  <<0x7F, 0x00>>
  |> types.decode_array_type
  |> should_equal_helper(
    #(
      structure_types.ArrayType(structure_types.FieldType(
        structure_types.ValTypeStorageType(structure_types.I32ValType),
        structure_types.Const,
      )),
      <<>>,
    ),
  )
  <<0x78, 0x00>>
  |> types.decode_array_type
  |> should_equal_helper(
    #(
      structure_types.ArrayType(structure_types.FieldType(
        structure_types.I8StorageType,
        structure_types.Const,
      )),
      <<>>,
    ),
  )
  <<0x77, 0x00>>
  |> types.decode_array_type
  |> should_equal_helper(
    #(
      structure_types.ArrayType(structure_types.FieldType(
        structure_types.I16StorageType,
        structure_types.Const,
      )),
      <<>>,
    ),
  )
  <<0x7F, 0x01>>
  |> types.decode_array_type
  |> should_equal_helper(
    #(
      structure_types.ArrayType(structure_types.FieldType(
        structure_types.ValTypeStorageType(structure_types.I32ValType),
        structure_types.Var,
      )),
      <<>>,
    ),
  )
  <<0x78, 0x01>>
  |> types.decode_array_type
  |> should_equal_helper(
    #(
      structure_types.ArrayType(structure_types.FieldType(
        structure_types.I8StorageType,
        structure_types.Var,
      )),
      <<>>,
    ),
  )
  <<0x77, 0x01>>
  |> types.decode_array_type
  |> should_equal_helper(
    #(
      structure_types.ArrayType(structure_types.FieldType(
        structure_types.I16StorageType,
        structure_types.Var,
      )),
      <<>>,
    ),
  )
}

pub fn decode_struct_type_test() {
  <<0>>
  |> types.decode_struct_type
  |> should.be_ok
  |> should.equal(#(structure_types.StructType(finger_tree.empty), <<>>))

  <<6, 0x7F, 0x00, 0x78, 0x00, 0x77, 0x00, 0x7F, 0x01, 0x78, 0x01, 0x77, 0x01>>
  |> types.decode_struct_type
  |> should.be_ok
  |> pair.map_first(fn(e) { e.ft |> finger_tree.to_list })
  |> should.equal(
    #(
      [
        structure_types.FieldType(
          structure_types.ValTypeStorageType(structure_types.I32ValType),
          structure_types.Const,
        ),
        structure_types.FieldType(
          structure_types.I8StorageType,
          structure_types.Const,
        ),
        structure_types.FieldType(
          structure_types.I16StorageType,
          structure_types.Const,
        ),
        structure_types.FieldType(
          structure_types.ValTypeStorageType(structure_types.I32ValType),
          structure_types.Var,
        ),
        structure_types.FieldType(
          structure_types.I8StorageType,
          structure_types.Var,
        ),
        structure_types.FieldType(
          structure_types.I16StorageType,
          structure_types.Var,
        ),
      ],
      <<>>,
    ),
  )
}

fn func_type_equals(a: structure_types.FuncType, b: structure_types.FuncType) {
  a.parameters
  |> finger_tree.to_list
  |> should.equal(b.parameters |> finger_tree.to_list)
  a.results
  |> finger_tree.to_list
  |> should.equal(b.results |> finger_tree.to_list)
}

fn struct_type_equals(
  a: structure_types.StructType,
  b: structure_types.StructType,
) {
  a.ft
  |> finger_tree.to_list
  |> should.equal(b.ft |> finger_tree.to_list)
}

fn array_type_equals(a: structure_types.ArrayType, b: structure_types.ArrayType) {
  a.ft |> should.equal(b.ft)
}

fn composite_type_equals(
  a: structure_types.CompositeType,
  b: structure_types.CompositeType,
) {
  case a, b {
    a, b if a == b -> Nil
    structure_types.FuncCompositeType(a), structure_types.FuncCompositeType(b) ->
      func_type_equals(a, b)
    structure_types.StructCompositeType(a),
      structure_types.StructCompositeType(b)
    -> struct_type_equals(a, b)
    structure_types.ArrayCompositeType(a), structure_types.ArrayCompositeType(b)
    -> array_type_equals(a, b)
    _, _ -> panic as "Values were not equal"
  }
}

fn decode_type_equals_helper(
  bits: BitArray,
  expected: u,
  decode_fn: fn(BitArray) -> Result(#(u, BitArray), String),
  equals_fn: fn(u, u) -> Nil,
) {
  decode_fn(bits)
  |> should.be_ok
  |> pair.map_first(equals_fn(_, expected))
  |> pair.map_second(should.equal(_, <<>>))
}

pub fn decode_composite_type_test() {
  decode_type_equals_helper(
    <<0x60, 0x00, 0x00>>,
    structure_types.FuncCompositeType(structure_types.FuncType(
      finger_tree.new(),
      finger_tree.new(),
    )),
    types.decode_comp_type,
    composite_type_equals,
  )

  decode_type_equals_helper(
    <<
      0x5F, 0x06, 0x7F, 0x00, 0x78, 0x00, 0x77, 0x00, 0x7F, 0x01, 0x78, 0x01,
      0x77, 0x01,
    >>,
    structure_types.StructCompositeType(
      structure_types.StructType(
        finger_tree.from_list([
          structure_types.FieldType(
            structure_types.ValTypeStorageType(structure_types.I32ValType),
            structure_types.Const,
          ),
          structure_types.FieldType(
            structure_types.I8StorageType,
            structure_types.Const,
          ),
          structure_types.FieldType(
            structure_types.I16StorageType,
            structure_types.Const,
          ),
          structure_types.FieldType(
            structure_types.ValTypeStorageType(structure_types.I32ValType),
            structure_types.Var,
          ),
          structure_types.FieldType(
            structure_types.I8StorageType,
            structure_types.Var,
          ),
          structure_types.FieldType(
            structure_types.I16StorageType,
            structure_types.Var,
          ),
        ]),
      ),
    ),
    types.decode_comp_type,
    composite_type_equals,
  )

  decode_type_equals_helper(
    <<0x5E, 0x7F, 0x00>>,
    structure_types.ArrayCompositeType(
      structure_types.ArrayType(structure_types.FieldType(
        structure_types.ValTypeStorageType(structure_types.I32ValType),
        structure_types.Const,
      )),
    ),
    types.decode_comp_type,
    composite_type_equals,
  )
  decode_type_equals_helper(
    <<0x5E, 0x77, 0x01>>,
    structure_types.ArrayCompositeType(
      structure_types.ArrayType(structure_types.FieldType(
        structure_types.I16StorageType,
        structure_types.Var,
      )),
    ),
    types.decode_comp_type,
    composite_type_equals,
  )
}

fn rec_type_equals(
  actual: structure_types.RecType,
  expected: structure_types.RecType,
) {
  actual.st
}
