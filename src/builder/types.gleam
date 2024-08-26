import gleam/dict.{type Dict}
import internal/finger_tree
import internal/structure/types.{
  type FieldType, type RecType, type TypeIDX, type ValType, AnyHeapType,
  AnyRefType, ArrayHeapType, ArrayRefType, ArrayType, ConcreteHeapType, Const,
  EqHeapType, EqRefType, ExternHeapType, ExternRefType, F32ValType, F64ValType,
  FieldType, FuncHeapType, FuncRefType, FuncTypeBlockType, HeapTypeRefType,
  I16StorageType, I31HeapType, I31RefType, I32ValType, I64ValType, I8StorageType,
  NoExternHeapType, NoExternRefType, NoFuncHeapType, NoFuncRefType, NoneHeapType,
  NoneRefType, RecType, RefTypeValType, StructHeapType, StructRefType,
  StructType, V128ValType, ValTypeBlockType, ValTypeStorageType, Var,
  VoidBlockType,
}

pub type TypeIndexes {
  TypeIndexes(index: Dict(String, TypeIDX), rec_types: Dict(TypeIDX, RecType))
}

// array
// func
// struct

/// Generate an Array type to be used in the generated module
pub fn array_type(field_type: FieldType) {
  ArrayType(field_type)
}

pub fn i32_field_type(mut: Bool) {
  FieldType(ValTypeStorageType(I32ValType), case mut {
    True -> Var
    False -> Const
  })
}

pub fn i64_field_type(mut: Bool) {
  FieldType(ValTypeStorageType(I64ValType), case mut {
    True -> Var
    False -> Const
  })
}

pub fn f32_field_type(mut: Bool) {
  FieldType(ValTypeStorageType(F32ValType), case mut {
    True -> Var
    False -> Const
  })
}

pub fn f64_field_type(mut: Bool) {
  FieldType(ValTypeStorageType(F64ValType), case mut {
    True -> Var
    False -> Const
  })
}

pub fn v128_field_type(mut: Bool) {
  FieldType(ValTypeStorageType(V128ValType), case mut {
    True -> Var
    False -> Const
  })
}

pub fn i8_field_type(mut: Bool) {
  FieldType(I8StorageType, case mut {
    True -> Var
    False -> Const
  })
}

pub fn i16_field_type(mut: Bool) {
  FieldType(I16StorageType, case mut {
    True -> Var
    False -> Const
  })
}

pub fn concrete_field_type(type_idx: TypeIDX) {
  FieldType(
    ValTypeStorageType(
      RefTypeValType(HeapTypeRefType(ConcreteHeapType(type_idx), False)),
    ),
    Const,
  )
}

pub fn struct_type(fields: List(FieldType)) {
  StructType(finger_tree.from_list(fields))
}

pub opaque type TypeIndexBuilder {
  TypeIndexBuilder(rec_types: Dict(TypeIDX, RecType))
}

pub fn new() -> TypeIndexBuilder {
  TypeIndexBuilder(rec_types: dict.new())
}

pub fn void_block_type() {
  VoidBlockType
}

pub fn val_type_block_type(vt: ValType) {
  ValTypeBlockType(vt)
}

pub fn func_type_block_type(ft: TypeIDX) {
  FuncTypeBlockType(ft)
}

pub fn unwrap() -> Result(TypeIndexes, String) {
  todo
}

pub fn any_heap_type() {
  AnyHeapType
}

pub fn any_ref_type() {
  AnyRefType
}

pub fn array_heap_type() {
  ArrayHeapType
}

pub fn array_ref_type() {
  ArrayRefType
}

pub fn eq_heap_type() {
  EqHeapType
}

pub fn eq_ref_type() {
  EqRefType
}

pub fn extern_heap_type() {
  ExternHeapType
}

pub fn extern_ref_type() {
  ExternRefType
}

pub fn func_heap_type() {
  FuncHeapType
}

pub fn func_ref_type() {
  FuncRefType
}

pub fn i31_heap_type() {
  I31HeapType
}

pub fn i31_ref_type() {
  I31RefType
}

pub fn no_extern_heap_type() {
  NoExternHeapType
}

pub fn no_extern_ref_type() {
  NoExternRefType
}

pub fn no_func_heap_type() {
  NoFuncHeapType
}

pub fn no_func_ref_type() {
  NoFuncRefType
}

pub fn none_heap_type() {
  NoneHeapType
}

pub fn none_ref_type() {
  NoneRefType
}

pub fn ref_type_val_type() {
  RefTypeValType
}

pub fn struct_heap_type() {
  StructHeapType
}

pub fn struct_ref_type() {
  StructRefType
}
