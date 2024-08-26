import gleam/option.{type Option, None, Some}
import gleam/result
import internal/finger_tree.{type FingerTree}
import internal/structure/numbers.{type U32}
import internal/structure/types.{
  type ArrayType, type CompositeType, type DefType, type FieldType,
  type FuncType, type HeapType, type RecType, type RefType, type StorageType,
  type StructType, type SubType, type TypeIDX, type ValType, AnyHeapType,
  ArrayCompositeType, ArrayHeapType, ArrayType, ConcreteHeapType,
  DefTypeReference, EqHeapType, ExternHeapType, FuncCompositeType, FuncHeapType,
  HeapTypeRefType, I31HeapType, NoExternHeapType, NoFuncHeapType, NoneHeapType,
  RecType, RecTypeIDX, StructCompositeType, StructHeapType, SubType, TypeIDX,
}

type VisitorCallBack(ctx, type_type) =
  fn(ctx, type_type) -> Result(#(ctx, type_type), String)

pub type TypeVisitor(ctx) {
  TypeVisitor(
    heap_type_visitor: Option(VisitorCallBack(ctx, HeapType)),
    ref_type_visitor: Option(VisitorCallBack(ctx, RefType)),
    val_type_visitor: Option(VisitorCallBack(ctx, ValType)),
    func_type_visitor: Option(VisitorCallBack(ctx, FuncType)),
    struct_type_visitor: Option(VisitorCallBack(ctx, StructType)),
    array_type_visitor: Option(VisitorCallBack(ctx, ArrayType)),
    field_type_visitor: Option(VisitorCallBack(ctx, FieldType)),
    storage_type_visitor: Option(VisitorCallBack(ctx, StorageType)),
    composite_type_visitor: Option(VisitorCallBack(ctx, CompositeType)),
    rec_type_visitor: Option(VisitorCallBack(ctx, RecType)),
    sub_type_visitor: Option(VisitorCallBack(ctx, SubType)),
    type_idx_visitor: Option(VisitorCallBack(ctx, TypeIDX)),
    rec_type_idx_visitor: Option(VisitorCallBack(ctx, TypeIDX)),
    def_type_reference_visitor: Option(VisitorCallBack(ctx, TypeIDX)),
    func_heap_type_visitor: Option(VisitorCallBack(ctx, HeapType)),
    no_func_heap_type_visitor: Option(VisitorCallBack(ctx, HeapType)),
    extern_heap_type_visitor: Option(VisitorCallBack(ctx, HeapType)),
    no_extern_heap_type_visitor: Option(VisitorCallBack(ctx, HeapType)),
    any_heap_type_visitor: Option(VisitorCallBack(ctx, HeapType)),
    eq_heap_type_visitor: Option(VisitorCallBack(ctx, HeapType)),
    i31_heap_type_visitor: Option(VisitorCallBack(ctx, HeapType)),
    struct_heap_type_visitor: Option(VisitorCallBack(ctx, HeapType)),
    array_heap_type_visitor: Option(VisitorCallBack(ctx, HeapType)),
    none_heap_type_visitor: Option(VisitorCallBack(ctx, HeapType)),
    concrete_heap_type_visitor: Option(VisitorCallBack(ctx, HeapType)),
    heap_type_ref_type_visitor: Option(VisitorCallBack(ctx, RefType)),
    any_ref_type_visitor: Option(VisitorCallBack(ctx, RefType)),
    eq_ref_type_visitor: Option(VisitorCallBack(ctx, RefType)),
    i31_ref_type_visitor: Option(VisitorCallBack(ctx, RefType)),
    struct_ref_type_visitor: Option(VisitorCallBack(ctx, RefType)),
    array_ref_type_visitor: Option(VisitorCallBack(ctx, RefType)),
    func_ref_type_visitor: Option(VisitorCallBack(ctx, RefType)),
    extern_ref_type_visitor: Option(VisitorCallBack(ctx, RefType)),
    none_ref_type_visitor: Option(VisitorCallBack(ctx, RefType)),
    no_func_ref_type_visitor: Option(VisitorCallBack(ctx, RefType)),
    no_extern_ref_type_visitor: Option(VisitorCallBack(ctx, RefType)),
    v128_val_type_visitor: Option(VisitorCallBack(ctx, ValType)),
    i32_val_type_visitor: Option(VisitorCallBack(ctx, ValType)),
    i64_val_type_visitor: Option(VisitorCallBack(ctx, ValType)),
    f32_val_type_visitor: Option(VisitorCallBack(ctx, ValType)),
    f64_val_type_visitor: Option(VisitorCallBack(ctx, ValType)),
    ref_type_val_type_visitor: Option(VisitorCallBack(ctx, ValType)),
    val_type_storage_type_visitor: Option(VisitorCallBack(ctx, StorageType)),
    i8_storage_type_visitor: Option(VisitorCallBack(ctx, StorageType)),
    i16_storage_type_visitor: Option(VisitorCallBack(ctx, StorageType)),
    func_composite_type_visitor: Option(VisitorCallBack(ctx, CompositeType)),
    struct_composite_type_visitor: Option(VisitorCallBack(ctx, CompositeType)),
    array_composite_type_visitor: Option(VisitorCallBack(ctx, CompositeType)),
    concrete_array_type_visitor: Option(VisitorCallBack(ctx, TypeIDX)),
    concrete_func_type_visitor: Option(VisitorCallBack(ctx, TypeIDX)),
    concrete_struct_type_visitor: Option(VisitorCallBack(ctx, TypeIDX)),
    def_type_visitor: Option(VisitorCallBack(ctx, DefType)),
  )
}

pub fn on_heap_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, HeapType),
) {
  TypeVisitor(..visitor, heap_type_visitor: Some(cb))
}

pub fn on_ref_type(visitor: TypeVisitor(ctx), cb: VisitorCallBack(ctx, RefType)) {
  TypeVisitor(..visitor, ref_type_visitor: Some(cb))
}

pub fn on_val_type(visitor: TypeVisitor(ctx), cb: VisitorCallBack(ctx, ValType)) {
  TypeVisitor(..visitor, val_type_visitor: Some(cb))
}

pub fn on_func_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, FuncType),
) {
  TypeVisitor(..visitor, func_type_visitor: Some(cb))
}

pub fn on_struct_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, StructType),
) {
  TypeVisitor(..visitor, struct_type_visitor: Some(cb))
}

pub fn on_array_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, ArrayType),
) {
  TypeVisitor(..visitor, array_type_visitor: Some(cb))
}

pub fn on_field_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, FieldType),
) {
  TypeVisitor(..visitor, field_type_visitor: Some(cb))
}

pub fn on_storage_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, StorageType),
) {
  TypeVisitor(..visitor, storage_type_visitor: Some(cb))
}

pub fn on_composite_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, CompositeType),
) {
  TypeVisitor(..visitor, composite_type_visitor: Some(cb))
}

pub fn on_rec_type(visitor: TypeVisitor(ctx), cb: VisitorCallBack(ctx, RecType)) {
  TypeVisitor(..visitor, rec_type_visitor: Some(cb))
}

pub fn on_sub_type(visitor: TypeVisitor(ctx), cb: VisitorCallBack(ctx, SubType)) {
  TypeVisitor(..visitor, sub_type_visitor: Some(cb))
}

pub fn on_type_idx(visitor: TypeVisitor(ctx), cb: VisitorCallBack(ctx, TypeIDX)) {
  TypeVisitor(..visitor, type_idx_visitor: Some(cb))
}

pub fn on_rec_type_idx(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, TypeIDX),
) {
  TypeVisitor(..visitor, rec_type_idx_visitor: Some(cb))
}

pub fn on_def_type_reference(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, TypeIDX),
) {
  TypeVisitor(..visitor, def_type_reference_visitor: Some(cb))
}

pub fn on_func_heap_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, HeapType),
) {
  TypeVisitor(..visitor, func_heap_type_visitor: Some(cb))
}

pub fn on_no_func_heap_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, HeapType),
) {
  TypeVisitor(..visitor, no_func_heap_type_visitor: Some(cb))
}

pub fn on_extern_heap_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, HeapType),
) {
  TypeVisitor(..visitor, extern_heap_type_visitor: Some(cb))
}

pub fn on_no_extern_heap_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, HeapType),
) {
  TypeVisitor(..visitor, no_extern_heap_type_visitor: Some(cb))
}

pub fn on_any_heap_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, HeapType),
) {
  TypeVisitor(..visitor, any_heap_type_visitor: Some(cb))
}

pub fn on_eq_heap_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, HeapType),
) {
  TypeVisitor(..visitor, eq_heap_type_visitor: Some(cb))
}

pub fn on_i31_heap_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, HeapType),
) {
  TypeVisitor(..visitor, i31_heap_type_visitor: Some(cb))
}

pub fn on_struct_heap_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, HeapType),
) {
  TypeVisitor(..visitor, struct_heap_type_visitor: Some(cb))
}

pub fn on_array_heap_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, HeapType),
) {
  TypeVisitor(..visitor, array_heap_type_visitor: Some(cb))
}

pub fn on_none_heap_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, HeapType),
) {
  TypeVisitor(..visitor, none_heap_type_visitor: Some(cb))
}

pub fn on_concrete_heap_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, HeapType),
) {
  TypeVisitor(..visitor, concrete_heap_type_visitor: Some(cb))
}

pub fn on_heap_type_ref_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, RefType),
) {
  TypeVisitor(..visitor, heap_type_ref_type_visitor: Some(cb))
}

pub fn on_any_ref_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, RefType),
) {
  TypeVisitor(..visitor, any_ref_type_visitor: Some(cb))
}

pub fn on_eq_ref_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, RefType),
) {
  TypeVisitor(..visitor, eq_ref_type_visitor: Some(cb))
}

pub fn on_i31_ref_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, RefType),
) {
  TypeVisitor(..visitor, i31_ref_type_visitor: Some(cb))
}

pub fn on_struct_ref_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, RefType),
) {
  TypeVisitor(..visitor, struct_ref_type_visitor: Some(cb))
}

pub fn on_array_ref_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, RefType),
) {
  TypeVisitor(..visitor, array_ref_type_visitor: Some(cb))
}

pub fn on_func_ref_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, RefType),
) {
  TypeVisitor(..visitor, func_ref_type_visitor: Some(cb))
}

pub fn on_extern_ref_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, RefType),
) {
  TypeVisitor(..visitor, extern_ref_type_visitor: Some(cb))
}

pub fn on_none_ref_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, RefType),
) {
  TypeVisitor(..visitor, none_ref_type_visitor: Some(cb))
}

pub fn on_no_func_ref_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, RefType),
) {
  TypeVisitor(..visitor, no_func_ref_type_visitor: Some(cb))
}

pub fn on_no_extern_ref_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, RefType),
) {
  TypeVisitor(..visitor, no_extern_ref_type_visitor: Some(cb))
}

pub fn on_v128_val_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, ValType),
) {
  TypeVisitor(..visitor, v128_val_type_visitor: Some(cb))
}

pub fn on_i32_val_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, ValType),
) {
  TypeVisitor(..visitor, i32_val_type_visitor: Some(cb))
}

pub fn on_i64_val_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, ValType),
) {
  TypeVisitor(..visitor, i64_val_type_visitor: Some(cb))
}

pub fn on_f32_val_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, ValType),
) {
  TypeVisitor(..visitor, f32_val_type_visitor: Some(cb))
}

pub fn on_f64_val_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, ValType),
) {
  TypeVisitor(..visitor, f64_val_type_visitor: Some(cb))
}

pub fn on_ref_type_val_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, ValType),
) {
  TypeVisitor(..visitor, ref_type_val_type_visitor: Some(cb))
}

pub fn on_val_type_storage_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, StorageType),
) {
  TypeVisitor(..visitor, val_type_storage_type_visitor: Some(cb))
}

pub fn on_i8_storage_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, StorageType),
) {
  TypeVisitor(..visitor, i8_storage_type_visitor: Some(cb))
}

pub fn on_i16_storage_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, StorageType),
) {
  TypeVisitor(..visitor, i16_storage_type_visitor: Some(cb))
}

pub fn on_func_composite_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, CompositeType),
) {
  TypeVisitor(..visitor, func_composite_type_visitor: Some(cb))
}

pub fn on_struct_composite_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, CompositeType),
) {
  TypeVisitor(..visitor, struct_composite_type_visitor: Some(cb))
}

pub fn on_array_composite_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, CompositeType),
) {
  TypeVisitor(..visitor, array_composite_type_visitor: Some(cb))
}

pub fn on_concrete_array_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, TypeIDX),
) {
  TypeVisitor(..visitor, concrete_array_type_visitor: Some(cb))
}

pub fn on_concrete_func_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, TypeIDX),
) {
  TypeVisitor(..visitor, concrete_func_type_visitor: Some(cb))
}

pub fn on_concrete_struct_type(
  visitor: TypeVisitor(ctx),
  cb: VisitorCallBack(ctx, TypeIDX),
) {
  TypeVisitor(..visitor, concrete_struct_type_visitor: Some(cb))
}

pub fn on_def_type(visitor: TypeVisitor(ctx), cb: VisitorCallBack(ctx, DefType)) {
  TypeVisitor(..visitor, def_type_visitor: Some(cb))
}

pub fn new_visitor() -> TypeVisitor(ctx) {
  TypeVisitor(
    heap_type_visitor: None,
    ref_type_visitor: None,
    val_type_visitor: None,
    func_type_visitor: None,
    struct_type_visitor: None,
    array_type_visitor: None,
    field_type_visitor: None,
    storage_type_visitor: None,
    composite_type_visitor: None,
    rec_type_visitor: None,
    sub_type_visitor: None,
    type_idx_visitor: None,
    rec_type_idx_visitor: None,
    def_type_reference_visitor: None,
    func_heap_type_visitor: None,
    no_func_heap_type_visitor: None,
    extern_heap_type_visitor: None,
    no_extern_heap_type_visitor: None,
    any_heap_type_visitor: None,
    eq_heap_type_visitor: None,
    i31_heap_type_visitor: None,
    struct_heap_type_visitor: None,
    array_heap_type_visitor: None,
    none_heap_type_visitor: None,
    concrete_heap_type_visitor: None,
    heap_type_ref_type_visitor: None,
    any_ref_type_visitor: None,
    eq_ref_type_visitor: None,
    i31_ref_type_visitor: None,
    struct_ref_type_visitor: None,
    array_ref_type_visitor: None,
    func_ref_type_visitor: None,
    extern_ref_type_visitor: None,
    none_ref_type_visitor: None,
    no_func_ref_type_visitor: None,
    no_extern_ref_type_visitor: None,
    v128_val_type_visitor: None,
    i32_val_type_visitor: None,
    i64_val_type_visitor: None,
    f32_val_type_visitor: None,
    f64_val_type_visitor: None,
    ref_type_val_type_visitor: None,
    val_type_storage_type_visitor: None,
    i8_storage_type_visitor: None,
    i16_storage_type_visitor: None,
    func_composite_type_visitor: None,
    struct_composite_type_visitor: None,
    array_composite_type_visitor: None,
    concrete_array_type_visitor: None,
    concrete_func_type_visitor: None,
    concrete_struct_type_visitor: None,
    def_type_visitor: None,
  )
}

pub fn visit_array_type(ctx: ctx, at: ArrayType, visitor: TypeVisitor(ctx)) {
  let visitor_result = case visitor.array_type_visitor {
    Some(cb) -> cb(ctx, at)
    None -> Ok(#(ctx, at))
  }
  use #(ctx, ArrayType(ft)) <- result.try(visitor_result)
  use #(ctx, ft) <- result.map(visit_field_type(ctx, ft, visitor))
  #(ctx, ArrayType(ft))
}

pub fn visit_composite_type(
  ctx: ctx,
  ct: CompositeType,
  visitor: TypeVisitor(ctx),
) {
  let visitor_result = case visitor.composite_type_visitor {
    Some(cb) -> cb(ctx, ct)
    None -> Ok(#(ctx, ct))
  }

  use #(ctx, ct) <- result.try(visitor_result)
  let visitor_result = case ct {
    FuncCompositeType(_) ->
      case visitor.func_composite_type_visitor {
        Some(cb) -> cb(ctx, ct)
        None -> Ok(#(ctx, ct))
      }
    StructCompositeType(_) ->
      case visitor.struct_composite_type_visitor {
        Some(cb) -> cb(ctx, ct)
        None -> Ok(#(ctx, ct))
      }
    ArrayCompositeType(_) ->
      case visitor.array_composite_type_visitor {
        Some(cb) -> cb(ctx, ct)
        None -> Ok(#(ctx, ct))
      }
  }

  use #(ctx, ct) <- result.try(visitor_result)
  case ct {
    FuncCompositeType(ft) -> {
      use #(ctx, ft) <- result.map(visit_func_type(ctx, ft, visitor))
      #(ctx, FuncCompositeType(ft))
    }
    StructCompositeType(st) -> {
      use #(ctx, st) <- result.map(visit_struct_type(ctx, st, visitor))
      #(ctx, StructCompositeType(st))
    }
    ArrayCompositeType(at) -> {
      use #(ctx, at) <- result.map(visit_array_type(ctx, at, visitor))
      #(ctx, ArrayCompositeType(at))
    }
  }
}

pub fn visit_field_type(ctx: ctx, rt: FieldType, visitor: TypeVisitor(ctx)) {
  todo
}

pub fn visit_func_type(ctx: ctx, rt: FuncType, visitor: TypeVisitor(ctx)) {
  todo
}

pub fn visit_heap_type(ctx: ctx, ht: HeapType, visitor: TypeVisitor(ctx)) {
  let visitor_result = case visitor.heap_type_visitor {
    Some(cb) -> cb(ctx, ht)
    None -> Ok(#(ctx, ht))
  }
  use #(ctx, ht) <- result.try(visitor_result)

  let visitor_result = case ht {
    FuncHeapType ->
      case visitor.func_heap_type_visitor {
        Some(cb) -> cb(ctx, ht)
        None -> Ok(#(ctx, ht))
      }
    NoFuncHeapType ->
      case visitor.no_func_heap_type_visitor {
        Some(cb) -> cb(ctx, ht)
        None -> Ok(#(ctx, ht))
      }
    ExternHeapType ->
      case visitor.extern_heap_type_visitor {
        Some(cb) -> cb(ctx, ht)
        None -> Ok(#(ctx, ht))
      }
    NoExternHeapType ->
      case visitor.no_extern_heap_type_visitor {
        Some(cb) -> cb(ctx, ht)
        None -> Ok(#(ctx, ht))
      }
    AnyHeapType ->
      case visitor.any_heap_type_visitor {
        Some(cb) -> cb(ctx, ht)
        None -> Ok(#(ctx, ht))
      }
    EqHeapType ->
      case visitor.eq_heap_type_visitor {
        Some(cb) -> cb(ctx, ht)
        None -> Ok(#(ctx, ht))
      }
    I31HeapType ->
      case visitor.i31_heap_type_visitor {
        Some(cb) -> cb(ctx, ht)
        None -> Ok(#(ctx, ht))
      }
    StructHeapType ->
      case visitor.struct_heap_type_visitor {
        Some(cb) -> cb(ctx, ht)
        None -> Ok(#(ctx, ht))
      }
    ArrayHeapType ->
      case visitor.array_heap_type_visitor {
        Some(cb) -> cb(ctx, ht)
        None -> Ok(#(ctx, ht))
      }
    NoneHeapType ->
      case visitor.none_heap_type_visitor {
        Some(cb) -> cb(ctx, ht)
        None -> Ok(#(ctx, ht))
      }
    ConcreteHeapType(_) ->
      case visitor.concrete_heap_type_visitor {
        Some(cb) -> cb(ctx, ht)
        None -> Ok(#(ctx, ht))
      }
    _ -> Ok(#(ctx, ht))
  }

  use #(ctx, ht) <- result.try(visitor_result)
  case ht {
    ConcreteHeapType(idx) -> {
      let visitor_result = visit_type_idx(ctx, idx, visitor)

      use #(ctx, idx) <- result.map(visitor_result)
      #(ctx, ConcreteHeapType(idx))
    }
    _ -> Ok(#(ctx, ht))
  }
}

pub fn visit_rec_type(ctx: ctx, rt: RecType, visitor: TypeVisitor(ctx)) {
  let visitor_result = case visitor.rec_type_visitor {
    Some(cb) -> cb(ctx, rt)
    None -> Ok(#(ctx, rt))
  }

  use #(ctx, rt) <- result.try(visitor_result)
  let RecType(sub_types) = rt

  use #(ctx, sub_types) <- result.map(visit_sub_types(ctx, sub_types, visitor))
  #(ctx, RecType(sub_types))
}

pub fn visit_ref_type(ctx: ctx, rt: RefType, visitor: TypeVisitor(ctx)) {
  let visitor_result = case visitor.ref_type_visitor {
    Some(cb) -> cb(ctx, rt)
    None -> Ok(#(ctx, rt))
  }
  use #(ctx, rt) <- result.try(visitor_result)
  case rt {
    HeapTypeRefType(ht, mut) -> {
      use #(ctx, ht) <- result.map(visit_heap_type(ctx, ht, visitor))

      #(ctx, HeapTypeRefType(ht, mut))
    }
    _ -> Ok(#(ctx, rt))
  }
}

pub fn visit_storage_type(ctx: ctx, rt: StorageType, visitor: TypeVisitor(ctx)) {
  todo
}

pub fn visit_struct_type(ctx: ctx, rt: StructType, visitor: TypeVisitor(ctx)) {
  todo
}

pub fn visit_sub_types(
  ctx: ctx,
  sub_types: FingerTree(SubType),
  visitor: TypeVisitor(ctx),
) {
  sub_types
  |> finger_tree.try_reducel(#(ctx, finger_tree.new()), fn(init, sub_type) {
    let #(ctx, acc) = init
    use #(ctx, sub_type) <- result.map(visit_sub_type(ctx, sub_type, visitor))
    #(ctx, acc |> finger_tree.push(sub_type))
  })
}

pub fn visit_sub_type(ctx: ctx, st: SubType, visitor: TypeVisitor(ctx)) {
  let visitor_result = case visitor.sub_type_visitor {
    Some(cb) -> cb(ctx, st)
    None -> Ok(#(ctx, st))
  }

  use #(ctx, st) <- result.try(visitor_result)
  let SubType(final, t, ct) = st

  use #(ctx, t) <- result.try(visit_type_idxs(ctx, t, visitor))
  use #(ctx, ct) <- result.map(visit_composite_type(ctx, ct, visitor))

  #(ctx, SubType(final, t, ct))
}

pub fn visit_type_idxs(
  ctx: ctx,
  idxs: FingerTree(TypeIDX),
  visitor: TypeVisitor(ctx),
) {
  idxs
  |> finger_tree.try_reducel(#(ctx, finger_tree.new()), fn(init, idx) {
    let #(ctx, acc) = init
    use #(ctx, idx) <- result.map(visit_type_idx(ctx, idx, visitor))
    #(ctx, acc |> finger_tree.push(idx))
  })
}

pub fn visit_type_idx(ctx: ctx, idx: TypeIDX, visitor: TypeVisitor(ctx)) {
  let visitor_result = case idx {
    TypeIDX(_) ->
      case visitor.type_idx_visitor {
        Some(cb) -> cb(ctx, idx)
        None -> Ok(#(ctx, idx))
      }
    RecTypeIDX(_) ->
      case visitor.rec_type_idx_visitor {
        Some(cb) -> cb(ctx, idx)
        None -> Ok(#(ctx, idx))
      }
    DefTypeReference(_) ->
      case visitor.def_type_reference_visitor {
        Some(cb) -> cb(ctx, idx)
        None -> Ok(#(ctx, idx))
      }
  }

  use #(ctx, idx) <- result.try(visitor_result)
  case idx, visitor.def_type_visitor {
    DefTypeReference(dt), Some(cb) -> {
      use #(ctx, dt) <- result.map(cb(ctx, dt))
      #(ctx, DefTypeReference(dt))
    }
    _, _ -> Ok(#(ctx, idx))
  }
}

pub fn visit_val_type(ctx: ctx, rt: ValType, visitor: TypeVisitor(ctx)) {
  todo
}

pub fn visit_vec(
  ctx: ctx,
  items: FingerTree(type_type),
  visitor: fn(ctx, type_type) -> Result(#(ctx, type_type), String),
) -> Result(#(ctx, FingerTree(type_type)), String) {
  finger_tree.try_reducel(items, #(ctx, finger_tree.new()), fn(init, item) {
    let #(ctx, items) = init
    use #(ctx, item) <- result.map(visitor(ctx, item))
    #(ctx, items |> finger_tree.push(item))
  })
}
