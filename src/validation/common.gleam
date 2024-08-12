import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import structure/types.{
  type AbstractHeapType, type ArrayType, type CompositeType, type Context,
  type DefType, type ExternType, type FieldType, type FuncType, type GlobalType,
  type HeapType, type Instruction, type InstructionType, type LocalType,
  type MemType, type NumType, type PackedType, type RecType, type RefType,
  type ResultType, type ResultTypes, type StorageType, type StructType,
  type SubType, type TableType, type ValType, type VecType, AnyHeapType,
  AnyRefType, ArrayCompositeType, ArrayHeapType, ArrayRefType, ArrayType,
  BotHeapType, DefType, DefTypeHeapType, EightResultTypes, EqHeapType, EqRefType,
  ExternHeapType, ExternRefType, F32ValType, F64ValType, FieldType,
  FiveResultTypes, FourResultTypes, FuncCompositeType, FuncExternType,
  FuncHeapType, FuncRefType, FuncType, GlobalExternType, GlobalType,
  HeapTypeRefType, I31HeapType, I31RefType, I32ValType, I64ValType, LocalType,
  MemExternType, NineResultTypes, NoExternHeapType, NoFuncHeapType, NoneHeapType,
  NullExternRefType, NullFuncRefType, NullRefType, OneResultType, RecType,
  RecTypeHeapType, RefTypeValType, ResultType, SevenResultTypes, SixResultTypes,
  StructCompositeType, StructHeapType, StructRefType, StructType, SubType,
  TableExternType, TableType, TenResultTypes, ThreeResultTypes, TwoResultTypes,
  UnrolledSubType, V128ValType, ValTypeStorageType, VecTypeValType,
}

type NumTypeVisitor =
  fn(Context, NumType) -> Result(#(Context, NumType), String)

type VecTypeVisitor =
  fn(Context, VecType) -> Result(#(Context, VecType), String)

type HeapTypeVisitor =
  fn(Context, HeapType) -> Result(#(Context, HeapType), String)

type AbstractHeapTypeVisitor =
  fn(Context, AbstractHeapType) -> Result(#(Context, AbstractHeapType), String)

type RefTypeVisitor =
  fn(Context, RefType) -> Result(#(Context, RefType), String)

type ValTypeVisitor =
  fn(Context, ValType) -> Result(#(Context, ValType), String)

type ResultTypeVisitor =
  fn(Context, ResultType) -> Result(#(Context, ResultType), String)

type ResultTypesVisitor =
  fn(Context, ResultTypes) -> Result(#(Context, ResultTypes), String)

type FuncTypeVisitor =
  fn(Context, FuncType) -> Result(#(Context, FuncType), String)

type StructTypeVisitor =
  fn(Context, StructType) -> Result(#(Context, StructType), String)

type ArrayTypeVisitor =
  fn(Context, ArrayType) -> Result(#(Context, ArrayType), String)

type FieldTypeVisitor =
  fn(Context, FieldType) -> Result(#(Context, FieldType), String)

type StorageTypeVisitor =
  fn(Context, StorageType) -> Result(#(Context, StorageType), String)

type PackedTypeVisitor =
  fn(Context, PackedType) -> Result(#(Context, PackedType), String)

type CompositeTypeVisitor =
  fn(Context, CompositeType) -> Result(#(Context, CompositeType), String)

type RecTypeVisitor =
  fn(Context, RecType) -> Result(#(Context, RecType), String)

type SubTypeVisitor =
  fn(Context, SubType) -> Result(#(Context, SubType), String)

type MemTypeVisitor =
  fn(Context, MemType) -> Result(#(Context, MemType), String)

type TableTypeVisitor =
  fn(Context, TableType) -> Result(#(Context, TableType), String)

type GlobalTypeVisitor =
  fn(Context, GlobalType) -> Result(#(Context, GlobalType), String)

type ExternTypeVisitor =
  fn(Context, ExternType) -> Result(#(Context, ExternType), String)

type InstructionVisitor =
  fn(Context, Instruction) -> Result(#(Context, Instruction), String)

type DefTypeVisitor =
  fn(Context, DefType) -> Result(#(Context, DefType), String)

type LocalTypeVisitor =
  fn(Context, LocalType) -> Result(#(Context, LocalType), String)

pub type TypeVisitor {
  TypeVisitor(
    num_type: Option(NumTypeVisitor),
    vec_type: Option(VecTypeVisitor),
    heap_type: Option(HeapTypeVisitor),
    abstract_heap_type: Option(AbstractHeapTypeVisitor),
    ref_type: Option(RefTypeVisitor),
    val_type: Option(ValTypeVisitor),
    result_type: Option(ResultTypeVisitor),
    result_types: Option(ResultTypesVisitor),
    func_type: Option(FuncTypeVisitor),
    struct_type: Option(StructTypeVisitor),
    array_type: Option(ArrayTypeVisitor),
    field_type: Option(FieldTypeVisitor),
    storage_type: Option(StorageTypeVisitor),
    packed_type: Option(PackedTypeVisitor),
    composite_type: Option(CompositeTypeVisitor),
    rec_type: Option(RecTypeVisitor),
    sub_type: Option(SubTypeVisitor),
    mem_type: Option(MemTypeVisitor),
    table_type: Option(TableTypeVisitor),
    global_type: Option(GlobalTypeVisitor),
    extern_type: Option(ExternTypeVisitor),
    instruction_type: Option(InstructionVisitor),
    def_type: Option(DefTypeVisitor),
    local_type: Option(LocalTypeVisitor),
  )
}

pub fn on_num_type(v: TypeVisitor, vs: NumTypeVisitor) {
  TypeVisitor(..v, num_type: Some(vs))
}

pub fn on_vec_type(v: TypeVisitor, vs: VecTypeVisitor) {
  TypeVisitor(..v, vec_type: Some(vs))
}

pub fn on_heap_type(v: TypeVisitor, vs: HeapTypeVisitor) {
  TypeVisitor(..v, heap_type: Some(vs))
}

pub fn on_abstract_heap_type(v: TypeVisitor, vs: AbstractHeapTypeVisitor) {
  TypeVisitor(..v, abstract_heap_type: Some(vs))
}

pub fn on_ref_type(v: TypeVisitor, vs: RefTypeVisitor) {
  TypeVisitor(..v, ref_type: Some(vs))
}

pub fn on_val_type(v: TypeVisitor, vs: ValTypeVisitor) {
  TypeVisitor(..v, val_type: Some(vs))
}

pub fn on_result_type(v: TypeVisitor, vs: ResultTypeVisitor) {
  TypeVisitor(..v, result_type: Some(vs))
}

pub fn on_result_types(v: TypeVisitor, vs: ResultTypesVisitor) {
  TypeVisitor(..v, result_types: Some(vs))
}

pub fn on_func_type(v: TypeVisitor, vs: FuncTypeVisitor) {
  TypeVisitor(..v, func_type: Some(vs))
}

pub fn on_struct_type(v: TypeVisitor, vs: StructTypeVisitor) {
  TypeVisitor(..v, struct_type: Some(vs))
}

pub fn on_array_type(v: TypeVisitor, vs: ArrayTypeVisitor) {
  TypeVisitor(..v, array_type: Some(vs))
}

pub fn on_field_type(v: TypeVisitor, vs: FieldTypeVisitor) {
  TypeVisitor(..v, field_type: Some(vs))
}

pub fn on_storage_type(v: TypeVisitor, vs: StorageTypeVisitor) {
  TypeVisitor(..v, storage_type: Some(vs))
}

pub fn on_packed_type(v: TypeVisitor, vs: PackedTypeVisitor) {
  TypeVisitor(..v, packed_type: Some(vs))
}

pub fn on_composite_type(v: TypeVisitor, vs: CompositeTypeVisitor) {
  TypeVisitor(..v, composite_type: Some(vs))
}

pub fn on_rec_type(v: TypeVisitor, vs: RecTypeVisitor) {
  TypeVisitor(..v, rec_type: Some(vs))
}

pub fn on_sub_type(v: TypeVisitor, vs: SubTypeVisitor) {
  TypeVisitor(..v, sub_type: Some(vs))
}

pub fn on_mem_type(v: TypeVisitor, vs: MemTypeVisitor) {
  TypeVisitor(..v, mem_type: Some(vs))
}

pub fn on_table_type(v: TypeVisitor, vs: TableTypeVisitor) {
  TypeVisitor(..v, table_type: Some(vs))
}

pub fn on_global_type(v: TypeVisitor, vs: GlobalTypeVisitor) {
  TypeVisitor(..v, global_type: Some(vs))
}

pub fn on_extern_type(v: TypeVisitor, vs: ExternTypeVisitor) {
  TypeVisitor(..v, extern_type: Some(vs))
}

pub fn on_instruction(v: TypeVisitor, vs: InstructionVisitor) {
  TypeVisitor(..v, instruction_type: Some(vs))
}

pub fn on_def_type(v: TypeVisitor, vs: DefTypeVisitor) {
  TypeVisitor(..v, def_type: Some(vs))
}

pub fn on_local_type(v: TypeVisitor, vs: LocalTypeVisitor) {
  TypeVisitor(..v, local_type: Some(vs))
}

pub fn visitor_new() {
  TypeVisitor(
    num_type: None,
    vec_type: None,
    heap_type: None,
    abstract_heap_type: None,
    ref_type: None,
    val_type: None,
    result_type: None,
    result_types: None,
    func_type: None,
    struct_type: None,
    array_type: None,
    field_type: None,
    storage_type: None,
    packed_type: None,
    composite_type: None,
    rec_type: None,
    sub_type: None,
    mem_type: None,
    table_type: None,
    global_type: None,
    extern_type: None,
    instruction_type: None,
    def_type: None,
    local_type: None,
  )
}

pub fn visit_num_type(
  ctx: Context,
  ty: NumType,
  visitor: TypeVisitor,
) -> Result(#(Context, NumType), String) {
  case visitor.num_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  }
}

pub fn visit_vec_type(
  ctx: Context,
  ty: VecType,
  visitor: TypeVisitor,
) -> Result(#(Context, VecType), String) {
  case visitor.vec_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  }
}

fn do_visit_heap_types(
  ctx: Context,
  ht: List(HeapType),
  acc: List(HeapType),
  visitor: TypeVisitor,
) -> Result(#(Context, List(HeapType)), String) {
  case ht {
    [] -> Ok(#(ctx, acc))
    [ht, ..rest] -> {
      use #(ctx, ht) <- result.try(visit_heap_type(ctx, ht, visitor))
      use a <- result.map(do_visit_heap_types(ctx, rest, [ht, ..acc], visitor))
      a
    }
  }
}

pub fn visit_heap_type(
  ctx: Context,
  ty: HeapType,
  visitor: TypeVisitor,
) -> Result(#(Context, HeapType), String) {
  use #(ctx, ht) <- result.try(case visitor.heap_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  case ht {
    FuncHeapType
    | NoFuncHeapType
    | ExternHeapType
    | NoExternHeapType
    | AnyHeapType
    | EqHeapType
    | I31HeapType
    | StructHeapType
    | ArrayHeapType
    | NoneHeapType
    | BotHeapType
    | RecTypeHeapType(_) -> Ok(#(ctx, ht))

    DefTypeHeapType(dt) -> {
      use #(ctx, dt) <- result.map(visit_def_type(ctx, dt, visitor))
      #(ctx, DefTypeHeapType(dt))
    }
  }
}

pub fn visit_abstract_heap_type(
  ctx: Context,
  ty: AbstractHeapType,
  visitor: TypeVisitor,
) -> Result(#(Context, AbstractHeapType), String) {
  case visitor.abstract_heap_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  }
}

pub fn visit_ref_type(
  ctx: Context,
  ty: RefType,
  visitor: TypeVisitor,
) -> Result(#(Context, RefType), String) {
  use #(ctx, rt) <- result.try(case visitor.ref_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  case rt {
    HeapTypeRefType(ht, null) -> {
      use #(ctx, ht) <- result.map(visit_heap_type(ctx, ht, visitor))
      #(ctx, HeapTypeRefType(ht, null))
    }
    AnyRefType
    | EqRefType
    | I31RefType
    | StructRefType
    | ArrayRefType
    | FuncRefType
    | ExternRefType
    | NullRefType
    | NullFuncRefType
    | NullExternRefType -> Ok(#(ctx, rt))
  }
}

pub fn visit_val_type(
  ctx: Context,
  ty: ValType,
  visitor: TypeVisitor,
) -> Result(#(Context, ValType), String) {
  use #(ctx, vt) <- result.try(case visitor.val_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  case vt {
    V128ValType
    | I32ValType
    | I64ValType
    | F32ValType
    | F64ValType
    | VecTypeValType -> Ok(#(ctx, vt))

    RefTypeValType(rt) -> {
      use #(ctx, rt) <- result.map(visit_ref_type(ctx, rt, visitor))
      #(ctx, RefTypeValType(rt))
    }
  }
}

pub fn visit_result_types(
  ctx: Context,
  ty: ResultTypes,
  visitor: TypeVisitor,
) -> Result(#(Context, ResultTypes), String) {
  use #(ctx, rt) <- result.try(case visitor.result_types {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  case rt {
    OneResultType(v0) -> {
      use #(ctx, v0) <- result.map(visit_val_type(ctx, v0, visitor))
      #(ctx, OneResultType(v0))
    }
    TwoResultTypes(v0, v1) -> {
      use #(ctx, v0) <- result.try(visit_val_type(ctx, v0, visitor))
      use #(ctx, v1) <- result.map(visit_val_type(ctx, v1, visitor))
      #(ctx, TwoResultTypes(v0, v1))
    }
    ThreeResultTypes(v0, v1, v2) -> {
      use #(ctx, v0) <- result.try(visit_val_type(ctx, v0, visitor))
      use #(ctx, v1) <- result.try(visit_val_type(ctx, v1, visitor))
      use #(ctx, v2) <- result.map(visit_val_type(ctx, v2, visitor))
      #(ctx, ThreeResultTypes(v0, v1, v2))
    }
    FourResultTypes(v0, v1, v2, v3) -> {
      use #(ctx, v0) <- result.try(visit_val_type(ctx, v0, visitor))
      use #(ctx, v1) <- result.try(visit_val_type(ctx, v1, visitor))
      use #(ctx, v2) <- result.try(visit_val_type(ctx, v2, visitor))
      use #(ctx, v3) <- result.map(visit_val_type(ctx, v3, visitor))
      #(ctx, FourResultTypes(v0, v1, v2, v3))
    }
    FiveResultTypes(v0, v1, v2, v3, v4) -> {
      use #(ctx, v0) <- result.try(visit_val_type(ctx, v0, visitor))
      use #(ctx, v1) <- result.try(visit_val_type(ctx, v1, visitor))
      use #(ctx, v2) <- result.try(visit_val_type(ctx, v2, visitor))
      use #(ctx, v3) <- result.try(visit_val_type(ctx, v3, visitor))
      use #(ctx, v4) <- result.map(visit_val_type(ctx, v4, visitor))
      #(ctx, FiveResultTypes(v0, v1, v2, v3, v4))
    }
    SixResultTypes(v0, v1, v2, v3, v4, v5) -> {
      use #(ctx, v0) <- result.try(visit_val_type(ctx, v0, visitor))
      use #(ctx, v1) <- result.try(visit_val_type(ctx, v1, visitor))
      use #(ctx, v2) <- result.try(visit_val_type(ctx, v2, visitor))
      use #(ctx, v3) <- result.try(visit_val_type(ctx, v3, visitor))
      use #(ctx, v4) <- result.try(visit_val_type(ctx, v4, visitor))
      use #(ctx, v5) <- result.map(visit_val_type(ctx, v5, visitor))
      #(ctx, SixResultTypes(v0, v1, v2, v3, v4, v5))
    }
    SevenResultTypes(v0, v1, v2, v3, v4, v5, v6) -> {
      use #(ctx, v0) <- result.try(visit_val_type(ctx, v0, visitor))
      use #(ctx, v1) <- result.try(visit_val_type(ctx, v1, visitor))
      use #(ctx, v2) <- result.try(visit_val_type(ctx, v2, visitor))
      use #(ctx, v3) <- result.try(visit_val_type(ctx, v3, visitor))
      use #(ctx, v4) <- result.try(visit_val_type(ctx, v4, visitor))
      use #(ctx, v5) <- result.try(visit_val_type(ctx, v5, visitor))
      use #(ctx, v6) <- result.map(visit_val_type(ctx, v6, visitor))
      #(ctx, SevenResultTypes(v0, v1, v2, v3, v4, v5, v6))
    }
    EightResultTypes(v0, v1, v2, v3, v4, v5, v6, v7) -> {
      use #(ctx, v0) <- result.try(visit_val_type(ctx, v0, visitor))
      use #(ctx, v1) <- result.try(visit_val_type(ctx, v1, visitor))
      use #(ctx, v2) <- result.try(visit_val_type(ctx, v2, visitor))
      use #(ctx, v3) <- result.try(visit_val_type(ctx, v3, visitor))
      use #(ctx, v4) <- result.try(visit_val_type(ctx, v4, visitor))
      use #(ctx, v5) <- result.try(visit_val_type(ctx, v5, visitor))
      use #(ctx, v6) <- result.try(visit_val_type(ctx, v6, visitor))
      use #(ctx, v7) <- result.map(visit_val_type(ctx, v7, visitor))
      #(ctx, EightResultTypes(v0, v1, v2, v3, v4, v5, v6, v7))
    }
    NineResultTypes(v0, v1, v2, v3, v4, v5, v6, v7, v8) -> {
      use #(ctx, v0) <- result.try(visit_val_type(ctx, v0, visitor))
      use #(ctx, v1) <- result.try(visit_val_type(ctx, v1, visitor))
      use #(ctx, v2) <- result.try(visit_val_type(ctx, v2, visitor))
      use #(ctx, v3) <- result.try(visit_val_type(ctx, v3, visitor))
      use #(ctx, v4) <- result.try(visit_val_type(ctx, v4, visitor))
      use #(ctx, v5) <- result.try(visit_val_type(ctx, v5, visitor))
      use #(ctx, v6) <- result.try(visit_val_type(ctx, v6, visitor))
      use #(ctx, v7) <- result.try(visit_val_type(ctx, v7, visitor))
      use #(ctx, v8) <- result.map(visit_val_type(ctx, v8, visitor))
      #(ctx, NineResultTypes(v0, v1, v2, v3, v4, v5, v6, v7, v8))
    }
    TenResultTypes(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9) -> {
      use #(ctx, v0) <- result.try(visit_val_type(ctx, v0, visitor))
      use #(ctx, v1) <- result.try(visit_val_type(ctx, v1, visitor))
      use #(ctx, v2) <- result.try(visit_val_type(ctx, v2, visitor))
      use #(ctx, v3) <- result.try(visit_val_type(ctx, v3, visitor))
      use #(ctx, v4) <- result.try(visit_val_type(ctx, v4, visitor))
      use #(ctx, v5) <- result.try(visit_val_type(ctx, v5, visitor))
      use #(ctx, v6) <- result.try(visit_val_type(ctx, v6, visitor))
      use #(ctx, v7) <- result.try(visit_val_type(ctx, v7, visitor))
      use #(ctx, v8) <- result.try(visit_val_type(ctx, v8, visitor))
      use #(ctx, v9) <- result.map(visit_val_type(ctx, v9, visitor))
      #(ctx, TenResultTypes(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9))
    }
    _ -> Ok(#(ctx, rt))
  }
}

pub fn visit_result_type(
  ctx: Context,
  ty: ResultType,
  visitor: TypeVisitor,
) -> Result(#(Context, ResultType), String) {
  use #(ctx, rt) <- result.try(case visitor.result_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  use #(ctx, parameters) <- result.try(visit_result_types(
    ctx,
    ty.parameters,
    visitor,
  ))
  use #(ctx, results) <- result.map(visit_result_types(ctx, ty.result, visitor))

  #(ctx, ResultType(parameters, results))
}

/// This type means something needs to be calculated based on the stack
pub fn visit_func_type(
  ctx: Context,
  ty: FuncType,
  visitor: TypeVisitor,
) -> Result(#(Context, FuncType), String) {
  use #(ctx, ft) <- result.try(case visitor.func_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  use #(ctx, rt) <- result.map(visit_result_type(ctx, ft.rt, visitor))

  #(ctx, FuncType(rt))
}

pub fn visit_struct_type(
  ctx: Context,
  ty: StructType,
  visitor: TypeVisitor,
) -> Result(#(Context, StructType), String) {
  use #(ctx, st) <- result.try(case visitor.struct_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  use #(ctx, ft) <- result.map(do_visit_field_types(ctx, st.ft, [], visitor))

  #(ctx, StructType(ft))
}

fn do_visit_field_types(
  ctx: Context,
  ft: List(FieldType),
  acc: List(FieldType),
  visitor: TypeVisitor,
) {
  case ft {
    [] -> Ok(#(ctx, acc |> list.reverse()))
    [ft, ..rest] -> {
      use #(ctx, ft) <- result.try(visit_field_type(ctx, ft, visitor))
      do_visit_field_types(ctx, rest, [ft, ..acc], visitor)
    }
  }
}

pub fn visit_array_type(
  ctx: Context,
  ty: ArrayType,
  visitor: TypeVisitor,
) -> Result(#(Context, ArrayType), String) {
  use #(ctx, at) <- result.try(case visitor.array_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  use #(ctx, ft) <- result.map(visit_field_type(ctx, at.ft, visitor))

  #(ctx, ArrayType(ft))
}

pub fn visit_field_type(
  ctx: Context,
  ty: FieldType,
  visitor: TypeVisitor,
) -> Result(#(Context, FieldType), String) {
  use #(ctx, ft) <- result.try(case visitor.field_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  use #(ctx, st) <- result.map(visit_storage_type(ctx, ft.st, visitor))

  #(ctx, FieldType(ft.mut, st))
}

pub fn visit_storage_type(
  ctx: Context,
  ty: StorageType,
  visitor: TypeVisitor,
) -> Result(#(Context, StorageType), String) {
  use #(ctx, st) <- result.try(case visitor.storage_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  case st {
    ValTypeStorageType(vt) -> {
      use #(ctx, vt) <- result.map(visit_val_type(ctx, vt, visitor))
      #(ctx, ValTypeStorageType(vt))
    }
    _ -> Ok(#(ctx, st))
  }
}

pub fn visit_packed_type(
  ctx: Context,
  ty: PackedType,
  visitor: TypeVisitor,
) -> Result(#(Context, PackedType), String) {
  case visitor.packed_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  }
}

pub fn visit_composite_type(
  ctx: Context,
  ty: CompositeType,
  visitor: TypeVisitor,
) -> Result(#(Context, CompositeType), String) {
  use #(ctx, ct) <- result.try(case visitor.composite_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

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

pub fn visit_rec_type(
  ctx: Context,
  ty: RecType,
  visitor: TypeVisitor,
) -> Result(#(Context, RecType), String) {
  use #(ctx, rt) <- result.try(case visitor.rec_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  use #(ctx, st) <- result.map(do_visit_sub_types(ctx, rt.st, [], visitor))

  #(ctx, RecType(st))
}

fn do_visit_sub_types(
  ctx: Context,
  st: List(SubType),
  acc: List(SubType),
  visitor: TypeVisitor,
) {
  case st {
    [] -> Ok(#(ctx, acc |> list.reverse()))
    [st, ..rest] -> {
      use #(ctx, st) <- result.try(visit_sub_type(ctx, st, visitor))
      do_visit_sub_types(ctx, rest, [st, ..acc], visitor)
    }
  }
}

pub fn visit_sub_type(
  ctx: Context,
  ty: SubType,
  visitor: TypeVisitor,
) -> Result(#(Context, SubType), String) {
  use #(ctx, st) <- result.try(case visitor.sub_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  case st {
    // SubType(final: Bool, t: Vec(TypeIDX), ct: CompositeType)
    SubType(f, t, ct) -> {
      use #(ctx, ct) <- result.map(visit_composite_type(ctx, ct, visitor))
      #(ctx, SubType(f, t, ct))
    }
    // UnrolledSubType(final: Bool, ht: Vec(HeapType), ct: CompositeType)
    UnrolledSubType(f, ht, ct) -> {
      use #(ctx, ht) <- result.try(do_visit_heap_types(ctx, ht, [], visitor))
      use #(ctx, ct) <- result.map(visit_composite_type(ctx, ct, visitor))
      #(ctx, UnrolledSubType(f, ht, ct))
    }
  }
}

pub fn visit_mem_type(
  ctx: Context,
  ty: MemType,
  visitor: TypeVisitor,
) -> Result(#(Context, MemType), String) {
  case visitor.mem_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  }
}

pub fn visit_table_type(
  ctx: Context,
  ty: TableType,
  visitor: TypeVisitor,
) -> Result(#(Context, TableType), String) {
  use #(ctx, tt) <- result.try(case visitor.table_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  use #(ctx, rt) <- result.map(visit_ref_type(ctx, tt.t, visitor))

  #(ctx, TableType(tt.limits, rt))
}

pub fn visit_global_type(
  ctx: Context,
  ty: GlobalType,
  visitor: TypeVisitor,
) -> Result(#(Context, GlobalType), String) {
  use #(ctx, gt) <- result.try(case visitor.global_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  use #(ctx, vt) <- result.map(visit_val_type(ctx, gt.vt, visitor))

  #(ctx, GlobalType(gt.mut, vt))
}

pub fn visit_extern_type(
  ctx: Context,
  ty: ExternType,
  visitor: TypeVisitor,
) -> Result(#(Context, ExternType), String) {
  use #(ctx, et) <- result.try(case visitor.extern_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  case et {
    FuncExternType(dt) -> {
      use #(ctx, dt) <- result.map(visit_def_type(ctx, dt, visitor))
      #(ctx, FuncExternType(dt))
    }
    TableExternType(tt) -> {
      use #(ctx, tt) <- result.map(visit_table_type(ctx, tt, visitor))
      #(ctx, TableExternType(tt))
    }
    MemExternType(mt) -> {
      use #(ctx, mt) <- result.map(visit_mem_type(ctx, mt, visitor))
      #(ctx, MemExternType(mt))
    }
    GlobalExternType(gt) -> {
      use #(ctx, gt) <- result.map(visit_global_type(ctx, gt, visitor))
      #(ctx, GlobalExternType(gt))
    }
  }
}

pub fn visit_instruction(
  ctx: Context,
  ty: Instruction,
  visitor: TypeVisitor,
) -> Result(#(Context, Instruction), String) {
  case visitor.instruction_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  }
  // TODO: visit each type of instruction
}

pub fn visit_def_type(
  ctx: Context,
  ty: DefType,
  visitor: TypeVisitor,
) -> Result(#(Context, DefType), String) {
  use #(ctx, dt) <- result.try(case visitor.def_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  use #(ctx, rt) <- result.map(visit_rec_type(ctx, dt.rt, visitor))

  #(ctx, DefType(rt, dt.idx))
}

pub fn visit_local_type(
  ctx: Context,
  ty: LocalType,
  visitor: TypeVisitor,
) -> Result(#(Context, LocalType), String) {
  use #(ctx, lt) <- result.try(case visitor.local_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  use #(ctx, vt) <- result.map(visit_val_type(ctx, lt.t, visitor))

  #(ctx, LocalType(lt.initialized, vt))
}
