import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import internal/binary/values
import internal/finger_tree.{type FingerTree}
import internal/structure/numbers
import internal/structure/types.{
  type AbstractHeapType, type ArrayType, type BlockType, type CompositeType,
  type DefType, type ExternType, type FieldType, type FuncType, type GlobalType,
  type HeapType, type Instruction, type InstructionType, type LocalType,
  type MemType, type NumType, type PackedType, type RecType, type RefType,
  type ResultType, type StorageType, type StructType, type SubType,
  type TableType, type TypeIDX, type ValType, type VecType, AnyHeapType,
  AnyRefType, ArrayCompositeType, ArrayHeapType, ArrayRefType, ArrayType,
  BotHeapType, ConcreteHeapType, DefType, EqHeapType, EqRefType, ExternHeapType,
  ExternRefType, F32ValType, F64ValType, FieldType, FuncCompositeType,
  FuncExternType, FuncHeapType, FuncRefType, FuncType, FuncTypeBlockType,
  GlobalExternType, GlobalType, HeapTypeRefType, I31HeapType, I31RefType,
  I32ValType, I64ValType, LocalType, MemExternType, NoExternHeapType,
  NoExternRefType, NoFuncHeapType, NoFuncRefType, NoneHeapType, NoneRefType,
  RecType, RefTypeValType, ResultType, StructCompositeType, StructHeapType,
  StructRefType, StructType, SubType, TableExternType, TableType, V128ValType,
  ValTypeBlockType, ValTypeStorageType, VoidBlockType,
}
import internal/validation/types.{type Context, type TypeVisitor} as validation_types

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
  case visitor.heap_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
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
    | NoneRefType
    | NoFuncRefType
    | NoExternRefType -> Ok(#(ctx, rt))
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
    | V128ValType -> Ok(#(ctx, vt))

    RefTypeValType(rt) -> {
      use #(ctx, rt) <- result.map(visit_ref_type(ctx, rt, visitor))
      #(ctx, RefTypeValType(rt))
    }
  }
}

fn do_visit_val_types(
  ctx: Context,
  ft: FingerTree(ValType),
  acc: FingerTree(ValType),
  visitor: TypeVisitor,
) {
  case ft |> finger_tree.shift {
    Ok(#(u, ft)) -> {
      use #(ctx, u) <- result.try(visit_val_type(ctx, u, visitor))
      do_visit_val_types(ctx, ft, acc |> finger_tree.push(u), visitor)
    }
    Error(_) -> Ok(#(ctx, acc))
  }
}

pub fn visit_result_type(
  ctx: Context,
  ty: ResultType,
  visitor: TypeVisitor,
) -> Result(#(Context, ResultType), String) {
  use #(ctx, rt) <- result.try(case visitor.result_types {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  })

  use #(ctx, parameters) <- result.try(do_visit_val_types(
    ctx,
    rt.parameters,
    finger_tree.new(),
    visitor,
  ))
  use #(ctx, result) <- result.map(do_visit_val_types(
    ctx,
    rt.result,
    finger_tree.new(),
    visitor,
  ))
  #(ctx, ResultType(parameters, result))
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

  use #(ctx, ft) <- result.map(do_visit_field_types(
    ctx,
    st.ft,
    finger_tree.new(),
    visitor,
  ))

  #(ctx, StructType(ft))
}

fn do_visit_field_types(
  ctx: Context,
  ft: FingerTree(FieldType),
  acc: FingerTree(FieldType),
  visitor: TypeVisitor,
) {
  case ft |> finger_tree.shift {
    Ok(#(u, ft)) -> {
      use #(ctx, u) <- result.try(visit_field_type(ctx, u, visitor))
      do_visit_field_types(ctx, ft, acc |> finger_tree.push(u), visitor)
    }
    Error(_) -> Ok(#(ctx, acc))
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

  #(ctx, FieldType(st, ft.mut))
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

  use #(ctx, st) <- result.map(do_visit_sub_types(
    ctx,
    rt.st,
    finger_tree.new(),
    visitor,
  ))

  #(ctx, RecType(st))
}

fn do_visit_sub_types(
  ctx: Context,
  st: FingerTree(SubType),
  acc: FingerTree(SubType),
  visitor: TypeVisitor,
) {
  case st |> finger_tree.shift {
    Error(_) -> Ok(#(ctx, acc))
    Ok(#(st, rest)) -> {
      use #(ctx, st) <- result.try(visit_sub_type(ctx, st, visitor))
      do_visit_sub_types(ctx, rest, acc |> finger_tree.push(st), visitor)
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
    // SubType(final: Bool, t: List(TypeIDX), ct: CompositeType)
    SubType(f, t, ct) -> {
      use #(ctx, ct) <- result.map(visit_composite_type(ctx, ct, visitor))
      #(ctx, SubType(f, t, ct))
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

  #(ctx, TableType(rt, tt.limits))
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

  #(ctx, GlobalType(vt, gt.mut))
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

pub fn def_type_expand(dt: DefType) {
  let DefType(RecType(st), idx) = dt
  use _ <- result.map_error(do_def_type_expand(st, idx))
  Error("Invalid def type")
}

fn do_def_type_expand(st: FingerTree(SubType), idx: Int) {
  case idx {
    0 -> {
      use #(st, _) <- result.map(st |> finger_tree.shift)
      Ok(st.ct)
    }
    t if t > 0 -> {
      use #(_, rest) <- result.try(st |> finger_tree.shift)
      do_def_type_expand(rest, t - 1)
    }
    _ -> Error(Nil)
  }
}
