import gleam/option.{None, Some}
import gleam/result
import internal/finger_tree.{type FingerTree}
import internal/structure/types.{
  type AbstractHeapType, type ArrayType, type CompositeType, type DefType,
  type ExternType, type FieldType, type FuncType, type GlobalType, type HeapType,
  type Instruction, type LocalIDX, type LocalType, type MemType, type NumType,
  type PackedType, type RecType, type RefType, type StorageType, type StructType,
  type SubType, type TableType, type ValType, type VecType, AnyHeapType,
  AnyRefType, ArrayCompositeType, ArrayHeapType, ArrayRefType, ArrayType,
  BotHeapType, BotValType, ConcreteHeapType, Const, DefType, DefTypeHeapType,
  EqHeapType, EqRefType, ExternHeapType, ExternRefType, F32ValType, F64ValType,
  FieldType, FuncCompositeType, FuncExternType, FuncHeapType, FuncRefType,
  FuncType, GlobalExternType, GlobalType, HeapTypeRefType, I16StorageType,
  I31HeapType, I31RefType, I32ValType, I64ValType, I8StorageType, LocalType,
  Loop, MemExternType, NoExternHeapType, NoExternRefType, NoFuncHeapType,
  NoFuncRefType, NoneHeapType, NoneRefType, RecType, RefTypeValType,
  StructCompositeType, StructHeapType, StructRefType, StructType, SubType,
  TableExternType, TableType, V128ValType, ValTypeStorageType, Var,
  unwrap_local_idx,
}

import internal/validation/types.{
  type Context, type CtrlFrame, type TypeVisitor, type ValidationState, Context,
  CtrlFrame, Stacks,
} as _

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
    | V128ValType
    | BotValType -> Ok(#(ctx, vt))

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
  ty: FingerTree(ValType),
  visitor: TypeVisitor,
) -> Result(#(Context, FingerTree(ValType)), String) {
  case visitor.result_type {
    Some(f) -> f(ctx, ty)
    None -> Ok(#(ctx, ty))
  }
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

  use #(ctx, parameters) <- result.try(visit_result_type(
    ctx,
    ft.parameters,
    visitor,
  ))
  use #(ctx, results) <- result.map(visit_result_type(ctx, ft.results, visitor))

  #(ctx, FuncType(parameters, results))
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
  "Invalid def type"
}

fn do_def_type_expand(st: FingerTree(SubType), idx: Int) {
  case idx {
    0 -> {
      use #(st, _) <- result.map(st |> finger_tree.shift)
      st.ct
    }
    t if t > 0 -> {
      use #(_, rest) <- result.try(st |> finger_tree.shift)
      do_def_type_expand(rest, t - 1)
    }
    _ -> Error(Nil)
  }
}

pub fn is_num(ty: ValType) {
  case ty {
    I32ValType -> True
    I64ValType -> True
    F32ValType -> True
    F64ValType -> True
    _ -> False
  }
}

pub fn is_vec(ty: ValType) {
  case ty {
    V128ValType -> True
    _ -> False
  }
}

pub fn is_ref(ty: ValType) {
  case ty {
    I32ValType -> False
    I64ValType -> False
    F32ValType -> False
    F64ValType -> False
    V128ValType -> False
    _ -> True
  }
}

pub fn unpack_field(t: FieldType) {
  case t {
    FieldType(I16StorageType, _) -> I32ValType
    FieldType(I8StorageType, _) -> I32ValType
    FieldType(ValTypeStorageType(vt), _) -> vt
  }
}

pub fn comp_type_is_func(ct: CompositeType) {
  case ct {
    FuncCompositeType(_) -> True
    _ -> False
  }
}

pub fn comp_type_is_struct_type(ct: CompositeType) {
  case ct {
    StructCompositeType(_) -> True
    _ -> False
  }
}

pub fn comp_type_is_array_type(ct: CompositeType) {
  case ct {
    ArrayCompositeType(_) -> True
    _ -> False
  }
}

pub fn top_heap_type(t: HeapType) {
  case t {
    AnyHeapType
    | EqHeapType
    | I31HeapType
    | StructHeapType
    | ArrayHeapType
    | NoneHeapType -> Ok(AnyHeapType)
    FuncHeapType | NoFuncHeapType -> Ok(FuncHeapType)
    ExternHeapType | NoExternHeapType -> Ok(ExternHeapType)
    DefTypeHeapType(dt) -> {
      use ct <- result.map(def_type_expand(dt))
      case ct {
        FuncCompositeType(_) -> FuncHeapType
        _ -> AnyHeapType
      }
    }
    ConcreteHeapType(_) | BotHeapType -> Error("Invalid heap type")
  }
}

pub fn push_val(state: ValidationState, val: ValType) {
  let #(ctx, stacks) = state
  #(ctx, Stacks(..stacks, val_stack: stacks.val_stack |> finger_tree.push(val)))
}

pub fn pop_val(state: ValidationState) {
  use #(frame, _) <- result.try(
    { state.1 }.ctrl_stack
    |> finger_tree.pop
    |> result.replace_error("Invalid control frame state"),
  )
  let CtrlFrame(_, _, _, val_height, _, unreachable) = frame

  let size = { state.1 }.val_stack |> finger_tree.size
  case size, val_height, unreachable {
    a, b, True if a == b -> Ok(#(BotValType, state))
    a, b, False if a == b -> Error("Stack underflow")
    _, _, _ -> {
      use #(val, val_stack) <- result.map(
        { state.1 }.val_stack
        |> finger_tree.pop
        |> result.replace_error("Stack underflow"),
      )
      #(val, #(state.0, Stacks(..state.1, val_stack: val_stack)))
    }
  }
}

pub fn pop_val_expect(state: ValidationState, expected: ValType) {
  use #(val, state) <- result.try(pop_val(state))
  case match_val_type(state, val, expected) {
    Ok(_) -> Ok(#(val, state))
    Error(_) -> Error("ValType mismatch")
  }
}

pub fn pop_num(state: ValidationState) {
  use #(num, state) <- result.try(
    pop_val(state)
    |> result.replace_error("Stack underflow"),
  )
  case is_num(num) {
    True -> Ok(#(num, state))
    False -> Error("ValType mismatch")
  }
}

pub fn pop_ref(state: ValidationState) {
  use #(num, state) <- result.try(
    pop_val(state)
    |> result.replace_error("Stack underflow"),
  )
  case is_ref(num) {
    True -> Ok(#(num, state))
    False -> Error("ValType mismatch")
  }
}

pub fn push_vals(state: ValidationState, vals: FingerTree(ValType)) {
  let #(ctx, stacks) = state
  #(
    ctx,
    Stacks(..stacks, val_stack: stacks.val_stack |> finger_tree.append(vals)),
  )
}

pub fn pop_vals(state: ValidationState, expected: FingerTree(ValType)) {
  do_pop_vals(state, expected, finger_tree.new())
}

fn do_pop_vals(
  state: ValidationState,
  expected: FingerTree(ValType),
  acc: FingerTree(ValType),
) {
  case expected |> finger_tree.pop {
    Error(_) -> Ok(#(acc, state))
    Ok(#(expected, rest_expected)) ->
      case pop_val(state) {
        Ok(#(actual, new_state)) -> {
          case match_val_type(state, actual, expected) {
            Ok(_) ->
              do_pop_vals(
                new_state,
                rest_expected,
                acc |> finger_tree.push(actual),
              )
            Error(_) -> Error("ValType mismatch")
          }
        }
        Error(_) -> Error("Stack underflow")
      }
  }
}

pub fn get_local(state: ValidationState, idx: LocalIDX) {
  let idx = idx |> unwrap_local_idx
  use val <- result.map({ state.0 }.locals |> finger_tree.get(idx))
  #(val, state)
}

pub fn set_local(state: ValidationState, idx: LocalIDX, val: LocalType) {
  let idx = idx |> unwrap_local_idx
  use locals <- result.map(
    { state.0 }.locals
    |> finger_tree.set(idx, val)
    |> result.replace_error("unable to set local"),
  )
  #(Context(..state.0, locals: locals), state.1)
}

pub fn reset_locals(state: ValidationState, height: Int) {
  #(
    Context(
      ..state.0,
      locals: { state.0 }.locals
        |> finger_tree.map_index(fn(v, idx) {
          case idx >= height {
            True -> LocalType(False, v.t)
            False -> v
          }
        }),
    ),
    state.1,
  )
}

pub fn push_ctrl(
  state: ValidationState,
  opcode: Instruction,
  in: FingerTree(ValType),
  out: FingerTree(ValType),
) {
  let frame =
    CtrlFrame(
      opcode: opcode,
      start_types: in,
      end_types: out,
      val_height: { state.1 }.val_stack |> finger_tree.size,
      init_height: { state.1 }.init_stack |> finger_tree.size,
      unreachable: False,
    )

  let #(ctx, stacks) = state
  #(
    ctx,
    Stacks(..stacks, ctrl_stack: stacks.ctrl_stack |> finger_tree.push(frame)),
  )
}

pub fn pop_ctrl(state: ValidationState) {
  let #(ctx, stacks) = state
  use #(frame, new_ctrl_stack) <- result.try(
    stacks.ctrl_stack
    |> finger_tree.pop
    |> result.replace_error("Stack underflow"),
  )
  let state = #(ctx, Stacks(..stacks, ctrl_stack: new_ctrl_stack))
  use #(_, state) <- result.try(state |> pop_vals(frame.end_types))
  case { state.1 }.val_stack |> finger_tree.size == frame.init_height {
    False -> Error("Stack underflow")
    True -> Ok(reset_locals(state, frame.init_height))
  }
}

pub fn label_types(frame: CtrlFrame) {
  case frame.opcode {
    Loop(_, _) -> frame.start_types
    _ -> frame.end_types
  }
}

pub fn unreachable(state: ValidationState) {
  let #(ctx, stacks) = state
  use #(last_frame, new_ctrl_stack) <- result.try(
    stacks.ctrl_stack
    |> finger_tree.pop
    |> result.replace_error("Stack underflow"),
  )
  use #(new_val_stack, _) <- result.map(
    stacks.val_stack
    |> finger_tree.take(last_frame.val_height)
    |> result.replace_error("Stack underflow"),
  )
  #(
    ctx,
    Stacks(
      ..stacks,
      val_stack: new_val_stack,
      ctrl_stack: new_ctrl_stack
        |> finger_tree.push(CtrlFrame(..last_frame, unreachable: True)),
    ),
  )
}

pub fn match_vec(
  state: ValidationState,
  actual: FingerTree(u),
  expected: FingerTree(u),
  f: fn(ValidationState, u, u) -> Result(Nil, String),
) {
  case actual |> finger_tree.size, expected |> finger_tree.size {
    a, b if a == b -> do_match_vec(state, actual, expected, f)
    _, _ -> Error("Vector size mismatch")
  }
}

fn do_match_vec(
  state: ValidationState,
  actual: FingerTree(u),
  expected: FingerTree(u),
  f: fn(ValidationState, u, u) -> Result(Nil, String),
) {
  case actual |> finger_tree.shift, expected |> finger_tree.shift {
    Error(Nil), Error(Nil) -> Ok(Nil)
    Ok(#(actual_val, new_actual)), Ok(#(expected_val, new_expected)) ->
      case f(state, actual_val, expected_val) {
        Ok(Nil) -> do_match_vec(state, new_actual, new_expected, f)
        Error(_) -> Error("Vector element mismatch")
      }
    _, _ -> Error("Vector element mismatch")
  }
}

pub fn match_val_type(
  state: ValidationState,
  actual: ValType,
  expected: ValType,
) {
  case actual, expected {
    RefTypeValType(rt1), RefTypeValType(rt2) -> match_ref_type(state, rt1, rt2)
    a, b if a == b -> Ok(Nil)
    BotValType, _ -> Ok(Nil)
    _, _ -> Error("ValType mismatch")
  }
}

fn normalize_ref_type(rt: RefType) {
  case rt {
    HeapTypeRefType(ht, null) -> HeapTypeRefType(ht, null)
    AnyRefType -> HeapTypeRefType(AnyHeapType, True)
    EqRefType -> HeapTypeRefType(EqHeapType, True)
    I31RefType -> HeapTypeRefType(I31HeapType, True)
    StructRefType -> HeapTypeRefType(StructHeapType, True)
    ArrayRefType -> HeapTypeRefType(ArrayHeapType, True)
    FuncRefType -> HeapTypeRefType(FuncHeapType, True)
    ExternRefType -> HeapTypeRefType(ExternHeapType, True)
    NoneRefType -> HeapTypeRefType(NoneHeapType, True)
    NoFuncRefType -> HeapTypeRefType(NoFuncHeapType, True)
    NoExternRefType -> HeapTypeRefType(NoExternHeapType, True)
  }
}

pub fn match_ref_type(
  state: ValidationState,
  actual: RefType,
  expected: RefType,
) {
  case actual |> normalize_ref_type, expected |> normalize_ref_type {
    HeapTypeRefType(ht1, False), HeapTypeRefType(ht2, _)
    | HeapTypeRefType(ht1, _), HeapTypeRefType(ht2, True)
    -> match_heap_type(state, ht1, ht2)
    _, _ -> Error("RefType mismatch")
  }
}

pub fn match_heap_type(
  state: ValidationState,
  actual: HeapType,
  expected: HeapType,
) {
  case actual, expected {
    a, b if a == b -> Ok(Nil)
    NoExternHeapType, NoExternHeapType | NoExternHeapType, ExternHeapType ->
      Ok(Nil)
    NoFuncHeapType, NoFuncHeapType | NoFuncHeapType, FuncHeapType -> Ok(Nil)
    NoFuncHeapType, ConcreteHeapType(idx) -> {
      let #(ctx, _) = state
      use dt <- result.try(
        ctx.types |> finger_tree.get(idx |> types.unwrap_type_idx),
      )
      case def_type_expand(dt) {
        Ok(FuncCompositeType(_)) -> Ok(Nil)
        _ -> Error("HeapType mismatch")
      }
    }
    NoFuncHeapType, DefTypeHeapType(dt) -> {
      case def_type_expand(dt) {
        Ok(FuncCompositeType(_)) -> Ok(Nil)
        _ -> Error("HeapType mismatch")
      }
    }
    ConcreteHeapType(idx1), ConcreteHeapType(idx2) -> {
      let #(ctx, _) = state
      use dt1 <- result.try(
        ctx.types |> finger_tree.get(idx1 |> types.unwrap_type_idx),
      )
      use dt2 <- result.try(
        ctx.types |> finger_tree.get(idx2 |> types.unwrap_type_idx),
      )
      case def_type_expand(dt1), def_type_expand(dt2) {
        Ok(FuncCompositeType(ft1)), Ok(FuncCompositeType(ft2)) ->
          match_func_type(state, ft1, ft2)
        Ok(StructCompositeType(st1)), Ok(StructCompositeType(st2)) ->
          match_struct_type(state, st1, st2)
        Ok(ArrayCompositeType(at1)), Ok(ArrayCompositeType(at2)) ->
          match_array_type(state, at1, at2)
        _, _ -> Error("HeapType mismatch")
      }
    }
    DefTypeHeapType(dt1), ConcreteHeapType(idx2) -> {
      let #(ctx, _) = state
      use dt2 <- result.try(
        ctx.types |> finger_tree.get(idx2 |> types.unwrap_type_idx),
      )
      case def_type_expand(dt1), def_type_expand(dt2) {
        Ok(FuncCompositeType(ft1)), Ok(FuncCompositeType(ft2)) ->
          match_func_type(state, ft1, ft2)
        Ok(StructCompositeType(st1)), Ok(StructCompositeType(st2)) ->
          match_struct_type(state, st1, st2)
        Ok(ArrayCompositeType(at1)), Ok(ArrayCompositeType(at2)) ->
          match_array_type(state, at1, at2)
        _, _ -> Error("HeapType mismatch")
      }
    }
    ConcreteHeapType(idx1), DefTypeHeapType(dt2) -> {
      let #(ctx, _) = state
      use dt1 <- result.try(
        ctx.types |> finger_tree.get(idx1 |> types.unwrap_type_idx),
      )
      case def_type_expand(dt1), def_type_expand(dt2) {
        Ok(FuncCompositeType(ft1)), Ok(FuncCompositeType(ft2)) ->
          match_func_type(state, ft1, ft2)
        Ok(StructCompositeType(st1)), Ok(StructCompositeType(st2)) ->
          match_struct_type(state, st1, st2)
        Ok(ArrayCompositeType(at1)), Ok(ArrayCompositeType(at2)) ->
          match_array_type(state, at1, at2)
        _, _ -> Error("HeapType mismatch")
      }
    }
    DefTypeHeapType(dt1), DefTypeHeapType(dt2) -> {
      case def_type_expand(dt1), def_type_expand(dt2) {
        Ok(FuncCompositeType(ft1)), Ok(FuncCompositeType(ft2)) ->
          match_func_type(state, ft1, ft2)
        Ok(StructCompositeType(st1)), Ok(StructCompositeType(st2)) ->
          match_struct_type(state, st1, st2)
        Ok(ArrayCompositeType(at1)), Ok(ArrayCompositeType(at2)) ->
          match_array_type(state, at1, at2)
        _, _ -> Error("HeapType mismatch")
      }
    }
    ConcreteHeapType(idx), b -> {
      let #(ctx, _) = state
      use dt <- result.try(
        ctx.types |> finger_tree.get(idx |> types.unwrap_type_idx),
      )
      case def_type_expand(dt), b {
        Ok(ArrayCompositeType(_)), ArrayHeapType
        | Ok(ArrayCompositeType(_)), EqHeapType
        | Ok(ArrayCompositeType(_)), AnyHeapType
        | Ok(StructCompositeType(_)), StructHeapType
        | Ok(StructCompositeType(_)), EqHeapType
        | Ok(StructCompositeType(_)), AnyHeapType
        | Ok(FuncCompositeType(_)), FuncHeapType
        -> Ok(Nil)
        _, _ -> Error("HeapType mismatch")
      }
    }
    NoneHeapType, ArrayHeapType
    | NoneHeapType, StructHeapType
    | NoneHeapType, I31HeapType
    | NoneHeapType, EqHeapType
    | NoneHeapType, AnyHeapType
    | ArrayHeapType, EqHeapType
    | ArrayHeapType, AnyHeapType
    | StructHeapType, EqHeapType
    | StructHeapType, AnyHeapType
    | I31HeapType, EqHeapType
    | I31HeapType, AnyHeapType
    | EqHeapType, AnyHeapType
    -> Ok(Nil)

    _, _ -> Error("HeapType mismatch")
  }
}

pub fn match_func_type(state: ValidationState, ft1: FuncType, ft2: FuncType) {
  use _ <- result.try(
    state
    |> match_result_type(ft1.parameters, ft2.parameters),
  )
  state
  |> match_result_type(ft1.results, ft2.results)
}

pub fn match_result_type(
  state: ValidationState,
  rt1: FingerTree(ValType),
  rt2: FingerTree(ValType),
) {
  match_vec(state, rt1, rt2, match_val_type)
}

pub fn match_struct_type(
  state: ValidationState,
  st1: StructType,
  st2: StructType,
) {
  case st1.ft |> finger_tree.size, st2.ft |> finger_tree.size {
    a, b if a >= b -> do_match_field_types(state, st1.ft, st2.ft)
    _, _ -> Error("StructType mismatch")
  }
}

fn do_match_field_types(
  state: ValidationState,
  ft1: FingerTree(FieldType),
  ft2: FingerTree(FieldType),
) {
  case ft1 |> finger_tree.shift, ft2 |> finger_tree.shift {
    Ok(#(ft1, next_ft1)), Ok(#(ft2, next_ft2)) ->
      case match_field_type(state, ft1, ft2) {
        Ok(_) -> do_match_field_types(state, next_ft1, next_ft2)
        Error(_) -> Error("StructType mismatch")
      }
    Ok(_), _ -> Ok(Nil)
    _, _ -> Error("StructType mismatch")
  }
}

pub fn match_field_type(state: ValidationState, ft1: FieldType, ft2: FieldType) {
  case ft1, ft2 {
    FieldType(st1, Const), FieldType(st2, Const)
    | FieldType(st1, Var), FieldType(st2, Var)
    -> match_storage_type(state, st1, st2)
    _, _ -> Error("FieldType mismatch")
  }
}

pub fn match_storage_type(
  state: ValidationState,
  st1: StorageType,
  st2: StorageType,
) {
  case st1, st2 {
    a, b if a == b -> Ok(Nil)
    ValTypeStorageType(vt1), ValTypeStorageType(vt2) ->
      match_val_type(state, vt1, vt2)
    _, _ -> Error("StorageType mismatch")
  }
}

pub fn match_array_type(state: ValidationState, at1: ArrayType, at2: ArrayType) {
  let ArrayType(ft1) = at1
  let ArrayType(ft2) = at2
  match_field_type(state, ft1, ft2)
}
