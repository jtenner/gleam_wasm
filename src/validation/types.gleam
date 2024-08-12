import gleam/list
import gleam/result
import structure/common as structure_common
import structure/numbers.{type U32, u32, unwrap_u32}
import structure/types.{
  type Context, type DefType, type HeapType, type RecType, type RefType,
  type SubType, AnyHeapType, AnyRefType, ArrayHeapType, ArrayRefType, DefType,
  DefTypeHeapType, EqHeapType, EqRefType, ExternHeapType, ExternRefType,
  FuncHeapType, FuncRefType, HeapTypeRefType, I31HeapType, I31RefType,
  NoExternHeapType, NoFuncHeapType, NoneHeapType, NullExternRefType,
  NullFuncRefType, NullRefType, RecTypeHeapType, StructHeapType, StructRefType,
  SubType, UnrolledSubType,
}
import validation/common

pub fn ref_type_difference(rt1: RefType, rt2: RefType) {
  case rt2 {
    HeapTypeRefType(_, False) -> rt1 |> ref_type_as_non_nullable
    _ -> rt1
  }
}

fn ref_type_as_non_nullable(rt: RefType) {
  case rt {
    HeapTypeRefType(ht, _) -> HeapTypeRefType(ht, False)
    AnyRefType -> HeapTypeRefType(AnyHeapType, False)
    EqRefType -> HeapTypeRefType(EqHeapType, False)
    I31RefType -> HeapTypeRefType(I31HeapType, False)
    StructRefType -> HeapTypeRefType(StructHeapType, False)
    ArrayRefType -> HeapTypeRefType(ArrayHeapType, False)
    FuncRefType -> HeapTypeRefType(FuncHeapType, False)
    ExternRefType -> HeapTypeRefType(ExternHeapType, False)
    NullRefType -> HeapTypeRefType(NoneHeapType, False)
    NullFuncRefType -> HeapTypeRefType(NoFuncHeapType, False)
    NullExternRefType -> HeapTypeRefType(NoExternHeapType, False)
  }
}

fn type_idx_to_def_type(ctx: Context, idx: U32) {
  case ctx.types |> list.drop(idx |> unwrap_u32) {
    [dt, ..] -> Ok(#(ctx, dt))
    _ -> Error("Invalid type index")
  }
}

fn do_map_type_indicies_to_def_types(
  ctx: Context,
  t: List(U32),
  result: List(DefType),
) {
  case t {
    [] -> Ok(#(ctx, result |> list.reverse))
    [idx, ..rest] -> {
      use #(ctx, dt) <- result.try(type_idx_to_def_type(ctx, idx))
      do_map_type_indicies_to_def_types(ctx, rest, [dt, ..result])
    }
  }
}

fn types_index_of(dts: List(DefType), target_dt: DefType, acc: Int) {
  case dts {
    [] -> Error("DefType not found")
    [dt, ..] if dt == target_dt -> Ok(acc)
    [_, ..rest] -> types_index_of(rest, target_dt, acc + 1)
  }
}

fn do_map_heap_types_to_type_indicies(
  ctx: Context,
  dt: List(HeapType),
  result: List(U32),
) {
  case dt {
    [] -> Ok(#(ctx, result |> list.reverse))
    [DefTypeHeapType(dt), ..rest] -> {
      use idx <- result.try(ctx.types |> types_index_of(dt, 0))
      use val <- result.try(u32(idx))
      do_map_heap_types_to_type_indicies(ctx, rest, [val, ..result])
    }
    _ -> Error("DefType not found")
  }
}

fn roll_visit_sub_type(ctx: Context, st: SubType) {
  case st {
    SubType(final, t, ct) -> {
      use #(ctx, dt) <- result.map(
        do_map_type_indicies_to_def_types(ctx, t, []),
      )
      #(ctx, UnrolledSubType(final, dt |> list.map(DefTypeHeapType), ct))
    }
    _ -> Ok(#(ctx, st))
  }
}

fn unroll_visit_sub_type(ctx: Context, st: SubType) {
  case st {
    UnrolledSubType(final, dt, ct) -> {
      use #(ctx, t) <- result.map(
        do_map_heap_types_to_type_indicies(ctx, dt, []),
      )
      #(ctx, SubType(final, t, ct))
    }
    _ -> Ok(#(ctx, st))
  }
}

fn roll_visit_heap_type(ctx: Context, st: HeapType) {
  case st {
    RecTypeHeapType(idx) -> {
      use #(ctx, dt) <- result.map(type_idx_to_def_type(ctx, idx))
      #(ctx, DefTypeHeapType(dt))
    }
    _ -> Ok(#(ctx, st))
  }
}

fn unroll_visit_heap_type(ctx: Context, st: HeapType) {
  case st {
    DefTypeHeapType(dt) -> {
      use idx <- result.map(ctx.types |> types_index_of(dt, 0))
      use idx <- result.map(u32(idx))
      #(ctx, RecTypeHeapType(idx))
    }
    _ -> Ok(#(ctx, st))
  }
}

pub fn roll_rec_type(ctx: Context, rt: RecType) {
  common.visitor_new()
  |> common.on_sub_type(roll_visit_sub_type)
  |> common.on_heap_type(roll_visit_heap_type)
  |> common.visit_rec_type(ctx, rt, _)
}

pub fn unroll_rec_type(ctx: Context, rt: RecType) {
  common.visitor_new()
  |> common.on_sub_type(unroll_visit_sub_type)
  |> common.on_heap_type(unroll_visit_heap_type)
  |> common.visit_rec_type(ctx, rt, _)
}
