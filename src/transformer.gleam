import gleam/option.{type Option, None, Some}
import gleam/result.{map, try}
import internal/finger_tree.{type FingerTree, push, shift}
import internal/structure/common
import internal/structure/modules.{
  type BinaryModule, type CustomSection, type TypeSection, CustomSection,
  TypeSection,
}
import internal/structure/numbers
import internal/structure/types.{
  type DefType, type RecType, type SubType, type TypeIDX, DefType,
  DefTypeReference, RecType, RecTypeIDX, SubType, TypeIDX,
}

pub type DoVisitCallback(ctx, node_type) =
  fn(ModuleVisitor(ctx), ctx, node_type) -> Result(#(ctx, node_type), String)

pub type VisitorCallback(ctx, node_type) =
  Option(fn(ctx, node_type) -> Result(#(ctx, node_type), String))

pub type ModuleVisitor(ctx) {
  ModuleVisitor(
    module: VisitorCallback(ctx, BinaryModule),
    custom_section: VisitorCallback(ctx, CustomSection),
    type_section: VisitorCallback(ctx, TypeSection),
    def_type: VisitorCallback(ctx, DefType),
    rec_type: VisitorCallback(ctx, RecType),
    sub_type: VisitorCallback(ctx, SubType),
    type_idx: VisitorCallback(ctx, TypeIDX),
    rec_type_idx: VisitorCallback(ctx, TypeIDX),
    // utility visitors
    name: VisitorCallback(ctx, String),
    data: VisitorCallback(ctx, BitArray),
  )
}

fn do_visit_nodes(
  visit_func: DoVisitCallback(ctx, node_type),
  visitor: ModuleVisitor(ctx),
  ctx,
  nodes: FingerTree(node_type),
  acc: FingerTree(node_type),
) {
  case shift(nodes) {
    Ok(#(node, rest)) -> {
      use #(ctx, node) <- try(visit_func(visitor, ctx, node))
      do_visit_nodes(visit_func, visitor, ctx, rest, acc |> push(node))
    }
    Error(Nil) -> Ok(#(ctx, acc))
  }
}

pub fn visit_module(
  visitor: ModuleVisitor(ctx),
  ctx,
  mod: BinaryModule,
) -> Result(#(ctx, mod), String) {
  use #(ctx, mod) <- try(case visitor.module {
    Some(f) -> f(ctx, mod)
    None -> Ok(#(ctx, mod))
  })

  use #(ctx, custom_0) <- try(do_visit_nodes(
    visit_custom_section,
    visitor,
    ctx,
    mod.custom_0 |> option.unwrap(finger_tree.empty),
    finger_tree.empty,
  ))

  // 1: type section
  use #(ctx, type_section) <- try(visit_type_section(visitor, ctx, mod.types))

  use #(ctx, custom_1) <- try(do_visit_nodes(
    visit_custom_section,
    visitor,
    ctx,
    mod.custom_1 |> option.unwrap(finger_tree.empty),
    finger_tree.empty,
  ))

  todo
}

pub fn visit_custom_section(
  visitor: ModuleVisitor(ctx),
  ctx,
  custom_section: CustomSection,
) {
  use #(ctx, custom_section) <- try(case visitor.custom_section {
    Some(f) -> f(ctx, custom_section)
    None -> Ok(#(ctx, custom_section))
  })

  use #(ctx, name) <- try(visit_name(visitor, ctx, custom_section.name))
  use #(ctx, data) <- map(visit_data(visitor, ctx, custom_section.data))
  #(ctx, CustomSection(name, data))
}

pub fn visit_name(visitor: ModuleVisitor(ctx), ctx, name: String) {
  case visitor.name {
    Some(f) -> f(ctx, name)
    None -> Ok(#(ctx, name))
  }
}

pub fn visit_data(visitor: ModuleVisitor(ctx), ctx, data: BitArray) {
  case visitor.data {
    Some(f) -> f(ctx, data)
    None -> Ok(#(ctx, data))
  }
}

pub fn visit_type_section(
  visitor: ModuleVisitor(ctx),
  ctx,
  type_section: Option(TypeSection),
) {
  case type_section {
    None -> Ok(#(ctx, type_section))
    Some(type_section) -> {
      use #(ctx, type_section) <- try(case visitor.type_section {
        Some(f) -> f(ctx, type_section)
        _ -> Ok(#(ctx, type_section))
      })

      use #(ctx, types) <- map(do_visit_nodes(
        visit_rec_type,
        visitor,
        ctx,
        type_section.types,
        finger_tree.empty,
      ))

      #(ctx, Some(TypeSection(types)))
    }
  }
}

pub fn visit_rec_type(visitor: ModuleVisitor(ctx), ctx, rt: RecType) {
  use #(ctx, rt) <- try(case visitor.rec_type {
    Some(f) -> f(ctx, rt)
    None -> Ok(#(ctx, rt))
  })
  use #(ctx, sub_types) <- map(do_visit_nodes(
    visit_sub_type,
    visitor,
    ctx,
    rt.sub_types,
    finger_tree.empty,
  ))

  #(ctx, RecType(sub_types))
}

pub fn visit_sub_type(visitor: ModuleVisitor(ctx), ctx, st: SubType) {
  use #(ctx, SubType(final, type_idxs, ct)) <- try(case visitor.sub_type {
    Some(f) -> f(ctx, st)
    _ -> Ok(#(ctx, st))
  })

  use #(ctx, type_idxs) <- try(do_visit_nodes(
    visit_type_idx,
    visitor,
    ctx,
    type_idxs,
    finger_tree.empty,
  ))

  use #(ctx, comp_type) <- map(visit_composite_type(visitor, ctx, ct))

  #(ctx, SubType(final, type_idxs, comp_type))
}

pub fn visit_type_idx(visitor: ModuleVisitor(ctx), ctx, type_idx: TypeIDX) {
  use #(ctx, type_idx) <- try(case visitor.type_idx {
    Some(f) -> f(ctx, type_idx)
    _ -> Ok(#(ctx, type_idx))
  })

  case type_idx {
    TypeIDX(type_idx) -> Ok(#(ctx, TypeIDX(type_idx)))
    RecTypeIDX(_) -> visit_rec_type_idx(visitor, ctx, type_idx)
    DefTypeReference(def_type) -> {
      use #(ctx, def_type) <- map(visit_def_type(visitor, ctx, def_type))
      #(ctx, DefTypeReference(ctx, def_type))
    }
  }
}

pub fn visit_rec_type_idx(
  visitor: ModuleVisitor(ctx),
  ctx,
  rec_type_idx: TypeIDX,
) {
  case visitor.rec_type_idx {
    Some(f) -> f(ctx, rec_type_idx)
    _ -> Ok(#(ctx, rec_type_idx))
  }
}

pub fn visit_def_type(visitor: ModuleVisitor(ctx), ctx, def_type: DefType) {
  case visitor.def_type {
    Some(f) -> f(ctx, def_type)
    _ -> Ok(#(ctx, def_type))
  }
}
