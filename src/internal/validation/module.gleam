import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import internal/structure/modules.{type Module} as structure_modules
import internal/structure/types.{
  type DefType, type RecType, type SubType, DefType,
} as structure_types
import internal/validation/common
import internal/validation/types.{type Context, Context}

pub fn validate_module(mod: Module) {
  let ctx = Context(..types.new_context(), types: roll_up_types(mod))
}

fn roll_up_types(mod: Module) -> List(DefType) {
  mod.types
  |> list.flat_map(fn(rt) {
    rt.st
    |> list.index_map(fn(_, idx) { DefType(rt, idx) })
  })
}
