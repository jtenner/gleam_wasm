import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import structure/modules.{type Module} as structure_modules
import structure/types.{type DefType, type RecType, type SubType, DefType} as structure_types
import validation/common
import validation/types.{type Context, Context}

pub fn validate_module(mod: Module) {
  let ctx = Context(..types.new_context(), types: roll_up_types(mod))
}

fn roll_up_types(mod: Module) -> List(DefType) {
  mod.types
  |> list.flat_map(fn(rt) {
    rt.st
    |> list.index_map(fn(a, idx) { DefType(rt, idx) })
  })
}
