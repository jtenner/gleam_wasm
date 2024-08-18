import gleam/bit_array
import gleam/bytes_builder.{type BytesBuilder}
import gleam/option.{type Option, None, Some}
import gleam/result

import internal/binary/common
import internal/binary/types.{
  encode_expression, encode_func_idx, encode_global_idx, encode_global_type,
  encode_mem_idx, encode_mem_type, encode_rec_type, encode_table_idx,
  encode_table_type, encode_type_idx,
} as binary_types
import internal/binary/values.{encode_u32}

import internal/finger_tree.{type FingerTree}
import internal/structure/modules.{
  type BinaryModule, type CustomSection, type ElementSection, type ExportSection,
  type FunctionSection, type GlobalSection, type ImportSection,
  type MemorySection, type StartSection, type TableSection, type TypeSection,
}
import internal/structure/numbers.{u32}
import internal/structure/types.{
  type Elem, type Export, type Expr, type Global, type Import, type RecType,
  type RefType, type Table, ActiveElemMode, DeclarativeElemMode, Elem,
  FuncExport, FuncImport, FuncRefType, GlobalExport, GlobalImport,
  HeapTypeRefType, MemExport, MemImport, PassiveElemMode, RefFunc, Table,
  TableExport, TableImport,
} as structure_types

pub fn encode_module(module: BinaryModule) {
  let builder = bytes_builder.new()
  use builder <- result.try(encode_custom_sections(builder, module.custom_0))
  use builder <- result.try(encode_type_section(builder, module.types))
  use builder <- result.try(encode_custom_sections(builder, module.custom_1))
  use builder <- result.try(encode_import_section(builder, module.imports))
  use builder <- result.try(encode_custom_sections(builder, module.custom_2))
  use builder <- result.try(encode_function_section(builder, module.functions))
  use builder <- result.try(encode_custom_sections(builder, module.custom_3))
  use builder <- result.try(encode_table_section(builder, module.tables))
  use builder <- result.try(encode_custom_sections(builder, module.custom_4))
  use builder <- result.try(encode_memory_section(builder, module.memories))
  use builder <- result.try(encode_custom_sections(builder, module.custom_5))
  use builder <- result.try(encode_global_section(builder, module.globals))
  use builder <- result.try(encode_custom_sections(builder, module.custom_6))
  use builder <- result.try(encode_export_section(builder, module.exports))
  use builder <- result.try(encode_custom_sections(builder, module.custom_7))
  use builder <- result.try(encode_start_section(builder, module.start))
  use builder <- result.try(encode_custom_sections(builder, module.custom_8))
  use builder <- result.try(encode_element_section(builder, module.elements))
  use builder <- result.try(encode_custom_sections(builder, module.custom_9))
}

pub fn encode_custom_sections(
  builder: BytesBuilder,
  sections: Option(FingerTree(CustomSection)),
) {
  case sections {
    Some(sections) ->
      Ok(finger_tree.fold(sections, builder, encode_custom_section))
    None -> Ok(builder)
  }
}

pub fn encode_custom_section(builder: BytesBuilder, section: CustomSection) {
  let size = bit_array.byte_size(section.data)
  let assert Ok(size) = u32(size)
  builder
  |> bytes_builder.append(<<0x00>>)
  |> encode_u32(size)
  |> bytes_builder.append(section.data)
}

pub fn encode_type_section(builder: BytesBuilder, section: Option(TypeSection)) {
  case section {
    Some(section) -> {
      use section_builder <- result.try(
        builder |> common.encode_vec(section.types, encode_rec_type),
      )
      use size <- result.map(section_builder |> bytes_builder.byte_size |> u32)
      section_builder
      |> bytes_builder.append(<<0x01>>)
      |> encode_u32(size)
      |> bytes_builder.append_builder(section_builder)
    }
    None -> Ok(builder)
  }
}

pub fn encode_import(builder: BytesBuilder, import_: Import) {
  case import_ {
    FuncImport(mod, name, type_idx) -> {
      use builder <- result.try(builder |> common.encode_string(mod))
      use builder <- result.try(builder |> common.encode_string(name))
      builder |> encode_type_idx(type_idx)
    }
    TableImport(mod, name, table_type) -> {
      use builder <- result.try(builder |> common.encode_string(mod))
      use builder <- result.try(builder |> common.encode_string(name))
      builder |> encode_table_type(table_type)
    }
    MemImport(mod, name, mem_type) -> {
      use builder <- result.try(builder |> common.encode_string(mod))
      use builder <- result.try(builder |> common.encode_string(name))
      builder |> encode_mem_type(mem_type)
    }
    GlobalImport(mod, name, global_type) -> {
      use builder <- result.try(builder |> common.encode_string(mod))
      use builder <- result.try(builder |> common.encode_string(name))
      builder |> encode_global_type(global_type)
    }
  }
}

pub fn encode_import_section(
  builder: BytesBuilder,
  section: Option(ImportSection),
) {
  case section {
    Some(section) -> {
      use section_builder <- result.try(
        bytes_builder.new() |> common.encode_vec(section.imports, encode_import),
      )

      use size <- result.map(section_builder |> bytes_builder.byte_size |> u32)
      builder
      |> bytes_builder.append(<<0x01>>)
      |> encode_u32(size)
      |> bytes_builder.append_builder(section_builder)
    }
    None -> Ok(builder)
  }
}

pub fn encode_function_section(
  builder: BytesBuilder,
  function_section: Option(FunctionSection),
) {
  case function_section {
    None -> Ok(builder)
    Some(function_section) -> {
      use section_builder <- result.try(
        bytes_builder.new()
        |> common.encode_vec(function_section.funcs, encode_type_idx),
      )
      use size <- result.map(section_builder |> bytes_builder.byte_size |> u32)

      builder
      |> bytes_builder.append(<<3>>)
      |> encode_u32(size)
      |> bytes_builder.append_builder(section_builder)
    }
  }
}

pub fn encode_table(builder: BytesBuilder, table: Table) {
  case table {
    Table(type_, None) -> builder |> encode_table_type(type_)
    Table(type_, Some(expr)) -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x40, 0x00>>)
        |> encode_table_type(type_),
      )
      builder |> encode_expression(expr)
    }
  }
}

pub fn encode_table_section(
  builder: BytesBuilder,
  table_section: Option(TableSection),
) {
  case table_section {
    None -> Ok(builder)
    Some(table_section) -> {
      use section_builder <- result.try(
        bytes_builder.new()
        |> common.encode_vec(table_section.tables, encode_table),
      )
      use size <- result.map(section_builder |> bytes_builder.byte_size |> u32)
      builder
      |> bytes_builder.append(<<4>>)
      |> encode_u32(size)
      |> bytes_builder.append_builder(section_builder)
    }
  }
}

pub fn encode_memory_section(
  builder: BytesBuilder,
  memory_section: Option(MemorySection),
) {
  case memory_section {
    None -> Ok(builder)
    Some(memory_section) -> {
      use section_builder <- result.try(
        bytes_builder.new()
        |> common.encode_vec(memory_section.mts, encode_mem_type),
      )
      use size <- result.map(section_builder |> bytes_builder.byte_size |> u32)
      builder
      |> bytes_builder.append(<<5>>)
      |> encode_u32(size)
      |> bytes_builder.append_builder(section_builder)
    }
  }
}

pub fn encode_global(builder: BytesBuilder, global: Global) {
  use builder <- result.try(builder |> encode_global_type(global.type_))
  builder |> encode_expression(global.init)
}

pub fn encode_global_section(
  builder: BytesBuilder,
  global_section: Option(GlobalSection),
) {
  case global_section {
    None -> Ok(builder)
    Some(global_section) -> {
      use section_builder <- result.try(
        bytes_builder.new()
        |> common.encode_vec(global_section.globals, encode_global),
      )
      use size <- result.map(section_builder |> bytes_builder.byte_size |> u32)
      builder
      |> bytes_builder.append(<<6>>)
      |> encode_u32(size)
      |> bytes_builder.append_builder(section_builder)
    }
  }
}

pub fn encode_export(builder: BytesBuilder, export_: Export) {
  use builder <- result.try(builder |> common.encode_string(export_.name))
  case export_ {
    FuncExport(_, func_idx) ->
      builder
      |> bytes_builder.append(<<0x00>>)
      |> encode_func_idx(func_idx)
    TableExport(_, table_idx) ->
      builder
      |> bytes_builder.append(<<0x01>>)
      |> encode_table_idx(table_idx)
    MemExport(_, mem_idx) ->
      builder
      |> bytes_builder.append(<<0x02>>)
      |> encode_mem_idx(mem_idx)
    GlobalExport(_, global_idx) ->
      builder
      |> bytes_builder.append(<<0x03>>)
      |> encode_global_idx(global_idx)
  }
}

pub fn encode_export_section(
  builder: BytesBuilder,
  export_section: Option(ExportSection),
) {
  case export_section {
    None -> Ok(builder)
    Some(export_section) -> {
      use section_builder <- result.try(
        bytes_builder.new()
        |> common.encode_vec(export_section.exports, encode_export),
      )
      use size <- result.map(section_builder |> bytes_builder.byte_size |> u32)
      builder
      |> bytes_builder.append(<<7>>)
      |> encode_u32(size)
      |> bytes_builder.append_builder(section_builder)
    }
  }
}

pub fn encode_start_section(
  builder: BytesBuilder,
  start_section: Option(StartSection),
) {
  case start_section {
    None -> Ok(builder)
    Some(start_section) -> {
      builder
      |> bytes_builder.append(<<8>>)
      |> encode_func_idx(start_section.start)
    }
  }
}

pub fn encode_element(builder: BytesBuilder, element: Elem) {
  let Elem(type_, init, mode) = element

  case mode {
    ActiveElemMode(idx, offset) -> {
      let idx = idx |> unwrap_u32
      case type_, idx {
        HeapTypeRefType(FuncHeapType), 0 -> {
          // if the index is 0, and the heap type is FuncHeapType
          use builder <- result.try(
            builder
            |> bytes_builder.append(<<0x00>>)
            |> encode_expression(offset),
          )
          use init <- result.try(
            init |> finger_tree.try_map(expression_to_func_idx),
          )
          builder
          |> common.encode_vec(init, encode_func_idx)
        }
      }
    }
    PassiveElemMode -> {
      case type_ {
        HeapTypeRefType(FuncHeapType) -> {
          use init <- result.try(
            init |> finger_tree.try_map(expression_to_func_idx),
          )
          builder
          |> bytes_builder.append(<<0x01, 0x00>>)
          |> common.encode_vec(init, encode_func_idx)
        }
      }
    }
  }
}

fn expression_to_func_idx(expr: Expr) {
  use #(inst, rest) <- result.try(
    expr.insts
    |> finger_tree.shift
    |> result.map_error(fn(_) { "expected RefFunc" }),
  )

  case inst, rest |> finger_tree.size {
    RefFunc(idx), 0 -> Ok(idx)
    _, _ -> Error("expected RefFunc")
  }
}
