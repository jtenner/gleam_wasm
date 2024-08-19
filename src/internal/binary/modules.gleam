import gleam/bit_array
import gleam/bytes_builder.{type BytesBuilder}
import gleam/option.{type Option, None, Some}
import gleam/result
import internal/binary/common
import internal/binary/types.{
  encode_expression, encode_func_idx, encode_global_idx, encode_global_type,
  encode_locals, encode_mem_idx, encode_mem_type, encode_rec_type,
  encode_ref_type, encode_table_idx, encode_table_type, encode_type_idx,
}
import internal/binary/values.{encode_u32}

import internal/finger_tree.{type FingerTree}
import internal/structure/modules.{
  type BinaryModule, type CodeSection, type CustomSection, type DataCountSection,
  type DataSection, type ElementSection, type ExportSection,
  type FunctionSection, type GlobalSection, type ImportSection,
  type MemorySection, type StartSection, type TableSection, type TypeSection,
  CustomSection,
}
import internal/structure/numbers
import internal/structure/types.{
  type Code, type Data, type DataMode, type Elem, type Export, type Expr,
  type Global, type Import, type RecType, type RefType, type Table, ActiveData,
  ActiveElemMode, Data, DeclarativeElemMode, Elem, Expr, FuncExport,
  FuncHeapType, FuncImport, FuncRefType, GlobalExport, GlobalImport,
  HeapTypeRefType, I32Const, MemExport, MemImport, PassiveData, PassiveElemMode,
  RefFunc, Table, TableExport, TableIDX, TableImport,
} as structure_types

pub fn encode_module(module: BinaryModule) {
  let builder = bytes_builder.new()
  use builder <- result.try(encode_custom_sections(builder, module.custom_0))
  use builder <- result.try(
    builder |> common.encode_option(module.types, encode_type_section),
  )
  use builder <- result.try(encode_custom_sections(builder, module.custom_1))
  use builder <- result.try(
    builder |> common.encode_option(module.imports, encode_import_section),
  )
  use builder <- result.try(encode_custom_sections(builder, module.custom_2))
  use builder <- result.try(
    builder |> common.encode_option(module.functions, encode_function_section),
  )
  use builder <- result.try(encode_custom_sections(builder, module.custom_3))
  use builder <- result.try(
    builder |> common.encode_option(module.tables, encode_table_section),
  )
  use builder <- result.try(encode_custom_sections(builder, module.custom_4))
  use builder <- result.try(
    builder |> common.encode_option(module.memories, encode_memory_section),
  )
  use builder <- result.try(encode_custom_sections(builder, module.custom_5))
  use builder <- result.try(
    builder |> common.encode_option(module.globals, encode_global_section),
  )
  use builder <- result.try(encode_custom_sections(builder, module.custom_6))
  use builder <- result.try(
    builder |> common.encode_option(module.exports, encode_export_section),
  )
  use builder <- result.try(encode_custom_sections(builder, module.custom_7))
  use builder <- result.try(
    builder |> common.encode_option(module.start, encode_start_section),
  )
  use builder <- result.try(encode_custom_sections(builder, module.custom_8))
  use builder <- result.try(
    builder |> common.encode_option(module.elements, encode_element_section),
  )
  use builder <- result.try(encode_custom_sections(builder, module.custom_9))
  use builder <- result.try(
    builder |> common.encode_option(module.code, encode_code_section),
  )
  use builder <- result.try(encode_custom_sections(builder, module.custom_10))
  use builder <- result.try(
    builder |> common.encode_option(module.data, encode_data_section),
  )
  use builder <- result.try(encode_custom_sections(builder, module.custom_11))
  use builder <- result.try(
    builder
    |> common.encode_option(module.data_count, encode_data_count_section),
  )
  use builder <- result.map(encode_custom_sections(builder, module.custom_12))
  builder |> bytes_builder.to_bit_array
}

pub fn decode_custom_sections(bits: BitArray) {
  case bits {
    <<0x00>> -> {
      use #(size, bits) <- result.try(values.decode_u32(bits))
      let size = size |> numbers.unwrap_u32
      let start_length = bits |> bit_array.byte_size
      use #(name, bits) <- result.try(values.decode_string(bits))
      use #(data_size, bits) <- result.try(values.decode_u32(bits))
      let data_size = data_size |> numbers.unwrap_u32
      use #(data, bits) <- result.try(common.decode_bytes(bits, data_size))
      let end_length = bits |> bit_array.byte_size
      case size == { end_length - start_length } {
        True -> Ok(#(CustomSection(name, bits), bits))
        False -> Error("Bytelength mismatch")
      }
    }
    _ -> Error("Not a custom section")
  }
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
  let assert Ok(size) = numbers.u32(size)
  builder
  |> bytes_builder.append(<<0x00>>)
  |> encode_u32(size)
  |> bytes_builder.append(section.data)
}

pub fn encode_type_section(builder: BytesBuilder, section: TypeSection) {
  use section_builder <- result.try(
    builder |> common.encode_vec(section.types, encode_rec_type),
  )
  use size <- result.map(
    section_builder |> bytes_builder.byte_size |> numbers.u32,
  )
  section_builder
  |> bytes_builder.append(<<0x01>>)
  |> encode_u32(size)
  |> bytes_builder.append_builder(section_builder)
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

pub fn encode_import_section(builder: BytesBuilder, section: ImportSection) {
  use section_builder <- result.try(
    bytes_builder.new() |> common.encode_vec(section.imports, encode_import),
  )

  use size <- result.map(
    section_builder |> bytes_builder.byte_size |> numbers.u32,
  )
  builder
  |> bytes_builder.append(<<0x01>>)
  |> encode_u32(size)
  |> bytes_builder.append_builder(section_builder)
}

pub fn encode_function_section(
  builder: BytesBuilder,
  function_section: FunctionSection,
) {
  use section_builder <- result.try(
    bytes_builder.new()
    |> common.encode_vec(function_section.funcs, encode_type_idx),
  )
  use size <- result.map(
    section_builder |> bytes_builder.byte_size |> numbers.u32,
  )

  builder
  |> bytes_builder.append(<<3>>)
  |> encode_u32(size)
  |> bytes_builder.append_builder(section_builder)
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

pub fn encode_table_section(builder: BytesBuilder, table_section: TableSection) {
  use section_builder <- result.try(
    bytes_builder.new()
    |> common.encode_vec(table_section.tables, encode_table),
  )
  use size <- result.map(
    section_builder |> bytes_builder.byte_size |> numbers.u32,
  )
  builder
  |> bytes_builder.append(<<4>>)
  |> encode_u32(size)
  |> bytes_builder.append_builder(section_builder)
}

pub fn encode_memory_section(
  builder: BytesBuilder,
  memory_section: MemorySection,
) {
  use section_builder <- result.try(
    bytes_builder.new()
    |> common.encode_vec(memory_section.mts, encode_mem_type),
  )
  use size <- result.map(
    section_builder |> bytes_builder.byte_size |> numbers.u32,
  )
  builder
  |> bytes_builder.append(<<5>>)
  |> encode_u32(size)
  |> bytes_builder.append_builder(section_builder)
}

pub fn encode_global(builder: BytesBuilder, global: Global) {
  use builder <- result.try(builder |> encode_global_type(global.type_))
  builder |> encode_expression(global.init)
}

pub fn encode_global_section(
  builder: BytesBuilder,
  global_section: GlobalSection,
) {
  use section_builder <- result.try(
    bytes_builder.new()
    |> common.encode_vec(global_section.globals, encode_global),
  )
  use size <- result.map(
    section_builder |> bytes_builder.byte_size |> numbers.u32,
  )
  builder
  |> bytes_builder.append(<<6>>)
  |> encode_u32(size)
  |> bytes_builder.append_builder(section_builder)
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
  export_section: ExportSection,
) {
  use section_builder <- result.try(
    bytes_builder.new()
    |> common.encode_vec(export_section.exports, encode_export),
  )
  use size <- result.map(
    section_builder |> bytes_builder.byte_size |> numbers.u32,
  )
  builder
  |> bytes_builder.append(<<7>>)
  |> encode_u32(size)
  |> bytes_builder.append_builder(section_builder)
}

pub fn encode_start_section(builder: BytesBuilder, start_section: StartSection) {
  builder
  |> bytes_builder.append(<<8>>)
  |> encode_func_idx(start_section.start)
}

pub fn encode_element_segment(builder: BytesBuilder, element: Elem) {
  let Elem(type_, init, mode) = element
  let assert Ok(zero) = numbers.u32(0x00)
  let table_idx_zero = TableIDX(zero)

  case type_, mode {
    // Type: 0
    // this segment initializes the first table with the given function indexes at offset [Expr]
    // [Active with func idx] 0x00 Offset Expr, init:FuncIDX*
    HeapTypeRefType(FuncHeapType, False), ActiveElemMode(table_idx, offset)
      if table_idx == table_idx_zero
    -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x00>>)
        |> encode_expression(offset),
      )
      use init <- result.try(finger_tree.try_map(init, expression_to_func_idx))
      builder
      |> common.encode_vec(init, encode_func_idx)
    }

    // Type: 1
    // this segment can only be used with a `table.init` instruction with an array of function indexes
    // [Passive with func idx] 0x01 0x00 init:FuncIDX*
    HeapTypeRefType(FuncHeapType, False), PassiveElemMode -> {
      use init <- result.try(finger_tree.try_map(init, expression_to_func_idx))
      builder
      |> bytes_builder.append(<<0x01, 0x00>>)
      |> common.encode_vec(init, encode_func_idx)
    }

    // Note: because it's implied table index is 0, variant 4 must be tried as a special case first
    // Type: 4
    // this segment initializes the first table with the given expressions at offset [Expr]
    // [Active with function expressions] 0x04 Offset Expr, init:Expr*
    HeapTypeRefType(FuncHeapType, False), ActiveElemMode(table_idx, offset)
      if table_idx == table_idx_zero
    -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x04>>)
        |> encode_expression(offset),
      )
      builder |> common.encode_vec(init, encode_expression)
    }

    // Type: 2
    // this segment initializes the table at index [x] with the given function indexes at offset [Expr]
    // [Active with func idx] 0x02 x:TableIDX offset:Expr 0x00 init:FuncIDX*
    HeapTypeRefType(FuncHeapType, False), ActiveElemMode(table_idx, offset) -> {
      use init <- result.try(finger_tree.try_map(init, expression_to_func_idx))
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x02>>)
        |> encode_table_idx(table_idx),
      )
      use builder <- result.try(builder |> encode_expression(offset))
      builder
      |> bytes_builder.append(<<0x00>>)
      |> common.encode_vec(init, encode_func_idx)
    }

    // Type: 3
    // this segment pre-initializes an array of function indexes for future use
    // [Declarative with func idx] 0x03 0x00 init:FuncIDX*
    HeapTypeRefType(FuncHeapType, False), DeclarativeElemMode -> {
      use init <- result.try(finger_tree.try_map(init, expression_to_func_idx))
      builder
      |> bytes_builder.append(<<0x03, 0x00>>)
      |> common.encode_vec(init, encode_func_idx)
    }

    // Type: 5
    // this segment can only be used with a `table.init` instruction with an array of expressions of
    // the given reftype
    // [Passive with reftype expressions] 0x05 rt:Reftype init:Expr*
    type_, PassiveElemMode -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x05>>)
        |> encode_ref_type(type_),
      )
      builder |> common.encode_vec(init, encode_expression)
    }

    // Type: 6
    // this segment initializes the given table with the given expressions of the given reftype at
    // the provided offset
    // [Active with reftype expressions] 0x06 x:TableIDX offset:Expr rt:Reftype init:Expr*
    type_, ActiveElemMode(table_idx, offset) -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x06>>)
        |> encode_table_idx(table_idx),
      )
      use builder <- result.try(builder |> encode_expression(offset))
      use builder <- result.try(builder |> encode_ref_type(type_))
      builder |> common.encode_vec(init, encode_expression)
    }

    // Type: 7
    // this segment pre-initializes an array of expressions of the given reftype for future use
    // [Declarative with reftype expressions] 0x07 rt:Reftype init:Expr*
    type_, DeclarativeElemMode -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x07>>)
        |> encode_ref_type(type_),
      )
      builder |> common.encode_vec(init, encode_expression)
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

pub fn encode_element_section(
  builder: BytesBuilder,
  element_section: ElementSection,
) {
  use section_builder <- result.try(
    bytes_builder.new()
    |> common.encode_vec(element_section.elems, encode_element_segment),
  )
  use size <- result.map(
    section_builder |> bytes_builder.byte_size |> numbers.u32,
  )
  builder
  |> bytes_builder.append(<<0x09>>)
  |> encode_u32(size)
  |> bytes_builder.append_builder(section_builder)
}

fn encode_code_segment(builder: BytesBuilder, code: Code) {
  use code_builder <- result.try(
    bytes_builder.new()
    |> common.encode_vec(code.locals, encode_locals),
  )
  use code_builder <- result.try(code_builder |> encode_expression(code.body))
  use size <- result.map(code_builder |> bytes_builder.byte_size |> numbers.u32)
  builder
  |> encode_u32(size)
  |> bytes_builder.append_builder(code_builder)
}

pub fn encode_code_section(builder: BytesBuilder, code_section: CodeSection) {
  use section_builder <- result.try(
    bytes_builder.new()
    |> common.encode_vec(code_section.codes, encode_code_segment),
  )
  use size <- result.map(
    section_builder |> bytes_builder.byte_size |> numbers.u32,
  )
  builder
  |> bytes_builder.append(<<0x0A>>)
  |> encode_u32(size)
  |> bytes_builder.append_builder(section_builder)
}

fn encode_data_segment(builder: BytesBuilder, data: Data) {
  let Data(mode, mem, offset, init) = data
  use size <- result.try(init |> bit_array.byte_size |> numbers.u32)

  case mode {
    // type: 1
    PassiveData -> {
      Ok(
        builder
        |> bytes_builder.append(<<0x01>>)
        |> encode_u32(size)
        |> bytes_builder.append(init),
      )
    }
    ActiveData -> {
      let offset = case offset {
        Some(offset) -> offset
        None -> {
          let assert Ok(offset) = numbers.i32(0)
          Expr(finger_tree.from_list([I32Const(offset)]))
        }
      }
      case mem {
        // type: 0
        None -> {
          use builder <- result.map(
            builder
            |> bytes_builder.append(<<0x00>>)
            |> encode_expression(offset),
          )
          builder
          |> encode_u32(size)
          |> bytes_builder.append(init)
        }
        // type: 2
        Some(mem) -> {
          use builder <- result.try(
            builder
            |> bytes_builder.append(<<0x02>>)
            |> encode_mem_idx(mem),
          )
          use builder <- result.map(
            builder
            |> encode_expression(offset),
          )
          builder
          |> encode_u32(size)
          |> bytes_builder.append(init)
        }
      }
    }
  }
}

pub fn encode_data_section(builder: BytesBuilder, data_section: DataSection) {
  use section_builder <- result.try(
    bytes_builder.new()
    |> common.encode_vec(data_section.data, encode_data_segment),
  )
  use size <- result.map(
    section_builder |> bytes_builder.byte_size |> numbers.u32,
  )
  builder
  |> bytes_builder.append(<<0x0B>>)
  |> encode_u32(size)
  |> bytes_builder.append_builder(section_builder)
}

pub fn encode_data_count_section(
  builder: BytesBuilder,
  data_count_section: DataCountSection,
) {
  Ok(
    builder
    |> bytes_builder.append(<<0x0C>>)
    |> encode_u32(data_count_section.count),
  )
}
