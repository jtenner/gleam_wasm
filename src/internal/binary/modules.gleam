import gleam/bit_array
import gleam/bytes_builder.{type BytesBuilder}
import gleam/option.{type Option, None, Some}
import gleam/result
import internal/binary/common.{encode_bytes_builder_vec}
import internal/binary/types.{
  decode_expression, decode_func_idx, decode_global_idx, decode_global_type,
  decode_mem_idx, decode_mem_type, decode_rec_type, decode_ref_type,
  decode_table_idx, decode_table_type, decode_type_idx, decode_val_type,
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
  BinaryModule, CodeSection, CustomSection, DataCountSection, DataSection,
  ElementSection, ExportSection, FunctionSection, GlobalSection, ImportSection,
  MemorySection, StartSection, TableSection, TypeSection, binary_module_new,
}
import internal/structure/numbers
import internal/structure/types.{
  type Code, type Data, type Elem, type Export, type Expr, type FuncIDX,
  type Global, type Import, type RecType, type RefType, type Table, ActiveData,
  ActiveElemMode, Code, DeclarativeElemMode, Elem, Expr, FuncExport,
  FuncHeapType, FuncImport, FuncRefType, Global, GlobalExport, GlobalImport,
  HeapTypeRefType, I32Const, Locals, MemExport, MemImport, PassiveData,
  PassiveElemMode, RefFunc, RefNull, Table, TableExport, TableIDX, TableImport,
  ref_type_unwrap_heap_type,
} as structure_types

/// Encode a binary module, returning either a BitArray or an Error
pub fn encode_module(module: BinaryModule) {
  let builder = bytes_builder.new()

  // Each section can be prepended with an unlimited number of custom sections
  use builder <- result.try(encode_custom_sections(builder, module.custom_0))
  // The type section has id 0x01, and is always first if it exists in the module
  use builder <- result.try(
    builder |> common.encode_option(module.types, encode_type_section),
  )

  // The next section is always the import section if it exists with id 0x02
  use builder <- result.try(encode_custom_sections(builder, module.custom_1))
  use builder <- result.try(
    builder |> common.encode_option(module.imports, encode_import_section),
  )

  // The next section is always the function section if it exists with id 0x03
  use builder <- result.try(encode_custom_sections(builder, module.custom_2))
  use builder <- result.try(
    builder |> common.encode_option(module.functions, encode_function_section),
  )

  // The next section is always the table section if it exists with id 0x04
  use builder <- result.try(encode_custom_sections(builder, module.custom_3))
  use builder <- result.try(
    builder |> common.encode_option(module.tables, encode_table_section),
  )

  // The next section is always the memory section if it exists with id 0x05
  use builder <- result.try(encode_custom_sections(builder, module.custom_4))
  use builder <- result.try(
    builder |> common.encode_option(module.memories, encode_memory_section),
  )

  // The next section is always the global section if it exists with id 0x06
  use builder <- result.try(encode_custom_sections(builder, module.custom_5))
  use builder <- result.try(
    builder |> common.encode_option(module.globals, encode_global_section),
  )

  // The next section is always the export section if it exists with id 0x07
  use builder <- result.try(encode_custom_sections(builder, module.custom_6))
  use builder <- result.try(
    builder |> common.encode_option(module.exports, encode_export_section),
  )

  // The next section is always the start section if it exists with id 0x08
  use builder <- result.try(encode_custom_sections(builder, module.custom_7))
  use builder <- result.try(
    builder |> common.encode_option(module.start, encode_start_section),
  )

  // The next section is always the element section if it exists with id 0x09
  use builder <- result.try(encode_custom_sections(builder, module.custom_8))
  use builder <- result.try(
    builder |> common.encode_option(module.elements, encode_element_section),
  )

  // The next section is always the code section if it exists with id 0x0a
  use builder <- result.try(encode_custom_sections(builder, module.custom_9))
  use builder <- result.try(
    builder |> common.encode_option(module.code, encode_code_section),
  )

  // The next section is always the data section if it exists with id 0x0b
  use builder <- result.try(encode_custom_sections(builder, module.custom_10))
  use builder <- result.try(
    builder |> common.encode_option(module.data, encode_data_section),
  )

  // The next section is always the custom section if it exists with id 0x0c
  use builder <- result.try(encode_custom_sections(builder, module.custom_11))
  use builder <- result.try(
    builder
    |> common.encode_option(module.data_count, encode_data_count_section),
  )

  // The last sections, according to the Wasm Spec, are always custom sections
  use builder <- result.map(encode_custom_sections(builder, module.custom_12))
  builder |> bytes_builder.to_bit_array
}

pub fn decode_module(bits: BitArray) {
  use #(custom_0, rest) <- result.try(decode_custom_sections(bits))
  use #(types, rest) <- result.try(decode_type_section(rest))
  use #(custom_1, rest) <- result.try(decode_custom_sections(rest))
  use #(imports, rest) <- result.try(decode_import_section(rest))
  use #(custom_2, rest) <- result.try(decode_custom_sections(rest))
  use #(functions, rest) <- result.try(decode_function_section(rest))
  use #(custom_3, rest) <- result.try(decode_custom_sections(rest))
  use #(tables, rest) <- result.try(decode_table_section(rest))
  use #(custom_4, rest) <- result.try(decode_custom_sections(rest))
  use #(memories, rest) <- result.try(decode_memory_section(rest))
  use #(custom_5, rest) <- result.try(decode_custom_sections(rest))
  use #(globals, rest) <- result.try(decode_global_section(rest))
  use #(custom_6, rest) <- result.try(decode_custom_sections(rest))
  use #(exports, rest) <- result.try(decode_export_section(rest))
  use #(custom_7, rest) <- result.try(decode_custom_sections(rest))
  use #(start, rest) <- result.try(decode_start_section(rest))
  use #(custom_8, rest) <- result.try(decode_custom_sections(rest))
  use #(elements, rest) <- result.try(decode_elememt_section(rest))
  use #(custom_9, rest) <- result.try(decode_custom_sections(rest))
  use #(code, rest) <- result.try(decode_code_section(rest))
  use #(custom_10, rest) <- result.try(decode_custom_sections(rest))
  use #(data, rest) <- result.try(decode_data_section(rest))
  use #(custom_11, rest) <- result.try(decode_custom_sections(rest))
  use #(data_count, rest) <- result.try(decode_data_count_section(rest))
  use #(custom_12, rest) <- result.map(decode_custom_sections(rest))
  #(
    BinaryModule(
      custom_0,
      types,
      custom_1,
      imports,
      custom_2,
      functions,
      custom_3,
      tables,
      custom_4,
      memories,
      custom_5,
      globals,
      custom_6,
      exports,
      custom_7,
      start,
      custom_8,
      elements,
      custom_9,
      code,
      custom_10,
      data,
      custom_11,
      data_count,
      custom_12,
    ),
    rest,
  )
}

pub fn decode_data_count_section(bits: BitArray) {
  case bits {
    <<0x0C, rest:bits>> -> {
      use #(count, rest) <- result.map(values.decode_u32(rest))
      #(Some(DataCountSection(count)), rest)
    }
    _ -> Ok(#(None, bits))
  }
}

pub fn decode_data(bits: BitArray) {
  use #(data_type, rest) <- result.try(values.decode_u32(bits))
  case data_type |> numbers.unwrap_u32 {
    0 -> {
      use #(offset, rest) <- result.try(decode_expression(rest))
      use #(data, rest) <- result.map(common.decode_byte_vec(rest))
      let assert Ok(mem_idx) = numbers.u32(0)
      let mem_idx = structure_types.MemIDX(mem_idx)

      #(ActiveData(mem_idx, offset, data), rest)
    }
    1 -> {
      use #(data, rest) <- result.map(common.decode_byte_vec(rest))
      #(PassiveData(data), rest)
    }
    2 -> {
      use #(mem_idx, rest) <- result.try(decode_mem_idx(rest))
      use #(offset, rest) <- result.try(decode_expression(rest))
      use #(data, rest) <- result.map(common.decode_byte_vec(rest))
      #(ActiveData(mem_idx, offset, data), rest)
    }
    _ -> Error("Invalid data type")
  }
}

fn do_decode_data_section(bits: BitArray) {
  use #(data, rest) <- result.map(common.decode_vec(bits, decode_data))
  #(DataSection(data), rest)
}

pub fn decode_data_section(bits: BitArray) {
  common.decode_section(bits, 0x0B, do_decode_data_section)
}

fn decode_locals(bits: BitArray) {
  use #(count, rest) <- result.try(values.decode_u32(bits))
  use #(vt, rest) <- result.map(decode_val_type(rest))
  #(Locals(count, vt), rest)
}

pub fn decode_code_segment(bits: BitArray) {
  use #(size, rest) <- result.try(values.decode_u32(bits))
  let size = size |> numbers.unwrap_u32
  use #(code_bytes, rest) <- result.try(common.decode_bytes(rest, size))
  use #(locals, code_bytes) <- result.try(common.decode_vec(
    code_bytes,
    decode_locals,
  ))
  use #(body, code_bytes) <- result.try(decode_expression(code_bytes))

  case code_bytes {
    <<>> -> Ok(#(Code(locals, body), rest))
    _ -> Error("Invalid code segment")
  }
}

fn do_decode_code_section(bits: BitArray) {
  use #(codes, rest) <- result.map(common.decode_vec(bits, decode_code_segment))
  #(CodeSection(codes), rest)
}

pub fn decode_code_section(bits: BitArray) {
  common.decode_section(bits, 0x0A, do_decode_code_section)
}

fn func_idx_to_expr(expr: FuncIDX) {
  Expr([RefFunc(expr)] |> finger_tree.from_list)
}

pub fn decode_elememt(bits: BitArray) {
  use #(elem_type, rest) <- result.try(values.decode_u32(bits))
  let elem_type = elem_type |> numbers.unwrap_u32
  case elem_type {
    0 -> {
      use #(expr, rest) <- result.try(decode_expression(rest))
      use #(idx, rest) <- result.map(common.decode_vec(rest, decode_func_idx))
      let assert Ok(zero) = numbers.u32(0x00)
      let table_idx_zero = TableIDX(zero)

      #(
        Elem(
          HeapTypeRefType(FuncHeapType, False),
          idx |> finger_tree.map(func_idx_to_expr),
          ActiveElemMode(table_idx_zero, expr),
        ),
        rest,
      )
    }
    1 -> {
      use #(_, rest) <- result.try(common.expect_decode_byte(rest, 0x00))
      use #(idx, rest) <- result.map(common.decode_vec(rest, decode_func_idx))
      #(
        Elem(
          HeapTypeRefType(FuncHeapType, False),
          idx |> finger_tree.map(func_idx_to_expr),
          PassiveElemMode,
        ),
        rest,
      )
    }
    2 -> {
      use #(table_idx, rest) <- result.try(decode_table_idx(rest))
      use #(expr, rest) <- result.try(decode_expression(rest))
      use #(idx, rest) <- result.map(common.decode_vec(rest, decode_func_idx))

      #(
        Elem(
          HeapTypeRefType(FuncHeapType, False),
          idx |> finger_tree.map(func_idx_to_expr),
          ActiveElemMode(table_idx, expr),
        ),
        rest,
      )
    }
    3 -> {
      use #(_, rest) <- result.try(common.expect_decode_byte(rest, 0x00))
      use #(idx, rest) <- result.map(common.decode_vec(rest, decode_func_idx))

      #(
        Elem(
          HeapTypeRefType(FuncHeapType, False),
          idx |> finger_tree.map(func_idx_to_expr),
          DeclarativeElemMode,
        ),
        rest,
      )
    }
    4 -> {
      use #(offset, rest) <- result.try(decode_expression(rest))
      use #(init, rest) <- result.map(common.decode_vec(rest, decode_expression))
      let assert Ok(zero) = numbers.u32(0x00)
      let table_idx_zero = TableIDX(zero)
      #(
        Elem(
          HeapTypeRefType(FuncHeapType, False),
          init,
          ActiveElemMode(table_idx_zero, offset),
        ),
        rest,
      )
    }
    5 -> {
      use #(rt, rest) <- result.try(decode_ref_type(rest))
      use #(init, rest) <- result.map(common.decode_vec(rest, decode_expression))
      #(Elem(rt, init, DeclarativeElemMode), rest)
    }
    6 -> {
      use #(table_idx, rest) <- result.try(decode_table_idx(rest))
      use #(offset, rest) <- result.try(decode_expression(rest))
      use #(rt, rest) <- result.try(decode_ref_type(rest))
      use #(init, rest) <- result.map(common.decode_vec(rest, decode_expression))
      #(Elem(rt, init, ActiveElemMode(table_idx, offset)), rest)
    }
    7 -> {
      use #(rt, rest) <- result.try(decode_ref_type(rest))
      use #(init, rest) <- result.map(common.decode_vec(rest, decode_expression))
      #(Elem(rt, init, PassiveElemMode), rest)
    }
    _ -> Error("Invalid element segment type")
  }
}

fn do_decode_element_segment(bits: BitArray) {
  use #(elems, rest) <- result.map(common.decode_vec(bits, decode_elememt))
  #(ElementSection(elems), rest)
}

pub fn decode_elememt_section(bits: BitArray) {
  common.decode_section(bits, 0x09, do_decode_element_segment)
}

fn do_decode_start_section(bits: BitArray) {
  use #(start_idx, rest) <- result.map(decode_func_idx(bits))
  #(StartSection(start_idx), rest)
}

pub fn decode_start_section(bits: BitArray) {
  common.decode_section(bits, 0x08, do_decode_start_section)
}

fn decode_export(bits: BitArray) {
  use #(name, rest) <- result.try(common.decode_string(bits))
  case rest {
    <<0, rest:bits>> -> {
      use #(func_idx, rest) <- result.map(decode_func_idx(rest))
      #(FuncExport(name, func_idx), rest)
    }
    <<1, rest:bits>> -> {
      use #(table_idx, rest) <- result.map(decode_table_idx(rest))
      #(TableExport(name, table_idx), rest)
    }
    <<2, rest:bits>> -> {
      use #(mem_idx, rest) <- result.map(decode_mem_idx(rest))
      #(MemExport(name, mem_idx), rest)
    }
    <<3, rest:bits>> -> {
      use #(global_idx, rest) <- result.map(decode_global_idx(rest))
      #(GlobalExport(name, global_idx), rest)
    }
    _ -> Error("expected export")
  }
}

fn do_decode_export_section(bits: BitArray) {
  use #(exports, rest) <- result.map(common.decode_vec(bits, decode_export))
  #(ExportSection(exports), rest)
}

pub fn decode_export_section(bits: BitArray) {
  common.decode_section(bits, 7, do_decode_export_section)
}

fn decode_global(bits: BitArray) {
  use #(gt, rest) <- result.try(decode_global_type(bits))
  use #(expr, rest) <- result.map(decode_expression(rest))
  #(Global(gt, expr), rest)
}

fn do_decode_global_section(bits: BitArray) {
  use #(globals, rest) <- result.map(common.decode_vec(bits, decode_global))
  #(GlobalSection(globals), rest)
}

pub fn decode_global_section(bits: BitArray) {
  common.decode_section(bits, 6, do_decode_global_section)
}

fn do_decode_memory_section(bits: BitArray) {
  use #(mems, rest) <- result.map(common.decode_vec(bits, decode_mem_type))
  #(MemorySection(mems), rest)
}

pub fn decode_memory_section(bits: BitArray) {
  common.decode_section(bits, 5, do_decode_memory_section)
}

fn decode_table(bits: BitArray) {
  case bits {
    <<0x40, 0x00, rest:bits>> -> {
      use #(tt, rest) <- result.try(decode_table_type(rest))
      use #(expr, rest) <- result.map(decode_expression(rest))
      #(Table(tt, Some(expr)), rest)
    }
    _ -> {
      use #(tt, rest) <- result.map(decode_table_type(bits))
      let ht = ref_type_unwrap_heap_type(tt.t)
      let expr = RefNull(ht)
      let expr = finger_tree.from_list([expr])
      let expr = Some(Expr(expr))
      #(Table(tt, expr), rest)
    }
  }
}

fn do_decode_table_section(bits: BitArray) {
  use #(types, rest) <- result.map(common.decode_vec(bits, decode_table))
  #(TableSection(types), rest)
}

pub fn decode_table_section(bits: BitArray) {
  common.decode_section(bits, 4, do_decode_table_section)
}

fn do_decode_function_section(bits: BitArray) {
  use #(indices, rest) <- result.map(common.decode_vec(bits, decode_type_idx))
  #(FunctionSection(indices), rest)
}

pub fn decode_function_section(bits: BitArray) {
  common.decode_section(bits, 3, do_decode_function_section)
}

fn decode_import(bits: BitArray) {
  use #(mod, rest) <- result.try(common.decode_string(bits))
  use #(name, rest) <- result.try(common.decode_string(rest))
  case rest {
    <<0x00, rest:bits>> -> {
      use #(type_idx, rest) <- result.map(decode_type_idx(rest))
      #(FuncImport(mod, name, type_idx), rest)
    }
    <<0x01, rest:bits>> -> {
      use #(tt, rest) <- result.map(decode_table_type(rest))
      #(TableImport(mod, name, tt), rest)
    }
    <<0x02, rest:bits>> -> {
      use #(mt, rest) <- result.map(decode_mem_type(rest))
      #(MemImport(mod, name, mt), rest)
    }
    <<0x03, rest:bits>> -> {
      use #(gt, rest) <- result.map(decode_global_type(rest))
      #(GlobalImport(mod, name, gt), rest)
    }
    _ -> Error("Invalid import")
  }
}

fn do_decode_import_section(bits: BitArray) {
  use #(imports, rest) <- result.map(common.decode_vec(bits, decode_import))
  #(ImportSection(imports), rest)
}

pub fn decode_import_section(bits: BitArray) {
  common.decode_section(bits, 2, do_decode_import_section)
}

fn do_decode_type_section(bits: BitArray) {
  use #(types, left) <- result.map(common.decode_vec(bits, decode_rec_type))
  #(TypeSection(types), left)
}

pub fn decode_type_section(bits: BitArray) {
  common.decode_section(bits, 1, do_decode_type_section)
}

pub fn decode_custom_sections(bits: BitArray) {
  do_decode_custom_sections(bits, finger_tree.new())
}

fn decode_custom_section(bits: BitArray) {
  use #(name, rest) <- result.try(common.decode_string(bits))
  use #(data_size, rest) <- result.try(values.decode_u32(rest))
  let data_size = data_size |> numbers.unwrap_u32
  use #(data, rest) <- result.try(common.decode_bytes(rest, data_size))
  case rest {
    <<>> -> Ok(#(CustomSection(name, data), rest))
    _ -> Error("Invalid custom section")
  }
}

fn do_decode_custom_sections(bits: BitArray, acc: FingerTree(CustomSection)) {
  use #(section, rest) <- result.try(common.decode_section(
    bits,
    0x00,
    decode_custom_section,
  ))
  case section {
    Some(section) ->
      do_decode_custom_sections(rest, acc |> finger_tree.push(section))
    None -> Ok(#(Some(acc), rest))
  }
}

pub fn encode_custom_sections(
  builder: BytesBuilder,
  sections: Option(FingerTree(CustomSection)),
) {
  case sections {
    Some(sections) ->
      finger_tree.try_reducel(sections, builder, encode_custom_section)
    None -> Ok(builder)
  }
}

pub fn encode_custom_section(builder: BytesBuilder, section: CustomSection) {
  use section_builder <- result.try(
    bytes_builder.new()
    |> common.encode_string(section.name),
  )
  use section_builder <- result.try(
    section_builder |> common.encode_byte_vec(section.data),
  )
  builder
  |> bytes_builder.append(<<0x00>>)
  |> encode_bytes_builder_vec(section_builder)
}

pub fn encode_type_section(builder: BytesBuilder, section: TypeSection) {
  use section_builder <- result.try(
    bytes_builder.new() |> common.encode_vec(section.types, encode_rec_type),
  )
  builder
  |> bytes_builder.append(<<0x01>>)
  |> encode_bytes_builder_vec(section_builder)
}

pub fn encode_import(builder: BytesBuilder, import_: Import) {
  case import_ {
    FuncImport(mod, name, type_idx) -> {
      use builder <- result.try(builder |> common.encode_string(mod))
      use builder <- result.try(builder |> common.encode_string(name))
      builder
      |> bytes_builder.append(<<0x00>>)
      |> encode_type_idx(type_idx)
    }
    TableImport(mod, name, table_type) -> {
      use builder <- result.try(builder |> common.encode_string(mod))
      use builder <- result.try(builder |> common.encode_string(name))
      builder
      |> bytes_builder.append(<<0x01>>)
      |> encode_table_type(table_type)
    }
    MemImport(mod, name, mem_type) -> {
      use builder <- result.try(builder |> common.encode_string(mod))
      use builder <- result.try(builder |> common.encode_string(name))
      builder
      |> bytes_builder.append(<<0x02>>)
      |> encode_mem_type(mem_type)
    }
    GlobalImport(mod, name, global_type) -> {
      use builder <- result.try(builder |> common.encode_string(mod))
      use builder <- result.try(builder |> common.encode_string(name))
      builder
      |> bytes_builder.append(<<0x03>>)
      |> encode_global_type(global_type)
    }
  }
}

pub fn encode_import_section(builder: BytesBuilder, section: ImportSection) {
  use section_builder <- result.try(
    bytes_builder.new() |> common.encode_vec(section.imports, encode_import),
  )
  builder
  |> bytes_builder.append(<<0x02>>)
  |> encode_bytes_builder_vec(section_builder)
}

pub fn encode_function_section(
  builder: BytesBuilder,
  function_section: FunctionSection,
) {
  use section_builder <- result.try(
    bytes_builder.new()
    |> common.encode_vec(function_section.funcs, encode_type_idx),
  )

  builder
  |> bytes_builder.append(<<0x03>>)
  |> encode_bytes_builder_vec(section_builder)
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
  builder
  |> bytes_builder.append(<<0x04>>)
  |> encode_bytes_builder_vec(section_builder)
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
  // let Data(mode, mem, offset, init) = data
  // use size <- result.try(init |> bit_array.byte_size |> numbers.u32)
  use index_zero <- result.try(numbers.u32(0))
  let index_zero = structure_types.MemIDX(index_zero)
  case data {
    ActiveData(mem, offset, init) if mem == index_zero -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x00>>)
        |> encode_expression(offset),
      )
      builder |> common.encode_byte_vec(init)
    }
    PassiveData(init) -> {
      builder
      |> bytes_builder.append(<<0x01>>)
      |> common.encode_byte_vec(init)
    }
    ActiveData(mem, offset, init) -> {
      use builder <- result.try(
        builder
        |> bytes_builder.append(<<0x02>>)
        |> encode_mem_idx(mem),
      )
      use builder <- result.try(builder |> encode_expression(offset))
      builder |> common.encode_byte_vec(init)
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
