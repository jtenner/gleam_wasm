import gleam/list
import gleam/option.{type Option}
import internal/finger_tree.{type FingerTree}
import internal/structure/numbers.{type U32}
import internal/structure/types.{
  type Expr, type GlobalType, type MemType, type RecType, type RefType,
  type TableType, type ValType,
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#modules
pub type Module {
  Module(
    types: FingerTree(RecType),
    funcs: FingerTree(Func),
    tables: FingerTree(Table),
    mems: FingerTree(Mem),
    globals: FingerTree(Global),
    elems: FingerTree(Elem),
    datas: FingerTree(Data),
    start: Option(Start),
    imports: FingerTree(Import),
    exports: FingerTree(Export),
  )
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#functions
pub type Func {
  Func(type_: U32, locals: FingerTree(Local), body: Expr)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#functions
pub type Local {
  Local(type_: ValType)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#tables
pub type Table {
  Table(type_: TableType, init: Expr)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#memories
pub type Mem {
  Mem(type_: MemType)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#globals
pub type Global {
  Global(type_: GlobalType, init: Expr)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#element-segments
pub type Elem {
  Elem(type_: RefType, init: FingerTree(Expr), mode: ElemMode)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#element-segments
pub type ElemMode {
  PassiveElemMode
  ActiveElemMode(table: U32, offset: Expr)
  DeclarativeElemMode
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#data-segments
pub type Data {
  Data(init: BitArray, mode: DataMode)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#data-segments
pub type DataMode {
  PassiveDataMode
  ActiveDataMode(memory: U32, offset: Expr)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#start-function
pub type Start {
  Start(func: U32)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#exports
pub type Export {
  Export(name: String, desc: ExportDesc)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#exports
pub type ExportDesc {
  ExportFunc(func: U32)
  ExportTable(table: U32)
  ExportMem(mem: U32)
  ExportGlobal(global: U32)
}

pub fn export_is_func(export: Export) -> Bool {
  case export.desc {
    ExportFunc(_) -> True
    _ -> False
  }
}

/// Conventions: https://webassembly.github.io/gc/core/syntax/modules.html#exports
pub fn export_is_table(export: Export) -> Bool {
  case export.desc {
    ExportTable(_) -> True
    _ -> False
  }
}

pub fn export_is_mem(export: Export) -> Bool {
  case export.desc {
    ExportMem(_) -> True
    _ -> False
  }
}

pub fn export_is_global(export: Export) -> Bool {
  case export.desc {
    ExportGlobal(_) -> True
    _ -> False
  }
}

pub fn funcs(exports: FingerTree(Export)) -> FingerTree(Export) {
  exports |> finger_tree.filter(export_is_func)
}

pub fn tables(exports: FingerTree(Export)) -> FingerTree(Export) {
  exports |> finger_tree.filter(export_is_table)
}

pub fn mems(exports: FingerTree(Export)) -> FingerTree(Export) {
  exports |> finger_tree.filter(export_is_mem)
}

pub fn globals(exports: FingerTree(Export)) -> FingerTree(Export) {
  exports |> finger_tree.filter(export_is_global)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#imports
pub type Import {
  Import(module: String, name: String, desc: ImportDesc)
}

/// Please see: https://webassembly.github.io/gc/core/syntax/modules.html#imports
pub type ImportDesc {
  ImportFunc(type_: U32)
  ImportTable(type_: TableType)
  ImportMem(type_: MemType)
  ImportGlobal(type_: GlobalType)
}
