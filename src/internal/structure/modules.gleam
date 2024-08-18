import gleam/option.{type Option}
import internal/finger_tree.{type FingerTree}
import internal/structure/numbers.{type U32}
import internal/structure/types.{
  type Code, type Data, type Elem, type Export, type FuncIDX, type Global,
  type Import, type MemType, type RecType, type Table, type TypeIDX,
}

pub type BinaryModule {
  BinaryModule(
    custom_0: Option(FingerTree(CustomSection)),
    types: Option(TypeSection),
    custom_1: Option(FingerTree(CustomSection)),
    imports: Option(ImportSection),
    custom_2: Option(FingerTree(CustomSection)),
    functions: Option(FunctionSection),
    custom_3: Option(FingerTree(CustomSection)),
    tables: Option(TableSection),
    custom_4: Option(FingerTree(CustomSection)),
    memories: Option(MemorySection),
    custom_5: Option(FingerTree(CustomSection)),
    globals: Option(GlobalSection),
    custom_6: Option(FingerTree(CustomSection)),
    exports: Option(ExportSection),
    custom_7: Option(FingerTree(CustomSection)),
    start: Option(StartSection),
    custom_8: Option(FingerTree(CustomSection)),
    elements: Option(ElementSection),
    custom_9: Option(FingerTree(CustomSection)),
    code: Option(CodeSection),
    custom_10: Option(FingerTree(CustomSection)),
    data: Option(DataSection),
    custom_11: Option(FingerTree(CustomSection)),
    data_count: Option(DataCountSection),
    custom_12: Option(FingerTree(CustomSection)),
  )
}

pub type CustomSection {
  CustomSection(data: BitArray)
}

pub type TypeSection {
  TypeSection(types: FingerTree(RecType))
}

pub type ImportSection {
  ImportSection(imports: FingerTree(Import))
}

pub type FunctionSection {
  FunctionSection(funcs: FingerTree(TypeIDX))
}

pub type TableSection {
  TableSection(tables: FingerTree(Table))
}

pub type MemorySection {
  MemorySection(mts: FingerTree(MemType))
}

pub type GlobalSection {
  GlobalSection(globals: FingerTree(Global))
}

pub type ExportSection {
  ExportSection(exports: FingerTree(Export))
}

pub type StartSection {
  StartSection(start: FuncIDX)
}

pub type ElementSection {
  ElementSection(elems: FingerTree(Elem))
}

pub type CodeSection {
  CodeSection(codes: FingerTree(Code))
}

pub type DataSection {
  DataSection(data: FingerTree(Data))
}

pub type DataCountSection {
  DataCountSection(count: U32)
}
