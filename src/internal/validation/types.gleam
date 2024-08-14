import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import internal/finger_tree.{type FingerTree}
import internal/structure/common as structure_common
import internal/structure/numbers.{type U32, u32, unwrap_u32}
import internal/structure/types.{
  type AbstractHeapType, type ArrayType, type BlockType, type CompositeType,
  type DefType, type ExternType, type FieldType, type FuncIDX, type FuncType,
  type GlobalType, type HeapType, type Instruction, type LocalType, type MemType,
  type NumType, type PackedType, type RecType, type RefType, type ResultType,
  type StorageType, type StructType, type SubType, type TableType, type TypeIDX,
  type ValType, type VecType, AnyHeapType, AnyRefType, ArrayHeapType,
  ArrayRefType, ConcreteHeapType, DefType, EqHeapType, EqRefType, ExternHeapType,
  ExternRefType, FuncCompositeType, FuncHeapType, FuncRefType, FuncTypeBlockType,
  HeapTypeRefType, I31HeapType, I31RefType, NoExternHeapType, NoExternRefType,
  NoFuncHeapType, NoFuncRefType, NoneHeapType, NoneRefType, RecType,
  StructHeapType, StructRefType, SubType, TypeIDX,
}

/// Please see: https://webassembly.github.io/gc/core/valid/conventions.html#contexts
pub type Context {
  Context(
    types: FingerTree(DefType),
    funcs: FingerTree(DefType),
    tables: FingerTree(TableType),
    mems: FingerTree(MemType),
    globals: FingerTree(GlobalType),
    elems: FingerTree(RefType),
    datas: FingerTree(Bool),
    locals: FingerTree(LocalType),
    labels: FingerTree(ResultType),
    return: Option(ResultType),
    refs: FingerTree(FuncIDX),
  )
}

pub fn new_context() -> Context {
  Context(
    types: finger_tree.new(),
    funcs: finger_tree.new(),
    tables: finger_tree.new(),
    mems: finger_tree.new(),
    globals: finger_tree.new(),
    elems: finger_tree.new(),
    datas: finger_tree.new(),
    locals: finger_tree.new(),
    labels: finger_tree.new(),
    return: None,
    refs: finger_tree.new(),
  )
}

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
    NoneRefType -> HeapTypeRefType(NoneHeapType, False)
    NoFuncRefType -> HeapTypeRefType(NoFuncHeapType, False)
    NoExternRefType -> HeapTypeRefType(NoExternHeapType, False)
  }
}

fn validate_heap_type(ctx: Context, ht: HeapType) {
  case ht {
    ConcreteHeapType(TypeIDX(i)) -> {
      let idx = i |> unwrap_u32

      case ctx.types |> finger_tree.size > idx {
        True -> Ok(#(ctx, ht))
        False -> Error("Type index out of bounds")
      }
    }
    _ -> Ok(#(ctx, ht))
  }
}

/// Block types are valid if their type indexes point to a Composite Function
/// type
fn validate_block_type(ctx: Context, bt: BlockType) {
  case bt {
    FuncTypeBlockType(TypeIDX(idx)) -> {
      let idx = idx |> unwrap_u32
      case idx < ctx.funcs |> finger_tree.size {
        True -> Ok(#(ctx, bt))
        False -> Error("Type index out of bounds")
      }
    }
    _ -> Ok(#(ctx, bt))
  }
}

pub const type_validator = TypeVisitor(
  type_idx: // type indicies are valid when their parent rec types are valid
  // after being rolled up
  None,
  num_type: // Number types are always valid
  None,
  vec_type: // Vec types are always valid
  None,
  heap_type: // Heap types are valid if they are abstract, otherwise the index must be validated.
  // However, since `TypeIDX` instances can be transformed into RecTypeIDX
  Some(validate_heap_type),
  abstract_heap_type: // Abstract heap types are always valid
  None,
  ref_type: // Ref types are valid if their heap types are valid
  None,
  val_type: // Val types are always valid, unless they are RefTypes, in which case
  // the ValType is valid if the RefType is valid which is done by the `validate_heap_type`
  // function.
  None,
  block_type: // Block types are valid if their val types are valid
  // When the block type is a TypeIDX, the type should be defined, and
  // the func type should be defined with the same TypeIDX.
  // The expansion of C.funcs[TypeIDX] must be a function
  Some(validate_block_type),
  result_type: // Result type and result types are always valid as long as the
  // value types within are valid which is handled by validating the
  // valtype
  None,
  result_types: None,
  func_type: // FuncTypes are valid it their result types are valid
  None,
  struct_type: // Struct types are valid if their field types are valid
  None,
  array_type: // Array types are valid if their element types are valid
  None,
  field_type: // Field types are valid if their storage type is valid
  None,
  storage_type: // storage types are valid if they are packed, or if their
  // ValType is valid
  None,
  packed_type: // packed types are always valid 
  None,
  composite_type: // Composite types are always valid as long as their
  // component types are valid
  None,
  rec_type: None,
  sub_type: // Please see the validate_sub_type function above
  None,
  mem_type: // Some(validate_sub_type),
  // mem types must always be in range
  None,
  table_type: // Some(validate_mem_type),
  // table types, global types and extern types are always valid as long as
  // their components are valid
  None,
  global_type: None,
  extern_type: None,
  instruction_type: // Please see the validate_instruction_type function above
  None,
  def_type: //Some(validate_instruction_type),
  // DefTypes must be properly indexed
  None,
  local_type: //Some(validate_def_type),
  // local types are valid if their valtypes are valid
  None,
)

type NumTypeVisitor =
  fn(Context, NumType) -> Result(#(Context, NumType), String)

type VecTypeVisitor =
  fn(Context, VecType) -> Result(#(Context, VecType), String)

type HeapTypeVisitor =
  fn(Context, HeapType) -> Result(#(Context, HeapType), String)

type AbstractHeapTypeVisitor =
  fn(Context, AbstractHeapType) -> Result(#(Context, AbstractHeapType), String)

type RefTypeVisitor =
  fn(Context, RefType) -> Result(#(Context, RefType), String)

type ValTypeVisitor =
  fn(Context, ValType) -> Result(#(Context, ValType), String)

type ResultTypeVisitor =
  fn(Context, ResultType) -> Result(#(Context, ResultType), String)

type ResultTypesVisitor =
  fn(Context, ResultType) -> Result(#(Context, ResultType), String)

type FuncTypeVisitor =
  fn(Context, FuncType) -> Result(#(Context, FuncType), String)

type StructTypeVisitor =
  fn(Context, StructType) -> Result(#(Context, StructType), String)

type ArrayTypeVisitor =
  fn(Context, ArrayType) -> Result(#(Context, ArrayType), String)

type FieldTypeVisitor =
  fn(Context, FieldType) -> Result(#(Context, FieldType), String)

type StorageTypeVisitor =
  fn(Context, StorageType) -> Result(#(Context, StorageType), String)

type PackedTypeVisitor =
  fn(Context, PackedType) -> Result(#(Context, PackedType), String)

type CompositeTypeVisitor =
  fn(Context, CompositeType) -> Result(#(Context, CompositeType), String)

type RecTypeVisitor =
  fn(Context, RecType) -> Result(#(Context, RecType), String)

type SubTypeVisitor =
  fn(Context, SubType) -> Result(#(Context, SubType), String)

type MemTypeVisitor =
  fn(Context, MemType) -> Result(#(Context, MemType), String)

type TableTypeVisitor =
  fn(Context, TableType) -> Result(#(Context, TableType), String)

type GlobalTypeVisitor =
  fn(Context, GlobalType) -> Result(#(Context, GlobalType), String)

type ExternTypeVisitor =
  fn(Context, ExternType) -> Result(#(Context, ExternType), String)

type InstructionVisitor =
  fn(Context, Instruction) -> Result(#(Context, Instruction), String)

type DefTypeVisitor =
  fn(Context, DefType) -> Result(#(Context, DefType), String)

type LocalTypeVisitor =
  fn(Context, LocalType) -> Result(#(Context, LocalType), String)

type BlockTypeVisitor =
  fn(Context, BlockType) -> Result(#(Context, BlockType), String)

type TypeIDXVisitor =
  fn(Context, TypeIDX) -> Result(#(Context, TypeIDX), String)

pub type TypeVisitor {
  TypeVisitor(
    type_idx: Option(TypeIDXVisitor),
    num_type: Option(NumTypeVisitor),
    vec_type: Option(VecTypeVisitor),
    heap_type: Option(HeapTypeVisitor),
    abstract_heap_type: Option(AbstractHeapTypeVisitor),
    ref_type: Option(RefTypeVisitor),
    val_type: Option(ValTypeVisitor),
    block_type: Option(BlockTypeVisitor),
    result_type: Option(ResultTypeVisitor),
    result_types: Option(ResultTypesVisitor),
    func_type: Option(FuncTypeVisitor),
    struct_type: Option(StructTypeVisitor),
    array_type: Option(ArrayTypeVisitor),
    field_type: Option(FieldTypeVisitor),
    storage_type: Option(StorageTypeVisitor),
    packed_type: Option(PackedTypeVisitor),
    composite_type: Option(CompositeTypeVisitor),
    rec_type: Option(RecTypeVisitor),
    sub_type: Option(SubTypeVisitor),
    mem_type: Option(MemTypeVisitor),
    table_type: Option(TableTypeVisitor),
    global_type: Option(GlobalTypeVisitor),
    extern_type: Option(ExternTypeVisitor),
    instruction_type: Option(InstructionVisitor),
    def_type: Option(DefTypeVisitor),
    local_type: Option(LocalTypeVisitor),
  )
}

pub fn on_type_idx(v: TypeVisitor, vs: TypeIDXVisitor) {
  TypeVisitor(..v, type_idx: Some(vs))
}

pub fn on_num_type(v: TypeVisitor, vs: NumTypeVisitor) {
  TypeVisitor(..v, num_type: Some(vs))
}

pub fn on_vec_type(v: TypeVisitor, vs: VecTypeVisitor) {
  TypeVisitor(..v, vec_type: Some(vs))
}

pub fn on_heap_type(v: TypeVisitor, vs: HeapTypeVisitor) {
  TypeVisitor(..v, heap_type: Some(vs))
}

pub fn on_abstract_heap_type(v: TypeVisitor, vs: AbstractHeapTypeVisitor) {
  TypeVisitor(..v, abstract_heap_type: Some(vs))
}

pub fn on_ref_type(v: TypeVisitor, vs: RefTypeVisitor) {
  TypeVisitor(..v, ref_type: Some(vs))
}

pub fn on_val_type(v: TypeVisitor, vs: ValTypeVisitor) {
  TypeVisitor(..v, val_type: Some(vs))
}

pub fn on_result_type(v: TypeVisitor, vs: ResultTypeVisitor) {
  TypeVisitor(..v, result_type: Some(vs))
}

pub fn on_result_types(v: TypeVisitor, vs: ResultTypesVisitor) {
  TypeVisitor(..v, result_types: Some(vs))
}

pub fn on_func_type(v: TypeVisitor, vs: FuncTypeVisitor) {
  TypeVisitor(..v, func_type: Some(vs))
}

pub fn on_struct_type(v: TypeVisitor, vs: StructTypeVisitor) {
  TypeVisitor(..v, struct_type: Some(vs))
}

pub fn on_array_type(v: TypeVisitor, vs: ArrayTypeVisitor) {
  TypeVisitor(..v, array_type: Some(vs))
}

pub fn on_field_type(v: TypeVisitor, vs: FieldTypeVisitor) {
  TypeVisitor(..v, field_type: Some(vs))
}

pub fn on_storage_type(v: TypeVisitor, vs: StorageTypeVisitor) {
  TypeVisitor(..v, storage_type: Some(vs))
}

pub fn on_packed_type(v: TypeVisitor, vs: PackedTypeVisitor) {
  TypeVisitor(..v, packed_type: Some(vs))
}

pub fn on_composite_type(v: TypeVisitor, vs: CompositeTypeVisitor) {
  TypeVisitor(..v, composite_type: Some(vs))
}

pub fn on_rec_type(v: TypeVisitor, vs: RecTypeVisitor) {
  TypeVisitor(..v, rec_type: Some(vs))
}

pub fn on_sub_type(v: TypeVisitor, vs: SubTypeVisitor) {
  TypeVisitor(..v, sub_type: Some(vs))
}

pub fn on_mem_type(v: TypeVisitor, vs: MemTypeVisitor) {
  TypeVisitor(..v, mem_type: Some(vs))
}

pub fn on_table_type(v: TypeVisitor, vs: TableTypeVisitor) {
  TypeVisitor(..v, table_type: Some(vs))
}

pub fn on_global_type(v: TypeVisitor, vs: GlobalTypeVisitor) {
  TypeVisitor(..v, global_type: Some(vs))
}

pub fn on_extern_type(v: TypeVisitor, vs: ExternTypeVisitor) {
  TypeVisitor(..v, extern_type: Some(vs))
}

pub fn on_instruction(v: TypeVisitor, vs: InstructionVisitor) {
  TypeVisitor(..v, instruction_type: Some(vs))
}

pub fn on_def_type(v: TypeVisitor, vs: DefTypeVisitor) {
  TypeVisitor(..v, def_type: Some(vs))
}

pub fn on_local_type(v: TypeVisitor, vs: LocalTypeVisitor) {
  TypeVisitor(..v, local_type: Some(vs))
}

pub fn visitor_new() {
  TypeVisitor(
    type_idx: None,
    num_type: None,
    vec_type: None,
    heap_type: None,
    abstract_heap_type: None,
    ref_type: None,
    val_type: None,
    block_type: None,
    result_type: None,
    result_types: None,
    func_type: None,
    struct_type: None,
    array_type: None,
    field_type: None,
    storage_type: None,
    packed_type: None,
    composite_type: None,
    rec_type: None,
    sub_type: None,
    mem_type: None,
    table_type: None,
    global_type: None,
    extern_type: None,
    instruction_type: None,
    def_type: None,
    local_type: None,
  )
}
