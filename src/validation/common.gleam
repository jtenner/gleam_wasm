import gleam/option.{type Option}
import structure/types.{
  type AbstractHeapType, type ArrayType, type CompositeType, type Context,
  type DefType, type ExternType, type FieldType, type FuncType, type GlobalType,
  type HeapType, type InstructionType, type LocalType, type MemType,
  type NumType, type PackedType, type RecType, type RefType, type ResultType,
  type StorageType, type StructType, type SubType, type TableType, type ValType,
  type VecType, AnyHeapType, AnyRefType, ArrayHeapType, ArrayRefType, EqHeapType,
  EqRefType, ExternHeapType, ExternRefType, FuncHeapType, FuncRefType,
  HeapTypeRefType, I31HeapType, I31RefType, NoExternHeapType, NoExternRefType,
  NoFuncHeapType, NoFuncRefType, NoneHeapType, NoneRefType, StructHeapType,
  StructRefType,
}
import structure/values.{type U32, type Vec, Vec}

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

type NumTypeVisitor =
  Option(fn(Context, NumType) -> #(Context, NumType))

type VecTypeVisitor =
  Option(fn(Context, VecType) -> #(Context, VecType))

type HeapTypeVisitor =
  Option(fn(Context, HeapType) -> #(Context, HeapType))

type AbstractHeapTypeVisitor =
  Option(fn(Context, AbstractHeapType) -> #(Context, AbstractHeapType))

type RefTypeVisitor =
  Option(fn(Context, RefType) -> #(Context, RefType))

type ValTypeVisitor =
  Option(fn(Context, ValType) -> #(Context, ValType))

type ResultTypeVisitor =
  Option(fn(Context, ResultType) -> #(Context, ResultType))

type FuncTypeVisitor =
  Option(fn(Context, FuncType) -> #(Context, FuncType))

type StructTypeVisitor =
  Option(fn(Context, StructType) -> #(Context, StructType))

type ArrayTypeVisitor =
  Option(fn(Context, ArrayType) -> #(Context, ArrayType))

type FieldTypeVisitor =
  Option(fn(Context, FieldType) -> #(Context, FieldType))

type StorageTypeVisitor =
  Option(fn(Context, StorageType) -> #(Context, StorageType))

type PackedTypeVisitor =
  Option(fn(Context, PackedType) -> #(Context, PackedType))

type CompositeTypeVisitor =
  Option(fn(Context, CompositeType) -> #(Context, CompositeType))

type RecTypeVisitor =
  Option(fn(Context, RecType) -> #(Context, RecType))

type SubTypeVisitor =
  Option(fn(Context, SubType) -> #(Context, SubType))

type MemTypeVisitor =
  Option(fn(Context, MemType) -> #(Context, MemType))

type TableTypeVisitor =
  Option(fn(Context, TableType) -> #(Context, TableType))

type GlobalTypeVisitor =
  Option(fn(Context, GlobalType) -> #(Context, GlobalType))

type ExternTypeVisitor =
  Option(fn(Context, ExternType) -> #(Context, ExternType))

type InstructionTypeVisitor =
  Option(fn(Context, InstructionType) -> #(Context, InstructionType))

type DefTypeVisitor =
  Option(fn(Context, DefType) -> #(Context, DefType))

type LocalTypeVisitor =
  Option(fn(Context, LocalType) -> #(Context, LocalType))

pub type TypeVisitor {
  TypeVisitor(
    num_type: NumTypeVisitor,
    vec_type: VecTypeVisitor,
    heap_type: HeapTypeVisitor,
    abstract_heap_type: AbstractHeapTypeVisitor,
    ref_type: RefTypeVisitor,
    val_type: ValTypeVisitor,
    result_type: ResultTypeVisitor,
    func_type: FuncTypeVisitor,
    struct_type: StructTypeVisitor,
    array_type: ArrayTypeVisitor,
    field_type: FieldTypeVisitor,
    storage_type: StorageTypeVisitor,
    packed_type: PackedTypeVisitor,
    composite_type: CompositeTypeVisitor,
    rec_type: RecTypeVisitor,
    sub_type: SubTypeVisitor,
    mem_type: MemTypeVisitor,
    table_type: TableTypeVisitor,
    global_type: GlobalTypeVisitor,
    extern_type: ExternTypeVisitor,
    instruction_type: InstructionTypeVisitor,
    def_type: DefTypeVisitor,
    local_type: LocalTypeVisitor,
  )
}

pub fn visitor_new() {
  TypeVisitor(
    num_type: None,
    vec_type: None,
    heap_type: None,
    abstract_heap_type: None,
    ref_type: None,
    val_type: None,
    result_type: None,
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

fn visit_num_type(
  ctx: Context,
  ty: NumType,
  cb: fn(NumType) -> NumType,
) -> #(Context, NumType) {
  todo
}

fn visit_vec_type(
  ctx: Context,
  ty: VecType,
  cb: fn(VecType) -> VecType,
) -> #(Context, VecType) {
  todo
}

fn visit_heap_type(
  ctx: Context,
  ty: HeapType,
  cb: fn(HeapType) -> HeapType,
) -> #(Context, HeapType) {
  todo
}

fn visit_abstract_heap_type(
  ctx: Context,
  ty: AbstractHeapType,
  cb: fn(AbstractHeapType) -> AbstractHeapType,
) -> #(Context, AbstractHeapType) {
  todo
}

fn visit_ref_type(
  ctx: Context,
  ty: RefType,
  cb: fn(RefType) -> RefType,
) -> #(Context, RefType) {
  todo
}

fn visit_val_type(
  ctx: Context,
  ty: ValType,
  cb: fn(ValType) -> ValType,
) -> #(Context, ValType) {
  todo
}

fn visit_result_type(
  ctx: Context,
  ty: ResultType,
  cb: fn(ResultType) -> ResultType,
) -> #(Context, ResultType) {
  todo
}

fn visit_func_type(
  ctx: Context,
  ty: FuncType,
  cb: fn(FuncType) -> FuncType,
) -> #(Context, FuncType) {
  todo
}

fn visit_struct_type(
  ctx: Context,
  ty: StructType,
  cb: fn(StructType) -> StructType,
) -> #(Context, StructType) {
  todo
}

fn visit_array_type(
  ctx: Context,
  ty: ArrayType,
  cb: fn(ArrayType) -> ArrayType,
) -> #(Context, ArrayType) {
  todo
}

fn visit_field_type(
  ctx: Context,
  ty: FieldType,
  cb: fn(FieldType) -> FieldType,
) -> #(Context, FieldType) {
  todo
}

fn visit_storage_type(
  ctx: Context,
  ty: StorageType,
  cb: fn(StorageType) -> StorageType,
) -> #(Context, StorageType) {
  todo
}

fn visit_packed_type(
  ctx: Context,
  ty: PackedType,
  cb: fn(PackedType) -> PackedType,
) -> #(Context, PackedType) {
  todo
}

fn visit_composite_type(
  ctx: Context,
  ty: CompositeType,
  cb: fn(CompositeType) -> CompositeType,
) -> #(Context, CompositeType) {
  todo
}

fn visit_rec_type(
  ctx: Context,
  ty: RecType,
  cb: fn(RecType) -> RecType,
) -> #(Context, RecType) {
  todo
}

fn visit_sub_type(
  ctx: Context,
  ty: SubType,
  cb: fn(SubType) -> SubType,
) -> #(Context, SubType) {
  todo
}

fn visit_mem_type(
  ctx: Context,
  ty: MemType,
  cb: fn(MemType) -> MemType,
) -> #(Context, MemType) {
  todo
}

fn visit_table_type(
  ctx: Context,
  ty: TableType,
  cb: fn(TableType) -> TableType,
) -> #(Context, TableType) {
  todo
}

fn visit_global_type(
  ctx: Context,
  ty: GlobalType,
  cb: fn(GlobalType) -> GlobalType,
) -> #(Context, GlobalType) {
  todo
}

fn visit_extern_type(
  ctx: Context,
  ty: ExternType,
  cb: fn(ExternType) -> ExternType,
) -> #(Context, ExternType) {
  todo
}

fn visit_instruction_type(
  ctx: Context,
  ty: InstructionType,
  cb: fn(InstructionType) -> InstructionType,
) -> #(Context, InstructionType) {
  todo
}

fn visit_def_type(
  ctx: Context,
  ty: DefType,
  cb: fn(DefType) -> DefType,
) -> #(Context, DefType) {
  todo
}

fn visit_local_type(
  ctx: Context,
  ty: LocalType,
  cb: fn(LocalType) -> LocalType,
) -> #(Context, LocalType) {
  todo
}
