import builder/expression_builder.{type ExpressionBuilder}
import internal/structure/types.{
  type FuncIDX, type HeapType, type RefType, RefAsNonNull, RefCast, RefEq,
  RefFunc, RefI31, RefIsNull, RefNull, RefTest,
}

pub fn as_non_null(builder: ExpressionBuilder) {
  builder
  |> expression_builder.push(RefAsNonNull)
}

pub fn cast(builder: ExpressionBuilder, ref_type: RefType) {
  builder
  |> expression_builder.push(RefCast(ref_type))
}

pub fn eq(builder: ExpressionBuilder) {
  builder
  |> expression_builder.push(RefEq)
}

pub fn func(builder: ExpressionBuilder, idx: FuncIDX) {
  builder
  |> expression_builder.push(RefFunc(idx))
}

pub fn i31(builder: ExpressionBuilder) {
  builder
  |> expression_builder.push(RefI31)
}

pub fn is_null(builder: ExpressionBuilder) {
  builder
  |> expression_builder.push(RefIsNull)
}

pub fn null(builder: ExpressionBuilder, ht: HeapType) {
  builder
  |> expression_builder.push(RefNull(ht))
}

pub fn test_(builder: ExpressionBuilder, ref_type: RefType) {
  builder
  |> expression_builder.push(RefTest(ref_type))
}
