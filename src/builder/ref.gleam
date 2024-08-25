import builder/expression_builder.{type ExpressionBuilder}
import internal/structure/types.{type FuncIDX, type HeapType, RefFunc, RefNull}

pub fn as_non_null(builder: ExpressionBuilder) {
  todo
}

pub fn cast(builder: ExpressionBuilder) {
  todo
}

pub fn eq(builder: ExpressionBuilder) {
  todo
}

pub fn func(builder: ExpressionBuilder, idx: FuncIDX) {
  builder
  |> expression_builder.push(RefFunc(idx))
}

pub fn i31(builder: ExpressionBuilder) {
  todo
}

pub fn is_null(builder: ExpressionBuilder) {
  todo
}

pub fn null(builder: ExpressionBuilder, ht: HeapType) {
  builder
  |> expression_builder.push(RefNull(ht))
}

pub fn test_(builder: ExpressionBuilder) {
  todo
}
