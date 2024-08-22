import builder/expression_builder.{type ExpressionBuilder}
import internal/structure/types.{type HeapType, RefNull}

pub fn as_non_null(func: ExpressionBuilder) {
  todo
}

pub fn cast(func: ExpressionBuilder) {
  todo
}

pub fn eq(func: ExpressionBuilder) {
  todo
}

pub fn func(func: ExpressionBuilder) {
  todo
}

pub fn i31(func: ExpressionBuilder) {
  todo
}

pub fn is_null(func: ExpressionBuilder) {
  todo
}

pub fn null(builder: ExpressionBuilder, ht: HeapType) {
  builder
  |> expression_builder.push(RefNull(ht))
}

pub fn test_(func: ExpressionBuilder) {
  todo
}
