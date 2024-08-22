import builder/expression_builder.{type ExpressionBuilder}
import internal/structure/numbers.{type I32}
import internal/structure/types.{I32Add, I32Const}

pub fn get_s(builder: ExpressionBuilder) {
  todo
}

pub fn get_u(builder: ExpressionBuilder) {
  todo
}

pub fn add(builder: ExpressionBuilder) {
  builder
  |> expression_builder.push(I32Add)
}

pub fn and(builder: ExpressionBuilder) {
  todo
}

pub fn clz(builder: ExpressionBuilder) {
  todo
}

pub fn const_(builder: ExpressionBuilder, val: I32) {
  builder
  |> expression_builder.push(I32Const(val))
}

pub fn ctz(builder: ExpressionBuilder) {
  todo
}

pub fn div_s(builder: ExpressionBuilder) {
  todo
}

pub fn div_u(builder: ExpressionBuilder) {
  todo
}

pub fn eq(builder: ExpressionBuilder) {
  todo
}

pub fn eqz(builder: ExpressionBuilder) {
  todo
}

pub fn extend16_s(builder: ExpressionBuilder) {
  todo
}

pub fn extend8_s(builder: ExpressionBuilder) {
  todo
}

pub fn ge_s(builder: ExpressionBuilder) {
  todo
}

pub fn ge_u(builder: ExpressionBuilder) {
  todo
}

pub fn gt_s(builder: ExpressionBuilder) {
  todo
}

pub fn gt_u(builder: ExpressionBuilder) {
  todo
}

pub fn le_s(builder: ExpressionBuilder) {
  todo
}

pub fn le_u(builder: ExpressionBuilder) {
  todo
}

pub fn load(builder: ExpressionBuilder) {
  todo
}

pub fn load16_s(builder: ExpressionBuilder) {
  todo
}

pub fn load16_u(builder: ExpressionBuilder) {
  todo
}

pub fn load8_s(builder: ExpressionBuilder) {
  todo
}

pub fn load8_u(builder: ExpressionBuilder) {
  todo
}

pub fn lt_s(builder: ExpressionBuilder) {
  todo
}

pub fn lt_u(builder: ExpressionBuilder) {
  todo
}

pub fn mul(builder: ExpressionBuilder) {
  todo
}

pub fn ne(builder: ExpressionBuilder) {
  todo
}

pub fn or(builder: ExpressionBuilder) {
  todo
}

pub fn popcnt(builder: ExpressionBuilder) {
  todo
}

pub fn reinterpret_f32(builder: ExpressionBuilder) {
  todo
}

pub fn reinterpret_f64(builder: ExpressionBuilder) {
  todo
}

pub fn rem_s(builder: ExpressionBuilder) {
  todo
}

pub fn rem_u(builder: ExpressionBuilder) {
  todo
}

pub fn rotl(builder: ExpressionBuilder) {
  todo
}

pub fn rotr(builder: ExpressionBuilder) {
  todo
}

pub fn shl(builder: ExpressionBuilder) {
  todo
}

pub fn shr_s(builder: ExpressionBuilder) {
  todo
}

pub fn shr_u(builder: ExpressionBuilder) {
  todo
}

pub fn store(builder: ExpressionBuilder) {
  todo
}

pub fn store16_s(builder: ExpressionBuilder) {
  todo
}

pub fn store16_u(builder: ExpressionBuilder) {
  todo
}

pub fn store8_s(builder: ExpressionBuilder) {
  todo
}

pub fn store8_u(builder: ExpressionBuilder) {
  todo
}

pub fn sub(builder: ExpressionBuilder) {
  todo
}

pub fn trunc_f32_s(builder: ExpressionBuilder) {
  todo
}

pub fn trunc_f32_u(builder: ExpressionBuilder) {
  todo
}

pub fn trunc_f64_s(builder: ExpressionBuilder) {
  todo
}

pub fn trunc_f64_u(builder: ExpressionBuilder) {
  todo
}

pub fn trunc_sat_f32_s(builder: ExpressionBuilder) {
  todo
}

pub fn trunc_sat_f32_u(builder: ExpressionBuilder) {
  todo
}

pub fn trunc_sat_f64_s(builder: ExpressionBuilder) {
  todo
}

pub fn trunc_sat_f64_u(builder: ExpressionBuilder) {
  todo
}

pub fn wrap_i64(builder: ExpressionBuilder) {
  todo
}

pub fn xor(builder: ExpressionBuilder) {
  todo
}
