pub fn between(val: Int, range: #(Int, Int)) {
  val >= range.0 && val <= range.1
}

pub fn or_panic(_: a) {
  panic
}
