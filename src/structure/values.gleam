import ieee_float.{type IEEEFloat}

pub type Vec(t) {
  Vec(count: Int, elements: List(t))
}

pub type I32 {
  I32(int: Int)
}

pub type I64 {
  I64(int: Int)
}

pub type U32 {
  U32(int: Int)
}

pub type U64 {
  U64(int: Int)
}

pub type I16 {
  I16(int: Int)
}

pub type I8 {
  I8(int: Int)
}

pub type S33 {
  S33(int: Int)
}

pub type V128Value {
  V128Value(bits: BitArray)
}

pub type F32 {
  F32(float: IEEEFloat)
}

pub type F64 {
  F64(float: IEEEFloat)
}
