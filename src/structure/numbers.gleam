import gleam/bit_array
import ieee_float.{type IEEEFloat}
import structure/common.{between}

pub opaque type I8 {
  I8(val: Int)
}

pub opaque type U8 {
  U8(val: Int)
}

pub opaque type I16 {
  I16(val: Int)
}

pub opaque type U16 {
  U16(val: Int)
}

pub opaque type I32 {
  I32(val: Int)
}

pub opaque type U32 {
  U32(val: Int)
}

pub opaque type I64 {
  I64(val: Int)
}

pub opaque type U64 {
  U64(val: Int)
}

pub opaque type F32 {
  F32(val: IEEEFloat)
}

pub opaque type F64 {
  F64(val: IEEEFloat)
}

pub opaque type S33 {
  S33(val: Int)
}

/// i8_range = #(-128, 127)
const i8_range = #(-128, 127)

/// u8_range = #(0, 255)
const u8_range = #(0, 255)

/// i16_range = #(-32_768, 32_767)
const i16_range = #(-32_768, 32_767)

/// u16_range = #(0, 65_535)
const u16_range = #(0, 65_535)

/// i32_range = #(-2_147_483_648, 2_147_483_647)
const i32_range = #(-2_147_483_648, 2_147_483_647)

/// u32_range = #(0, 4_294_967_295)
const u32_range = #(0, 4_294_967_295)

/// i64_range = #(-9_223_372_036_854_775_808, 9_223_372_036_854_775_807)
const i64_range = #(-9_223_372_036_854_775_808, 9_223_372_036_854_775_807)

/// u64_range = #(0, 18_446_744_073_709_551_615)
const u64_range = #(0, 18_446_744_073_709_551_615)

/// Note that an S33 is a signed 33-bit integer, but in WASM it will always be positive
const s33_range = #(0, 8_589_934_591)

pub fn i8(val: Int) -> Result(I8, String) {
  case val |> between(i8_range) {
    True -> Ok(I8(val))
    False -> Error("Integer out of range for I8")
  }
}

pub fn u8(val: Int) -> Result(U8, String) {
  case val |> between(u8_range) {
    True -> Ok(U8(val))
    False -> Error("Integer out of range for U8")
  }
}

pub fn i16(val: Int) -> Result(I16, String) {
  case val |> between(i16_range) {
    True -> Ok(I16(val))
    False -> Error("Integer out of range for I16")
  }
}

pub fn u16(val: Int) -> Result(U16, String) {
  case val |> between(u16_range) {
    True -> Ok(U16(val))
    False -> Error("Integer out of range for U16")
  }
}

pub fn i32(val: Int) -> Result(I32, String) {
  case val |> between(i32_range) {
    True -> Ok(I32(val))
    False -> Error("Integer out of range for I32")
  }
}

pub fn u32(val: Int) -> Result(U32, String) {
  case val |> between(u32_range) {
    True -> Ok(U32(val))
    False -> Error("Integer out of range for U32")
  }
}

pub fn i64(val: Int) -> Result(I64, String) {
  case val |> between(i64_range) {
    True -> Ok(I64(val))
    False -> Error("Integer out of range for I64")
  }
}

pub fn u64(val: Int) -> Result(U64, String) {
  case val |> between(u64_range) {
    True -> Ok(U64(val))
    False -> Error("Integer out of range for U64")
  }
}

pub fn s33(val: Int) -> Result(S33, String) {
  case val |> between(s33_range) {
    True -> Ok(S33(val))
    False -> Error("Integer out of range for S33")
  }
}

pub fn unwrap_i16(val: I16) {
  val.val
}

pub fn unwrap_i32(val: I32) {
  val.val
}

pub fn unwrap_i64(val: I64) {
  val.val
}

pub fn unwrap_i8(val: I8) {
  val.val
}

pub fn unwrap_s33(val: S33) {
  val.val
}

pub fn unwrap_u16(val: U16) {
  val.val
}

pub fn unwrap_u32(val: U32) {
  val.val
}

pub fn unwrap_u64(val: U64) {
  val.val
}

pub fn unwrap_u8(val: U8) {
  val.val
}

pub opaque type V128Value {
  V128Value(val: BitArray)
}

pub fn v128(val: BitArray) -> Result(V128Value, String) {
  case val |> bit_array.byte_size {
    16 -> Ok(V128Value(val))
    _ -> Error("V128 value must be 16 bytes long")
  }
}
