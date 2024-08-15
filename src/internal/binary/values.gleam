import gleam/bytes_builder.{type BytesBuilder}
import gleam/int
import gleam/list
import gleam/result
import ieee_float
import internal/finger_tree.{type FingerTree}
import internal/structure/common
import internal/structure/numbers.{
  type F32, type I16, type I32, type I64, type I8, type S33, type U16, type U32,
  type U64, type U8, f32, f64, i16, i32, i64, i8, s33, u16, u32, u64, u8,
  unwrap_i16, unwrap_i32, unwrap_i64, unwrap_i8, unwrap_s33, unwrap_u16,
  unwrap_u32, unwrap_u64, unwrap_u8,
}

pub fn decode_i8(bits: BitArray) {
  do_decode_signed(bits, i8, 8)
}

pub fn decode_u8(bits: BitArray) {
  do_decode_unsigned(bits, u8, 8)
}

pub fn decode_i16(bits: BitArray) {
  do_decode_signed(bits, i16, 16)
}

pub fn decode_u16(bits: BitArray) {
  do_decode_unsigned(bits, u16, 16)
}

pub fn decode_i32(bits: BitArray) {
  do_decode_signed(bits, i32, 32)
}

pub fn decode_u32(bits: BitArray) {
  do_decode_unsigned(bits, u32, 32)
}

pub fn decode_i64(bits: BitArray) {
  do_decode_signed(bits, i64, 64)
}

pub fn decode_u64(bits: BitArray) {
  do_decode_unsigned(bits, u64, 64)
}

pub fn decode_s33(bits: BitArray) {
  do_decode_signed(bits, s33, 33)
}

fn finish_decode(
  val: Result(u, String),
  rest: BitArray,
) -> Result(#(u, BitArray), String) {
  case val {
    Ok(val) -> Ok(#(val, rest))
    Error(err) -> Error(err)
  }
}

fn do_decode_signed(
  val: BitArray,
  make: fn(Int) -> Result(i, String),
  bit_count: Int,
) -> Result(#(i, BitArray), String) {
  case val {
    <<
      0b1:1,
      val9:7,
      0b1:1,
      val8:7,
      0b1:1,
      val7:7,
      0b1:1,
      val6:7,
      0b1:1,
      val5:7,
      0b1:1,
      val4:7,
      0b1:1,
      val3:7,
      0b1:1,
      val2:7,
      0b1:1,
      val1:7,
      0b0:1,
      val0:7-signed,
      rest:bits,
    >>
      if bit_count > 63
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val3)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val4)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val5)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val6)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val7)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val8)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val9),
      )
      |> finish_decode(rest)
    <<
      0b1:1,
      val8:7,
      0b1:1,
      val7:7,
      0b1:1,
      val6:7,
      0b1:1,
      val5:7,
      0b1:1,
      val4:7,
      0b1:1,
      val3:7,
      0b1:1,
      val2:7,
      0b1:1,
      val1:7,
      0b0:1,
      val0:7-signed,
      rest:bits,
    >>
      if bit_count > 56
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val3)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val4)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val5)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val6)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val7)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val8),
      )
      |> finish_decode(rest)
    <<
      0b1:1,
      val7:7,
      0b1:1,
      val6:7,
      0b1:1,
      val5:7,
      0b1:1,
      val4:7,
      0b1:1,
      val3:7,
      0b1:1,
      val2:7,
      0b1:1,
      val1:7,
      0b0:1,
      val0:7-signed,
      rest:bits,
    >>
      if bit_count > 49
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val3)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val4)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val5)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val6)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val7),
      )
      |> finish_decode(rest)
    <<
      0b1:1,
      val6:7,
      0b1:1,
      val5:7,
      0b1:1,
      val4:7,
      0b1:1,
      val3:7,
      0b1:1,
      val2:7,
      0b1:1,
      val1:7,
      0b0:1,
      val0:7-signed,
      rest:bits,
    >>
      if bit_count > 42
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val3)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val4)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val5)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val6),
      )
      |> finish_decode(rest)
    <<
      0b1:1,
      val5:7,
      0b1:1,
      val4:7,
      0b1:1,
      val3:7,
      0b1:1,
      val2:7,
      0b1:1,
      val1:7,
      0b0:1,
      val0:7-signed,
      rest:bits,
    >>
      if bit_count > 35
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val3)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val4)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val5),
      )
      |> finish_decode(rest)
    <<
      0b1:1,
      val4:7,
      0b1:1,
      val3:7,
      0b1:1,
      val2:7,
      0b1:1,
      val1:7,
      0b0:1,
      val0:7-signed,
      rest:bits,
    >>
      if bit_count > 28
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val3)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val4),
      )
      |> finish_decode(rest)
    <<
      0b1:1,
      val3:7,
      0b1:1,
      val2:7,
      0b1:1,
      val1:7,
      0b0:1,
      val0:7-signed,
      rest:bits,
    >>
      if bit_count > 21
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val3),
      )
      |> finish_decode(rest)
    <<0b1:1, val2:7, 0b1:1, val1:7, 0b0:1, val0:7-signed, rest:bits>>
      if bit_count > 14
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2),
      )
      |> finish_decode(rest)
    <<0b1:1, val1:7, 0b0:1, val0:7-signed, rest:bits>> if bit_count > 7 ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1),
      )
      |> finish_decode(rest)
    <<0b0:1, val0:7-signed, rest:bits>> if bit_count > 0 ->
      make(val0) |> finish_decode(rest)
    _ -> Error("Invalid number format")
  }
}

fn do_decode_unsigned(
  val: BitArray,
  make: fn(Int) -> Result(u, String),
  bit_count: Int,
) -> Result(#(u, BitArray), String) {
  case val {
    <<
      0b1:1,
      val9:7,
      0b1:1,
      val8:7,
      0b1:1,
      val7:7,
      0b1:1,
      val6:7,
      0b1:1,
      val5:7,
      0b1:1,
      val4:7,
      0b1:1,
      val3:7,
      0b1:1,
      val2:7,
      0b1:1,
      val1:7,
      0b0:1,
      val0:7,
      rest:bits,
    >>
      if bit_count > 63
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val3)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val4)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val5)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val6)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val7)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val8)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val9),
      )
      |> finish_decode(rest)
    <<
      0b1:1,
      val8:7,
      0b1:1,
      val7:7,
      0b1:1,
      val6:7,
      0b1:1,
      val5:7,
      0b1:1,
      val4:7,
      0b1:1,
      val3:7,
      0b1:1,
      val2:7,
      0b1:1,
      val1:7,
      0b0:1,
      val0:7,
      rest:bits,
    >>
      if bit_count > 56
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val3)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val4)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val5)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val6)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val7)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val8),
      )
      |> finish_decode(rest)
    <<
      0b1:1,
      val7:7,
      0b1:1,
      val6:7,
      0b1:1,
      val5:7,
      0b1:1,
      val4:7,
      0b1:1,
      val3:7,
      0b1:1,
      val2:7,
      0b1:1,
      val1:7,
      0b0:1,
      val0:7,
      rest:bits,
    >>
      if bit_count > 49
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val3)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val4)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val5)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val6)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val7),
      )
      |> finish_decode(rest)
    <<
      0b1:1,
      val6:7,
      0b1:1,
      val5:7,
      0b1:1,
      val4:7,
      0b1:1,
      val3:7,
      0b1:1,
      val2:7,
      0b1:1,
      val1:7,
      0b0:1,
      val0:7,
      rest:bits,
    >>
      if bit_count > 42
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val3)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val4)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val5)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val6),
      )
      |> finish_decode(rest)
    <<
      0b1:1,
      val5:7,
      0b1:1,
      val4:7,
      0b1:1,
      val3:7,
      0b1:1,
      val2:7,
      0b1:1,
      val1:7,
      0b0:1,
      val0:7,
      rest:bits,
    >>
      if bit_count > 35
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val3)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val4)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val5),
      )
      |> finish_decode(rest)
    <<
      0b1:1,
      val4:7,
      0b1:1,
      val3:7,
      0b1:1,
      val2:7,
      0b1:1,
      val1:7,
      0b0:1,
      val0:7,
      rest:bits,
    >>
      if bit_count > 28
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val3)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val4),
      )
      |> finish_decode(rest)
    <<0b1:1, val3:7, 0b1:1, val2:7, 0b1:1, val1:7, 0b0:1, val0:7, rest:bits>>
      if bit_count > 21
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val3),
      )
      |> finish_decode(rest)
    <<0b1:1, val2:7, 0b1:1, val1:7, 0b0:1, val0:7, rest:bits>>
      if bit_count > 14
    ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1)
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val2),
      )
      |> finish_decode(rest)
    <<0b1:1, val1:7, 0b0:1, val0:7, rest:bits>> if bit_count > 7 ->
      make(
        val0
        |> int.bitwise_shift_left(7)
        |> int.bitwise_or(val1),
      )
      |> finish_decode(rest)
    <<0b0:1, val0:7, rest:bits>> if bit_count > 0 ->
      make(val0) |> finish_decode(rest)
    _ -> Error("Invalid number format")
  }
}

fn do_encode_unsigned(builder: BytesBuilder, val: Int) {
  case val {
    val if val > 127 ->
      builder
      |> bytes_builder.append(<<0b1:1, val:7>>)
      |> do_encode_unsigned(val |> int.bitwise_shift_right(7))
    val ->
      builder
      |> bytes_builder.append(<<0b0:1, val:7>>)
  }
}

fn do_encode_signed(builder: BytesBuilder, val: Int) {
  case val {
    val if val >= 63 ->
      builder
      |> bytes_builder.append(<<0b1:1, val:7>>)
      |> do_encode_signed(val |> int.bitwise_shift_right(7))
    val if val < -64 ->
      builder
      |> bytes_builder.append(<<0b1:1, val:7>>)
      |> do_encode_signed(val |> int.bitwise_shift_right(7))
    val ->
      builder
      |> bytes_builder.append(<<0b0:1, val:7>>)
  }
}

pub fn encode_u16(builder: BytesBuilder, val: U16) {
  do_encode_unsigned(builder, val |> unwrap_u16)
}

pub fn encode_u32(builder: BytesBuilder, val: U32) {
  do_encode_unsigned(builder, val |> unwrap_u32)
}

pub fn encode_u64(builder: BytesBuilder, val: U64) {
  do_encode_unsigned(builder, val |> unwrap_u64)
}

pub fn encode_u8(builder: BytesBuilder, val: U8) {
  do_encode_unsigned(builder, val |> unwrap_u8)
}

pub fn encode_i16(builder: BytesBuilder, val: I16) {
  do_encode_signed(builder, val |> unwrap_i16)
}

pub fn encode_i32(builder: BytesBuilder, val: I32) {
  do_encode_signed(builder, val |> unwrap_i32)
}

pub fn encode_i64(builder: BytesBuilder, val: I64) {
  do_encode_signed(builder, val |> unwrap_i64)
}

pub fn encode_i8(builder: BytesBuilder, val: I8) {
  do_encode_signed(builder, val |> unwrap_i8)
}

pub fn encode_s33(builder: BytesBuilder, val: S33) {
  do_encode_signed(builder, val |> unwrap_s33)
}

pub fn decode_string(val: BitArray) -> Result(List(Int), String) {
  use #(byte_length, rest) <- result.try(decode_u32(val))
  do_decode_string(rest, byte_length |> unwrap_u32, finger_tree.new())
}

fn do_decode_string(
  rest: BitArray,
  rest_length: Int,
  acc: FingerTree(Int),
) -> Result(List(Int), String) {
  case rest {
    <<b, rest:bits>> if rest_length >= 1 && b < 0x80 ->
      do_decode_string(rest, rest_length - 1, acc |> finger_tree.push(b))
    <<b1, b2, rest:bits>> if rest_length >= 2 && b2 < 0xC0 -> {
      let c = 0x40 * { b1 - 0xC0 } + b2 - 0x80
      case c |> common.between(#(0x80, 0x800 - 1)) {
        True ->
          do_decode_string(rest, rest_length - 2, acc |> finger_tree.push(c))
        False -> Error("Invalid UTF-8 sequence")
      }
    }
    <<b1, b2, b3, rest:bits>> if rest_length >= 3 && b3 < 0xC0 && b2 < 0xC0 -> {
      let c = 0x1000 * { b1 - 0xE0 } + 0x40 * { b2 - 0x80 } + b3 - 0x80
      case
        c |> common.between(#(0x800, 0xD800 - 1)),
        c |> common.between(#(0xE000, 0x10000 - 1))
      {
        True, _ | _, True ->
          do_decode_string(rest, rest_length - 3, acc |> finger_tree.push(c))
        _, _ -> Error("Invalid UTF-8 sequence")
      }
    }
    <<b1, b2, b3, b4, rest:bits>>
      if rest_length >= 4 && b4 < 0xC0 && b3 < 0xC0 && b2 < 0xC0
    -> {
      let c =
        0x40000
        * { b1 - 0xF0 }
        + 0x1000
        * { b2 - 0x80 }
        + 0x40
        * { b3 - 0x80 }
        + b4
        - 0x80

      case c |> common.between(#(0x10000, 0x110000 - 1)) {
        True ->
          do_decode_string(rest, rest_length - 4, acc |> finger_tree.push(c))
        False -> Error("Invalid UTF-8 sequence")
      }
    }
    _ if rest_length == 0 -> Ok(acc |> finger_tree.to_list)
    _ -> Error("Invalid UTF-8 sequence")
  }
}
