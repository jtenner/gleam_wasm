import gleam/bit_array
import gleam/bytes_builder.{type BytesBuilder}
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import internal/binary/values.{decode_u32, encode_u32}
import internal/finger_tree.{type FingerTree}
import internal/structure/numbers.{u32, unwrap_u32}

pub fn encode_vec(
  builder: BytesBuilder,
  items: FingerTree(u),
  encode_fn: fn(BytesBuilder, u) -> Result(BytesBuilder, String),
) {
  use size <- result.try(finger_tree.size(items) |> u32)
  builder
  |> encode_u32(size)
  |> do_encode_vec(items, encode_fn)
}

fn do_encode_vec(
  builder: BytesBuilder,
  items: FingerTree(u),
  encode_fn: fn(BytesBuilder, u) -> Result(BytesBuilder, String),
) {
  case items |> finger_tree.shift {
    Error(_) -> Ok(builder)
    Ok(#(item, rest)) -> {
      use builder <- result.try(builder |> encode_fn(item))
      builder |> do_encode_vec(rest, encode_fn)
    }
  }
}

pub fn decode_vec(
  bits: BitArray,
  decode_fn: fn(BitArray) -> Result(#(u, BitArray), String),
) {
  use #(size, rest) <- result.try(decode_u32(bits))
  do_decode_vec(rest, size |> unwrap_u32, finger_tree.new(), decode_fn)
}

fn do_decode_vec(
  bits: BitArray,
  size: Int,
  acc: FingerTree(u),
  decode_fn: fn(BitArray) -> Result(#(u, BitArray), String),
) {
  case size {
    0 -> Ok(#(acc, bits))
    _ -> {
      use #(val, rest) <- result.try(decode_fn(bits))
      do_decode_vec(rest, size - 1, acc |> finger_tree.push(val), decode_fn)
    }
  }
}

pub fn encode_option(
  builder: BytesBuilder,
  val: Option(u),
  encode_fn: fn(BytesBuilder, u) -> Result(BytesBuilder, String),
) {
  case val {
    Some(u) -> encode_fn(builder, u)
    None -> Ok(builder)
  }
}

pub fn wrap_fold(f: fn(u, w) -> Result(u, v)) {
  fn(acc: Result(u, v), next: w) {
    case acc {
      Ok(u) -> f(u, next)
      Error(v) -> Error(v)
    }
  }
}

pub fn encode_string(builder: BytesBuilder, string: String) {
  let size = string |> string.byte_size
  use size <- result.map(size |> u32)
  builder
  |> encode_u32(size)
  |> bytes_builder.append_string(string)
}

pub fn decode_bytes(bits: BitArray, size: Int) {
  let size = size * 8
  case bits {
    <<bits:bits-size(size), rest:bits>> -> Ok(#(bits, rest))
    _ -> Error("Byte length mismatch")
  }
}

pub fn decode_string(val: BitArray) -> Result(#(String, BitArray), String) {
  use #(byte_length, rest) <- result.try(decode_u32(val))
  let byte_length = byte_length |> unwrap_u32
  let byte_length = byte_length * 8
  case rest {
    <<str:bits-size(byte_length), rest:bits>> -> {
      case str |> bit_array.to_string {
        Ok(str) -> Ok(#(str, rest))
        Error(_) -> Error("Invalid utf8")
      }
    }
    _ -> Error("Byte length mismatch")
  }
}

pub fn decode_section(
  bits: BitArray,
  section_id: Int,
  decode_section_fn: fn(BitArray) -> Result(#(u, BitArray), String),
) {
  case bits {
    <<id, rest:bits>> if id == section_id -> {
      use #(size, rest) <- result.try(decode_u32(rest))
      let size = size |> unwrap_u32
      use #(section_bytes, rest) <- result.try(decode_bytes(rest, size))
      use #(section, left) <- result.try(decode_section_fn(section_bytes))

      case left {
        <<>> -> Ok(#(Some(section), rest))
        _ -> Error("Invalid section")
      }
    }
    _ -> Ok(#(None, bits))
  }
}

pub fn expect_decode_byte(bits: BitArray, val: Int) {
  case bits {
    <<first, rest:bits>> if first == val -> Ok(#(first, rest))
    _ -> Error("Invalid byte")
  }
}
