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
