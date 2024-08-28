import gleam/bytes_builder
import gleam/io
import gleam/result
import internal/binary/common
import internal/binary/modules
import internal/binary/values
import internal/structure/numbers
import simplifile

fn try_section(bits: BitArray) {
  use #(section_id, rest) <- result.try(values.decode_u32(bits))
  use #(section_size, rest) <- result.try(values.decode_u32(rest))
  let section_size = section_size |> numbers.unwrap_u32
  use #(section_bits, rest) <- result.try(common.decode_bytes(
    rest,
    section_size,
  ))
  let section_id = section_id |> numbers.unwrap_u32
  io.debug(#("section", section_id, section_size))
  case section_id {
    0x0A -> {
      // code section. dump it
      use #(code_vec, _) <- result.try(common.decode_vec(
        section_bits,
        modules.decode_code_segment,
      ))
      use builder <- result.map(common.encode_vec(
        bytes_builder.new(),
        code_vec,
        modules.encode_code_segment,
      ))
      let actual_bits = builder |> bytes_builder.to_bit_array

      let assert Ok(Nil) =
        simplifile.write_bits("./temp/expected.hex", section_bits)
      let assert Ok(Nil) =
        simplifile.write_bits("./temp/actual.hex", actual_bits)

      panic as "All done"
    }
    _ -> try_section(rest)
  }
}

pub fn main() {
  let assert Ok(rest) =
    simplifile.read_bits("./temp/assemblyscript.release.wasm")
  let assert Ok(rest) =
    common.expect_decode_bytes(rest, <<
      0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00,
    >>)
  try_section(rest)
}
