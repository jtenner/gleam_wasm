import gleam/bit_array
import gleam/bytes_builder
import gleam/io
import gleam/result
import gleam/string
import internal/binary/common
import internal/binary/modules
import internal/binary/values
import internal/structure/numbers
import pprint
import simplifile

// fn try_section(bits: BitArray) {
//   use #(section_id, rest) <- result.try(values.decode_u32(bits))
//   use #(section_size, rest) <- result.try(values.decode_u32(rest))
//   let section_size = section_size |> numbers.unwrap_u32
//   use #(section_bits, rest) <- result.try(common.decode_bytes(
//     rest,
//     section_size,
//   ))
//   let section_id = section_id |> numbers.unwrap_u32
//   io.debug(#("section", section_id, section_size))
//   case section_id {
//     0x0A -> {
//       // code section. dump it
//       use #(code_vec, _) <- result.try(common.decode_vec(
//         section_bits,
//         modules.decode_code_segment,
//       ))
//       use builder <- result.map(common.encode_vec(
//         bytes_builder.new(),
//         code_vec,
//         modules.encode_code_segment,
//       ))
//       let actual_bits = builder |> bytes_builder.to_bit_array

//       let assert Ok(Nil) =
//         simplifile.write_bits("./temp/expected.hex", section_bits)
//       let assert Ok(Nil) =
//         simplifile.write_bits("./temp/actual.hex", actual_bits)

//       panic as "All done"
//     }
//     _ -> try_section(rest)
//   }
// }

pub fn main() {
  let assert Ok(rest) =
    simplifile.read_bits("./temp/assemblyscript.release.wasm")

  io.debug(#("In:", rest |> bit_array.byte_size))
  use #(module, rest) <- result.try(modules.decode_module(rest))

  // 0 bytes left?
  io.debug(#("bytes left:", rest |> bit_array.byte_size))

  use result <- result.map(modules.encode_module(module))
  // bytes should match original
  io.debug(#("Out:", result |> bit_array.byte_size))
  let assert Ok(_) =
    simplifile.write_bits("./temp/assemblyscript.re-encoded.wasm", result)
}
