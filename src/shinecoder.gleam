import gleam/bit_array
import gleam/bytes_builder.{type BytesBuilder}
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/result
import internal/binary/common
import internal/binary/modules
import internal/binary/values
import internal/structure/numbers
import pprint
import simplifile

fn do_section(
  section_id: Int,
  bits: BitArray,
  encoder: fn(BytesBuilder, u) -> Result(BytesBuilder, String),
  decoder: fn(BitArray) -> Result(#(Option(u), BitArray), String),
) {
  let assert Ok(#(Some(section), rest)) = decoder(bits)
  let assert Ok(#(_, sample)) = common.expect_decode_byte(bits, section_id)
  let assert Ok(#(num_u32, sample)) = values.decode_u32(sample)

  let num = num_u32 |> numbers.unwrap_u32

  let assert Ok(#(expected_data, _)) = common.decode_bytes(sample, num)
  let expected_data =
    bytes_builder.new()
    |> bytes_builder.append(<<section_id>>)
    |> values.encode_u32(num_u32)
    |> bytes_builder.append(expected_data)
    |> bytes_builder.to_bit_array

  let assert Ok(builder) = bytes_builder.new() |> encoder(section)
  let actual_data = builder |> bytes_builder.to_bit_array

  case actual_data == expected_data {
    True -> do_next_section(rest)
    _ -> {
      let debug_text =
        #("actual", actual_data, "expected", expected_data) |> pprint.format
      let assert Ok(_) = simplifile.write("temp/error.txt", debug_text)
      panic as "Invalid section!"
    }
  }
}

fn do_next_section(bits: BitArray) {
  case bits {
    <<0x00, _:bits>> -> {
      do_section(
        0,
        bits,
        modules.encode_custom_section,
        modules.decode_custom_section,
      )
    }
    <<0x01, _:bits>> -> {
      do_section(
        1,
        bits,
        modules.encode_type_section,
        modules.decode_type_section,
      )
    }
    <<0x02, _:bits>> -> {
      do_section(
        2,
        bits,
        modules.encode_import_section,
        modules.decode_import_section,
      )
    }
    <<0x03, _:bits>> -> {
      do_section(
        3,
        bits,
        modules.encode_function_section,
        modules.decode_function_section,
      )
    }
    <<0x04, _:bits>> -> {
      do_section(
        4,
        bits,
        modules.encode_table_section,
        modules.decode_table_section,
      )
    }
    <<0x05, _:bits>> -> {
      do_section(
        5,
        bits,
        modules.encode_memory_section,
        modules.decode_memory_section,
      )
    }
    <<0x06, _:bits>> -> {
      do_section(
        6,
        bits,
        modules.encode_global_section,
        modules.decode_global_section,
      )
    }
    <<0x07, _:bits>> -> {
      do_section(
        7,
        bits,
        modules.encode_export_section,
        modules.decode_export_section,
      )
    }
    <<0x08, _:bits>> -> {
      do_section(
        8,
        bits,
        modules.encode_start_section,
        modules.decode_start_section,
      )
    }
    <<0x09, _:bits>> -> {
      do_section(
        9,
        bits,
        modules.encode_element_section,
        modules.decode_element_section,
      )
    }
    <<0x0C, _:bits>> -> {
      do_section(
        12,
        bits,
        modules.encode_data_count_section,
        modules.decode_data_count_section,
      )
    }
    <<0x0A, _:bits>> -> {
      do_section(
        10,
        bits,
        modules.encode_code_section,
        modules.decode_code_section,
      )
    }
    <<0x0B, _:bits>> -> {
      do_section(
        11,
        bits,
        modules.encode_data_section,
        modules.decode_data_section,
      )
    }
    <<a, _:bits>> -> {
      io.debug(#("section", a))
      panic as "Invalid section!"
    }
    _ -> Nil
  }
}

pub fn main() {
  let assert Ok(bits) = simplifile.read_bits("temp/assemblyscript.release.wasm")
  // 0ASM 1000
  let assert Ok(rest) =
    common.expect_decode_bytes(bits, <<
      0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00,
    >>)

  do_next_section(rest)
}
