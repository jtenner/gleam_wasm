import gleam/bit_array
import gleam/io
import gleam/result
import internal/binary/modules
import pprint
import simplifile

pub fn main() {
  use bits <- result.map(
    simplifile.read_bits("./temp/assemblyscript.release.wasm")
    |> result.replace_error("Unable to open file"),
  )
  io.debug(#("loaded file", bits |> bit_array.byte_size))

  case modules.decode_module(bits) {
    Ok(module) -> {
      let #(module, left) = module
      let assert Ok(_) =
        simplifile.write(
          "./temp/assemblyscript.release.txt",
          module |> pprint.format,
        )
      Ok(Nil)
    }
    Error(err) -> {
      io.debug(Error(err))
    }
  }
}
