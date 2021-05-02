import Wasmtime
import qualified Data.ByteString as BS

main :: IO ()
main = do
  buf_wat <- BS.readFile "test/hello.wat"
  buf_wasm <- wat2wasm buf_wat
  print buf_wasm
