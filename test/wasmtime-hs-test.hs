import qualified Wasmtime.Raw as Raw

main :: IO ()
main = print =<< Raw.wasm_engine_new
