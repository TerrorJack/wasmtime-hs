import Foreign

main :: IO ()
main = print =<< c_wasm_engine_new

foreign import ccall unsafe "wasm_engine_new" c_wasm_engine_new :: IO (Ptr ())
