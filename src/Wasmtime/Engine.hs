module Wasmtime.Engine where

import UnliftIO.Foreign
import Wasmtime.Config
import Wasmtime.Error
import Wasmtime.Raw

newtype Engine = Engine (ForeignPtr WasmEngine)

newEngine :: Config -> IO Engine
newEngine c = do
  p_c <- toWasmConfig c
  p_e <- wasm_engine_new_with_config p_c
  checkNull p_e
  Engine <$> newForeignPtr p_wasm_engine_delete p_e

foreign import ccall unsafe "&wasm_engine_delete"
  p_wasm_engine_delete ::
    FinalizerPtr WasmEngine
