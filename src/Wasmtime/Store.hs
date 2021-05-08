module Wasmtime.Store where

import UnliftIO.Foreign
import Wasmtime.Engine
import Wasmtime.Error
import Wasmtime.Raw

newtype Store
  = Store (ForeignPtr WasmStore)

newStore :: Engine -> IO Store
newStore (Engine fp_e) = withForeignPtr fp_e $ \p_e -> do
  p_s <- wasm_store_new p_e
  checkNull p_s
  Store <$> newForeignPtr p_wasm_store_delete p_s

foreign import ccall unsafe "&wasm_store_delete"
  p_wasm_store_delete ::
    FinalizerPtr WasmStore
