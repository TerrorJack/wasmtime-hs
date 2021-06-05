module Wasmtime.Context where

import UnliftIO.Foreign
import Wasmtime.Engine
import Wasmtime.Error
import Wasmtime.Internal
import Wasmtime.Raw

newtype Context = Context (ForeignPtr WasmtimeContext)

newContext :: Engine -> IO Context
newContext (Engine fp_e) = withForeignPtr fp_e $ \p_e -> do
  p_s <- wasmtime_store_new p_e nullPtr nullFunPtr
  checkNull p_s
  fp_s <- newForeignPtr p_wasmtime_store_delete p_s
  p_c <- wasmtime_store_context p_s
  pure $ Context $ fp_s `setPtr` p_c

foreign import ccall unsafe "&wasmtime_store_delete"
  p_wasmtime_store_delete ::
    FinalizerPtr WasmtimeStore
