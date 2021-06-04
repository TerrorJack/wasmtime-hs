module Wasmtime.Context where

import GHC.ForeignPtr (ForeignPtr (..))
import GHC.Ptr (Ptr (..))
import UnliftIO.Foreign
import Wasmtime.Engine
import Wasmtime.Error
import Wasmtime.Raw

newtype Context = Context (ForeignPtr WasmtimeContext)

newContext :: Engine -> IO Context
newContext (Engine fp_e) = withForeignPtr fp_e $ \p_e -> do
  p_s <- wasmtime_store_new p_e nullPtr nullFunPtr
  checkNull p_s
  Ptr addr_ctx <- wasmtime_store_context p_s
  ForeignPtr _ c <- newForeignPtr p_wasmtime_store_delete p_s
  pure $ Context $ ForeignPtr addr_ctx c

foreign import ccall unsafe "&wasmtime_store_delete"
  p_wasmtime_store_delete ::
    FinalizerPtr WasmtimeStore
