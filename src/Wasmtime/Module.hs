module Wasmtime.Module where

import Data.ByteString (ByteString)
import UnliftIO.Foreign
import Wasmtime.ByteVec
import Wasmtime.Engine
import Wasmtime.Error
import Wasmtime.Raw

newtype Module
  = Module (ForeignPtr WasmModule)

newModule :: Engine -> ByteString -> IO Module
newModule (Engine fp_e) buf_m =
  withForeignPtr fp_e $ \p_e -> asWasmByteVec buf_m $ \p_buf ->
    alloca $ \p_ret -> do
      p_err <- wasmtime_module_new p_e p_buf p_ret
      checkError p_err
      p_m <- peek p_ret
      Module <$> newForeignPtr p_wasm_module_delete p_m

foreign import ccall unsafe "&wasm_module_delete"
  p_wasm_module_delete ::
    FinalizerPtr WasmModule
