module Wasmtime.Module where

import Data.ByteString (ByteString)
import UnliftIO.Foreign
import Wasmtime.ByteVec
import Wasmtime.Engine
import Wasmtime.Error
import qualified Wasmtime.Raw as Raw

newtype Module = Module (ForeignPtr Raw.WasmtimeModule)

newModule :: Engine -> ByteString -> IO Module
newModule (Engine fp_e) buf_m =
  withForeignPtr fp_e $ \p_e -> asWasmByteVec buf_m $ \p_buf buf_len ->
    alloca $ \p_ret -> do
      p_err <- Raw.wasmtime_module_new p_e p_buf buf_len p_ret
      checkError p_err
      p_m <- peek p_ret
      Module <$> newForeignPtr p_wasmtime_module_delete p_m

foreign import ccall unsafe "&wasmtime_module_delete"
  p_wasmtime_module_delete ::
    FinalizerPtr Raw.WasmtimeModule
