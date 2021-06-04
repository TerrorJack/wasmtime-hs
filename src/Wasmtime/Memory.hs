module Wasmtime.Memory where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import GHC.ForeignPtr (ForeignPtr (..))
import GHC.Ptr (Ptr (..))
import UnliftIO
import UnliftIO.Foreign
import Wasmtime.Context
import Wasmtime.Error
import qualified Wasmtime.Raw as Raw

newMemory :: Context -> Raw.WasmLimits -> IO Raw.WasmtimeMemory
newMemory (Context fp_c) l = withForeignPtr fp_c $ \p_c -> alloca $ \p_m -> do
  checkError
    =<< withWasmMemorytype l (\p_mt -> Raw.wasmtime_memory_new p_c p_mt p_m)
  peek p_m

fromMemory :: Context -> Raw.WasmtimeMemory -> IO ByteString
fromMemory (Context fp_c@(ForeignPtr _ c)) m = withForeignPtr fp_c $ \p_c ->
  with m $ \p_m -> do
    Ptr buf_addr <- Raw.wasmtime_memory_data p_c p_m
    buf_len <- Raw.wasmtime_memory_data_size p_c p_m
    evaluate $
      BS.fromForeignPtr (ForeignPtr buf_addr c) 0 (fromIntegral buf_len)

withWasmMemorytype ::
  Raw.WasmLimits -> (Ptr Raw.WasmMemorytype -> IO r) -> IO r
withWasmMemorytype l =
  bracket (with l Raw.wasm_memorytype_new) Raw.wasm_memorytype_delete
