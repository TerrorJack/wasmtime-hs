module Wasmtime.Memory where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import UnliftIO
import UnliftIO.Foreign
import Wasmtime.Context
import Wasmtime.Error
import Wasmtime.Internal
import qualified Wasmtime.Raw as Raw

newMemory :: Context -> Raw.WasmLimits -> IO Raw.WasmtimeMemory
newMemory (Context fp_c) l = withForeignPtr fp_c $ \p_c -> alloca $ \p_m -> do
  checkError
    =<< withWasmMemorytype l (\p_mt -> Raw.wasmtime_memory_new p_c p_mt p_m)
  peek p_m

fromMemory :: Context -> Raw.WasmtimeMemory -> IO ByteString
fromMemory (Context fp_c) m = withForeignPtr fp_c $ \p_c -> with m $ \p_m -> do
  buf_p <- Raw.wasmtime_memory_data p_c p_m
  buf_len <- Raw.wasmtime_memory_data_size p_c p_m
  evaluate $ BS.fromForeignPtr (fp_c `setPtr` buf_p) 0 (fromIntegral buf_len)

growMemory :: Context -> Raw.WasmtimeMemory -> Int -> IO ()
growMemory (Context fp_c) m delta =
  checkError
    =<< withForeignPtr
      fp_c
      ( \p_c ->
          with
            m
            (\p_m -> alloca (Raw.wasmtime_memory_grow p_c p_m (fromIntegral delta)))
      )

withWasmMemorytype ::
  Raw.WasmLimits -> (Ptr Raw.WasmMemorytype -> IO r) -> IO r
withWasmMemorytype l =
  bracket (with l Raw.wasm_memorytype_new) Raw.wasm_memorytype_delete
