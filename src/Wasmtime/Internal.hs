module Wasmtime.Internal where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import Foreign
import qualified Wasmtime.Raw as Raw

asWasmByteVec :: ByteString -> (Ptr Raw.WasmByteVec -> IO r) -> IO r
asWasmByteVec buf f = BS.unsafeUseAsCStringLen buf $ \(buf_p, buf_len) ->
  with (Raw.WasmByteVec (fromIntegral buf_len) (castPtr buf_p)) f

fromWasmByteVec :: Ptr Raw.WasmByteVec -> IO ByteString
fromWasmByteVec bv_p = do
  bv@(Raw.WasmByteVec buf_len buf_p) <- peek bv_p
  BS.unsafePackCStringFinalizer
    (castPtr buf_p)
    (fromIntegral buf_len)
    (with bv Raw.wasm_byte_vec_delete)
