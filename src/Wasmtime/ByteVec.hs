module Wasmtime.ByteVec where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import UnliftIO
import UnliftIO.Foreign
import qualified Wasmtime.Raw as Raw

asWasmByteVec :: ByteString -> (Ptr a -> CSize -> IO r) -> IO r
asWasmByteVec buf f = BS.unsafeUseAsCStringLen buf $
  \(buf_p, buf_len) -> f (castPtr buf_p) (fromIntegral buf_len)

fromWasmByteVec :: Ptr Raw.WasmByteVec -> IO ByteString
fromWasmByteVec bv_p = do
  bv@(Raw.WasmByteVec buf_len buf_p) <- peek bv_p
  BS.unsafePackCStringFinalizer
    (castPtr buf_p)
    (fromIntegral buf_len)
    (with bv Raw.wasm_byte_vec_delete)

fromOwnedWasmByteVec :: ForeignPtr a -> Ptr Raw.WasmByteVec -> IO ByteString
fromOwnedWasmByteVec fp p_bv = do
  Raw.WasmByteVec buf_len buf_p <- peek p_bv
  evaluate $
    BS.fromForeignPtr
      (castForeignPtr fp)
      (buf_p `minusPtr` unsafeForeignPtrToPtr fp)
      (fromIntegral buf_len)
