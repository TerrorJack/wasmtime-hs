module Wasmtime.Error
  ( Error (..),
    checkError,
  )
where

import Data.ByteString (ByteString)
import UnliftIO
import UnliftIO.Foreign
import Wasmtime.Internal
import qualified Wasmtime.Raw as Raw

newtype Error
  = Error ByteString
  deriving (Show)

instance Exception Error

checkError :: Ptr Raw.WasmtimeError -> IO ()
checkError err_p
  | err_p == nullPtr = pure ()
  | otherwise = throwIO =<< fromWasmtimeError err_p

fromWasmtimeError :: Ptr Raw.WasmtimeError -> IO Error
fromWasmtimeError err_p = alloca $ \bv_p -> do
  Raw.wasmtime_error_message err_p bv_p
  Raw.wasmtime_error_delete err_p
  Error <$> fromWasmByteVec (castPtr bv_p)
