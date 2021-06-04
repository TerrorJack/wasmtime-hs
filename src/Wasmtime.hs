module Wasmtime where

import Data.ByteString (ByteString)
import UnliftIO.Foreign
import Wasmtime.ByteVec
import Wasmtime.Error
import qualified Wasmtime.Raw as Raw

wat2wasm :: ByteString -> IO ByteString
wat2wasm buf_wat = asWasmByteVec buf_wat $ \p_wat wat_len ->
  alloca $ \p_wasm -> do
    checkError =<< Raw.wasmtime_wat2wasm p_wat wat_len p_wasm
    fromWasmByteVec p_wasm
