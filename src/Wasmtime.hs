module Wasmtime where

import Data.ByteString (ByteString)
import Foreign
import Wasmtime.Error
import Wasmtime.Internal
import qualified Wasmtime.Raw as Raw

wat2wasm :: ByteString -> IO ByteString
wat2wasm buf_wat = asWasmByteVec buf_wat $ \p_wat -> alloca $ \p_wasm -> do
  p_err <- Raw.wasmtime_wat2wasm p_wat p_wasm
  checkError p_err
  fromWasmByteVec p_wasm
