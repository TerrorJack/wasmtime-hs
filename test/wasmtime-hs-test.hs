{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.DeepSeq
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import System.Mem
import UnliftIO
import Wasmtime
import Wasmtime.ByteVec
import Wasmtime.Config
import Wasmtime.Context
import Wasmtime.Engine
import Wasmtime.Functype
import Wasmtime.Memory
import Wasmtime.Module
import qualified Wasmtime.Raw as Raw
import Wasmtime.Trap

main :: IO ()
main = do
  e <- newEngine defaultConfig
  c <- newContext e
  performGC
  buf_wat <- BS.readFile "test/hello.wat"
  buf_wasm <- wat2wasm buf_wat
  _ <- newModule e buf_wasm
  print buf_wasm
  t <- asWasmByteVec "asdf" $ \p_msg msg_len -> do
    p_t <- Raw.wasmtime_trap_new p_msg msg_len
    fromWasmTrap p_t
  print t
  print =<< newTrap "yolo"
  asWasmFunctype (Functype [] []) print
  m <- newMemory c $ Raw.WasmLimits 1 Raw.wasmLimitsMaxDefault
  bs_m <- fromMemory c m
  BS.unsafeUseAsCStringLen bs_m print
  growMemory c m 16
  bs_m' <- fromMemory c m
  BS.unsafeUseAsCStringLen bs_m' print
  performGC
  evaluate $ rnf $ show bs_m'
