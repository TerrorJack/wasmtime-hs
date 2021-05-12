{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import System.Mem
import UnliftIO.Foreign
import Wasmtime
import Wasmtime.ByteVec
import Wasmtime.Config
import Wasmtime.Engine
import Wasmtime.Functype
import Wasmtime.Module
import Wasmtime.Raw
import Wasmtime.Store
import Wasmtime.Trap

main :: IO ()
main = do
  e <- newEngine defaultConfig
  s <- newStore e
  performGC
  buf_wat <- BS.readFile "test/hello.wat"
  buf_wasm <- wat2wasm buf_wat
  _ <- newModule e buf_wasm
  print buf_wasm
  t <- do
    p_e <- wasm_engine_new
    p_s <- wasm_store_new p_e
    asWasmByteVec "asdf\0" $ \p_msg -> do
      p_t <- wasm_trap_new p_s (castPtr p_msg)
      fromWasmTrap p_t
  print t
  print =<< newTrap s "yolo"
  asWasmFunctype (Functype [] []) print
