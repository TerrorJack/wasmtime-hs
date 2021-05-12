{-# LANGUAGE RecordWildCards #-}

module Wasmtime.Functype where

import qualified Data.Vector as V
import UnliftIO
import UnliftIO.Foreign
import qualified Wasmtime.Raw as Raw
import Wasmtime.Valtype
import Wasmtime.Vec

data Functype = Functype
  { params :: !(V.Vector Valtype),
    results :: !(V.Vector Valtype)
  }

asWasmFunctype :: Functype -> (Ptr Raw.WasmFunctype -> IO r) -> IO r
asWasmFunctype Functype {..} cont = asWasmValtypeVec params $ \p_params ->
  asWasmValtypeVec results $ \p_results ->
    bracket
      (Raw.wasm_functype_new p_params p_results)
      Raw.wasm_functype_delete
      cont

asWasmValtypeVec ::
  V.Vector Valtype -> (Ptr Raw.WasmValtypeVec -> IO r) -> IO r
asWasmValtypeVec =
  asWasmVec Raw.WasmValtypeVec (\vt cont -> cont $ toWasmValtype vt)
