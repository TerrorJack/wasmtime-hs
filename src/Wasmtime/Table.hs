{-# LANGUAGE OverloadedStrings #-}

module Wasmtime.Table where

import Control.Monad
import UnliftIO
import UnliftIO.Foreign
import Wasmtime.Context
import Wasmtime.Error
import qualified Wasmtime.Raw as Raw
import Wasmtime.Valtype

newTable ::
  Context ->
  Valtype ->
  Raw.WasmLimits ->
  Raw.WasmtimeVal ->
  IO Raw.WasmtimeTable
newTable (Context fp_c) vt l v = alloca $ \p_t -> do
  checkError
    =<< withForeignPtr
      fp_c
      ( \p_c ->
          withWasmTabletype
            vt
            l
            (\p_vt -> with v (\p_v -> Raw.wasmtime_table_new p_c p_vt p_v p_t))
      )
  peek p_t

getTable :: Context -> Raw.WasmtimeTable -> Int -> IO Raw.WasmtimeVal
getTable (Context fp_c) t i = alloca $ \p_v -> do
  r <-
    toBool
      <$> withForeignPtr
        fp_c
        ( \p_c ->
            with t (\p_t -> Raw.wasmtime_table_get p_c p_t (fromIntegral i) p_v)
        )
  unless r $ throwIO $ Error "table element index out of bounds"
  peek p_v

setTable :: Context -> Raw.WasmtimeTable -> Int -> Raw.WasmtimeVal -> IO ()
setTable (Context fp_c) t i v =
  checkError
    =<< withForeignPtr
      fp_c
      ( \p_c ->
          with t (\p_t -> with v (Raw.wasmtime_table_set p_c p_t (fromIntegral i)))
      )

growTable :: Context -> Raw.WasmtimeTable -> Int -> Raw.WasmtimeVal -> IO ()
growTable (Context fp_c) t i v =
  checkError
    =<< withForeignPtr
      fp_c
      ( \p_c ->
          with
            t
            (\p_t -> with v (alloca . Raw.wasmtime_table_grow p_c p_t (fromIntegral i)))
      )

withWasmTabletype ::
  Valtype -> Raw.WasmLimits -> (Ptr Raw.WasmTabletype -> IO r) -> IO r
withWasmTabletype vt l =
  bracket
    (with l (Raw.wasm_tabletype_new (toWasmValtype vt)))
    Raw.wasm_tabletype_delete
