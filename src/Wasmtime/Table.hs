module Wasmtime.Table where

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

withWasmTabletype ::
  Valtype -> Raw.WasmLimits -> (Ptr Raw.WasmTabletype -> IO r) -> IO r
withWasmTabletype vt l =
  bracket
    (with l (Raw.wasm_tabletype_new (toWasmValtype vt)))
    Raw.wasm_tabletype_delete
