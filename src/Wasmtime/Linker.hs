{-# LANGUAGE RecordWildCards #-}

module Wasmtime.Linker where

import UnliftIO.Foreign
import Wasmtime.Engine
import Wasmtime.Error
import qualified Wasmtime.Raw as Raw

newtype Linker = Linker (ForeignPtr Raw.WasmtimeLinker)

newtype LinkerConfig = LinkerConfig {allowShadowing :: Bool}

newLinker :: LinkerConfig -> Engine -> IO Linker
newLinker LinkerConfig {..} (Engine fp_e) = withForeignPtr fp_e $ \p_e -> do
  p_l <- Raw.wasmtime_linker_new p_e
  checkNull p_l
  Raw.wasmtime_linker_allow_shadowing p_l $ fromBool allowShadowing
  Linker <$> newForeignPtr p_wasmtime_linker_delete p_l

foreign import ccall unsafe "&wasmtime_linker_delete"
  p_wasmtime_linker_delete ::
    FinalizerPtr Raw.WasmtimeLinker
