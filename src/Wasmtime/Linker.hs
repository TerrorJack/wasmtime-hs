{-# LANGUAGE RecordWildCards #-}

module Wasmtime.Linker where

import UnliftIO.Foreign
import Wasmtime.Error
import qualified Wasmtime.Raw as Raw
import Wasmtime.Store

newtype Linker
  = Linker (ForeignPtr Raw.WasmtimeLinker)

newtype LinkerConfig = LinkerConfig
  { allowShadowing :: Bool
  }

newLinker :: LinkerConfig -> Store -> IO Linker
newLinker LinkerConfig {..} (Store fp_s) = withForeignPtr fp_s $ \p_s -> do
  p_l <- Raw.wasmtime_linker_new p_s
  checkNull p_l
  Raw.wasmtime_linker_allow_shadowing p_l $ fromBool allowShadowing
  Linker <$> newForeignPtr p_wasmtime_linker_delete p_l

foreign import ccall unsafe "&wasmtime_linker_delete"
  p_wasmtime_linker_delete ::
    FinalizerPtr Raw.WasmtimeLinker
