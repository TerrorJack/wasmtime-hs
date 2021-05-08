{-# LANGUAGE RecordWildCards #-}

module Wasmtime.Config where

import Control.Monad
import UnliftIO.Foreign
import Wasmtime.Error
import Wasmtime.Raw

data Strategy
  = Auto
  | Cranelift
  | Lightbeam

data OptLevel
  = OptLevelNone
  | Speed
  | SpeedAndSize

data ProfilingStrategy
  = ProfilingStrategyNone
  | Jitdump
  | Vtune

data Config = Config
  { debugInfo :: !Bool,
    interruptable :: !Bool,
    consumeFuel :: !Bool,
    maxWasmStack :: !Word,
    wasmThreads :: !Bool,
    wasmReferenceTypes :: !Bool,
    wasmSIMD :: !Bool,
    wasmBulkMemory :: !Bool,
    wasmMultiValue :: !Bool,
    wasmModuleLinking :: !Bool,
    strategy :: !Strategy,
    craneliftDebugVerifier :: !Bool,
    craneliftOptLevel :: !OptLevel,
    profiler :: !ProfilingStrategy,
    staticMemoryMaximumSize :: !Word64,
    staticMemoryGuardSize :: !Word64,
    dynamicMemoryGuardSize :: !Word64,
    cacheConfigLoad :: !(Maybe FilePath)
  }

defaultConfig :: Config
defaultConfig =
  Config
    { debugInfo = False,
      interruptable = False,
      consumeFuel = False,
      maxWasmStack = 1 `shiftL` 20,
      wasmThreads = False,
      wasmReferenceTypes = True,
      wasmSIMD = False,
      wasmBulkMemory = True,
      wasmMultiValue = True,
      wasmModuleLinking = False,
      strategy = Auto,
      craneliftDebugVerifier = False,
      craneliftOptLevel = OptLevelNone,
      profiler = ProfilingStrategyNone,
      staticMemoryMaximumSize = 0x10000 * 0x10000,
      staticMemoryGuardSize = 0x80000000,
      dynamicMemoryGuardSize = 0x10000,
      cacheConfigLoad = Nothing
    }

toWasmConfig :: Config -> IO (Ptr WasmConfig)
toWasmConfig Config {..} = do
  p <- wasm_config_new
  checkNull p
  wasmtime_config_debug_info_set p $ fromBool debugInfo
  wasmtime_config_interruptable_set p $ fromBool interruptable
  wasmtime_config_consume_fuel_set p $ fromBool consumeFuel
  wasmtime_config_max_wasm_stack_set p $ fromIntegral maxWasmStack
  wasmtime_config_wasm_threads_set p $ fromBool wasmThreads
  wasmtime_config_wasm_reference_types_set p $ fromBool wasmReferenceTypes
  wasmtime_config_wasm_simd_set p $ fromBool wasmSIMD
  wasmtime_config_wasm_bulk_memory_set p $ fromBool wasmBulkMemory
  wasmtime_config_wasm_multi_value_set p $ fromBool wasmMultiValue
  wasmtime_config_wasm_module_linking_set p $ fromBool wasmModuleLinking
  checkError
    =<< wasmtime_config_strategy_set
      p
      ( case strategy of
          Auto -> wasmtimeStrategyAuto
          Cranelift -> wasmtimeStrategyCranelift
          Lightbeam -> wasmtimeStrategyLightbeam
      )
  wasmtime_config_cranelift_debug_verifier_set p $
    fromBool craneliftDebugVerifier
  wasmtime_config_cranelift_opt_level_set p $ case craneliftOptLevel of
    OptLevelNone -> wasmtimeOptLevelNone
    Speed -> wasmtimeOptLevelSpeed
    SpeedAndSize -> wasmtimeOptLevelSpeedAndSize
  checkError
    =<< wasmtime_config_profiler_set
      p
      ( case profiler of
          ProfilingStrategyNone -> wasmtimeProfilingStrategyNone
          Jitdump -> wasmtimeProfilingStrategyJitdump
          Vtune -> wasmtimeProfilingStrategyVtune
      )
  wasmtime_config_static_memory_maximum_size_set p $
    fromIntegral staticMemoryMaximumSize
  wasmtime_config_static_memory_guard_size_set p $
    fromIntegral staticMemoryGuardSize
  wasmtime_config_dynamic_memory_guard_size_set p $
    fromIntegral dynamicMemoryGuardSize
  maybe
    (pure ())
    (\fn -> withCString fn (checkError <=< wasmtime_config_cache_config_load p))
    cacheConfigLoad
  pure p
