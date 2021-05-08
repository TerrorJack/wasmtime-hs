{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

#include <wasmtime.h>

module Wasmtime.Raw.Types where

import UnliftIO.Foreign

newtype Byte
  = Byte CChar
  deriving (Storable)

newtype Float32
  = Float32 CFloat
  deriving (Storable)

newtype Float64
  = Float64 CDouble
  deriving (Storable)

newtype WasmByte
  = WasmByte Byte
  deriving (Storable)

data WasmByteVec = WasmByteVec
  { wasmByteVecSize :: !CSize,
    wasmByteVecData :: !(Ptr WasmByte)
  }

instance Storable WasmByteVec where
  sizeOf _ = #{size wasm_byte_vec_t}
  alignment _ = #{alignment wasm_byte_vec_t}
  peek p = WasmByteVec <$> #{peek wasm_byte_vec_t, size} p <*> #{peek wasm_byte_vec_t, data} p
  poke p WasmByteVec {..} = #{poke wasm_byte_vec_t, size} p wasmByteVecSize *> #{poke wasm_byte_vec_t, data} p wasmByteVecData

newtype WasmName
  = WasmName WasmByteVec
  deriving (Storable)

data WasmConfig

data WasmEngine

data WasmStore

newtype WasmMutability
  = WasmMutability Word8
  deriving (Storable)

#{enum WasmMutability, WasmMutability, wasmConst = WASM_CONST, wasmVar = WASM_VAR}

data WasmLimits = WasmLimits
  { wasmLimitsMin :: !Word32,
    wasmLimitsMax :: !Word32
  }

instance Storable WasmLimits where
  sizeOf _ = #{size wasm_limits_t}
  alignment _ = #{alignment wasm_limits_t}
  peek p = WasmLimits <$> #{peek wasm_limits_t, min} p <*> #{peek wasm_limits_t, max} p
  poke p WasmLimits {..} = #{poke wasm_limits_t, min} p wasmLimitsMin *> #{poke wasm_limits_t, max} p wasmLimitsMax

data WasmValtype

data WasmValtypeVec = WasmValtypeVec
  { wasmValtypeVecSize :: !CSize,
    wasmValtypeVecData :: !(Ptr (Ptr WasmValtype))
  }

instance Storable WasmValtypeVec where
  sizeOf _ = #{size wasm_valtype_vec_t}
  alignment _ = #{alignment wasm_valtype_vec_t}
  peek p = WasmValtypeVec <$> #{peek wasm_valtype_vec_t, size} p <*> #{peek wasm_valtype_vec_t, data} p
  poke p WasmValtypeVec {..} = #{poke wasm_valtype_vec_t, size} p wasmValtypeVecSize *> #{poke wasm_valtype_vec_t, data} p wasmValtypeVecData

newtype WasmValkind
  = WasmValkind Word8
  deriving (Eq, Storable)

#{enum WasmValkind, WasmValkind, wasmI32 = WASM_I32, wasmI64 = WASM_I64, wasmF32 = WASM_F32, wasmF64 = WASM_F64, wasmAnyref = WASM_ANYREF, wasmFuncref = WASM_FUNCREF}

data WasmFunctype

data WasmFunctypeVec = WasmFunctypeVec
  { wasmFunctypeVecSize :: !CSize,
    wasmFunctypeVecData :: !(Ptr (Ptr WasmFunctype))
  }

instance Storable WasmFunctypeVec where
  sizeOf _ = #{size wasm_functype_vec_t}
  alignment _ = #{alignment wasm_functype_vec_t}
  peek p = WasmFunctypeVec <$> #{peek wasm_functype_vec_t, size} p <*> #{peek wasm_functype_vec_t, data} p
  poke p WasmFunctypeVec {..} = #{poke wasm_functype_vec_t, size} p wasmFunctypeVecSize *> #{poke wasm_functype_vec_t, data} p wasmFunctypeVecData

data WasmGlobaltype

data WasmGlobaltypeVec = WasmGlobaltypeVec
  { wasmGlobaltypeVecSize :: !CSize,
    wasmGlobaltypeVecData :: !(Ptr (Ptr WasmGlobaltype))
  }

instance Storable WasmGlobaltypeVec where
  sizeOf _ = #{size wasm_globaltype_vec_t}
  alignment _ = #{alignment wasm_globaltype_vec_t}
  peek p = WasmGlobaltypeVec <$> #{peek wasm_globaltype_vec_t, size} p <*> #{peek wasm_globaltype_vec_t, data} p
  poke p WasmGlobaltypeVec {..} = #{poke wasm_globaltype_vec_t, size} p wasmGlobaltypeVecSize *> #{poke wasm_globaltype_vec_t, data} p wasmGlobaltypeVecData

data WasmTabletype

data WasmTabletypeVec = WasmTabletypeVec
  { wasmTabletypeVecSize :: !CSize,
    wasmTabletypeVecData :: !(Ptr (Ptr WasmTabletype))
  }

instance Storable WasmTabletypeVec where
  sizeOf _ = #{size wasm_tabletype_vec_t}
  alignment _ = #{alignment wasm_tabletype_vec_t}
  peek p = WasmTabletypeVec <$> #{peek wasm_tabletype_vec_t, size} p <*> #{peek wasm_tabletype_vec_t, data} p
  poke p WasmTabletypeVec {..} = #{poke wasm_tabletype_vec_t, size} p wasmTabletypeVecSize *> #{poke wasm_tabletype_vec_t, data} p wasmTabletypeVecData

data WasmMemorytype

data WasmMemorytypeVec = WasmMemorytypeVec
  { wasmMemorytypeVecSize :: !CSize,
    wasmMemorytypeVecData :: !(Ptr (Ptr WasmMemorytype))
  }

instance Storable WasmMemorytypeVec where
  sizeOf _ = #{size wasm_memorytype_vec_t}
  alignment _ = #{alignment wasm_memorytype_vec_t}
  peek p = WasmMemorytypeVec <$> #{peek wasm_memorytype_vec_t, size} p <*> #{peek wasm_memorytype_vec_t, data} p
  poke p WasmMemorytypeVec {..} = #{poke wasm_memorytype_vec_t, size} p wasmMemorytypeVecSize *> #{poke wasm_memorytype_vec_t, data} p wasmMemorytypeVecData

data WasmExterntype

data WasmExterntypeVec = WasmExterntypeVec
  { wasmExterntypeVecSize :: !CSize,
    wasmExterntypeVecData :: !(Ptr (Ptr WasmExterntype))
  }

instance Storable WasmExterntypeVec where
  sizeOf _ = #{size wasm_externtype_vec_t}
  alignment _ = #{alignment wasm_externtype_vec_t}
  peek p = WasmExterntypeVec <$> #{peek wasm_externtype_vec_t, size} p <*> #{peek wasm_externtype_vec_t, data} p
  poke p WasmExterntypeVec {..} = #{poke wasm_externtype_vec_t, size} p wasmExterntypeVecSize *> #{poke wasm_externtype_vec_t, data} p wasmExterntypeVecData

newtype WasmExternkind
  = WasmExternkind Word8
  deriving (Storable)

#{enum WasmExternkind, WasmExternkind, wasmExternFunc = WASM_EXTERN_FUNC, wasmExternGlobal = WASM_EXTERN_GLOBAL, wasmExternTable = WASM_EXTERN_TABLE, wasmExternMemory = WASM_EXTERN_MEMORY, wasmExternModule = WASM_EXTERN_MODULE, wasmExternInstance = WASM_EXTERN_INSTANCE}

data WasmImporttype

data WasmImporttypeVec = WasmImporttypeVec
  { wasmImporttypeVecSize :: !CSize,
    wasmImporttypeVecData :: !(Ptr (Ptr WasmImporttype))
  }

instance Storable WasmImporttypeVec where
  sizeOf _ = #{size wasm_importtype_vec_t}
  alignment _ = #{alignment wasm_importtype_vec_t}
  peek p = WasmImporttypeVec <$> #{peek wasm_importtype_vec_t, size} p <*> #{peek wasm_importtype_vec_t, data} p
  poke p WasmImporttypeVec {..} = #{poke wasm_importtype_vec_t, size} p wasmImporttypeVecSize *> #{poke wasm_importtype_vec_t, data} p wasmImporttypeVecData

data WasmExporttype

data WasmExporttypeVec = WasmExporttypeVec
  { wasmExporttypeVecSize :: !CSize,
    wasmExporttypeVecData :: !(Ptr (Ptr WasmExporttype))
  }

instance Storable WasmExporttypeVec where
  sizeOf _ = #{size wasm_exporttype_vec_t}
  alignment _ = #{alignment wasm_exporttype_vec_t}
  peek p = WasmExporttypeVec <$> #{peek wasm_exporttype_vec_t, size} p <*> #{peek wasm_exporttype_vec_t, data} p
  poke p WasmExporttypeVec {..} = #{poke wasm_exporttype_vec_t, size} p wasmExporttypeVecSize *> #{poke wasm_exporttype_vec_t, data} p wasmExporttypeVecData

data WasmVal
  = I32 !Int32
  | I64 !Int64
  | F32 !Float32
  | F64 !Float64
  | Anyref !(Ptr WasmRef)
  | Funcref !(Ptr WasmRef)

instance Storable WasmVal where
  sizeOf _ = #{size wasm_val_t}
  alignment _ = #{alignment wasm_val_t}
  peek p = do
    k <- #{peek wasm_val_t, kind} p
    if | k == wasmI32 -> I32 <$> #{peek wasm_val_t, of} p
       | k == wasmI64 -> I64 <$> #{peek wasm_val_t, of} p
       | k == wasmF32 -> F32 <$> #{peek wasm_val_t, of} p
       | k == wasmF64 -> F64 <$> #{peek wasm_val_t, of} p
       | k == wasmAnyref -> Anyref <$> #{peek wasm_val_t, of} p
       | k == wasmFuncref -> Funcref <$> #{peek wasm_val_t, of} p
       | otherwise -> fail "unreachable"
  poke p (I32 v) = #{poke wasm_val_t, kind} p wasmI32 *> #{poke wasm_val_t, of} p v
  poke p (I64 v) = #{poke wasm_val_t, kind} p wasmI64 *> #{poke wasm_val_t, of} p v
  poke p (F32 v) = #{poke wasm_val_t, kind} p wasmF32 *> #{poke wasm_val_t, of} p v
  poke p (F64 v) = #{poke wasm_val_t, kind} p wasmF64 *> #{poke wasm_val_t, of} p v
  poke p (Anyref v) = #{poke wasm_val_t, kind} p wasmAnyref *> #{poke wasm_val_t, of} p v
  poke p (Funcref v) = #{poke wasm_val_t, kind} p wasmFuncref *> #{poke wasm_val_t, of} p v

data WasmValVec = WasmValVec
  { wasmValVecSize :: !CSize,
    wasmValVecData :: !(Ptr WasmVal)
  }

instance Storable WasmValVec where
  sizeOf _ = #{size wasm_val_vec_t}
  alignment _ = #{alignment wasm_val_vec_t}
  peek p = WasmValVec <$> #{peek wasm_val_vec_t, size} p <*> #{peek wasm_val_vec_t, data} p
  poke p WasmValVec {..} = #{poke wasm_val_vec_t, size} p wasmValVecSize *> #{poke wasm_val_vec_t, data} p wasmValVecData

data WasmRef

data WasmFrame

data WasmFrameVec = WasmFrameVec
  { wasmFrameVecSize :: !CSize,
    wasmFrameVecData :: !(Ptr (Ptr WasmFrame))
  }

instance Storable WasmFrameVec where
  sizeOf _ = #{size wasm_frame_vec_t}
  alignment _ = #{alignment wasm_frame_vec_t}
  peek p = WasmFrameVec <$> #{peek wasm_frame_vec_t, size} p <*> #{peek wasm_frame_vec_t, data} p
  poke p WasmFrameVec {..} = #{poke wasm_frame_vec_t, size} p wasmFrameVecSize *> #{poke wasm_frame_vec_t, data} p wasmFrameVecData

newtype WasmMessage
  = WasmMessage WasmName
  deriving (Storable)

data WasmTrap

data WasmForeign

data WasmModule

data WasmSharedModule

data WasmFunc

newtype WasmFuncCallback
  = WasmFuncCallback
      (FunPtr (Ptr WasmValVec -> Ptr WasmValVec -> IO (Ptr WasmTrap)))
  deriving (Storable)

newtype WasmFuncCallbackWithEnv
  = WasmFuncCallbackWithEnv
      (FunPtr (Ptr () -> Ptr WasmValVec -> Ptr WasmValVec -> IO (Ptr WasmTrap)))
  deriving (Storable)

data WasmGlobal

data WasmTable

newtype WasmTableSize
  = WasmTableSize Word32
  deriving (Storable)

data WasmMemory

newtype WasmMemoryPages
  = WasmMemoryPages Word32
  deriving (Storable)

data WasmExtern

data WasmExternVec = WasmExternVec
  { wasmExternVecSize :: !CSize,
    wasmExternVecData :: !(Ptr (Ptr WasmExtern))
  }

instance Storable WasmExternVec where
  sizeOf _ = #{size wasm_extern_vec_t}
  alignment _ = #{alignment wasm_extern_vec_t}
  peek p = WasmExternVec <$> #{peek wasm_extern_vec_t, size} p <*> #{peek wasm_extern_vec_t, data} p
  poke p WasmExternVec {..} = #{poke wasm_extern_vec_t, size} p wasmExternVecSize *> #{poke wasm_extern_vec_t, data} p wasmExternVecData

data WasmInstance

data WasiConfig

data WasiInstance

data WasmtimeError

newtype WasmtimeStrategy
  = WasmtimeStrategy Word8
  deriving (Storable)

#{enum WasmtimeStrategy, WasmtimeStrategy, wasmtimeStrategyAuto = WASMTIME_STRATEGY_AUTO, wasmtimeStrategyCranelift = WASMTIME_STRATEGY_CRANELIFT, wasmtimeStrategyLightbeam = WASMTIME_STRATEGY_LIGHTBEAM}

newtype WasmtimeOptLevel
  = WasmtimeOptLevel Word8
  deriving (Storable)

#{enum WasmtimeOptLevel, WasmtimeOptLevel, wasmtimeOptLevelNone = WASMTIME_OPT_LEVEL_NONE, wasmtimeOptLevelSpeed = WASMTIME_OPT_LEVEL_SPEED, wasmtimeOptLevelSpeedAndSize = WASMTIME_OPT_LEVEL_SPEED_AND_SIZE}

newtype WasmtimeProfilingStrategy
  = WasmtimeProfilingStrategy Word8
  deriving (Storable)

#{enum WasmtimeProfilingStrategy, WasmtimeProfilingStrategy, wasmtimeProfilingStrategyNone = WASMTIME_PROFILING_STRATEGY_NONE, wasmtimeProfilingStrategyJitdump = WASMTIME_PROFILING_STRATEGY_JITDUMP, wasmtimeProfilingStrategyVtune = WASMTIME_PROFILING_STRATEGY_VTUNE}

data WasmtimeLinker

data WasmtimeCaller

newtype WasmtimeFuncCallback
  = WasmtimeFuncCallback
      (FunPtr (Ptr WasmtimeCaller -> Ptr WasmValVec -> Ptr WasmValVec -> IO (Ptr WasmTrap)))
  deriving (Storable)

newtype WasmtimeFuncCallbackWithEnv
  = WasmtimeFuncCallbackWithEnv
      (FunPtr (Ptr WasmtimeCaller -> Ptr () -> Ptr WasmValVec -> Ptr WasmValVec -> IO (Ptr WasmTrap)))
  deriving (Storable)

data WasmtimeInterruptHandle

newtype WasmtimeExternrefFinalizer
  = WasmtimeExternrefFinalizer (FunPtr (Ptr () -> IO ()))
  deriving (Storable)

data WasmInstancetype

data WasmInstancetypeVec = WasmInstancetypeVec
  { wasmInstancetypeVecSize :: !CSize,
    wasmInstancetypeVecData :: !(Ptr (Ptr WasmInstancetype))
  }

instance Storable WasmInstancetypeVec where
  sizeOf _ = #{size wasm_instancetype_vec_t}
  alignment _ = #{alignment wasm_instancetype_vec_t}
  peek p = WasmInstancetypeVec <$> #{peek wasm_instancetype_vec_t, size} p <*> #{peek wasm_instancetype_vec_t, data} p
  poke p WasmInstancetypeVec {..} = #{poke wasm_instancetype_vec_t, size} p wasmInstancetypeVecSize *> #{poke wasm_instancetype_vec_t, data} p wasmInstancetypeVecData

data WasmModuletype

data WasmModuletypeVec = WasmModuletypeVec
  { wasmModuletypeVecSize :: !CSize,
    wasmModuletypeVecData :: !(Ptr (Ptr WasmModuletype))
  }

instance Storable WasmModuletypeVec where
  sizeOf _ = #{size wasm_moduletype_vec_t}
  alignment _ = #{alignment wasm_moduletype_vec_t}
  peek p = WasmModuletypeVec <$> #{peek wasm_moduletype_vec_t, size} p <*> #{peek wasm_moduletype_vec_t, data} p
  poke p WasmModuletypeVec {..} = #{poke wasm_moduletype_vec_t, size} p wasmModuletypeVecSize *> #{poke wasm_moduletype_vec_t, data} p wasmModuletypeVecData
