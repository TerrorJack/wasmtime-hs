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

wasmLimitsMaxDefault :: Word32
wasmLimitsMaxDefault = #{const wasm_limits_max_default}

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

#{enum WasmValkind, WasmValkind, wasmI32 = WASM_I32, wasmI64 = WASM_I64, wasmF32 = WASM_F32, wasmF64 = WASM_F64, wasmAnyref = WASM_ANYREF, wasmFuncref = WASM_FUNCREF, wasmV128 = WASMTIME_V128}

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

#{enum WasmExternkind, WasmExternkind, wasmExternFunc = WASM_EXTERN_FUNC, wasmExternGlobal = WASM_EXTERN_GLOBAL, wasmExternTable = WASM_EXTERN_TABLE, wasmExternMemory = WASM_EXTERN_MEMORY}

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
  = WasmI32 !Int32
  | WasmI64 !Int64
  | WasmF32 !Float32
  | WasmF64 !Float64
  | WasmAnyref !(Ptr WasmRef)
  | WasmFuncref !(Ptr WasmRef)

instance Storable WasmVal where
  sizeOf _ = #{size wasm_val_t}
  alignment _ = #{alignment wasm_val_t}
  peek p = do
    k <- #{peek wasm_val_t, kind} p
    if | k == wasmI32 -> WasmI32 <$> #{peek wasm_val_t, of} p
       | k == wasmI64 -> WasmI64 <$> #{peek wasm_val_t, of} p
       | k == wasmF32 -> WasmF32 <$> #{peek wasm_val_t, of} p
       | k == wasmF64 -> WasmF64 <$> #{peek wasm_val_t, of} p
       | k == wasmAnyref -> WasmAnyref <$> #{peek wasm_val_t, of} p
       | k == wasmFuncref -> WasmFuncref <$> #{peek wasm_val_t, of} p
       | otherwise -> fail "unreachable"
  poke p (WasmI32 v) = #{poke wasm_val_t, kind} p wasmI32 *> #{poke wasm_val_t, of} p v
  poke p (WasmI64 v) = #{poke wasm_val_t, kind} p wasmI64 *> #{poke wasm_val_t, of} p v
  poke p (WasmF32 v) = #{poke wasm_val_t, kind} p wasmF32 *> #{poke wasm_val_t, of} p v
  poke p (WasmF64 v) = #{poke wasm_val_t, kind} p wasmF64 *> #{poke wasm_val_t, of} p v
  poke p (WasmAnyref v) = #{poke wasm_val_t, kind} p wasmAnyref *> #{poke wasm_val_t, of} p v
  poke p (WasmFuncref v) = #{poke wasm_val_t, kind} p wasmFuncref *> #{poke wasm_val_t, of} p v

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

data WasmtimeError

data WasmtimeFunc = WasmtimeFunc
  { wasmtimeFuncStoreId :: !Word64,
    wasmtimeFuncIndex :: !CSize
  }

instance Storable WasmtimeFunc where
  sizeOf _ = #{size wasmtime_func_t}
  alignment _ = #{alignment wasmtime_func_t}
  peek p = WasmtimeFunc <$> #{peek wasmtime_func_t, store_id} p <*> #{peek wasmtime_func_t, index} p
  poke p WasmtimeFunc {..} = #{poke wasmtime_func_t, store_id} p wasmtimeFuncStoreId *> #{poke wasmtime_func_t, index} p wasmtimeFuncIndex

data WasmtimeTable = WasmtimeTable
  { wasmtimeTableStoreId :: !Word64,
    wasmtimeTableIndex :: !CSize
  }

instance Storable WasmtimeTable where
  sizeOf _ = #{size wasmtime_table_t}
  alignment _ = #{alignment wasmtime_table_t}
  peek p = WasmtimeTable <$> #{peek wasmtime_table_t, store_id} p <*> #{peek wasmtime_table_t, index} p
  poke p WasmtimeTable {..} = #{poke wasmtime_table_t, store_id} p wasmtimeTableStoreId *> #{poke wasmtime_table_t, index} p wasmtimeTableIndex

data WasmtimeMemory = WasmtimeMemory
  { wasmtimeMemoryStoreId :: !Word64,
    wasmtimeMemoryIndex :: !CSize
  }

instance Storable WasmtimeMemory where
  sizeOf _ = #{size wasmtime_memory_t}
  alignment _ = #{alignment wasmtime_memory_t}
  peek p = WasmtimeMemory <$> #{peek wasmtime_memory_t, store_id} p <*> #{peek wasmtime_memory_t, index} p
  poke p WasmtimeMemory {..} = #{poke wasmtime_memory_t, store_id} p wasmtimeMemoryStoreId *> #{poke wasmtime_memory_t, index} p wasmtimeMemoryIndex

data WasmtimeInstance = WasmtimeInstance
  { wasmtimeInstanceStoreId :: !Word64,
    wasmtimeInstanceIndex :: !CSize
  }

instance Storable WasmtimeInstance where
  sizeOf _ = #{size wasmtime_instance_t}
  alignment _ = #{alignment wasmtime_instance_t}
  peek p = WasmtimeInstance <$> #{peek wasmtime_instance_t, store_id} p <*> #{peek wasmtime_instance_t, index} p
  poke p WasmtimeInstance {..} = #{poke wasmtime_instance_t, store_id} p wasmtimeInstanceStoreId *> #{poke wasmtime_instance_t, index} p wasmtimeInstanceIndex

data WasmtimeGlobal = WasmtimeGlobal
  { wasmtimeGlobalStoreId :: !Word64,
    wasmtimeGlobalIndex :: !CSize
  }

instance Storable WasmtimeGlobal where
  sizeOf _ = #{size wasmtime_global_t}
  alignment _ = #{alignment wasmtime_global_t}
  peek p = WasmtimeGlobal <$> #{peek wasmtime_global_t, store_id} p <*> #{peek wasmtime_global_t, index} p
  poke p WasmtimeGlobal {..} = #{poke wasmtime_global_t, store_id} p wasmtimeGlobalStoreId *> #{poke wasmtime_global_t, index} p wasmtimeGlobalIndex

newtype WasmtimeExternKind
  = WasmtimeExternKind Word8
  deriving (Eq, Storable)

#{enum WasmtimeExternKind, WasmtimeExternKind, wasmtimeExternFunc = WASMTIME_EXTERN_FUNC, wasmtimeExternGlobal = WASMTIME_EXTERN_GLOBAL, wasmtimeExternTable = WASMTIME_EXTERN_TABLE, wasmtimeExternMemory = WASMTIME_EXTERN_MEMORY, wasmtimeExternInstance = WASMTIME_EXTERN_INSTANCE, wasmtimeExternModule = WASMTIME_EXTERN_MODULE}

data WasmtimeExtern
  = Func !WasmtimeFunc
  | Global !WasmtimeGlobal
  | Table !WasmtimeTable
  | Memory !WasmtimeMemory
  | Instance !WasmtimeInstance
  | Module !(Ptr WasmtimeModule)

instance Storable WasmtimeExtern where
  sizeOf _ = #{size wasmtime_extern_t}
  alignment _ = #{alignment wasmtime_extern_t}
  peek p = do
    k <- #{peek wasmtime_extern_t, kind} p
    if | k == wasmtimeExternFunc -> Func <$> #{peek wasmtime_extern_t, of} p
       | k == wasmtimeExternGlobal -> Global <$> #{peek wasmtime_extern_t, of} p
       | k == wasmtimeExternTable -> Table <$> #{peek wasmtime_extern_t, of} p
       | k == wasmtimeExternMemory -> Memory <$> #{peek wasmtime_extern_t, of} p
       | k == wasmtimeExternInstance -> Instance <$> #{peek wasmtime_extern_t, of} p
       | k == wasmtimeExternModule -> Module <$> #{peek wasmtime_extern_t, of} p
       | otherwise -> fail "unreachable"
  poke p (Func v) = #{poke wasmtime_extern_t, kind} p wasmtimeExternFunc *> #{poke wasmtime_extern_t, of} p v
  poke p (Global v) = #{poke wasmtime_extern_t, kind} p wasmtimeExternGlobal *> #{poke wasmtime_extern_t, of} p v
  poke p (Table v) = #{poke wasmtime_extern_t, kind} p wasmtimeExternTable *> #{poke wasmtime_extern_t, of} p v
  poke p (Memory v) = #{poke wasmtime_extern_t, kind} p wasmtimeExternMemory *> #{poke wasmtime_extern_t, of} p v
  poke p (Instance v) = #{poke wasmtime_extern_t, kind} p wasmtimeExternInstance *> #{poke wasmtime_extern_t, of} p v
  poke p (Module v) = #{poke wasmtime_extern_t, kind} p wasmtimeExternModule *> #{poke wasmtime_extern_t, of} p v

data WasmtimeCaller

newtype WasmtimeFuncCallback
  = WasmtimeFuncCallback
      (FunPtr (Ptr () -> Ptr WasmtimeCaller -> Ptr WasmtimeVal -> CSize -> Ptr WasmtimeVal -> CSize -> IO (Ptr WasmTrap)))
  deriving (Storable)

data WasmtimeInstancetype

data WasmtimeLinker

data WasmtimeModuletype

data WasmtimeModule

data WasmtimeStore

data WasmtimeContext

data WasmtimeInterruptHandle

data WasmtimeExternref

newtype WasmtimeValkind
  = WasmtimeValkind Word8
  deriving (Eq, Storable)

#{enum WasmtimeValkind, WasmtimeValkind, wasmtimeI32 = WASMTIME_I32, wasmtimeI64 = WASMTIME_I64, wasmtimeF32 = WASMTIME_F32, wasmtimeF64 = WASMTIME_F64, wasmtimeV128 = WASMTIME_V128, wasmtimeFuncref = WASMTIME_FUNCREF, wasmtimeExternref = WASMTIME_EXTERNREF}

data WasmtimeVal
  = I32 !Int32
  | I64 !Int64
  | F32 !Float32
  | F64 !Float64
  | Funcref !WasmtimeFunc
  | Externref !(Ptr WasmtimeExternref)
  | V128 !Word64 !Word64

instance Storable WasmtimeVal where
  sizeOf _ = #{size wasmtime_val_t}
  alignment _ = #{alignment wasmtime_val_t}
  peek p = do
    k <- #{peek wasmtime_val_t, kind} p
    if | k == wasmtimeI32 -> I32 <$> #{peek wasmtime_val_t, of} p
       | k == wasmtimeI64 -> I64 <$> #{peek wasmtime_val_t, of} p
       | k == wasmtimeF32 -> F32 <$> #{peek wasmtime_val_t, of} p
       | k == wasmtimeF64 -> F64 <$> #{peek wasmtime_val_t, of} p
       | k == wasmtimeFuncref -> Funcref <$> #{peek wasmtime_val_t, of} p
       | k == wasmtimeExternref -> Externref <$> #{peek wasmtime_val_t, of} p
       | k == wasmtimeV128 -> V128 <$> #{peek wasmtime_val_t, of} p <*> #{peek wasmtime_val_t, of} (p `plusPtr` 8)
       | otherwise -> fail "unreachable"
  poke p (I32 v) = #{poke wasmtime_val_t, kind} p wasmtimeI32 *> #{poke wasmtime_val_t, of} p v
  poke p (I64 v) = #{poke wasmtime_val_t, kind} p wasmtimeI64 *> #{poke wasmtime_val_t, of} p v
  poke p (F32 v) = #{poke wasmtime_val_t, kind} p wasmtimeF32 *> #{poke wasmtime_val_t, of} p v
  poke p (F64 v) = #{poke wasmtime_val_t, kind} p wasmtimeF64 *> #{poke wasmtime_val_t, of} p v
  poke p (Funcref v) = #{poke wasmtime_val_t, kind} p wasmtimeFuncref *> #{poke wasmtime_val_t, of} p v
  poke p (Externref v) = #{poke wasmtime_val_t, kind} p wasmtimeExternref *> #{poke wasmtime_val_t, of} p v
  poke p (V128 l h) = #{poke wasmtime_val_t, kind} p wasmtimeV128 *> #{poke wasmtime_val_t, of} p l *> #{poke wasmtime_val_t, of} (p `plusPtr` 8) h
