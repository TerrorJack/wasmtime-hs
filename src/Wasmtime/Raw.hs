module Wasmtime.Raw
  ( module Wasmtime.Raw.Types,
    module Wasmtime.Raw,
  )
where

import UnliftIO.Foreign
import Wasmtime.Raw.Types

foreign import ccall unsafe "wasm_byte_vec_new_empty"
  wasm_byte_vec_new_empty ::
    Ptr WasmByteVec -> IO ()

foreign import ccall unsafe "wasm_byte_vec_new_uninitialized"
  wasm_byte_vec_new_uninitialized ::
    Ptr WasmByteVec -> CSize -> IO ()

foreign import ccall unsafe "wasm_byte_vec_new"
  wasm_byte_vec_new ::
    Ptr WasmByteVec -> CSize -> Ptr WasmByte -> IO ()

foreign import ccall unsafe "wasm_byte_vec_copy"
  wasm_byte_vec_copy ::
    Ptr WasmByteVec -> Ptr WasmByteVec -> IO ()

foreign import ccall unsafe "wasm_byte_vec_delete"
  wasm_byte_vec_delete ::
    Ptr WasmByteVec -> IO ()

foreign import ccall unsafe "wasm_config_delete"
  wasm_config_delete ::
    Ptr WasmConfig -> IO ()

foreign import ccall unsafe "wasm_config_new"
  wasm_config_new ::
    IO (Ptr WasmConfig)

foreign import ccall unsafe "wasm_engine_delete"
  wasm_engine_delete ::
    Ptr WasmEngine -> IO ()

foreign import ccall unsafe "wasm_engine_new"
  wasm_engine_new ::
    IO (Ptr WasmEngine)

foreign import ccall unsafe "wasm_engine_new_with_config"
  wasm_engine_new_with_config ::
    Ptr WasmConfig -> IO (Ptr WasmEngine)

foreign import ccall unsafe "wasm_store_delete"
  wasm_store_delete ::
    Ptr WasmStore -> IO ()

foreign import ccall unsafe "wasm_store_new"
  wasm_store_new ::
    Ptr WasmEngine -> IO (Ptr WasmStore)

foreign import ccall unsafe "wasm_valtype_delete"
  wasm_valtype_delete ::
    Ptr WasmValtype -> IO ()

foreign import ccall unsafe "wasm_valtype_vec_new_empty"
  wasm_valtype_vec_new_empty ::
    Ptr WasmValtypeVec -> IO ()

foreign import ccall unsafe "wasm_valtype_vec_new_uninitialized"
  wasm_valtype_vec_new_uninitialized ::
    Ptr WasmValtypeVec -> CSize -> IO ()

foreign import ccall unsafe "wasm_valtype_vec_new"
  wasm_valtype_vec_new ::
    Ptr WasmValtypeVec -> CSize -> Ptr (Ptr WasmValtype) -> IO ()

foreign import ccall unsafe "wasm_valtype_vec_copy"
  wasm_valtype_vec_copy ::
    Ptr WasmValtypeVec -> Ptr WasmValtypeVec -> IO ()

foreign import ccall unsafe "wasm_valtype_vec_delete"
  wasm_valtype_vec_delete ::
    Ptr WasmValtypeVec -> IO ()

foreign import ccall unsafe "wasm_valtype_copy"
  wasm_valtype_copy ::
    Ptr WasmValtype -> IO (Ptr WasmValtype)

foreign import ccall unsafe "wasm_valtype_new"
  wasm_valtype_new ::
    WasmValkind -> IO (Ptr WasmValtype)

foreign import ccall unsafe "wasm_valtype_kind"
  wasm_valtype_kind ::
    Ptr WasmValtype -> IO WasmValkind

foreign import ccall unsafe "wasm_functype_delete"
  wasm_functype_delete ::
    Ptr WasmFunctype -> IO ()

foreign import ccall unsafe "wasm_functype_vec_new_empty"
  wasm_functype_vec_new_empty ::
    Ptr WasmFunctypeVec -> IO ()

foreign import ccall unsafe "wasm_functype_vec_new_uninitialized"
  wasm_functype_vec_new_uninitialized ::
    Ptr WasmFunctypeVec -> CSize -> IO ()

foreign import ccall unsafe "wasm_functype_vec_new"
  wasm_functype_vec_new ::
    Ptr WasmFunctypeVec -> CSize -> Ptr (Ptr WasmFunctype) -> IO ()

foreign import ccall unsafe "wasm_functype_vec_copy"
  wasm_functype_vec_copy ::
    Ptr WasmFunctypeVec -> Ptr WasmFunctypeVec -> IO ()

foreign import ccall unsafe "wasm_functype_vec_delete"
  wasm_functype_vec_delete ::
    Ptr WasmFunctypeVec -> IO ()

foreign import ccall unsafe "wasm_functype_copy"
  wasm_functype_copy ::
    Ptr WasmFunctype -> IO (Ptr WasmFunctype)

foreign import ccall unsafe "wasm_functype_new"
  wasm_functype_new ::
    Ptr WasmValtypeVec -> Ptr WasmValtypeVec -> IO (Ptr WasmFunctype)

foreign import ccall unsafe "wasm_functype_params"
  wasm_functype_params ::
    Ptr WasmFunctype -> IO (Ptr WasmValtypeVec)

foreign import ccall unsafe "wasm_functype_results"
  wasm_functype_results ::
    Ptr WasmFunctype -> IO (Ptr WasmValtypeVec)

foreign import ccall unsafe "wasm_globaltype_delete"
  wasm_globaltype_delete ::
    Ptr WasmGlobaltype -> IO ()

foreign import ccall unsafe "wasm_globaltype_vec_new_empty"
  wasm_globaltype_vec_new_empty ::
    Ptr WasmGlobaltypeVec -> IO ()

foreign import ccall unsafe "wasm_globaltype_vec_new_uninitialized"
  wasm_globaltype_vec_new_uninitialized ::
    Ptr WasmGlobaltypeVec -> CSize -> IO ()

foreign import ccall unsafe "wasm_globaltype_vec_new"
  wasm_globaltype_vec_new ::
    Ptr WasmGlobaltypeVec -> CSize -> Ptr (Ptr WasmGlobaltype) -> IO ()

foreign import ccall unsafe "wasm_globaltype_vec_copy"
  wasm_globaltype_vec_copy ::
    Ptr WasmGlobaltypeVec -> Ptr WasmGlobaltypeVec -> IO ()

foreign import ccall unsafe "wasm_globaltype_vec_delete"
  wasm_globaltype_vec_delete ::
    Ptr WasmGlobaltypeVec -> IO ()

foreign import ccall unsafe "wasm_globaltype_copy"
  wasm_globaltype_copy ::
    Ptr WasmGlobaltype -> IO (Ptr WasmGlobaltype)

foreign import ccall unsafe "wasm_globaltype_new"
  wasm_globaltype_new ::
    Ptr WasmValtype -> WasmMutability -> IO (Ptr WasmGlobaltype)

foreign import ccall unsafe "wasm_globaltype_content"
  wasm_globaltype_content ::
    Ptr WasmGlobaltype -> IO (Ptr WasmValtype)

foreign import ccall unsafe "wasm_globaltype_mutability"
  wasm_globaltype_mutability ::
    Ptr WasmGlobaltype -> IO WasmMutability

foreign import ccall unsafe "wasm_tabletype_delete"
  wasm_tabletype_delete ::
    Ptr WasmTabletype -> IO ()

foreign import ccall unsafe "wasm_tabletype_vec_new_empty"
  wasm_tabletype_vec_new_empty ::
    Ptr WasmTabletypeVec -> IO ()

foreign import ccall unsafe "wasm_tabletype_vec_new_uninitialized"
  wasm_tabletype_vec_new_uninitialized ::
    Ptr WasmTabletypeVec -> CSize -> IO ()

foreign import ccall unsafe "wasm_tabletype_vec_new"
  wasm_tabletype_vec_new ::
    Ptr WasmTabletypeVec -> CSize -> Ptr (Ptr WasmTabletype) -> IO ()

foreign import ccall unsafe "wasm_tabletype_vec_copy"
  wasm_tabletype_vec_copy ::
    Ptr WasmTabletypeVec -> Ptr WasmTabletypeVec -> IO ()

foreign import ccall unsafe "wasm_tabletype_vec_delete"
  wasm_tabletype_vec_delete ::
    Ptr WasmTabletypeVec -> IO ()

foreign import ccall unsafe "wasm_tabletype_copy"
  wasm_tabletype_copy ::
    Ptr WasmTabletype -> IO (Ptr WasmTabletype)

foreign import ccall unsafe "wasm_tabletype_new"
  wasm_tabletype_new ::
    Ptr WasmValtype -> Ptr WasmLimits -> IO (Ptr WasmTabletype)

foreign import ccall unsafe "wasm_tabletype_element"
  wasm_tabletype_element ::
    Ptr WasmTabletype -> IO (Ptr WasmValtype)

foreign import ccall unsafe "wasm_tabletype_limits"
  wasm_tabletype_limits ::
    Ptr WasmTabletype -> IO (Ptr WasmLimits)

foreign import ccall unsafe "wasm_memorytype_delete"
  wasm_memorytype_delete ::
    Ptr WasmMemorytype -> IO ()

foreign import ccall unsafe "wasm_memorytype_vec_new_empty"
  wasm_memorytype_vec_new_empty ::
    Ptr WasmMemorytypeVec -> IO ()

foreign import ccall unsafe "wasm_memorytype_vec_new_uninitialized"
  wasm_memorytype_vec_new_uninitialized ::
    Ptr WasmMemorytypeVec -> CSize -> IO ()

foreign import ccall unsafe "wasm_memorytype_vec_new"
  wasm_memorytype_vec_new ::
    Ptr WasmMemorytypeVec -> CSize -> Ptr (Ptr WasmMemorytype) -> IO ()

foreign import ccall unsafe "wasm_memorytype_vec_copy"
  wasm_memorytype_vec_copy ::
    Ptr WasmMemorytypeVec -> Ptr WasmMemorytypeVec -> IO ()

foreign import ccall unsafe "wasm_memorytype_vec_delete"
  wasm_memorytype_vec_delete ::
    Ptr WasmMemorytypeVec -> IO ()

foreign import ccall unsafe "wasm_memorytype_copy"
  wasm_memorytype_copy ::
    Ptr WasmMemorytype -> IO (Ptr WasmMemorytype)

foreign import ccall unsafe "wasm_memorytype_new"
  wasm_memorytype_new ::
    Ptr WasmLimits -> IO (Ptr WasmMemorytype)

foreign import ccall unsafe "wasm_memorytype_limits"
  wasm_memorytype_limits ::
    Ptr WasmMemorytype -> IO (Ptr WasmLimits)

foreign import ccall unsafe "wasm_externtype_delete"
  wasm_externtype_delete ::
    Ptr WasmExterntype -> IO ()

foreign import ccall unsafe "wasm_externtype_vec_new_empty"
  wasm_externtype_vec_new_empty ::
    Ptr WasmExterntypeVec -> IO ()

foreign import ccall unsafe "wasm_externtype_vec_new_uninitialized"
  wasm_externtype_vec_new_uninitialized ::
    Ptr WasmExterntypeVec -> CSize -> IO ()

foreign import ccall unsafe "wasm_externtype_vec_new"
  wasm_externtype_vec_new ::
    Ptr WasmExterntypeVec -> CSize -> Ptr (Ptr WasmExterntype) -> IO ()

foreign import ccall unsafe "wasm_externtype_vec_copy"
  wasm_externtype_vec_copy ::
    Ptr WasmExterntypeVec -> Ptr WasmExterntypeVec -> IO ()

foreign import ccall unsafe "wasm_externtype_vec_delete"
  wasm_externtype_vec_delete ::
    Ptr WasmExterntypeVec -> IO ()

foreign import ccall unsafe "wasm_externtype_copy"
  wasm_externtype_copy ::
    Ptr WasmExterntype -> IO (Ptr WasmExterntype)

foreign import ccall unsafe "wasm_externtype_kind"
  wasm_externtype_kind ::
    Ptr WasmExterntype -> IO WasmExternkind

foreign import ccall unsafe "wasm_functype_as_externtype"
  wasm_functype_as_externtype ::
    Ptr WasmFunctype -> IO (Ptr WasmExterntype)

foreign import ccall unsafe "wasm_globaltype_as_externtype"
  wasm_globaltype_as_externtype ::
    Ptr WasmGlobaltype -> IO (Ptr WasmExterntype)

foreign import ccall unsafe "wasm_tabletype_as_externtype"
  wasm_tabletype_as_externtype ::
    Ptr WasmTabletype -> IO (Ptr WasmExterntype)

foreign import ccall unsafe "wasm_memorytype_as_externtype"
  wasm_memorytype_as_externtype ::
    Ptr WasmMemorytype -> IO (Ptr WasmExterntype)

foreign import ccall unsafe "wasm_externtype_as_functype"
  wasm_externtype_as_functype ::
    Ptr WasmExterntype -> IO (Ptr WasmFunctype)

foreign import ccall unsafe "wasm_externtype_as_globaltype"
  wasm_externtype_as_globaltype ::
    Ptr WasmExterntype -> IO (Ptr WasmGlobaltype)

foreign import ccall unsafe "wasm_externtype_as_tabletype"
  wasm_externtype_as_tabletype ::
    Ptr WasmExterntype -> IO (Ptr WasmTabletype)

foreign import ccall unsafe "wasm_externtype_as_memorytype"
  wasm_externtype_as_memorytype ::
    Ptr WasmExterntype -> IO (Ptr WasmMemorytype)

foreign import ccall unsafe "wasm_functype_as_externtype_const"
  wasm_functype_as_externtype_const ::
    Ptr WasmFunctype -> IO (Ptr WasmExterntype)

foreign import ccall unsafe "wasm_globaltype_as_externtype_const"
  wasm_globaltype_as_externtype_const ::
    Ptr WasmGlobaltype -> IO (Ptr WasmExterntype)

foreign import ccall unsafe "wasm_tabletype_as_externtype_const"
  wasm_tabletype_as_externtype_const ::
    Ptr WasmTabletype -> IO (Ptr WasmExterntype)

foreign import ccall unsafe "wasm_memorytype_as_externtype_const"
  wasm_memorytype_as_externtype_const ::
    Ptr WasmMemorytype -> IO (Ptr WasmExterntype)

foreign import ccall unsafe "wasm_externtype_as_functype_const"
  wasm_externtype_as_functype_const ::
    Ptr WasmExterntype -> IO (Ptr WasmFunctype)

foreign import ccall unsafe "wasm_externtype_as_globaltype_const"
  wasm_externtype_as_globaltype_const ::
    Ptr WasmExterntype -> IO (Ptr WasmGlobaltype)

foreign import ccall unsafe "wasm_externtype_as_tabletype_const"
  wasm_externtype_as_tabletype_const ::
    Ptr WasmExterntype -> IO (Ptr WasmTabletype)

foreign import ccall unsafe "wasm_externtype_as_memorytype_const"
  wasm_externtype_as_memorytype_const ::
    Ptr WasmExterntype -> IO (Ptr WasmMemorytype)

foreign import ccall unsafe "wasm_importtype_delete"
  wasm_importtype_delete ::
    Ptr WasmImporttype -> IO ()

foreign import ccall unsafe "wasm_importtype_vec_new_empty"
  wasm_importtype_vec_new_empty ::
    Ptr WasmImporttypeVec -> IO ()

foreign import ccall unsafe "wasm_importtype_vec_new_uninitialized"
  wasm_importtype_vec_new_uninitialized ::
    Ptr WasmImporttypeVec -> CSize -> IO ()

foreign import ccall unsafe "wasm_importtype_vec_new"
  wasm_importtype_vec_new ::
    Ptr WasmImporttypeVec -> CSize -> Ptr (Ptr WasmImporttype) -> IO ()

foreign import ccall unsafe "wasm_importtype_vec_copy"
  wasm_importtype_vec_copy ::
    Ptr WasmImporttypeVec -> Ptr WasmImporttypeVec -> IO ()

foreign import ccall unsafe "wasm_importtype_vec_delete"
  wasm_importtype_vec_delete ::
    Ptr WasmImporttypeVec -> IO ()

foreign import ccall unsafe "wasm_importtype_copy"
  wasm_importtype_copy ::
    Ptr WasmImporttype -> IO (Ptr WasmImporttype)

foreign import ccall unsafe "wasm_importtype_new"
  wasm_importtype_new ::
    Ptr WasmName ->
    Ptr WasmName ->
    Ptr WasmExterntype ->
    IO (Ptr WasmImporttype)

foreign import ccall unsafe "wasm_importtype_module"
  wasm_importtype_module ::
    Ptr WasmImporttype -> IO (Ptr WasmName)

foreign import ccall unsafe "wasm_importtype_name"
  wasm_importtype_name ::
    Ptr WasmImporttype -> IO (Ptr WasmName)

foreign import ccall unsafe "wasm_importtype_type"
  wasm_importtype_type ::
    Ptr WasmImporttype -> IO (Ptr WasmExterntype)

foreign import ccall unsafe "wasm_exporttype_delete"
  wasm_exporttype_delete ::
    Ptr WasmExporttype -> IO ()

foreign import ccall unsafe "wasm_exporttype_vec_new_empty"
  wasm_exporttype_vec_new_empty ::
    Ptr WasmExporttypeVec -> IO ()

foreign import ccall unsafe "wasm_exporttype_vec_new_uninitialized"
  wasm_exporttype_vec_new_uninitialized ::
    Ptr WasmExporttypeVec -> CSize -> IO ()

foreign import ccall unsafe "wasm_exporttype_vec_new"
  wasm_exporttype_vec_new ::
    Ptr WasmExporttypeVec -> CSize -> Ptr (Ptr WasmExporttype) -> IO ()

foreign import ccall unsafe "wasm_exporttype_vec_copy"
  wasm_exporttype_vec_copy ::
    Ptr WasmExporttypeVec -> Ptr WasmExporttypeVec -> IO ()

foreign import ccall unsafe "wasm_exporttype_vec_delete"
  wasm_exporttype_vec_delete ::
    Ptr WasmExporttypeVec -> IO ()

foreign import ccall unsafe "wasm_exporttype_copy"
  wasm_exporttype_copy ::
    Ptr WasmExporttype -> IO (Ptr WasmExporttype)

foreign import ccall unsafe "wasm_exporttype_new"
  wasm_exporttype_new ::
    Ptr WasmName -> Ptr WasmExterntype -> IO (Ptr WasmExporttype)

foreign import ccall unsafe "wasm_exporttype_name"
  wasm_exporttype_name ::
    Ptr WasmExporttype -> IO (Ptr WasmName)

foreign import ccall unsafe "wasm_exporttype_type"
  wasm_exporttype_type ::
    Ptr WasmExporttype -> IO (Ptr WasmExterntype)

foreign import ccall unsafe "wasm_val_delete"
  wasm_val_delete ::
    Ptr WasmVal -> IO ()

foreign import ccall unsafe "wasm_val_copy"
  wasm_val_copy ::
    Ptr WasmVal -> Ptr WasmVal -> IO ()

foreign import ccall unsafe "wasm_val_vec_new_empty"
  wasm_val_vec_new_empty ::
    Ptr WasmValVec -> IO ()

foreign import ccall unsafe "wasm_val_vec_new_uninitialized"
  wasm_val_vec_new_uninitialized ::
    Ptr WasmValVec -> CSize -> IO ()

foreign import ccall unsafe "wasm_val_vec_new"
  wasm_val_vec_new ::
    Ptr WasmValVec -> CSize -> Ptr WasmVal -> IO ()

foreign import ccall unsafe "wasm_val_vec_copy"
  wasm_val_vec_copy ::
    Ptr WasmValVec -> Ptr WasmValVec -> IO ()

foreign import ccall unsafe "wasm_val_vec_delete"
  wasm_val_vec_delete ::
    Ptr WasmValVec -> IO ()

foreign import ccall unsafe "wasm_ref_delete"
  wasm_ref_delete ::
    Ptr WasmRef -> IO ()

foreign import ccall unsafe "wasm_ref_copy"
  wasm_ref_copy ::
    Ptr WasmRef -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_same"
  wasm_ref_same ::
    Ptr WasmRef -> Ptr WasmRef -> IO CBool

foreign import ccall unsafe "wasm_ref_get_host_info"
  wasm_ref_get_host_info ::
    Ptr WasmRef -> IO (Ptr ())

foreign import ccall unsafe "wasm_ref_set_host_info"
  wasm_ref_set_host_info ::
    Ptr WasmRef -> Ptr () -> IO ()

foreign import ccall unsafe "wasm_ref_set_host_info_with_finalizer"
  wasm_ref_set_host_info_with_finalizer ::
    Ptr WasmRef -> Ptr () -> FunPtr (Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "wasm_frame_delete"
  wasm_frame_delete ::
    Ptr WasmFrame -> IO ()

foreign import ccall unsafe "wasm_frame_vec_new_empty"
  wasm_frame_vec_new_empty ::
    Ptr WasmFrameVec -> IO ()

foreign import ccall unsafe "wasm_frame_vec_new_uninitialized"
  wasm_frame_vec_new_uninitialized ::
    Ptr WasmFrameVec -> CSize -> IO ()

foreign import ccall unsafe "wasm_frame_vec_new"
  wasm_frame_vec_new ::
    Ptr WasmFrameVec -> CSize -> Ptr (Ptr WasmFrame) -> IO ()

foreign import ccall unsafe "wasm_frame_vec_copy"
  wasm_frame_vec_copy ::
    Ptr WasmFrameVec -> Ptr WasmFrameVec -> IO ()

foreign import ccall unsafe "wasm_frame_vec_delete"
  wasm_frame_vec_delete ::
    Ptr WasmFrameVec -> IO ()

foreign import ccall unsafe "wasm_frame_copy"
  wasm_frame_copy ::
    Ptr WasmFrame -> IO (Ptr WasmFrame)

foreign import ccall unsafe "wasm_frame_instance"
  wasm_frame_instance ::
    Ptr WasmFrame -> IO (Ptr WasmInstance)

foreign import ccall unsafe "wasm_frame_func_index"
  wasm_frame_func_index ::
    Ptr WasmFrame -> IO Word32

foreign import ccall unsafe "wasm_frame_func_offset"
  wasm_frame_func_offset ::
    Ptr WasmFrame -> IO CSize

foreign import ccall unsafe "wasm_frame_module_offset"
  wasm_frame_module_offset ::
    Ptr WasmFrame -> IO CSize

foreign import ccall unsafe "wasm_trap_delete"
  wasm_trap_delete ::
    Ptr WasmTrap -> IO ()

foreign import ccall unsafe "wasm_trap_copy"
  wasm_trap_copy ::
    Ptr WasmTrap -> IO (Ptr WasmTrap)

foreign import ccall unsafe "wasm_trap_same"
  wasm_trap_same ::
    Ptr WasmTrap -> Ptr WasmTrap -> IO CBool

foreign import ccall unsafe "wasm_trap_get_host_info"
  wasm_trap_get_host_info ::
    Ptr WasmTrap -> IO (Ptr ())

foreign import ccall unsafe "wasm_trap_set_host_info"
  wasm_trap_set_host_info ::
    Ptr WasmTrap -> Ptr () -> IO ()

foreign import ccall unsafe "wasm_trap_set_host_info_with_finalizer"
  wasm_trap_set_host_info_with_finalizer ::
    Ptr WasmTrap -> Ptr () -> FunPtr (Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "wasm_trap_as_ref"
  wasm_trap_as_ref ::
    Ptr WasmTrap -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_trap"
  wasm_ref_as_trap ::
    Ptr WasmRef -> IO (Ptr WasmTrap)

foreign import ccall unsafe "wasm_trap_as_ref_const"
  wasm_trap_as_ref_const ::
    Ptr WasmTrap -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_trap_const"
  wasm_ref_as_trap_const ::
    Ptr WasmRef -> IO (Ptr WasmTrap)

foreign import ccall unsafe "wasm_trap_new"
  wasm_trap_new ::
    Ptr WasmStore -> Ptr WasmMessage -> IO (Ptr WasmTrap)

foreign import ccall unsafe "wasm_trap_message"
  wasm_trap_message ::
    Ptr WasmTrap -> Ptr WasmMessage -> IO ()

foreign import ccall unsafe "wasm_trap_origin"
  wasm_trap_origin ::
    Ptr WasmTrap -> IO (Ptr WasmFrame)

foreign import ccall unsafe "wasm_trap_trace"
  wasm_trap_trace ::
    Ptr WasmTrap -> Ptr WasmFrameVec -> IO ()

foreign import ccall unsafe "wasm_foreign_delete"
  wasm_foreign_delete ::
    Ptr WasmForeign -> IO ()

foreign import ccall unsafe "wasm_foreign_copy"
  wasm_foreign_copy ::
    Ptr WasmForeign -> IO (Ptr WasmForeign)

foreign import ccall unsafe "wasm_foreign_same"
  wasm_foreign_same ::
    Ptr WasmForeign -> Ptr WasmForeign -> IO CBool

foreign import ccall unsafe "wasm_foreign_get_host_info"
  wasm_foreign_get_host_info ::
    Ptr WasmForeign -> IO (Ptr ())

foreign import ccall unsafe "wasm_foreign_set_host_info"
  wasm_foreign_set_host_info ::
    Ptr WasmForeign -> Ptr () -> IO ()

foreign import ccall unsafe "wasm_foreign_set_host_info_with_finalizer"
  wasm_foreign_set_host_info_with_finalizer ::
    Ptr WasmForeign -> Ptr () -> FunPtr (Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "wasm_foreign_as_ref"
  wasm_foreign_as_ref ::
    Ptr WasmForeign -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_foreign"
  wasm_ref_as_foreign ::
    Ptr WasmRef -> IO (Ptr WasmForeign)

foreign import ccall unsafe "wasm_foreign_as_ref_const"
  wasm_foreign_as_ref_const ::
    Ptr WasmForeign -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_foreign_const"
  wasm_ref_as_foreign_const ::
    Ptr WasmRef -> IO (Ptr WasmForeign)

foreign import ccall unsafe "wasm_foreign_new"
  wasm_foreign_new ::
    Ptr WasmStore -> IO (Ptr WasmForeign)

foreign import ccall unsafe "wasm_module_delete"
  wasm_module_delete ::
    Ptr WasmModule -> IO ()

foreign import ccall unsafe "wasm_module_copy"
  wasm_module_copy ::
    Ptr WasmModule -> IO (Ptr WasmModule)

foreign import ccall unsafe "wasm_module_same"
  wasm_module_same ::
    Ptr WasmModule -> Ptr WasmModule -> IO CBool

foreign import ccall unsafe "wasm_module_get_host_info"
  wasm_module_get_host_info ::
    Ptr WasmModule -> IO (Ptr ())

foreign import ccall unsafe "wasm_module_set_host_info"
  wasm_module_set_host_info ::
    Ptr WasmModule -> Ptr () -> IO ()

foreign import ccall unsafe "wasm_module_set_host_info_with_finalizer"
  wasm_module_set_host_info_with_finalizer ::
    Ptr WasmModule -> Ptr () -> FunPtr (Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "wasm_module_as_ref"
  wasm_module_as_ref ::
    Ptr WasmModule -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_module"
  wasm_ref_as_module ::
    Ptr WasmRef -> IO (Ptr WasmModule)

foreign import ccall unsafe "wasm_module_as_ref_const"
  wasm_module_as_ref_const ::
    Ptr WasmModule -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_module_const"
  wasm_ref_as_module_const ::
    Ptr WasmRef -> IO (Ptr WasmModule)

foreign import ccall unsafe "wasm_shared_module_delete"
  wasm_shared_module_delete ::
    Ptr WasmSharedModule -> IO ()

foreign import ccall unsafe "wasm_module_share"
  wasm_module_share ::
    Ptr WasmModule -> IO (Ptr WasmSharedModule)

foreign import ccall unsafe "wasm_module_obtain"
  wasm_module_obtain ::
    Ptr WasmStore -> Ptr WasmSharedModule -> IO (Ptr WasmModule)

foreign import ccall unsafe "wasm_module_new"
  wasm_module_new ::
    Ptr WasmStore -> Ptr WasmByteVec -> IO (Ptr WasmModule)

foreign import ccall unsafe "wasm_module_validate"
  wasm_module_validate ::
    Ptr WasmStore -> Ptr WasmByteVec -> IO CBool

foreign import ccall unsafe "wasm_module_imports"
  wasm_module_imports ::
    Ptr WasmModule -> Ptr WasmImporttypeVec -> IO ()

foreign import ccall unsafe "wasm_module_exports"
  wasm_module_exports ::
    Ptr WasmModule -> Ptr WasmExporttypeVec -> IO ()

foreign import ccall unsafe "wasm_module_serialize"
  wasm_module_serialize ::
    Ptr WasmModule -> Ptr WasmByteVec -> IO ()

foreign import ccall unsafe "wasm_module_deserialize"
  wasm_module_deserialize ::
    Ptr WasmStore -> Ptr WasmByteVec -> IO (Ptr WasmModule)

foreign import ccall unsafe "wasm_func_delete"
  wasm_func_delete ::
    Ptr WasmFunc -> IO ()

foreign import ccall unsafe "wasm_func_copy"
  wasm_func_copy ::
    Ptr WasmFunc -> IO (Ptr WasmFunc)

foreign import ccall unsafe "wasm_func_same"
  wasm_func_same ::
    Ptr WasmFunc -> Ptr WasmFunc -> IO CBool

foreign import ccall unsafe "wasm_func_get_host_info"
  wasm_func_get_host_info ::
    Ptr WasmFunc -> IO (Ptr ())

foreign import ccall unsafe "wasm_func_set_host_info"
  wasm_func_set_host_info ::
    Ptr WasmFunc -> Ptr () -> IO ()

foreign import ccall unsafe "wasm_func_set_host_info_with_finalizer"
  wasm_func_set_host_info_with_finalizer ::
    Ptr WasmFunc -> Ptr () -> FunPtr (Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "wasm_func_as_ref"
  wasm_func_as_ref ::
    Ptr WasmFunc -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_func"
  wasm_ref_as_func ::
    Ptr WasmRef -> IO (Ptr WasmFunc)

foreign import ccall unsafe "wasm_func_as_ref_const"
  wasm_func_as_ref_const ::
    Ptr WasmFunc -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_func_const"
  wasm_ref_as_func_const ::
    Ptr WasmRef -> IO (Ptr WasmFunc)

foreign import ccall unsafe "wasm_func_new"
  wasm_func_new ::
    Ptr WasmStore ->
    Ptr WasmFunctype ->
    WasmFuncCallback ->
    IO (Ptr WasmFunc)

foreign import ccall unsafe "wasm_func_new_with_env"
  wasm_func_new_with_env ::
    Ptr WasmStore ->
    Ptr WasmFunctype ->
    WasmFuncCallbackWithEnv ->
    Ptr () ->
    FunPtr (Ptr () -> IO ()) ->
    IO (Ptr WasmFunc)

foreign import ccall unsafe "wasm_func_type"
  wasm_func_type ::
    Ptr WasmFunc -> IO (Ptr WasmFunctype)

foreign import ccall unsafe "wasm_func_param_arity"
  wasm_func_param_arity ::
    Ptr WasmFunc -> IO CSize

foreign import ccall unsafe "wasm_func_result_arity"
  wasm_func_result_arity ::
    Ptr WasmFunc -> IO CSize

foreign import ccall safe "wasm_func_call"
  wasm_func_call ::
    Ptr WasmFunc -> Ptr WasmValVec -> Ptr WasmValVec -> IO (Ptr WasmTrap)

foreign import ccall unsafe "wasm_global_delete"
  wasm_global_delete ::
    Ptr WasmGlobal -> IO ()

foreign import ccall unsafe "wasm_global_copy"
  wasm_global_copy ::
    Ptr WasmGlobal -> IO (Ptr WasmGlobal)

foreign import ccall unsafe "wasm_global_same"
  wasm_global_same ::
    Ptr WasmGlobal -> Ptr WasmGlobal -> IO CBool

foreign import ccall unsafe "wasm_global_get_host_info"
  wasm_global_get_host_info ::
    Ptr WasmGlobal -> IO (Ptr ())

foreign import ccall unsafe "wasm_global_set_host_info"
  wasm_global_set_host_info ::
    Ptr WasmGlobal -> Ptr () -> IO ()

foreign import ccall unsafe "wasm_global_set_host_info_with_finalizer"
  wasm_global_set_host_info_with_finalizer ::
    Ptr WasmGlobal -> Ptr () -> FunPtr (Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "wasm_global_as_ref"
  wasm_global_as_ref ::
    Ptr WasmGlobal -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_global"
  wasm_ref_as_global ::
    Ptr WasmRef -> IO (Ptr WasmGlobal)

foreign import ccall unsafe "wasm_global_as_ref_const"
  wasm_global_as_ref_const ::
    Ptr WasmGlobal -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_global_const"
  wasm_ref_as_global_const ::
    Ptr WasmRef -> IO (Ptr WasmGlobal)

foreign import ccall unsafe "wasm_global_new"
  wasm_global_new ::
    Ptr WasmStore ->
    Ptr WasmGlobaltype ->
    Ptr WasmVal ->
    IO (Ptr WasmGlobal)

foreign import ccall unsafe "wasm_global_type"
  wasm_global_type ::
    Ptr WasmGlobal -> IO (Ptr WasmGlobaltype)

foreign import ccall unsafe "wasm_global_get"
  wasm_global_get ::
    Ptr WasmGlobal -> Ptr WasmVal -> IO ()

foreign import ccall unsafe "wasm_global_set"
  wasm_global_set ::
    Ptr WasmGlobal -> Ptr WasmVal -> IO ()

foreign import ccall unsafe "wasm_table_delete"
  wasm_table_delete ::
    Ptr WasmTable -> IO ()

foreign import ccall unsafe "wasm_table_copy"
  wasm_table_copy ::
    Ptr WasmTable -> IO (Ptr WasmTable)

foreign import ccall unsafe "wasm_table_same"
  wasm_table_same ::
    Ptr WasmTable -> Ptr WasmTable -> IO CBool

foreign import ccall unsafe "wasm_table_get_host_info"
  wasm_table_get_host_info ::
    Ptr WasmTable -> IO (Ptr ())

foreign import ccall unsafe "wasm_table_set_host_info"
  wasm_table_set_host_info ::
    Ptr WasmTable -> Ptr () -> IO ()

foreign import ccall unsafe "wasm_table_set_host_info_with_finalizer"
  wasm_table_set_host_info_with_finalizer ::
    Ptr WasmTable -> Ptr () -> FunPtr (Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "wasm_table_as_ref"
  wasm_table_as_ref ::
    Ptr WasmTable -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_table"
  wasm_ref_as_table ::
    Ptr WasmRef -> IO (Ptr WasmTable)

foreign import ccall unsafe "wasm_table_as_ref_const"
  wasm_table_as_ref_const ::
    Ptr WasmTable -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_table_const"
  wasm_ref_as_table_const ::
    Ptr WasmRef -> IO (Ptr WasmTable)

foreign import ccall unsafe "wasm_table_new"
  wasm_table_new ::
    Ptr WasmStore -> Ptr WasmTabletype -> Ptr WasmRef -> IO (Ptr WasmTable)

foreign import ccall unsafe "wasm_table_type"
  wasm_table_type ::
    Ptr WasmTable -> IO (Ptr WasmTabletype)

foreign import ccall unsafe "wasm_table_get"
  wasm_table_get ::
    Ptr WasmTable -> WasmTableSize -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_table_set"
  wasm_table_set ::
    Ptr WasmTable -> WasmTableSize -> Ptr WasmRef -> IO CBool

foreign import ccall unsafe "wasm_table_size"
  wasm_table_size ::
    Ptr WasmTable -> IO WasmTableSize

foreign import ccall unsafe "wasm_table_grow"
  wasm_table_grow ::
    Ptr WasmTable -> WasmTableSize -> Ptr WasmRef -> IO CBool

foreign import ccall unsafe "wasm_memory_delete"
  wasm_memory_delete ::
    Ptr WasmMemory -> IO ()

foreign import ccall unsafe "wasm_memory_copy"
  wasm_memory_copy ::
    Ptr WasmMemory -> IO (Ptr WasmMemory)

foreign import ccall unsafe "wasm_memory_same"
  wasm_memory_same ::
    Ptr WasmMemory -> Ptr WasmMemory -> IO CBool

foreign import ccall unsafe "wasm_memory_get_host_info"
  wasm_memory_get_host_info ::
    Ptr WasmMemory -> IO (Ptr ())

foreign import ccall unsafe "wasm_memory_set_host_info"
  wasm_memory_set_host_info ::
    Ptr WasmMemory -> Ptr () -> IO ()

foreign import ccall unsafe "wasm_memory_set_host_info_with_finalizer"
  wasm_memory_set_host_info_with_finalizer ::
    Ptr WasmMemory -> Ptr () -> FunPtr (Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "wasm_memory_as_ref"
  wasm_memory_as_ref ::
    Ptr WasmMemory -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_memory"
  wasm_ref_as_memory ::
    Ptr WasmRef -> IO (Ptr WasmMemory)

foreign import ccall unsafe "wasm_memory_as_ref_const"
  wasm_memory_as_ref_const ::
    Ptr WasmMemory -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_memory_const"
  wasm_ref_as_memory_const ::
    Ptr WasmRef -> IO (Ptr WasmMemory)

foreign import ccall unsafe "wasm_memory_new"
  wasm_memory_new ::
    Ptr WasmStore -> Ptr WasmMemorytype -> IO (Ptr WasmMemory)

foreign import ccall unsafe "wasm_memory_type"
  wasm_memory_type ::
    Ptr WasmMemory -> IO (Ptr WasmMemorytype)

foreign import ccall unsafe "wasm_memory_data"
  wasm_memory_data ::
    Ptr WasmMemory -> IO (Ptr Byte)

foreign import ccall unsafe "wasm_memory_data_size"
  wasm_memory_data_size ::
    Ptr WasmMemory -> IO CSize

foreign import ccall unsafe "wasm_memory_size"
  wasm_memory_size ::
    Ptr WasmMemory -> IO WasmMemoryPages

foreign import ccall unsafe "wasm_memory_grow"
  wasm_memory_grow ::
    Ptr WasmMemory -> WasmMemoryPages -> IO CBool

foreign import ccall unsafe "wasm_extern_delete"
  wasm_extern_delete ::
    Ptr WasmExtern -> IO ()

foreign import ccall unsafe "wasm_extern_copy"
  wasm_extern_copy ::
    Ptr WasmExtern -> IO (Ptr WasmExtern)

foreign import ccall unsafe "wasm_extern_same"
  wasm_extern_same ::
    Ptr WasmExtern -> Ptr WasmExtern -> IO CBool

foreign import ccall unsafe "wasm_extern_get_host_info"
  wasm_extern_get_host_info ::
    Ptr WasmExtern -> IO (Ptr ())

foreign import ccall unsafe "wasm_extern_set_host_info"
  wasm_extern_set_host_info ::
    Ptr WasmExtern -> Ptr () -> IO ()

foreign import ccall unsafe "wasm_extern_set_host_info_with_finalizer"
  wasm_extern_set_host_info_with_finalizer ::
    Ptr WasmExtern -> Ptr () -> FunPtr (Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "wasm_extern_as_ref"
  wasm_extern_as_ref ::
    Ptr WasmExtern -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_extern"
  wasm_ref_as_extern ::
    Ptr WasmRef -> IO (Ptr WasmExtern)

foreign import ccall unsafe "wasm_extern_as_ref_const"
  wasm_extern_as_ref_const ::
    Ptr WasmExtern -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_extern_const"
  wasm_ref_as_extern_const ::
    Ptr WasmRef -> IO (Ptr WasmExtern)

foreign import ccall unsafe "wasm_extern_vec_new_empty"
  wasm_extern_vec_new_empty ::
    Ptr WasmExternVec -> IO ()

foreign import ccall unsafe "wasm_extern_vec_new_uninitialized"
  wasm_extern_vec_new_uninitialized ::
    Ptr WasmExternVec -> CSize -> IO ()

foreign import ccall unsafe "wasm_extern_vec_new"
  wasm_extern_vec_new ::
    Ptr WasmExternVec -> CSize -> Ptr (Ptr WasmExtern) -> IO ()

foreign import ccall unsafe "wasm_extern_vec_copy"
  wasm_extern_vec_copy ::
    Ptr WasmExternVec -> Ptr WasmExternVec -> IO ()

foreign import ccall unsafe "wasm_extern_vec_delete"
  wasm_extern_vec_delete ::
    Ptr WasmExternVec -> IO ()

foreign import ccall unsafe "wasm_extern_kind"
  wasm_extern_kind ::
    Ptr WasmExtern -> IO WasmExternkind

foreign import ccall unsafe "wasm_extern_type"
  wasm_extern_type ::
    Ptr WasmExtern -> IO (Ptr WasmExterntype)

foreign import ccall unsafe "wasm_func_as_extern"
  wasm_func_as_extern ::
    Ptr WasmFunc -> IO (Ptr WasmExtern)

foreign import ccall unsafe "wasm_global_as_extern"
  wasm_global_as_extern ::
    Ptr WasmGlobal -> IO (Ptr WasmExtern)

foreign import ccall unsafe "wasm_table_as_extern"
  wasm_table_as_extern ::
    Ptr WasmTable -> IO (Ptr WasmExtern)

foreign import ccall unsafe "wasm_memory_as_extern"
  wasm_memory_as_extern ::
    Ptr WasmMemory -> IO (Ptr WasmExtern)

foreign import ccall unsafe "wasm_extern_as_func"
  wasm_extern_as_func ::
    Ptr WasmExtern -> IO (Ptr WasmFunc)

foreign import ccall unsafe "wasm_extern_as_global"
  wasm_extern_as_global ::
    Ptr WasmExtern -> IO (Ptr WasmGlobal)

foreign import ccall unsafe "wasm_extern_as_table"
  wasm_extern_as_table ::
    Ptr WasmExtern -> IO (Ptr WasmTable)

foreign import ccall unsafe "wasm_extern_as_memory"
  wasm_extern_as_memory ::
    Ptr WasmExtern -> IO (Ptr WasmMemory)

foreign import ccall unsafe "wasm_func_as_extern_const"
  wasm_func_as_extern_const ::
    Ptr WasmFunc -> IO (Ptr WasmExtern)

foreign import ccall unsafe "wasm_global_as_extern_const"
  wasm_global_as_extern_const ::
    Ptr WasmGlobal -> IO (Ptr WasmExtern)

foreign import ccall unsafe "wasm_table_as_extern_const"
  wasm_table_as_extern_const ::
    Ptr WasmTable -> IO (Ptr WasmExtern)

foreign import ccall unsafe "wasm_memory_as_extern_const"
  wasm_memory_as_extern_const ::
    Ptr WasmMemory -> IO (Ptr WasmExtern)

foreign import ccall unsafe "wasm_extern_as_func_const"
  wasm_extern_as_func_const ::
    Ptr WasmExtern -> IO (Ptr WasmFunc)

foreign import ccall unsafe "wasm_extern_as_global_const"
  wasm_extern_as_global_const ::
    Ptr WasmExtern -> IO (Ptr WasmGlobal)

foreign import ccall unsafe "wasm_extern_as_table_const"
  wasm_extern_as_table_const ::
    Ptr WasmExtern -> IO (Ptr WasmTable)

foreign import ccall unsafe "wasm_extern_as_memory_const"
  wasm_extern_as_memory_const ::
    Ptr WasmExtern -> IO (Ptr WasmMemory)

foreign import ccall unsafe "wasm_instance_delete"
  wasm_instance_delete ::
    Ptr WasmInstance -> IO ()

foreign import ccall unsafe "wasm_instance_copy"
  wasm_instance_copy ::
    Ptr WasmInstance -> IO (Ptr WasmInstance)

foreign import ccall unsafe "wasm_instance_same"
  wasm_instance_same ::
    Ptr WasmInstance -> Ptr WasmInstance -> IO CBool

foreign import ccall unsafe "wasm_instance_get_host_info"
  wasm_instance_get_host_info ::
    Ptr WasmInstance -> IO (Ptr ())

foreign import ccall unsafe "wasm_instance_set_host_info"
  wasm_instance_set_host_info ::
    Ptr WasmInstance -> Ptr () -> IO ()

foreign import ccall unsafe "wasm_instance_set_host_info_with_finalizer"
  wasm_instance_set_host_info_with_finalizer ::
    Ptr WasmInstance -> Ptr () -> FunPtr (Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "wasm_instance_as_ref"
  wasm_instance_as_ref ::
    Ptr WasmInstance -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_instance"
  wasm_ref_as_instance ::
    Ptr WasmRef -> IO (Ptr WasmInstance)

foreign import ccall unsafe "wasm_instance_as_ref_const"
  wasm_instance_as_ref_const ::
    Ptr WasmInstance -> IO (Ptr WasmRef)

foreign import ccall unsafe "wasm_ref_as_instance_const"
  wasm_ref_as_instance_const ::
    Ptr WasmRef -> IO (Ptr WasmInstance)

foreign import ccall safe "wasm_instance_new"
  wasm_instance_new ::
    Ptr WasmStore ->
    Ptr WasmModule ->
    Ptr WasmExternVec ->
    Ptr (Ptr WasmTrap) ->
    IO (Ptr WasmInstance)

foreign import ccall unsafe "wasm_instance_exports"
  wasm_instance_exports ::
    Ptr WasmInstance -> Ptr WasmExternVec -> IO ()

foreign import ccall unsafe "wasi_config_delete"
  wasi_config_delete ::
    Ptr WasiConfig -> IO ()

foreign import ccall unsafe "wasi_config_new"
  wasi_config_new ::
    IO (Ptr WasiConfig)

foreign import ccall unsafe "wasi_config_set_argv"
  wasi_config_set_argv ::
    Ptr WasiConfig -> CInt -> Ptr CString -> IO ()

foreign import ccall unsafe "wasi_config_inherit_argv"
  wasi_config_inherit_argv ::
    Ptr WasiConfig -> IO ()

foreign import ccall unsafe "wasi_config_set_env"
  wasi_config_set_env ::
    Ptr WasiConfig -> CInt -> Ptr CString -> Ptr CString -> IO ()

foreign import ccall unsafe "wasi_config_inherit_env"
  wasi_config_inherit_env ::
    Ptr WasiConfig -> IO ()

foreign import ccall unsafe "wasi_config_set_stdin_file"
  wasi_config_set_stdin_file ::
    Ptr WasiConfig -> CString -> IO CBool

foreign import ccall unsafe "wasi_config_inherit_stdin"
  wasi_config_inherit_stdin ::
    Ptr WasiConfig -> IO ()

foreign import ccall unsafe "wasi_config_set_stdout_file"
  wasi_config_set_stdout_file ::
    Ptr WasiConfig -> CString -> IO CBool

foreign import ccall unsafe "wasi_config_inherit_stdout"
  wasi_config_inherit_stdout ::
    Ptr WasiConfig -> IO ()

foreign import ccall unsafe "wasi_config_set_stderr_file"
  wasi_config_set_stderr_file ::
    Ptr WasiConfig -> CString -> IO CBool

foreign import ccall unsafe "wasi_config_inherit_stderr"
  wasi_config_inherit_stderr ::
    Ptr WasiConfig -> IO ()

foreign import ccall unsafe "wasi_config_preopen_dir"
  wasi_config_preopen_dir ::
    Ptr WasiConfig -> CString -> CString -> IO CBool

foreign import ccall unsafe "wasmtime_config_debug_info_set"
  wasmtime_config_debug_info_set ::
    Ptr WasmConfig -> CBool -> IO ()

foreign import ccall unsafe "wasmtime_config_interruptable_set"
  wasmtime_config_interruptable_set ::
    Ptr WasmConfig -> CBool -> IO ()

foreign import ccall unsafe "wasmtime_config_consume_fuel_set"
  wasmtime_config_consume_fuel_set ::
    Ptr WasmConfig -> CBool -> IO ()

foreign import ccall unsafe "wasmtime_config_max_wasm_stack_set"
  wasmtime_config_max_wasm_stack_set ::
    Ptr WasmConfig -> CSize -> IO ()

foreign import ccall unsafe "wasmtime_config_wasm_threads_set"
  wasmtime_config_wasm_threads_set ::
    Ptr WasmConfig -> CBool -> IO ()

foreign import ccall unsafe "wasmtime_config_wasm_reference_types_set"
  wasmtime_config_wasm_reference_types_set ::
    Ptr WasmConfig -> CBool -> IO ()

foreign import ccall unsafe "wasmtime_config_wasm_simd_set"
  wasmtime_config_wasm_simd_set ::
    Ptr WasmConfig -> CBool -> IO ()

foreign import ccall unsafe "wasmtime_config_wasm_bulk_memory_set"
  wasmtime_config_wasm_bulk_memory_set ::
    Ptr WasmConfig -> CBool -> IO ()

foreign import ccall unsafe "wasmtime_config_wasm_multi_value_set"
  wasmtime_config_wasm_multi_value_set ::
    Ptr WasmConfig -> CBool -> IO ()

foreign import ccall unsafe "wasmtime_config_wasm_multi_memory_set"
  wasmtime_config_wasm_multi_memory_set ::
    Ptr WasmConfig -> CBool -> IO ()

foreign import ccall unsafe "wasmtime_config_wasm_module_linking_set"
  wasmtime_config_wasm_module_linking_set ::
    Ptr WasmConfig -> CBool -> IO ()

foreign import ccall unsafe "wasmtime_config_strategy_set"
  wasmtime_config_strategy_set ::
    Ptr WasmConfig -> WasmtimeStrategy -> IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_config_cranelift_debug_verifier_set"
  wasmtime_config_cranelift_debug_verifier_set ::
    Ptr WasmConfig -> CBool -> IO ()

foreign import ccall unsafe "wasmtime_config_cranelift_opt_level_set"
  wasmtime_config_cranelift_opt_level_set ::
    Ptr WasmConfig -> WasmtimeOptLevel -> IO ()

foreign import ccall unsafe "wasmtime_config_profiler_set"
  wasmtime_config_profiler_set ::
    Ptr WasmConfig -> WasmtimeProfilingStrategy -> IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_config_static_memory_maximum_size_set"
  wasmtime_config_static_memory_maximum_size_set ::
    Ptr WasmConfig -> Word64 -> IO ()

foreign import ccall unsafe "wasmtime_config_static_memory_guard_size_set"
  wasmtime_config_static_memory_guard_size_set ::
    Ptr WasmConfig -> Word64 -> IO ()

foreign import ccall unsafe "wasmtime_config_dynamic_memory_guard_size_set"
  wasmtime_config_dynamic_memory_guard_size_set ::
    Ptr WasmConfig -> Word64 -> IO ()

foreign import ccall unsafe "wasmtime_config_cache_config_load"
  wasmtime_config_cache_config_load ::
    Ptr WasmConfig -> CString -> IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_error_delete"
  wasmtime_error_delete ::
    Ptr WasmtimeError -> IO ()

foreign import ccall unsafe "wasmtime_error_message"
  wasmtime_error_message ::
    Ptr WasmtimeError -> Ptr WasmName -> IO ()

foreign import ccall unsafe "wasmtime_extern_delete"
  wasmtime_extern_delete ::
    Ptr WasmtimeExtern -> IO ()

foreign import ccall unsafe "wasmtime_extern_type"
  wasmtime_extern_type ::
    Ptr WasmtimeContext -> Ptr WasmtimeExtern -> IO (Ptr WasmExterntype)

foreign import ccall unsafe "wasmtime_func_new"
  wasmtime_func_new ::
    Ptr WasmtimeContext ->
    Ptr WasmFunctype ->
    WasmtimeFuncCallback ->
    Ptr () ->
    FunPtr (Ptr () -> IO ()) ->
    Ptr WasmtimeFunc ->
    IO ()

foreign import ccall unsafe "wasmtime_func_type"
  wasmtime_func_type ::
    Ptr WasmtimeContext -> Ptr WasmtimeFunc -> IO (Ptr WasmFunctype)

foreign import ccall safe "wasmtime_func_call"
  wasmtime_func_call ::
    Ptr WasmtimeContext ->
    Ptr WasmtimeFunc ->
    Ptr WasmtimeVal ->
    CSize ->
    Ptr WasmtimeVal ->
    CSize ->
    Ptr (Ptr WasmTrap) ->
    IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_caller_export_get"
  wasmtime_caller_export_get ::
    Ptr WasmtimeCaller -> CString -> CSize -> Ptr WasmtimeExtern -> IO CBool

foreign import ccall unsafe "wasmtime_caller_context"
  wasmtime_caller_context ::
    Ptr WasmtimeCaller -> IO (Ptr WasmtimeContext)

foreign import ccall unsafe "wasmtime_global_new"
  wasmtime_global_new ::
    Ptr WasmtimeContext ->
    Ptr WasmGlobaltype ->
    Ptr WasmtimeVal ->
    Ptr WasmtimeGlobal ->
    IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_global_type"
  wasmtime_global_type ::
    Ptr WasmtimeContext -> Ptr WasmtimeGlobal -> IO (Ptr WasmGlobaltype)

foreign import ccall unsafe "wasmtime_global_get"
  wasmtime_global_get ::
    Ptr WasmtimeContext -> Ptr WasmtimeGlobal -> Ptr WasmtimeVal -> IO ()

foreign import ccall unsafe "wasmtime_global_set"
  wasmtime_global_set ::
    Ptr WasmtimeContext ->
    Ptr WasmtimeGlobal ->
    Ptr WasmtimeVal ->
    IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_instancetype_delete"
  wasmtime_instancetype_delete ::
    Ptr WasmtimeInstancetype -> IO ()

foreign import ccall unsafe "wasmtime_instancetype_exports"
  wasmtime_instancetype_exports ::
    Ptr WasmtimeInstancetype -> Ptr WasmExporttypeVec -> IO ()

foreign import ccall unsafe "wasmtime_instancetype_as_externtype"
  wasmtime_instancetype_as_externtype ::
    Ptr WasmtimeInstancetype -> IO (Ptr WasmExterntype)

foreign import ccall unsafe "wasmtime_externtype_as_instancetype"
  wasmtime_externtype_as_instancetype ::
    Ptr WasmExterntype -> IO (Ptr WasmtimeInstancetype)

foreign import ccall safe "wasmtime_instance_new"
  wasmtime_instance_new ::
    Ptr WasmtimeContext ->
    Ptr WasmtimeModule ->
    Ptr WasmtimeExtern ->
    CSize ->
    Ptr WasmtimeInstance ->
    Ptr (Ptr WasmTrap) ->
    IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_instance_type"
  wasmtime_instance_type ::
    Ptr WasmtimeContext ->
    Ptr WasmtimeInstance ->
    IO (Ptr WasmtimeInstancetype)

foreign import ccall unsafe "wasmtime_instance_export_get"
  wasmtime_instance_export_get ::
    Ptr WasmtimeContext ->
    Ptr WasmtimeInstance ->
    CString ->
    CSize ->
    Ptr WasmtimeExtern ->
    IO CBool

foreign import ccall unsafe "wasmtime_instance_export_nth"
  wasmtime_instance_export_nth ::
    Ptr WasmtimeContext ->
    Ptr WasmtimeInstance ->
    CSize ->
    Ptr CString ->
    Ptr CSize ->
    Ptr WasmtimeExtern ->
    IO CBool

foreign import ccall unsafe "wasmtime_linker_new"
  wasmtime_linker_new ::
    Ptr WasmEngine -> IO (Ptr WasmtimeLinker)

foreign import ccall unsafe "wasmtime_linker_delete"
  wasmtime_linker_delete ::
    Ptr WasmtimeLinker -> IO ()

foreign import ccall unsafe "wasmtime_linker_allow_shadowing"
  wasmtime_linker_allow_shadowing ::
    Ptr WasmtimeLinker -> CBool -> IO ()

foreign import ccall unsafe "wasmtime_linker_define"
  wasmtime_linker_define ::
    Ptr WasmtimeLinker ->
    CString ->
    CSize ->
    CString ->
    CSize ->
    Ptr WasmtimeExtern ->
    IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_linker_define_wasi"
  wasmtime_linker_define_wasi ::
    Ptr WasmtimeLinker -> IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_linker_define_instance"
  wasmtime_linker_define_instance ::
    Ptr WasmtimeLinker ->
    Ptr WasmtimeContext ->
    CString ->
    CSize ->
    Ptr WasmtimeInstance ->
    IO (Ptr WasmtimeError)

foreign import ccall safe "wasmtime_linker_instantiate"
  wasmtime_linker_instantiate ::
    Ptr WasmtimeLinker ->
    Ptr WasmtimeContext ->
    Ptr WasmtimeModule ->
    Ptr WasmtimeInstance ->
    Ptr (Ptr WasmTrap) ->
    IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_linker_module"
  wasmtime_linker_module ::
    Ptr WasmtimeLinker ->
    Ptr WasmtimeContext ->
    CString ->
    CSize ->
    Ptr WasmtimeModule ->
    IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_linker_get_default"
  wasmtime_linker_get_default ::
    Ptr WasmtimeLinker ->
    Ptr WasmtimeContext ->
    CString ->
    CSize ->
    Ptr WasmtimeFunc ->
    IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_linker_get"
  wasmtime_linker_get ::
    Ptr WasmtimeLinker ->
    Ptr WasmtimeContext ->
    CString ->
    CSize ->
    CString ->
    CSize ->
    Ptr WasmtimeExtern ->
    IO CBool

foreign import ccall unsafe "wasmtime_memory_new"
  wasmtime_memory_new ::
    Ptr WasmtimeContext ->
    Ptr WasmMemorytype ->
    Ptr WasmtimeMemory ->
    IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_memory_type"
  wasmtime_memory_type ::
    Ptr WasmtimeContext -> Ptr WasmtimeMemory -> IO (Ptr WasmMemorytype)

foreign import ccall unsafe "wasmtime_memory_data"
  wasmtime_memory_data ::
    Ptr WasmtimeContext -> Ptr WasmtimeMemory -> IO (Ptr Word8)

foreign import ccall unsafe "wasmtime_memory_data_size"
  wasmtime_memory_data_size ::
    Ptr WasmtimeContext -> Ptr WasmtimeMemory -> IO CSize

foreign import ccall unsafe "wasmtime_memory_size"
  wasmtime_memory_size ::
    Ptr WasmtimeContext -> Ptr WasmtimeMemory -> IO Word32

foreign import ccall unsafe "wasmtime_memory_grow"
  wasmtime_memory_grow ::
    Ptr WasmtimeContext ->
    Ptr WasmtimeMemory ->
    Word32 ->
    Ptr Word32 ->
    IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_moduletype_delete"
  wasmtime_moduletype_delete ::
    Ptr WasmtimeModuletype -> IO ()

foreign import ccall unsafe "wasmtime_moduletype_imports"
  wasmtime_moduletype_imports ::
    Ptr WasmtimeModuletype -> Ptr WasmImporttypeVec -> IO ()

foreign import ccall unsafe "wasmtime_moduletype_exports"
  wasmtime_moduletype_exports ::
    Ptr WasmtimeModuletype -> Ptr WasmExporttypeVec -> IO ()

foreign import ccall unsafe "wasmtime_moduletype_as_externtype"
  wasmtime_moduletype_as_externtype ::
    Ptr WasmtimeModuletype -> IO (Ptr WasmExterntype)

foreign import ccall unsafe "wasmtime_externtype_as_moduletype"
  wasmtime_externtype_as_moduletype ::
    Ptr WasmExterntype -> IO (Ptr WasmtimeModuletype)

foreign import ccall unsafe "wasmtime_module_new"
  wasmtime_module_new ::
    Ptr WasmEngine ->
    Ptr Word8 ->
    CSize ->
    Ptr (Ptr WasmtimeModule) ->
    IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_module_delete"
  wasmtime_module_delete ::
    Ptr WasmtimeModule -> IO ()

foreign import ccall unsafe "wasmtime_module_clone"
  wasmtime_module_clone ::
    Ptr WasmtimeModule -> IO (Ptr WasmtimeModule)

foreign import ccall unsafe "wasmtime_module_validate"
  wasmtime_module_validate ::
    Ptr WasmEngine -> Ptr Word8 -> CSize -> IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_module_type"
  wasmtime_module_type ::
    Ptr WasmtimeModule -> IO (Ptr WasmtimeModuletype)

foreign import ccall unsafe "wasmtime_module_serialize"
  wasmtime_module_serialize ::
    Ptr WasmtimeModule -> Ptr WasmByteVec -> IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_module_deserialize"
  wasmtime_module_deserialize ::
    Ptr WasmEngine ->
    Ptr Word8 ->
    CSize ->
    Ptr (Ptr WasmtimeModule) ->
    IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_store_new"
  wasmtime_store_new ::
    Ptr WasmEngine ->
    Ptr () ->
    FunPtr (Ptr () -> IO ()) ->
    IO (Ptr WasmtimeStore)

foreign import ccall unsafe "wasmtime_store_context"
  wasmtime_store_context ::
    Ptr WasmtimeStore -> IO (Ptr WasmtimeContext)

foreign import ccall unsafe "wasmtime_store_delete"
  wasmtime_store_delete ::
    Ptr WasmtimeStore -> IO ()

foreign import ccall unsafe "wasmtime_context_get_data"
  wasmtime_context_get_data ::
    Ptr WasmtimeContext -> IO (Ptr ())

foreign import ccall unsafe "wasmtime_context_set_data"
  wasmtime_context_set_data ::
    Ptr WasmtimeContext -> Ptr () -> IO ()

foreign import ccall unsafe "wasmtime_context_gc"
  wasmtime_context_gc ::
    Ptr WasmtimeContext -> IO ()

foreign import ccall unsafe "wasmtime_context_add_fuel"
  wasmtime_context_add_fuel ::
    Ptr WasmtimeContext -> Word64 -> IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_context_fuel_consumed"
  wasmtime_context_fuel_consumed ::
    Ptr WasmtimeContext -> Ptr Word64 -> IO CBool

foreign import ccall unsafe "wasmtime_context_set_wasi"
  wasmtime_context_set_wasi ::
    Ptr WasmtimeContext -> Ptr WasiConfig -> IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_interrupt_handle_new"
  wasmtime_interrupt_handle_new ::
    Ptr WasmtimeContext -> IO (Ptr WasmtimeInterruptHandle)

foreign import ccall unsafe "wasmtime_interrupt_handle_interrupt"
  wasmtime_interrupt_handle_interrupt ::
    Ptr WasmtimeInterruptHandle -> IO ()

foreign import ccall unsafe "wasmtime_interrupt_handle_delete"
  wasmtime_interrupt_handle_delete ::
    Ptr WasmtimeInterruptHandle -> IO ()

foreign import ccall unsafe "wasmtime_table_new"
  wasmtime_table_new ::
    Ptr WasmtimeContext ->
    Ptr WasmTabletype ->
    Ptr WasmtimeVal ->
    Ptr WasmtimeTable ->
    IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_table_type"
  wasmtime_table_type ::
    Ptr WasmtimeContext -> Ptr WasmtimeTable -> IO (Ptr WasmTabletype)

foreign import ccall unsafe "wasmtime_table_get"
  wasmtime_table_get ::
    Ptr WasmtimeContext ->
    Ptr WasmtimeTable ->
    Word32 ->
    Ptr WasmtimeVal ->
    IO CBool

foreign import ccall unsafe "wasmtime_table_set"
  wasmtime_table_set ::
    Ptr WasmtimeContext ->
    Ptr WasmtimeTable ->
    Word32 ->
    Ptr WasmtimeVal ->
    IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_table_size"
  wasmtime_table_size ::
    Ptr WasmtimeContext -> Ptr WasmtimeTable -> IO Word32

foreign import ccall unsafe "wasmtime_table_grow"
  wasmtime_table_grow ::
    Ptr WasmtimeContext ->
    Ptr WasmtimeTable ->
    Word32 ->
    Ptr WasmtimeVal ->
    Ptr Word32 ->
    IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_trap_new"
  wasmtime_trap_new ::
    CString -> CSize -> IO (Ptr WasmTrap)

foreign import ccall unsafe "wasmtime_trap_exit_status"
  wasmtime_trap_exit_status ::
    Ptr WasmTrap -> Ptr CInt -> IO CBool

foreign import ccall unsafe "wasmtime_frame_func_name"
  wasmtime_frame_func_name ::
    Ptr WasmFrame -> IO (Ptr WasmName)

foreign import ccall unsafe "wasmtime_frame_module_name"
  wasmtime_frame_module_name ::
    Ptr WasmFrame -> IO (Ptr WasmName)

foreign import ccall unsafe "wasmtime_externref_new"
  wasmtime_externref_new ::
    Ptr () -> FunPtr (Ptr () -> IO ()) -> IO (Ptr WasmtimeExternref)

foreign import ccall unsafe "wasmtime_externref_data"
  wasmtime_externref_data ::
    Ptr WasmtimeExternref -> IO (Ptr ())

foreign import ccall unsafe "wasmtime_externref_clone"
  wasmtime_externref_clone ::
    Ptr WasmtimeExternref -> IO (Ptr WasmtimeExternref)

foreign import ccall unsafe "wasmtime_externref_delete"
  wasmtime_externref_delete ::
    Ptr WasmtimeExternref -> IO ()

foreign import ccall unsafe "wasmtime_val_delete"
  wasmtime_val_delete ::
    Ptr WasmtimeVal -> IO ()

foreign import ccall unsafe "wasmtime_val_copy"
  wasmtime_val_copy ::
    Ptr WasmtimeVal -> Ptr WasmtimeVal -> IO ()

foreign import ccall unsafe "wasmtime_wat2wasm"
  wasmtime_wat2wasm ::
    CString -> CSize -> Ptr WasmByteVec -> IO (Ptr WasmtimeError)
