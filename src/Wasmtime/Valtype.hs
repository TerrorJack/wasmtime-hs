module Wasmtime.Valtype where

import System.IO.Unsafe
import UnliftIO.Foreign
import qualified Wasmtime.Raw as Raw

data Valtype
  = I32
  | I64
  | F32
  | F64
  | Anyref
  | Funcref
  | V128

toWasmValtype :: Valtype -> Ptr Raw.WasmValtype
toWasmValtype I32 = pI32
toWasmValtype I64 = pI64
toWasmValtype F32 = pF32
toWasmValtype F64 = pF64
toWasmValtype Anyref = pAnyref
toWasmValtype Funcref = pFuncref
toWasmValtype V128 = pV128

{-# NOINLINE pI32 #-}
pI32 :: Ptr Raw.WasmValtype
pI32 = unsafePerformIO $ Raw.wasm_valtype_new Raw.wasmI32

{-# NOINLINE pI64 #-}
pI64 :: Ptr Raw.WasmValtype
pI64 = unsafePerformIO $ Raw.wasm_valtype_new Raw.wasmI64

{-# NOINLINE pF32 #-}
pF32 :: Ptr Raw.WasmValtype
pF32 = unsafePerformIO $ Raw.wasm_valtype_new Raw.wasmF32

{-# NOINLINE pF64 #-}
pF64 :: Ptr Raw.WasmValtype
pF64 = unsafePerformIO $ Raw.wasm_valtype_new Raw.wasmF64

{-# NOINLINE pAnyref #-}
pAnyref :: Ptr Raw.WasmValtype
pAnyref = unsafePerformIO $ Raw.wasm_valtype_new Raw.wasmAnyref

{-# NOINLINE pFuncref #-}
pFuncref :: Ptr Raw.WasmValtype
pFuncref = unsafePerformIO $ Raw.wasm_valtype_new Raw.wasmFuncref

{-# NOINLINE pV128 #-}
pV128 :: Ptr Raw.WasmValtype
pV128 = unsafePerformIO $ Raw.wasm_valtype_new Raw.wasmV128
