module Wasmtime.Frame where

import Data.ByteString (ByteString)
import UnliftIO.Foreign
import Wasmtime.ByteVec
import Wasmtime.Raw

data Frame = Frame
  { funcIndex :: !Word32,
    funcOffset :: !Word,
    moduleOffset :: !Word,
    funcName :: !ByteString,
    moduleName :: !ByteString
  }
  deriving (Show)

fromWasmFrame :: Ptr WasmFrame -> IO Frame
fromWasmFrame p_f = do
  func_index <- wasm_frame_func_index p_f
  func_offset <- fromIntegral <$> wasm_frame_func_offset p_f
  module_offset <- fromIntegral <$> wasm_frame_module_offset p_f
  fp_f <- newForeignPtr p_wasm_frame_delete p_f
  p_func_name <- castPtr <$> wasmtime_frame_func_name p_f
  func_name <-
    if p_func_name == nullPtr
      then pure mempty
      else fromOwnedWasmByteVec fp_f p_func_name
  p_module_name <- castPtr <$> wasmtime_frame_module_name p_f
  module_name <-
    if p_module_name == nullPtr
      then pure mempty
      else fromOwnedWasmByteVec fp_f p_module_name
  pure $ Frame func_index func_offset module_offset func_name module_name

foreign import ccall unsafe "&wasm_frame_delete"
  p_wasm_frame_delete ::
    FinalizerPtr WasmFrame
