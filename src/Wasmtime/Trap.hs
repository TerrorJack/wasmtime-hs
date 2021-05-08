module Wasmtime.Trap where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import UnliftIO
import UnliftIO.Foreign
import Wasmtime.ByteVec
import Wasmtime.Frame
import Wasmtime.Raw
import Wasmtime.Vec

data Trap = Trap
  { message :: !ByteString,
    trace :: !(V.Vector Frame),
    exitStatus :: !(Maybe Int)
  }
  deriving (Show)

instance Exception Trap

fromWasmTrap :: Ptr WasmTrap -> IO Trap
fromWasmTrap p_t = do
  _msg <- alloca $ \p_msg -> do
    wasm_trap_message p_t p_msg
    BS.init <$> fromWasmByteVec (castPtr p_msg)
  _fv <- alloca $ \p_v -> do
    wasm_trap_trace p_t p_v
    peek p_v
  _sv <-
    fromWasmVec
      (\(WasmFrameVec _size _) -> _size)
      (\(WasmFrameVec _ _data) -> _data)
      (with _fv wasm_frame_vec_delete)
      _fv
  _t <- fromWasmSV fromWasmFrame _sv
  _s <- alloca $ \p_s -> do
    _f <- toBool <$> wasmtime_trap_exit_status p_t p_s
    if _f then Just . fromIntegral <$> peek p_s else pure Nothing
  wasm_trap_delete p_t
  pure $ Trap _msg _t _s
