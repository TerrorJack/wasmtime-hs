module Wasmtime.Trap where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import UnliftIO
import UnliftIO.Foreign
import Wasmtime.ByteVec
import Wasmtime.Error
import Wasmtime.Frame
import Wasmtime.Raw
import Wasmtime.Vec

data Trap = Trap
  { wasmTrap :: !(ForeignPtr WasmTrap),
    message :: !ByteString,
    trace :: !(V.Vector Frame),
    exitStatus :: !(Maybe Int)
  }
  deriving (Show)

instance Exception Trap

fromWasmTrap :: Ptr WasmTrap -> IO Trap
fromWasmTrap p_t = do
  checkNull p_t
  fp_t <- newForeignPtr p_wasm_trap_delete p_t
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
  pure $ Trap fp_t _msg _t _s

newTrap :: ByteString -> IO Trap
newTrap bs_msg = asWasmByteVec bs_msg $
  \p_msg msg_len -> fromWasmTrap =<< wasmtime_trap_new p_msg msg_len

foreign import ccall unsafe "&wasm_trap_delete"
  p_wasm_trap_delete ::
    FinalizerPtr WasmTrap
