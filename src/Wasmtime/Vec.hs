module Wasmtime.Vec where

import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Bundle as BV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Storable as SV
import UnliftIO
import UnliftIO.Foreign

fromWasmVec ::
  (b -> CSize) -> (b -> Ptr (Ptr a)) -> IO () -> b -> IO (SV.Vector (Ptr a))
fromWasmVec b_size b_data b_fin b = do
  fp <- newGHCForeignPtr (b_data b) b_fin
  evaluate $ SV.unsafeFromForeignPtr0 fp $ fromIntegral $ b_size b

fromWasmSV :: (Ptr a -> IO b) -> SV.Vector (Ptr a) -> IO (V.Vector b)
fromWasmSV f sv = do
  v <- GV.unstreamM $ BV.mapM f $ BV.reVector $ GV.stream sv
  finalizeForeignPtr $ case SV.unsafeToForeignPtr sv of
    (fp, _, _) -> fp
  pure v
