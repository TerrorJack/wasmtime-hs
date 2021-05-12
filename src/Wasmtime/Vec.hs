module Wasmtime.Vec where

import Control.Monad.Cont
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

asWasmVec ::
  Storable v =>
  (CSize -> Ptr (Ptr w) -> v) ->
  (e -> (Ptr w -> IO r) -> IO r) ->
  V.Vector e ->
  (Ptr v -> IO r) ->
  IO r
asWasmVec v_constr e_as es v_with =
  runContT (GV.unstreamM $ BV.mapM (ContT . e_as) $ BV.reVector $ GV.stream es) $
    \sv -> SV.unsafeWith sv $
      \buf_p -> with (v_constr (fromIntegral $ SV.length sv) buf_p) v_with
