module Wasmtime.Internal where

import GHC.ForeignPtr (ForeignPtr (..))
import GHC.Ptr (Ptr (..))

setPtr :: ForeignPtr a -> Ptr b -> ForeignPtr b
setPtr (ForeignPtr _ c) (Ptr addr) = ForeignPtr addr c
