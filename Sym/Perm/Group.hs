{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--

module Sym.Perm.Group
    (
      compose
    , act
    ) where

import Sym.Perm
import Foreign
import Foreign.C.Types
import System.IO.Unsafe

marshal :: (Ptr CLong -> Ptr CLong -> CLong -> Ptr CLong -> CLong -> IO ())
        -> Perm -> Perm -> Perm
marshal op u v =
    unsafePerformIO $
    unsafeWith u $ \u' ->
    unsafeWith v $ \v' -> do
      let k = size u
      let n = size v
      let m = max k n
      unsafeNew m $ \p -> op p u' (fromIntegral k) v' (fromIntegral n)
{-# INLINE marshal #-}

foreign import ccall unsafe "group.h compose" c_compose
    :: Ptr CLong -> Ptr CLong -> CLong -> Ptr CLong -> CLong -> IO ()

-- | The product/composition of @u@ and @v@: if @w = u `compose` v@
-- then @w `at ` i = v \`at\` (u \`at\` i)@.
compose :: Perm -> Perm -> Perm
compose = marshal c_compose
{-# INLINE compose #-}

foreign import ccall unsafe "group.h act" c_act
    :: Ptr CLong -> Ptr CLong -> CLong -> Ptr CLong -> CLong -> IO ()

-- | The (left) group action of Perm on itself: if @w = u `act` v@
-- then @w `at ` (u \`at\` i) = v \`at\` i@.
act :: Perm -> Perm -> Perm
act = marshal c_act
{-# INLINE act #-}
