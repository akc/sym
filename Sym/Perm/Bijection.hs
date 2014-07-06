{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--

module Sym.Perm.Bijection
    (
      simionSchmidt
    , simionSchmidt'
    ) where

import Sym.Perm
import Foreign
import Foreign.C.Types
import System.IO.Unsafe

foreign import ccall unsafe "bij.h simion_schmidt" c_simion_schmidt
    :: Ptr CLong -> Ptr CLong -> CLong -> IO ()

foreign import ccall unsafe "bij.h simion_schmidt_inverse" c_simion_schmidt'
    :: Ptr CLong -> Ptr CLong -> CLong -> IO ()

marshal :: (Ptr CLong -> Ptr CLong -> CLong -> IO ()) -> Perm -> Perm
marshal bij w =
    unsafeDupablePerformIO . unsafeWith w $ \p -> do
      let n = size w
      unsafeNew n $ \q -> bij q p (fromIntegral n)
{-# INLINE marshal #-}

-- | The Simion-Schmidt bijection from Av(123) onto Av(132).
simionSchmidt :: Perm -> Perm
simionSchmidt = marshal c_simion_schmidt

-- | The inverse of the Simion-Schmidt bijection. It is a function
-- from Av(132) to Av(123).
simionSchmidt' :: Perm -> Perm
simionSchmidt' = marshal c_simion_schmidt'
