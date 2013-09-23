{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--

module Math.Perm.Simple
    (
     simple
    ) where

import Data.Perm
import Foreign
import Foreign.C.Types
import System.IO.Unsafe

foreign import ccall unsafe "simple.h simple" c_simple
    :: Ptr CLong -> CLong -> CInt

-- | Is the permutation simple?
simple :: Perm -> Bool
simple w = toBool . unsafeDupablePerformIO $
    let n = fromIntegral (size w)
    in unsafeWith w $ \ptr -> return $ c_simple ptr n
