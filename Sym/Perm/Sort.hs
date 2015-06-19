{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--

module Sym.Perm.Sort
    (
      stackSort
    , bubbleSort
    ) where

import Sym.Perm
import Foreign
import Foreign.C.Types
import System.IO.Unsafe

foreign import ccall unsafe "sortop.h stacksort" c_stacksort
    :: Ptr CLong -> Ptr CLong -> CLong -> IO ()

foreign import ccall unsafe "sortop.h bubblesort" c_bubblesort
    :: Ptr CLong -> Ptr CLong -> CLong -> IO ()

marshal :: (Ptr CLong -> Ptr CLong -> CLong -> IO ()) -> Perm -> Perm
marshal op w =
    unsafePerformIO . unsafeWith w $ \p -> do
      let n = size w
      unsafeNew n $ \q -> op q p (fromIntegral n)
{-# INLINE marshal #-}

-- | One pass of stack-sort.
stackSort :: Perm -> Perm
stackSort = marshal c_stacksort
{-# INLINE stackSort #-}

-- | One pass of bubble-sort.
bubbleSort :: Perm -> Perm
bubbleSort = marshal c_bubblesort
{-# INLINE bubbleSort #-}
