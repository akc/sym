{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--

module Sym.Perm.D8
    (
    -- * The group elements
      r0, r1, r2, r3
    , s0, s1, s2, s3

    -- * D8, the klein four-group, and orbits
    , d8
    , klein4
    , orbit
    , symmetryClasses
    , d8Classes
    , klein4Classes

    -- * Aliases
    , rotate
    , complement
    , reverse
    , inverse
    ) where

import Prelude           hiding (reverse)
import Data.List         hiding (reverse)
import Sym.Internal.Util
import Sym.Perm
import Foreign           hiding (complement, rotate)
import Foreign.C.Types
import System.IO.Unsafe


-- The group elements
-- ------------------

-- | Ration by 0 degrees, i.e. the identity map.
r0 :: Perm -> Perm
r0 w = w

-- | Ration by 90 degrees clockwise.
r1 :: Perm -> Perm
r1 = s2 . s1

-- | Ration by 2*90 = 180 degrees clockwise.
r2 :: Perm -> Perm
r2 = r1 . r1

-- | Ration by 3*90 = 270 degrees clockwise.
r3 :: Perm -> Perm
r3 = r2 . r1

-- | Reflection through a horizontal axis (also called 'complement').
s0 :: Perm -> Perm
s0 = complement

-- | Reflection through a vertical axis (also called 'reverse').
s1 :: Perm -> Perm
s1 = reverse

-- | Reflection through the main diagonal (also called 'inverse').
s2 :: Perm -> Perm
s2 = inverse

-- | Reflection through the anti-diagonal.
s3 :: Perm -> Perm
s3 = s1 . r1


-- D8, the klein four-group, and orbits
-- ------------------------------------

-- | The dihedral group of order 8 (the symmetries of a square); that is,
-- 
-- > d8 = [r0, r1, r2, r3, s0, s1, s2, s3]
-- 
d8 :: [Perm -> Perm]
d8 = [r0, r1, r2, r3, s0, s1, s2, s3]

-- | The Klein four-group (the symmetries of a non-equilateral
-- rectangle); that is,
-- 
-- > klein4 = [r0, r2, s0, s1]
-- 
klein4 :: [Perm -> Perm]
klein4 = [r0, r2, s0, s1]

-- | @orbit fs x@ is the orbit of @x@ under the /group/ of function @fs@. E.g.,
-- 
-- > orbit klein4 "2314" == ["1423","2314","3241","4132"]
-- 
orbit :: [Perm -> Perm] -> Perm -> [Perm]
orbit fs x = nubSort [ f x | f <- fs ]

-- | @symmetryClasses fs xs@ is the list of equivalence classes under
-- the action of the /group/ of functions @fs@.
symmetryClasses :: [Perm -> Perm] -> [Perm] -> [[Perm]]
symmetryClasses _  [] = []
symmetryClasses fs xs@(x:xt) = insert orb $ symmetryClasses fs ys
    where
      orb = [ w | w <- orbit fs x, w `elem` xs ]
      ys  = [ y | y <- xt, y `notElem` orb ]

-- | Symmetry classes with respect to D8.
d8Classes :: [Perm] -> [[Perm]]
d8Classes = symmetryClasses d8

-- | Symmetry classes with respect to Klein4
klein4Classes :: [Perm] -> [[Perm]]
klein4Classes = symmetryClasses klein4


-- Aliases
-- -------

marshal :: (Ptr CLong -> Ptr CLong -> CLong -> IO ()) -> Perm -> Perm
marshal op w =
    unsafeDupablePerformIO . unsafeWith w $ \p -> do
      let n = size w
      unsafeNew n $ \q -> op q p (fromIntegral n)
{-# INLINE marshal #-}

foreign import ccall unsafe "d8.h inverse" c_inverse
    :: Ptr CLong -> Ptr CLong -> CLong -> IO ()

-- | The group theoretical inverse: if @v = inverse u@ then
-- @v \`at\` (u \`at\` i) = i@.
inverse :: Perm -> Perm
inverse = marshal c_inverse
{-# INLINE inverse #-}

foreign import ccall unsafe "d8.h reverse" c_reverse
    :: Ptr CLong -> Ptr CLong -> CLong -> IO ()

-- | The reverse of the given permutation: if @v = reverse u@ then
-- @v \`at\` i = u \`at\` (n-1-i)@.
reverse :: Perm -> Perm
reverse = marshal c_reverse
{-# INLINE reverse #-}

foreign import ccall unsafe "d8.h complement" c_complement
    :: Ptr CLong -> Ptr CLong -> CLong -> IO ()

-- | The complement of the given permutation: if @v = complement u@ then
-- @v \`at\` i = n - 1 - u \`at\` i@.
complement :: Perm -> Perm
complement = marshal c_complement
{-# INLINE complement #-}

-- | @rotate = r1 = inverse . reverse@
rotate :: Perm -> Perm
rotate = r1
