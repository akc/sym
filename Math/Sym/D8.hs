-- |
-- Module      : Math.Sym.D8
-- Copyright   : (c) Anders Claesson 2012
-- License     : BSD-style
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- 
-- The dihedral group of order 8 acting on permutations.
-- 
-- To avoid name clashes this module is best imported @qualified@;
-- e.g.
-- 
-- > import qualified Math.Sym.D8 as D8
-- 

module Math.Sym.D8
    (
    -- * The group elements
      r0            -- :: Perm a => a -> a
    , r1            -- :: Perm a => a -> a
    , r2            -- :: Perm a => a -> a
    , r3            -- :: Perm a => a -> a
    , s0            -- :: Perm a => a -> a
    , s1            -- :: Perm a => a -> a
    , s2            -- :: Perm a => a -> a
    , s3            -- :: Perm a => a -> a

    -- * D8, the klein four-group, and orbits
    , d8            -- :: Perm a => [a -> a]
    , klein4        -- :: Perm a => [a -> a]
    , orbit         -- :: Ord a => Perm a => [a -> a] -> a -> [a]

    -- * Aliases
    , id            -- :: Perm a => a -> a
    , rotate        -- :: Perm a => a -> a
    , complement    -- :: Perm a => a -> a
    , reverse       -- :: Perm a => a -> a
    , inverse       -- :: Perm a => a -> a
    ) where

import Prelude hiding (reverse, id)
import Data.List (group, sort)
import Math.Sym (Perm (size), fromVector, act)
import qualified Math.Sym (inverse)
import Math.Sym.Internal (revIdperm)

-- The group elements
-- ------------------

r0, r1, r2, r3, s0, s1, s2, s3 :: Perm a => a -> a

-- | Ration by 0 degrees, i.e. the identity map.
r0 w = w

-- | Ration by 90 degrees clockwise.
r1 = s2 . s1

-- | Ration by 2*90 = 180 degrees clockwise.
r2 = r1 . r1

-- | Ration by 3*90 = 270 degrees clockwise.
r3 = r2 . r1

-- | Reflection through a horizontal axis (also called 'complement').
s0 = r1 . s2

-- | Reflection through a vertical axis (also called 'reverse').
s1 w = (fromVector . revIdperm . size) w `act` w

-- | Reflection through the main diagonal (also called 'inverse').
s2 = Math.Sym.inverse

-- | Reflection through the anti-diagonal.
s3 = s1 . r1

-- D8, the klein four-group, and orbits
-- ------------------------------------

d8, klein4 :: Perm a => [a -> a]

-- | The dihedral group of order 8 (the symmetries of a square); that is,
-- 
-- > d8 = [r0, r1, r2, r3, s0, s1, s2, s3]
-- 
d8 = [r0, r1, r2, r3, s0, s1, s2, s3]

-- | The Klein four-group (the symmetries of a non-equilateral
-- rectangle); that is,
-- 
-- > klein4 = [r0, r2, s0, s1]
-- 
klein4 = [r0, r2, s0, s1]

-- | @orbit fs x@ is the orbit of @x@ under the functions in @fs@. E.g.,
-- 
-- > orbit klein4 "2314" == ["1423","2314","3241","4132"]
-- 
orbit :: Ord a => Perm a => [a -> a] -> a -> [a]
orbit fs x = map head . group $ sort [ f x | f <- fs ]

-- Aliases
-- -------

id, rotate, complement, reverse, inverse :: Perm a => a -> a

-- | @id = r0@
id = r0

-- | @rotate = r1@
rotate = r1

-- | @complement = s0@
complement = s0

-- | @reverse = s1@
reverse = s1

-- | @inverse = s2@
inverse = s2
