{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--

module Math.Perm.Pattern
    (
      Pattern
    , Set
    , ordiso
    , subsets
    , copiesOf
    , contains
    , avoids
    , avoidsAll
    , avoiders
    , minima
    , maxima
    , coeff
    ) where

import Data.Perm (Perm, perms)
import Data.Perm.Internal
import Data.CLongArray
import Foreign
import Foreign.C.Types
import System.IO.Unsafe

-- | Pattern is just an alias for permutation.
type Pattern = Perm

foreign import ccall unsafe "ordiso.h ordiso" c_ordiso
    :: Ptr CLong -> Ptr CLong -> Ptr CLong -> CLong -> CInt

-- | @ordiso u v m@ determines whether the subword in @v@ specified by
-- @m@ is order isomorphic to @u@.
ordiso :: Perm -> Perm -> Set -> Bool
ordiso u v m =
    let k = fromIntegral (size u)
    in unsafeDupablePerformIO $
       unsafeWith u $ \u' ->
       unsafeWith v $ \v' ->
       unsafeWith m $ \m' ->
           return . toBool $ c_ordiso u' v' m' k
{-# INLINE ordiso #-}

-- | @copies p w@ is the list of sets that represent copies of @p@ in @w@.
copiesOf :: Pattern -> Perm -> [Set]
copiesOf p w = filter (ordiso p w) $ subsets (size w) (size p)
{-# INLINE copiesOf #-}

-- | @w `contains` p@ is a predicate determining if @w@ contains the pattern @p@.
contains :: Perm -> Pattern -> Bool
w `contains` p = not $ w `avoids` p

-- | @w `avoids` p@ is a predicate determining if @w@ avoids the pattern @p@.
avoids :: Perm -> Pattern -> Bool
w `avoids` p = null $ copiesOf p w

-- | @w `avoidsAll` ps@ is a predicate determining if @w@ avoids the patterns @ps@.
avoidsAll :: Perm -> [Pattern] -> Bool
w `avoidsAll` ps = all (w `avoids`) ps

-- | @avoiders ps ws@ is the list of permutations in @ws@ avoiding the
-- patterns in @ps@.
avoiders :: [Pattern] -> [Perm] -> [Perm]
avoiders ps ws = foldl (flip avoiders1) ws ps

-- @avoiders1 p ws@ is the list of permutations in @ws@ avoiding the
-- pattern @p@.
avoiders1 :: Pattern -> [Perm] -> [Perm]
avoiders1 _ [] = []
avoiders1 q vs@(v:_) = filter avoids_q us ++ filter (`avoids` q) ws
    where
      n = size v
      k = size q
      (us, ws) = span (\u -> size u == n) vs
      xs = subsets n k
      avoids_q u = not $ any (ordiso q u) xs

-- | The set of minimal elements with respect to containment.
minima :: [Pattern] -> [Pattern]
minima [] = []
minima ws = let (v:vs) = normalize ws
            in v : minima (avoiders [v] vs)

-- | The set of maximal elements with respect to containment.
maxima :: [Pattern] -> [Pattern]
maxima [] = []
maxima ws = let (v:vs) = reverse $ normalize ws
            in v : maxima (filter (avoids v) vs)

-- | @coeff f v@ is the coefficient of @v@ when expanding the
-- permutation statistic @f@ as a sum of permutations/patterns. See
-- Petter Brändén and Anders Claesson: Mesh patterns and the expansion
-- of permutation statistics as sums of permutation patterns, The
-- Electronic Journal of Combinatorics 18(2) (2011),
-- <http://www.combinatorics.org/ojs/index.php/eljc/article/view/v18i2p5>.
coeff :: (Pattern -> Int) -> Pattern -> Int
coeff f v = f v + sum [ (-1)^(k - j) * c * f u |
                        j <- [0 .. k-1]
                      , u <- perms j
                      , let c = length $ copiesOf u v
                      , c > 0
                      ] where k = size v
