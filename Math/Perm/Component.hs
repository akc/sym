-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--
-- Components of permutations.
-- 

module Math.Perm.Component
    (
      components
    , skewComponents
    , leftMaxima
    , leftMinima
    , rightMaxima
    , rightMinima
    ) where

import Foreign
import System.IO.Unsafe
import Data.Perm
import qualified Math.Perm.D8 as D8

-- Positions /i/ such that /max{ w[j] : j <= i } = i/. These positions
-- mark the boundaries of components.
comps :: Perm -> [Int]
comps w = unsafeDupablePerformIO . unsafeWith w $ go [] 0 0
    where
      n = size w
      go ks m i p
        | i >= n = return (reverse ks)
        | otherwise =
            do y <- fromIntegral `fmap` peek p
               let p'  = advancePtr p 1
               let i'  = i+1
               let m'  = if y > m then y else m
               let ks' = if m' == i then i:ks else ks
               go ks' m' i' p'

-- | The list of (plus) components.
components :: Perm -> [Perm]
components w =
    let ds = 0 : map (+1) (comps w)
        ks = zipWith (-) (tail ds) ds
        ws = unsafeSlice ks w
    in zipWith (\d v -> imap (\_ x -> x - fromIntegral d) v) ds ws

-- | The list of skew components, also called minus components.
skewComponents :: Perm -> [Perm]
skewComponents = map D8.complement . components . D8.complement

records :: (a -> a -> Bool) -> [a] -> [a]
records _ []     = []
records f (x:xs) = recs [x] xs
    where
      recs rs@(r:_) (y:ys) = recs ((if f r y then y else r):rs) ys
      recs rs       _      = rs

-- | For each position, left-to-right, records the largest value seen
-- thus far.
leftMaxima :: Perm -> [Int]
leftMaxima w = reverse $ records (<) (toList w)

-- | For each position, left-to-right, records the smallest value seen
-- thus far.
leftMinima :: Perm -> [Int]
leftMinima w = reverse $ records (>) (toList w)

-- | For each position, /right-to-left/, records the largest value seen
-- thus far.
rightMaxima :: Perm -> [Int]
rightMaxima w = records (<) (reverse (toList w))

-- | For each position, /right-to-left/, records the smallest value seen
-- thus far.
rightMinima :: Perm -> [Int]
rightMinima w = records (>) (reverse (toList w))
