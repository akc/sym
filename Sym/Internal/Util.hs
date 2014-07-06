-- |
-- Copyright   : Anders Claesson 2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--

module Sym.Internal.Util
    (
      minima
    , maxima
    , kSubsets
    , powerset
    , nubSort
    ) where

import Data.List
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S

-- | The set of minimal elements with respect to inclusion.
minima :: Ord a => [Set a] -> [Set a]
minima = minima' . sortBy (comparing S.size)
  where
    minima' [] = []
    minima' (x:xs) = x : minima' [ y | y<-xs, not (x `S.isSubsetOf` y) ]

-- | The set of maximal elements with respect to the given order.
maxima :: Ord a => [Set a] -> [Set a]
maxima = maxima' . sortBy (comparing $ \x -> -S.size x)
  where
    maxima' [] = []
    maxima' (x:xs) = x : maxima' [ y | y<-xs, not (y `S.isSubsetOf` x) ]

-- | A list of all k element subsets of the given set.
kSubsets :: Ord a => Int -> Set a -> [Set a]
kSubsets 0 _    = [ S.empty ]
kSubsets k s 
    | S.null s  = []
    | otherwise = kSubsets k t ++ map (S.insert x) (kSubsets (k-1) t)
  where
    (x,t) = S.deleteFindMin s

-- | A list of all subsets of the given set.
powerset :: Ord a => Set a -> [Set a]
powerset s 
    | S.null s  = [s]
    | otherwise = ts ++ map (S.insert x) ts
  where
    (x,t) = S.deleteFindMin s; ts = powerset t

-- | Sort and remove duplicates.
nubSort :: Ord a => [a] -> [a]
nubSort = map head . group . sort
