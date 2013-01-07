-- |
-- Module      : Math.Sym.Class
-- Copyright   : (c) Anders Claesson 2012, 2013
-- License     : BSD-style
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- 
-- A permutation class is a downset in the poset of permutations
-- ordered by containment. This module provides definitions of some
-- common classes.

module Math.Sym.Class
    (
     av231, vee, caret, gt, lt, wedges, separables
    ) where

import Math.Sym (Perm, empty, one, (\+\), (/-/), ssum, normalize)
import Math.Sym.D8 as D8

-- | Av(231); also know as the stack sortable permutations.
av231 :: Perm a => Int -> [a]
av231 0 = [empty]
av231 n = do
  k <- [0..n-1]
  s <- streamAv231 !! k
  t <- streamAv231 !! (n-k-1)
  return $ s \+\ (one /-/ t)

streamAv231 :: Perm a => [[a]]
streamAv231 = map av231 [0..]

-- | The V-class is Av(132, 231). It is so named because the diagram
-- of a typical permutation in this class is shaped like a V.
vee :: Perm a => Int -> [a]
vee = (streamVee !!)

streamVee :: Perm a => [[a]]
streamVee = [empty] : [one] : zipWith (++) vee_n n_vee
    where
      n_vee = (map.map) (one /-/) ws
      vee_n = (map.map) (\+\ one) ws
      ws    = tail streamVee

-- | The ∧-class is Av(213, 312). It is so named because the diagram
-- of a typical permutation in this class is shaped like a ∧.
caret :: Perm a => Int -> [a]
caret = map D8.complement . vee

-- | The >-class is Av(132, 312). It is so named because the diagram
-- of a typical permutation in this class is shaped like a >.
gt :: Perm a => Int -> [a]
gt = map D8.rotate . vee

-- | The <-class is Av(213, 231). It is so named because the diagram
-- of a typical permutation in this class is shaped like a <.
lt :: Perm a => Int -> [a]
lt = map D8.reverse . gt

union :: (Ord a, Perm a) => [Int -> [a]] -> Int -> [a]
union cs n = normalize $ concat [ c n | c <- cs ]

-- | The union of 'vee', 'caret', 'gt' and 'lt'.
wedges :: (Ord a, Perm a) => Int -> [a]
wedges = union [vee, caret, gt, lt]

compositions :: Int -> Int -> [[Int]]
compositions 0 0 = [[]]
compositions 0 _ = []
compositions _ 0 = []
compositions k n = [1..n] >>= \i -> map (i:) (compositions (k-1) (n-i))

-- | The class of separable permutations; it is identical to Av(2413,3142).
separables :: Perm a => Int -> [a]
separables 0 = [empty]
separables 1 = [ one ]
separables n = pIndec n ++ mIndec n
    where
      comps  m = [2..m] >>= \k -> compositions k m
      pIndec 0 = []
      pIndec 1 = [one]
      pIndec m = comps m >>= map ssum . mapM (streamMIndec !!)
      mIndec m = map D8.complement $ pIndec m
      streamMIndec = map mIndec [0..]
