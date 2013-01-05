-- |
-- Module      : Math.Sym.Class
-- Copyright   : (c) Anders Claesson 2012
-- License     : BSD-style
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- 
-- A permutation class is a downset in the poset of permutations
-- ordered by containment. This module provides definitions of some
-- common classes.

module Math.Sym.Class
    (
     av231, vee, wedge, gt, lt, vorb
    ) where

import Math.Sym (Perm, empty, one, (\+\), (/-/), normalize)
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
-- of a typical permutation in this class is shaped like a wedge.
wedge :: Perm a => Int -> [a]
wedge = map D8.complement . vee

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

-- | The union of 'vee', 'wedge', 'gt' and 'lt'; the orbit of a V under rotation
vorb :: (Ord a, Perm a) => Int -> [a]
vorb = union [vee, wedge, gt, lt]
