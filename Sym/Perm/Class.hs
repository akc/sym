-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--

module Sym.Perm.Class
    (
      inc
    , dec
    , av1
    , av12
    , av21
    , av123
    , av132
    , av213
    , av231
    , av312
    , av321
    , av1243
    , av1324
    , av2134
    , av
    , vee
    , caret
    , gt
    , lt
    , wedges
    , separables
    , kLayered
    , layered
    , kFibonacci
    , fibonacci
    ) where

import Sym.Internal.Util
import Sym.Perm
import Sym.Perm.Bijection
import Sym.Perm.Constructions
import Sym.Perm.Pattern
import qualified Sym.Perm.D8 as D8

-- | The class of increasing permutations.
inc :: Int -> [Perm]
inc = av21

-- | The class of decreasing permutations.
dec :: Int -> [Perm]
dec = av12

-- | Av(1)
av1 :: Int -> [Perm]
av1 0 = [emptyperm]
av1 _ = []

-- | Av(12)
av12 :: Int -> [Perm]
av12 n = [ebb n]

-- | Av(21)
av21 :: Int -> [Perm]
av21 n = [idperm n]

-- | Av(123)
av123 :: Int -> [Perm]
av123 = map simionSchmidt' . av132

-- | Av(132)
av132 :: Int -> [Perm]
av132 = map D8.reverse . av231

-- | Av(213)
av213 :: Int -> [Perm]
av213 = map D8.complement . av231

-- | Av(231); also know as the stack sortable permutations.
av231 :: Int -> [Perm]
av231 0 = [emptyperm]
av231 n = do
  k <- [0..n-1]
  s <- streamAv231 !! k
  t <- streamAv231 !! (n-k-1)
  return $ s /+/ (one \-\ t)

streamAv231 :: [[Perm]]
streamAv231 = map av231 [0..]

-- | Av(312)
av312 :: Int -> [Perm]
av312 = map D8.inverse . av231

-- | Av(321)
av321 :: Int -> [Perm]
av321 = map D8.complement . av123

-- | Av(1243)
av1243 :: Int -> [Perm]
av1243 n = avoiders [fromList [0,1,3,2]] (perms n)

-- | Av(1324)
av1324 :: Int -> [Perm]
av1324 n = avoiders [fromList [0,2,1,3]] (perms n)

-- | Av(2134)
av2134 :: Int -> [Perm]
av2134 n = avoiders [fromList [1,0,2,3]] (perms n)

-- | Av(s) where s is a string of one or more patterns, using space as a
-- seperator.
av :: String -> Int -> [Perm]
av s = avoiders (map mkPerm (words s)) . perms

-- | The V-class is Av(132, 231). It is so named because the diagram of
-- a typical permutation in this class is shaped like a V.
vee :: Int -> [Perm]
vee = (streamVee !!)

streamVee :: [[Perm]]
streamVee = [emptyperm] : [one] : zipWith (++) vee_n n_vee
    where
      n_vee = (map.map) (one \-\) ws
      vee_n = (map.map) (/+/ one) ws
      ws    = tail streamVee

-- | The ∧-class is Av(213, 312). It is so named because the diagram of
-- a typical permutation in this class is shaped like a ∧.
caret :: Int -> [Perm]
caret = map D8.complement . vee

-- | The >-class is Av(132, 312). It is so named because the diagram of
-- a typical permutation in this class is shaped like a >.
gt :: Int -> [Perm]
gt = map D8.rotate . vee

-- | The <-class is Av(213, 231). It is so named because the diagram of
-- a typical permutation in this class is shaped like a <.
lt :: Int -> [Perm]
lt = map D8.reverse . gt

union :: [Int -> [Perm]] -> Int -> [Perm]
union cs n = nubSort $ concat [ c n | c <- cs ]

-- | The union of 'vee', 'caret', 'gt' and 'lt'.
wedges :: Int -> [Perm]
wedges = union [vee, caret, gt, lt]

compositions :: Int -> Int -> [[Int]]
compositions 0 0 = [[]]
compositions 0 _ = []
compositions _ 0 = []
compositions k n = [1..n] >>= \i -> map (i:) (compositions (k-1) (n-i))

boundedCompositions :: Int -> Int -> Int -> [[Int]]
boundedCompositions _ 0 0 = [[]]
boundedCompositions _ 0 _ = []
boundedCompositions _ _ 0 = []
boundedCompositions b k n = [1..b] >>= \i -> map (i:) (boundedCompositions b (k-1) (n-i))

-- | The class of separable permutations; it is identical to Av(2413,3142).
separables :: Int -> [Perm]
separables 0 = [emptyperm]
separables 1 = [one]
separables n = pIndec n ++ mIndec n
    where
      comps  m = [2..m] >>= \k -> compositions k m
      pIndec 0 = []
      pIndec 1 = [one]
      pIndec m = comps m >>= map skewSum . mapM (streamMIndec !!)
      mIndec m = map D8.complement $ pIndec m
      streamMIndec = map mIndec [0..]

-- | The class of layered permutations with /k/ layers.
kLayered :: Int -> Int -> [Perm]
kLayered k = map (directSum . map ebb) . compositions k

-- | The class of layered permutations.
layered :: Int -> [Perm]
layered n = [1..n] >>= flip kLayered n

-- | The class of Fibonacci permutations with /k/ layers. A /Fibonacci permutation/
-- is a layered permutation whose layers are all of size 1 or 2.
kFibonacci :: Int -> Int -> [Perm]
kFibonacci k = map (directSum . map ebb) . boundedCompositions 2 k

-- | The class of Fibonacci permutations. A /Fibonacci permutation/ is a
-- layered permutation whose layers are all of size 1 or 2.
fibonacci :: Int -> [Perm]
fibonacci n = [1..n] >>= flip kFibonacci n
