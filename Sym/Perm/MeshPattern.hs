{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright   : Anders Claesson 2014-2016
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--

-- TODO: Generalize interface and share with Sym.Perm.Pattern

module Sym.Perm.MeshPattern
    ( MeshPattern (..)
    , Mesh
    , Box
    , mkPattern
    , pattern
    , mesh
    , cols
    , rows
    , col
    , row
    , box
    , copiesOf
    , contains
    , avoids
    , avoidsAll
    , avoiders
    , kVincular
    , vincular
    , bivincular
    , meshPatterns
    ) where

import Data.List hiding (union)
import Sym.Internal.Size
import Sym.Perm
import Sym.Internal.SubSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Sym.Internal.Util

-- | A mesh is a, possibly empty, set of shaded boxes.
type Mesh = Set Box

-- | A box is represented by the coordinates of its southwest corner.
type Box  = (Int, Int)

type Point = (Int, Int)
type PermTwoLine = [Point]

data MeshPattern = MP
    { getPerm :: Perm
    , getMesh :: Mesh
    } deriving (Show, Eq, Ord)

instance Size MeshPattern where
    size = size . getPerm

mkPattern :: Ord a => [a] -> MeshPattern
mkPattern w = MP (mkPerm w) Set.empty

pattern :: Perm -> MeshPattern
pattern w = MP w Set.empty

mesh :: [Box] -> MeshPattern -> MeshPattern
mesh r (MP w s) = MP w . Set.union s $ Set.fromList r

cols :: [Int] -> MeshPattern -> MeshPattern
cols xs p@(MP w _) = mesh [ (x,y) | y <- [0..size w], x <- xs ] p

rows :: [Int] -> MeshPattern -> MeshPattern
rows ys p@(MP w _) = mesh [ (x,y) | x <- [0..size w], y <- ys ] p

col :: Int -> MeshPattern -> MeshPattern
col y = cols [y]

row :: Int -> MeshPattern -> MeshPattern
row x = rows [x]

box :: Box -> MeshPattern -> MeshPattern
box xy = mesh [xy]

kVincular :: Int -> Perm -> [MeshPattern]
kVincular k w = (flip cols (pattern w) . toList) `fmap` ((1+size w) `choose` k)

vincular :: Perm -> [MeshPattern]
vincular w = [0..1+size w] >>= flip kVincular w

bivincular :: Perm -> [MeshPattern]
bivincular w =
    [ foldr ((.) . either col row) id c $ pattern w | c <- choices ]
  where
    choices = powerset' $ [0..size w] >>= \z -> [Left z, Right z]
    powerset' = fmap Set.toList . powerset . Set.fromList

fullMesh :: Int -> Mesh
fullMesh n = let zs = [0..n] in Set.fromList [ (x,y) | x <- zs, y <- zs ]

meshPatterns :: Perm -> [MeshPattern]
meshPatterns w = [ MP w r | r <- powerset (fullMesh (size w)) ]

match' :: MeshPattern -> PermTwoLine -> PermTwoLine -> Bool
match' (MP u r) v w =
    and $ (u2==v2) : [ not $ f i j x y | (i,j) <- Set.toList r, (x,y) <- w ]
  where
    (v1, v2) = unzip v
    m  = 1 + length w
    xs = 0 : v1 ++ [m]
    ys = 0 : sort v2 ++ [m]
    u2 = map ((ys!!) . (+1)) (toList u)
    f i j x y = xs!!i < x && x < xs!!(i+1) && ys!!j < y && y < ys!!(j+1)

-- | @match p w m@ determines whether the subword in @w@ specified by
-- @m@ is an occurrence of @p@.
match :: MeshPattern -> Perm -> SubSeq -> Bool
match p w m = match' p v w'
  where
    w' = twoLine w
    v  = [ pt | pt@(x,_) <- w', x-1 `elem` toList m ]

twoLine :: Perm -> PermTwoLine
twoLine = zip [1..] . map (+1) . toList

-- | @copiesOf p w@ is the list of sets that represent copies of @p@ in @w@.
copiesOf :: MeshPattern -> Perm -> [SubSeq]
copiesOf p w = filter (match p w) $ size w `choose` size p
{-# INLINE copiesOf #-}

-- | @w `contains` p@ is a predicate determining if @w@ contains the pattern @p@.
contains :: Perm -> MeshPattern -> Bool
w `contains` p = not $ w `avoids` p

-- | @w `avoids` p@ is a predicate determining if @w@ avoids the pattern @p@.
avoids :: Perm -> MeshPattern -> Bool
w `avoids` p = null $ copiesOf p w

-- | @w `avoidsAll` ps@ is a predicate determining if @w@ avoids the patterns @ps@.
avoidsAll :: Perm -> [MeshPattern] -> Bool
w `avoidsAll` ps = all (w `avoids`) ps

-- | @avoiders ps ws@ is the list of permutations in @ws@ avoiding the
-- patterns in @ps@.
avoiders :: [MeshPattern] -> [Perm] -> [Perm]
avoiders ps ws = foldl (flip avoiders1) ws ps

-- @avoiders1 p ws@ is the list of permutations in @ws@ avoiding the
-- pattern @p@.
avoiders1 :: MeshPattern -> [Perm] -> [Perm]
avoiders1 _ [] = []
avoiders1 q vs@(v:_) = filter avoids_q us ++ filter (`avoids` q) ws
    where
      n = size v
      k = size q
      (us, ws) = span (\u -> size u == n) vs
      xs = n `choose` k
      avoids_q u = not $ any (match q u) xs
