{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Math.Sym
-- Copyright   : (c) Anders Claesson 2012
-- License     : BSD-style
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- 
-- Provides an efficient definition of standard permutations,
-- 'StPerm', together with an abstract class, 'Perm', whose
-- functionality is largely inherited from 'StPerm' using a group
-- action and the standardization map.

module Math.Sym
    (
    -- * Standard permutations
      StPerm
    , empty
    , one
    , toVector
    , fromVector
    , toList
    , fromList
    , (/-/)
    , bijection
    , unrankStPerm
    , sym

    -- * The permutation typeclass
    , Perm (..)

    -- * Generalize, normalize and cast
    , generalize
    , normalize
    , cast

    -- * Generating permutations
    , unrankPerm
    , randomPerm
    , perms

    -- * Sorting operators
    , stackSort
    , bubbleSort

    -- * Permutation patterns
    , copiesOf
    , avoids
    , avoiders
    , av

    -- * Single point extensions/deletions, shadows and downsets
    , del
    , shadow
    , downset
    , ext
    , coshadow

    -- * Left-to-right maxima and similar functions
    , lMaxima
    , lMinima
    , rMaxima
    , rMinima

    -- * Components and skew components
    , components
    , skewComponents

    -- * Simple permutations
    , simple

    -- * Subsets
    , Set
    , subsets
    ) where

import Control.Monad (liftM)
import Data.Ord (comparing)
import Data.Char (ord)
import Data.Monoid (Monoid(..))
import Data.Bits (Bits, bitSize, testBit, popCount, shiftL)
import Data.List (sort, sortBy, group)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as SV
    ( (!), Vector, toList, fromList, fromListN, empty, singleton
    , length, map, concat, splitAt
    )
import qualified Math.Sym.Internal as I
import Foreign.C.Types (CUInt(..))


-- Standard permutations
-- ---------------------

-- | By a /standard permutation/ we shall mean a permutations of
-- @[0..k-1]@.
newtype StPerm = StPerm { perm0 :: I.Perm0 } deriving Eq

instance Ord StPerm where
    compare u v = case comparing size u v of
                    EQ -> compare (perm0 u) (perm0 v)
                    x  -> x

instance Show StPerm where
    show = show . toVector

instance Monoid StPerm where
    mempty = fromVector SV.empty
    mappend u v = fromVector $ SV.concat [u', v']
        where
          u' = toVector u
          v' = SV.map ( + size u) $ toVector v


-- | The empty permutation.
empty :: StPerm
empty = StPerm SV.empty

-- | The one letter permutation.
one :: StPerm
one = StPerm $ SV.singleton 0

-- | Convert a standard permutation to a vector.
toVector :: StPerm -> Vector Int
toVector = perm0

-- | Convert a vector to a standard permutation. The vector should be a
-- permutation of the elements @[0..k-1]@ for some positive @k@. No
-- checks for this are done.
fromVector :: Vector Int -> StPerm
fromVector = StPerm

-- | Convert a standard permutation to a list.
toList :: StPerm -> [Int]
toList = SV.toList . toVector

-- | Convert a list to a standard permutation. The list should a
-- permutation of the elements @[0..k-1]@ for some positive @k@. No
-- checks for this are done.
fromList :: [Int] -> StPerm
fromList = fromVector . SV.fromList

infixl 6 /-/

-- | The /skew sum/ of two permutations. (A definition of the
-- /direct sum/ is provided by 'mappend' of the 'Monoid' instance for 'StPerm'.)
(/-/) :: StPerm -> StPerm -> StPerm
u /-/ v = fromVector $ SV.concat [u', v']
    where
      u' = SV.map ( + size v) $ toVector u
      v' = toVector v

-- | The bijective function defined by a standard permutation.
bijection :: StPerm -> Int -> Int
bijection w = (SV.!) (toVector w)

-- | @unrankStPerm n rank@ is the @rank@-th (Myrvold & Ruskey)
-- permutation of @[0..n-1]@. E.g.,
-- 
-- > unrankStPerm 16 19028390 == fromList [6,15,4,11,7,8,9,2,5,0,10,3,12,13,14,1]
-- 
unrankStPerm :: Int -> Integer -> StPerm
unrankStPerm n = fromVector . I.unrankPerm n

-- | The list of standard permutations of the given size (the symmetric group). E.g.,
-- 
-- > sym 2 == [fromList [0,1], fromList [1,0]]
-- 
sym :: Int -> [StPerm]
sym n = map (unrankStPerm n) [0 .. product [1 .. toInteger n] - 1]


-- The permutation typeclass
-- -------------------------

-- | The class of permutations. Minimal complete definition: 'st'
-- 'act' and 'idperm'. The default implementations of 'size' and
-- 'neutralize' can be somewhat slow, so you may want to implement
-- them as well.
class Perm a where

    -- | The standardization map. If there is an underlying linear
    -- order on @a@ then @st@ is determined by the unique order
    -- preserving map from @[0..]@ to that order. In any case, the
    -- standardization map should be equivariant with respect to the
    -- group action defined below; i.e., it should hold that
    -- 
    -- > st (u `act` v) == u `act` st v
    -- 
    st :: a -> StPerm

    -- | A (left) /group action/ of 'StPerm' on @a@. As for any group
    -- action it should hold that
    -- 
    -- > (u `act` v) `act` w == u `act` (v `act` w)   &&   neutralize u `act` v == v
    -- 
    act :: StPerm -> a -> a

    -- | The size of a permutation. The default implementation derived from
    -- 
    -- > size == size . st
    -- 
    -- This is not a circular definition as 'size' on 'StPerm' is
    -- implemented independently. If the implementation of 'st' is
    -- slow, then it can be worth while to override the standard
    -- definiton; any implementation should, however, satisfy the
    -- identity above.
    {-# INLINE size #-}
    size :: a -> Int
    size = size . st

    -- | The identity permutation of the given size.
    idperm :: Int -> a

    -- | The permutation obtained by acting on the given permutation
    -- with its own inverse; that is, the identity permutation on the
    -- same underlying set as the given permutation. It should hold
    -- that
    -- 
    -- > st (neutralize u) == neutralize (st u)
    -- > neutralize u == inverse (st u) `act` u
    -- > neutralize u == idperm (size u)
    -- 
    -- The default implementation uses the last of these three equations.
    {-# INLINE neutralize #-}
    neutralize :: a -> a
    neutralize = idperm . size

    -- | The group theoretical inverse. It should hold that
    -- 
    -- > inverse u == inverse (st u) `act` neutralize u
    -- 
    -- and this is the default implementation.
    {-# INLINE inverse #-}
    inverse :: a -> a
    inverse u = inverse (st u) `act` neutralize u

    -- | Predicate determining if two permutations are
    -- order-isomorphic. The default implementation uses
    -- 
    -- > u `ordiso` v  ==  u == st v
    -- 
    -- Equivalently, one could use
    -- 
    -- > u `ordiso` v  ==  inverse u `act` v == neutralize v
    -- 
    {-# INLINE ordiso #-}
    ordiso :: StPerm -> a -> Bool
    ordiso u v = u == st v

instance Perm StPerm where
    st         = id
    act u v    = fromVector $ I.act (toVector u) (toVector v)
    size       = I.size . toVector
    idperm     = fromVector . I.idperm
    inverse    = fromVector . I.inverse . toVector
    ordiso     = (==)

-- Auxiliary function: @w = act' u v@ iff @w[u[i]] = v[i]@.
-- Caveat: @act'@ is not a proper group action.
act' :: Ord a => [a] -> [b] -> [b]
act' u = map snd . sortBy (comparing fst) . zip u

actL :: StPerm -> [a] -> [a]
actL u = act' $ toList (inverse u)

stString :: String -> StPerm
stString = fromList . map f
    where
      f c | '1' <= c && c <= '9' = ord c - ord '1'
          | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 9
          | otherwise            = ord c - ord 'a' + 35

instance Perm String where
    st         = stString
    act        = actL
    inverse v  = act' v (neutralize v)
    size       = length
    idperm n   = take n $ ['1'..'9'] ++ ['A'..'Z'] ++ ['a'..]

instance Perm [Int] where
    st         = fromList . map (+(-1))
    act        = actL
    inverse v  = act' v (neutralize v)
    size       = length
    idperm n   = [1..n]


-- Generalize, normalize and cast
-- ------------------------------

-- | Generalize a function on 'StPerm' to a function on any permutations:
-- 
-- > generalize f v = f (st v) `act` neutralize v
-- 
-- Note that this will only work as intended if @f@ is size preserving.
generalize :: Perm a => (StPerm -> StPerm) -> a -> a
generalize f v = f (st v) `act` neutralize v

-- | Sort a list of permutations with respect to the standardization
-- and remove duplicates
normalize :: (Ord a, Perm a) => [a] -> [a]
normalize xs = map ((`act` idperm n) . head) . group $ sort ys
    where
      ys = map st xs
      n = maximum $ map size ys

-- | Cast a permutation of one type to another
cast :: (Perm a, Perm b) => a -> b
cast w = st w `act` idperm (size w)


-- Generating permutations
-- -----------------------

-- | @unrankPerm u rank@ is the @rank@-th (Myrvold & Ruskey)
-- permutation of size @n@. E.g.,
-- 
-- > unrankPerm 9 88888 == "561297843"
-- 
unrankPerm :: Perm a => Int -> Integer -> a
unrankPerm n = (`act` idperm n) . fromVector . I.unrankPerm n

-- | @randomPerm n@ is a random permutation of size @n@.
randomPerm :: Perm a => Int -> IO a
randomPerm n = ((`act` idperm n) . fromVector . I.fromLehmercode) `liftM` I.randomLehmercode n

-- | All permutations of a given size. E.g.,
-- 
-- > perms 3 == ["123","213","321","132","231","312"]
-- 
perms :: Perm a => Int -> [a]
perms n = map (`act` idperm n) $ sym n


-- Sorting operators
-- -----------------

-- | One pass of stack-sort.
stackSort :: Perm a => a -> a
stackSort = generalize (fromVector . I.stackSort . toVector)

-- | One pass of bubble-sort.
bubbleSort :: Perm a => a -> a
bubbleSort = generalize (fromVector . I.bubbleSort . toVector)


-- Permutation patterns
-- --------------------

-- | @copiesOf p w@ is the list of (indices of) copies of the pattern
-- @p@ in the permutation @w@. E.g.,
-- 
-- > copiesOf (st "21") "2431" == [fromList [1,2],fromList [0,3],fromList [1,3],fromList [2,3]]
-- 
copiesOf :: Perm a => StPerm -> a -> [Set]
copiesOf p w = I.copies subsets (toVector p) (toVector $ st w)

-- | @avoids w ps@ is a predicate determining if @w@ avoids the patterns @ps@.
avoids :: Perm a => a -> [StPerm] -> Bool
w `avoids` ps = all null [ copiesOf p w | p <- ps ]

-- | @avoiders ps vs@ is the list of permutations in @vs@ avoiding the
-- patterns @ps@. This is equivalent to the definition
-- 
-- > avoiders ps = filter (`avoids` ps)
-- 
-- but is usually much faster.
avoiders :: Perm a => [StPerm] -> [a] -> [a]
avoiders ps = I.avoiders subsets (toVector . st) (map toVector ps)

-- | @av ps n@ is the list of permutations of @[0..n-1]@ avoiding the
-- patterns @ps@. E.g.,
-- 
-- > map (length . av [st "132", st "321"]) [1..8] == [1,2,4,7,11,16,22,29]
-- 
av :: [StPerm] -> Int -> [StPerm]
av ps = avoiders ps . sym


-- Single point extensions/deletions, shadows and downsets
-- -------------------------------------------------------

-- | Delete the element at a given position
del :: Perm a => Int -> a -> a
del i = generalize $ fromVector . I.del i . toVector

-- | The list of all single point deletions
shadow :: (Ord a, Perm a) => [a] -> [a]
shadow ws = normalize [ del i w | w <- ws, i <- [0 .. size w - 1] ]

-- | The list of permutations that are contained in at least one of
-- the given permutaions
downset :: (Ord a, Perm a) => [a] -> [a]
downset = normalize . concat . downset'
    where
      downset' [] = []
      downset' ws = ws : downset' (shadow ws)

-- | Extend a permutation by inserting a new largest element at the
-- given position
ext :: Perm a => Int -> a -> a
ext i = generalize' $ fromVector . ext0 . toVector
    where
      generalize' f w = f (st w) `act` idperm (1+size w)
      ext0 w = SV.concat [u, SV.singleton (SV.length w), v]
          where
            (u,v) = SV.splitAt i w

-- | The list of all single point extensions
coshadow :: (Ord a, Perm a) => [a] -> [a]
coshadow ws = normalize [ ext i w | w <- ws, i <- [0 .. size w] ]


-- Left-to-right maxima and similar functions
-- ------------------------------------------

-- | The set of indices of left-to-right maxima.
lMaxima :: Perm a => a -> Set
lMaxima = I.lMaxima . toVector . st

-- | The set of indices of left-to-right minima.
lMinima :: Perm a => a -> Set
lMinima = I.lMaxima . I.complement . toVector . st

-- | The set of indices of right-to-left maxima.
rMaxima :: Perm a => a -> Set
rMaxima = I.rMaxima . toVector . st

-- | The set of indices of right-to-left minima.
rMinima :: Perm a => a -> Set
rMinima = I.rMaxima . I.complement . toVector . st


-- Components and skew components
---------------------------------

-- | The set of indices of components.
components :: Perm a => a -> Set
components = I.components . toVector . st

-- | The set of indices of skew components.
skewComponents :: Perm a => a -> Set
skewComponents = I.components . I.complement . toVector . st


-- Simple permutations
-- -------------------

-- | A predicate determining if a given permutation is simple.
simple :: Perm a => a -> Bool
simple = I.simple . toVector . st


-- Subsets
-- -------

-- | A set is represented by an increasing vector of non-negative
-- integers.
type Set = SV.Vector Int

-- A sub-class of 'Bits' used internally. Minimal complete definiton: 'next'.
class (Bits a, Integral a) => Bitmask a where
    -- | Lexicographically, the next bitmask with the same Hamming weight.
    next :: a -> a

    -- | @ones k m@ is the set of indices whose bits are set in
    -- @m@. Default implementation:
    -- 
    -- > ones m = fromListN (popCount m) $ filter (testBit m) [0..]
    -- 
    ones :: a -> Set
    ones m = SV.fromListN (popCount m) $ filter (testBit m) [0..]

instance Bitmask CUInt where
    next = I.nextCUInt
    ones = I.onesCUInt

instance Bitmask Integer where
    next = I.nextIntegral

-- @bitmasks n k@ is the list of bitmasks with Hamming weight @k@ and
-- size less than @2^n@.
bitmasks :: Bitmask a => Int -> Int -> [a]
bitmasks n k = take binomial (iterate next ((1 `shiftL` k) - 1))
    where
      n' = toInteger n
      k' = toInteger k
      binomial = fromIntegral $ product [n', n'-1 .. n'-k'+1] `div` product [1..k']

-- | @subsets n k@ is the list of subsets of @[0..n-1]@ with @k@
-- elements.
subsets :: Int -> Int -> [Set]
subsets n k = if n <= bitSize (0 :: CUInt)
              then map ones (bitmasks n k :: [CUInt])
              else map ones (bitmasks n k :: [Integer])
