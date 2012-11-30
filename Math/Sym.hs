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
    , toVector        -- :: StPerm -> Vector Int
    , fromVector      -- :: Vector Int -> StPerm
    , toList          -- :: StPerm -> [Int]
    , fromList        -- :: [Int] -> StPerm
    , (/-/)           -- :: StPerm -> StPerm -> StPerm
    , unrankStPerm    -- :: Int -> Integer -> StPerm
    , sym             -- :: Int -> [StPerm]

    -- * The permutation typeclass
    , Perm (..)

    -- * Generalize
    , generalize      -- :: Perm a => (StPerm -> StPerm) -> a -> a

    -- * Generating permutations
    , unrankPerm      -- :: Perm a => a -> Integer -> a
    , randomPerm      -- :: Perm a => a -> IO a
    , perms           -- :: Perm a => a -> [a]

    -- * Sorting operators
    , stackSort       -- :: Perm a => a -> a
    , bubbleSort      -- :: Perm a => a -> a

    -- * Permutation patterns
    , copies          -- :: Perm a => StPerm -> a -> [Set]
    , avoids          -- :: Perm a => [StPerm] -> a -> Bool
    , avoiders        -- :: Perm a => [StPerm] -> [a] -> [a]
    , av              -- :: [StPerm] -> Int -> [StPerm]

    -- * Simple permutations
    , simple          -- :: Perm a => a -> Bool

    -- * Subsets
    , Set
    , subsets         -- :: Int -> Int -> [Set]
    ) where

import Control.Monad (liftM)
import Data.Ord (comparing)
import Data.Monoid (Monoid(..))
import Data.Bits (Bits, bitSize, testBit, popCount, shiftL)
import Data.List (sort, sortBy)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as SV (Vector, toList, fromList, fromListN, empty, map, (++))
import qualified Math.Sym.Internal as I
import Foreign.C.Types (CUInt(..))


-- Standard permutations
-- ---------------------

-- | By a /standard permutation/ we shall mean a permutations of
-- @[0..k-1]@.
newtype StPerm = StPerm { perm0 :: I.Perm0 } deriving (Eq, Ord)

instance Show StPerm where
    show = show . toVector

instance Monoid StPerm where
    mempty = fromVector SV.empty
    mappend u v = fromVector $ (SV.++) u' v'
        where
          u' = toVector u
          v' = SV.map ( + size u) $ toVector v

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
u /-/ v = fromVector $ (SV.++) u' v'
    where
      u' = SV.map ( + size v) $ toVector u
      v' = toVector v

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

-- | The class of permutations. Minimal complete definition: 'st' and
-- 'act'. The default implementations of 'size' and 'idperm' can be
-- somewhat slow, so you may want to implement them as well.
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
    -- > (u `act` v) `act` w == u `act` (v `act` w)   &&   idperm u `act` v == v
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

    -- | The identity permutation on the same underlying set as the
    -- given permutation. It should hold that
    -- 
    -- > st (idperm u) == idperm (st u)
    -- 
    -- Group theoretically, it should also hold that @u . inverse u ==
    -- idperm u@. In terms of the group action this means
    -- 
    -- > idperm u == inverse (st u) `act` u
    -- 
    -- and this is the default implementation.
    {-# INLINE idperm #-}
    idperm :: a -> a
    idperm u = inverse (st u) `act` u

    -- | The group theoretical inverse. It should hold that
    -- 
    -- > inverse u == inverse (st u) `act` idperm u
    -- 
    -- and this is the default implementation.
    {-# INLINE inverse #-}
    inverse :: a -> a
    inverse u = inverse (st u) `act` idperm u

    -- | Predicate determining if two permutations are
    -- order-isomorphic. The default implementation uses
    -- 
    -- > u `ordiso` v  ==  u == st v
    -- 
    -- Equivalently, one could use
    -- 
    -- > u `ordiso` v  ==  inverse u `act` v == idperm v
    -- 
    {-# INLINE ordiso #-}
    ordiso :: StPerm -> a -> Bool
    ordiso u v = u == st v

instance Perm StPerm where
    st         = id
    act u v    = fromVector $ I.act (toVector u) (toVector v)
    size       = I.size . toVector
    idperm     = fromVector . I.idperm . size
    inverse    = fromVector . I.inverse . toVector
    ordiso     = (==)

-- Auxiliary function: @w = act' u v@ iff @w[u[i]] = v[i]@.
-- Caveat: @act'@ is not a proper group action.
act' :: Ord a => [a] -> [b] -> [b]
act' u = map snd . sortBy (comparing fst) . zip u

instance (Enum a, Ord a) => Perm [a] where
    st         = fromVector . I.st . I.fromList . map fromEnum
    act u      = act' $ toList (inverse u)
    inverse v  = act' v (idperm v)
    size       = length
    idperm     = sort


-- Generalize
-- ----------

-- | Generalize a function on 'StPerm' to a function on any permutations:
-- 
-- > generalize f v = f (st v) `act` idperm v
-- 
-- Note that this will only work as intended if @f@ is size preserving.
generalize :: Perm a => (StPerm -> StPerm) -> a -> a
generalize f v = f (st v) `act` idperm v


-- Generating permutations
-- -----------------------

-- | @unrankPerm u rank@ is the @rank@-th (Myrvold & Ruskey)
-- permutation of @u@. E.g.,
-- 
-- > unrankPerm ['1'..'9'] 88888 == "561297843"
-- 
unrankPerm :: Perm a => a -> Integer -> a
unrankPerm u = (`act` u) . fromVector . I.unrankPerm (size u)

-- | @randomPerm u@ is a random permutation of @u@.
randomPerm :: Perm a => a -> IO a
randomPerm u = ((`act` u) . fromVector . I.fromLehmercode) `liftM` I.randomLehmercode (size u)

-- | All permutations of a given permutation. E.g.,
-- 
-- > perms "123" == ["123","213","321","132","231","312"]
-- 
perms :: Perm a => a -> [a]
perms u = map (`act` u) $ sym (size u)


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

-- | @copies p w@ is the list of (indices of) copies of the pattern
-- @p@ in the permutation @w@. E.g.,
-- 
-- > copies (st "21") "2431" == [fromList [1,2],fromList [0,3],fromList [1,3],fromList [2,3]]
-- 
copies :: Perm a => StPerm -> a -> [Set]
copies p w = I.copies subsets (toVector p) (toVector $ st w)

-- | @avoids ps w@ is a predicate determining if @w@ avoids the patterns @ps@.
avoids :: Perm a => [StPerm] -> a -> Bool
avoids ps w = all null [ copies p w | p <- ps ]

-- | @avoiders ps v@ is the list of permutations of @v@ avoiding the
-- patterns @ps@. This is equivalent to the definition
-- 
-- > avoiders ps = filter (avoids ps)
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
