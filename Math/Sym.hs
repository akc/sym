{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Math.Sym
-- Copyright   : (c) Anders Claesson 2012, 2013
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
    , toList
    , fromList
    , sym

    -- * The permutation typeclass
    , Perm (..)
    , CharPerm (..)
    , IntPerm (..)

    -- * IntMaps as permutations
    , Perm2 (..)

    -- * Convenience functions
    , empty
    , one
    , toVector
    , fromVector
    , bijection
    , lift
    , lift2
    , normalize
    , cast

    -- * Constructions
    , (/+/)
    , dsum
    , (\-\)
    , ssum
    , inflate

    -- * Generating permutations
    , unrankPerm
    , randomPerm
    , perms

    -- * Sorting operators
    , stackSort
    , bubbleSort

    -- * Permutation patterns
    , Pattern (..)
    , stat
    , av
    , permClass

    -- * Poset functions
    , del
    , shadow
    , downset
    , ext
    , coshadow
    , minima
    , maxima
    , coeff

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
import Data.String (IsString(..))
import Data.Monoid (Monoid(..),(<>))
import Data.Bits (Bits, bitSize, testBit, popCount, shiftL)
import Data.List (sort, sortBy, group)
import Data.Vector.Storable (Vector)
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as M
    ( empty, size, elems, fromDistinctAscList, insert
    )
import qualified Data.Vector.Storable as SV
    ( (!), toList, fromList, fromListN, empty, singleton
    , length, map, concat, splitAt
    )
import Math.Sym.Internal (Perm0)
import qualified Math.Sym.Internal as I
import Foreign.C.Types (CUInt(..))


-- Standard permutations
-- ---------------------

-- | By a /standard permutation/ we shall mean a permutations of
-- @[0..k-1]@.
newtype StPerm = StPerm { perm0 :: Perm0 } deriving Eq

instance Ord StPerm where
    compare u v = case comparing size u v of
                    EQ -> compare (perm0 u) (perm0 v)
                    x  -> x

instance Show StPerm where
    show = show . toVector

instance Monoid StPerm where
    mempty = empty
    mappend = lift2 $ \u v -> SV.concat [u, SV.map ( + SV.length u) v]

-- | Convert a standard permutation to a list.
toList :: StPerm -> [Int]
toList = SV.toList . toVector

-- | Convert a list to a standard permutation. The list should a
-- permutation of the elements @[0..k-1]@ for some positive @k@. No
-- checks for this are done.
fromList :: [Int] -> StPerm
fromList = fromVector . SV.fromList

-- | The list of standard permutations of the given size (the symmetric group). E.g.,
-- 
-- > sym 2 == [fromList [0,1], fromList [1,0]]
-- 
sym :: Int -> [StPerm]
sym = perms


-- The permutation typeclass
-- -------------------------

-- | The class of permutations. Minimal complete definition: 'st',
-- 'act' and 'idperm'. The default implementation of 'size' can be
-- somewhat slow, so you may want to implement it as well.
class Ord a => Perm a where

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
    -- > (u `act` v) `act` w == u `act` (v `act` w)   &&   idperm n `act` v == v
    -- 
    -- where @v,w::a@ and @u::StPerm@ are of size @n@.
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

    -- | The group theoretical inverse. It should hold that
    -- 
    -- > inverse == unst . inverse . st
    -- 
    -- and this is the default implementation.
    {-# INLINE inverse #-}
    inverse :: a -> a
    inverse = unst . inverse . st

    -- | Predicate determining if two permutations are
    -- order-isomorphic. The default implementation uses
    -- 
    -- > u `ordiso` v  ==  u == st v
    -- 
    -- Equivalently, one could use
    -- 
    -- > u `ordiso` v  ==  inverse u `act` v == idperm (size u)
    -- 
    {-# INLINE ordiso #-}
    ordiso :: StPerm -> a -> Bool
    ordiso u v = u == st v

    -- | The inverse of the standardization function. For efficiency
    -- reasons we make the size of the permutation an argument to this
    -- function. It should hold that
    -- 
    -- > unst n w == w `act` idperm n
    -- 
    -- and this is the default implementation. An un-standardization
    -- function without the size argument is given by 'unst' below.
    {-# INLINE unstn #-}
    unstn :: Int -> StPerm -> a
    unstn n w = w `act` idperm n

    -- | The inverse of 'st'. It should hold that
    -- 
    -- > unst w == unstn (size w) w
    -- 
    -- and this is the default implementation.
    unst :: Perm a => StPerm -> a
    unst w = unstn (size w) w

instance Perm StPerm where
    st         = id
    act        = lift2 I.act
    size       = I.size . toVector
    idperm     = fromVector . I.idperm
    inverse    = lift I.inverse
    ordiso     = (==)
    unstn _    = id

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

idpermString :: Int -> String
idpermString n = take n $ ['1'..'9'] ++ ['A'..'Z'] ++ ['a'..]

-- | A String viewed as a permutation of its characters. The alphabet
-- is ordered as
-- 
-- > ['1'..'9'] ++ ['A'..'Z'] ++ ['a'..]
-- 
newtype CharPerm = CharPerm { chars :: String } deriving Eq

instance Show CharPerm where
    show w = "CharPerm " ++ show (chars w)

instance Ord CharPerm where
    compare u v = compare (st u) (st v)

instance IsString CharPerm where
    fromString = CharPerm

instance Perm CharPerm where
    st         = stString . chars
    act v      = CharPerm . actL v . chars
    inverse v  = CharPerm $ act' (chars v) (idpermString (size v))
    size       = length . chars
    idperm     = CharPerm . idpermString

-- For ghci convenience we also define a String instance of Perm
instance Perm String where
    st         = st . CharPerm
    act v      = chars . act v . CharPerm
    idperm     = chars . idperm

-- | A list of integers viewed as a permutation.
newtype IntPerm = IntPerm { ints :: [Int] } deriving Eq

instance Show IntPerm where
    show w = "IntPerm " ++ show (ints w)

instance Ord IntPerm where
    compare u v = compare (st u) (st v)

instance Perm IntPerm where
    st         = fromList . map (+(-1)) . ints
    act v      = IntPerm . actL v . ints
    inverse v  = IntPerm $ act' (ints v) [1 .. size v]
    size       = length . ints
    idperm n   = IntPerm [1..n]


-- IntMaps as permutations
-- -----------------------

-- | Type alias for @IntMap Int@. This can be thought of as a
-- permutations in two line notation.
newtype Perm2 = Perm2 { intmap :: IntMap Int } deriving Eq

instance Show Perm2 where
    show w = "Perm2 (" ++ show (intmap w) ++ ")"

instance Ord Perm2 where
    compare u v = compare (st u) (st v)

instance Perm Perm2 where
    st         = st . IntPerm . M.elems . intmap
    size       = M.size . intmap
    idperm n   = Perm2 $ M.fromDistinctAscList [ (i,i) | i <- [1..n] ]

    u `act` v  = Perm2 $ foldr (\i -> M.insert (1 + (SV.!) u' i) (v'!(i+1))) M.empty [0..n-1]
        where
          u' = toVector u
          v' = intmap v
          n  = SV.length u'


-- Convenience functions
-- ---------------------

-- | The empty permutation.
empty :: Perm a => a
empty = unst $ StPerm SV.empty

-- | The one letter permutation.
one :: Perm a => a
one = unst . StPerm $ SV.singleton 0

-- | Convert a permutation to a vector.
toVector :: Perm a => a -> Vector Int
toVector = perm0 . st

-- | Convert a vector to a permutation. The vector should be a
-- permutation of the elements @[0..k-1]@ for some positive @k@. No
-- checks for this are done.
fromVector :: Perm a => Vector Int -> a
fromVector = unst . StPerm

-- | The bijective function defined by a permutation.
bijection :: Perm a => a -> Int -> Int
bijection w = (SV.!) v where v = toVector w

-- | Lift a function on 'Vector Int' to a function on any permutations:
-- 
-- > lift f = fromVector . f . toVector
-- 
lift :: (Perm a, Perm b) => (Vector Int -> Vector Int) -> a -> b
lift f = fromVector . f . toVector

-- | Like 'lift' but for functions of two variables
lift2 :: (Perm a, Perm b, Perm c) =>
         (Vector Int -> Vector Int -> Vector Int) -> a -> b -> c
lift2 f u v = fromVector $ f (toVector u) (toVector v)

generalize :: (Perm a, Perm b) => (StPerm -> StPerm) -> a -> b
generalize f = unst . f . st

generalize2 :: (Perm a, Perm b, Perm c) => (StPerm -> StPerm -> StPerm) -> a -> b -> c
generalize2 f u v = unst $ f (st u) (st v)

-- | Sort a list of permutations with respect to the standardization
-- and remove duplicates
normalize :: Perm a => [a] -> [a]
normalize = map (unst . head) . group . sort . map st

-- | Cast a permutation of one type to another
cast :: (Perm a, Perm b) => a -> b
cast = generalize id


-- Constructions
-- -------------

infixl 6 /+/
infixl 6 \-\

-- | The /direct sum/ of two permutations.
(/+/) :: Perm a => a -> a -> a
(/+/) = generalize2 (<>)

-- | The direct sum of a list of permutations.
dsum :: Perm a => [a] -> a
dsum = foldr (/+/) empty

-- | The /skew sum/ of two permutations.
(\-\) :: Perm a => a -> a -> a
(\-\) = lift2 $ \u v -> SV.concat [SV.map ( + SV.length v) u, v]

-- | The skew sum of a list of permutations.
ssum :: Perm a => [a] -> a
ssum = foldr (\-\) empty

-- | @inflate w vs@ is the /inflation/ of @w@ by @vs@. It is the
-- permutation of length @sum (map size vs)@ obtained by replacing
-- each entry @w!i@ by an interval that is order isomorphic to @vs!i@
-- in such a way that the intervals are order isomorphic to @w@. In
-- particular,
-- 
-- > u /+/ v == inflate "12" [u,v]
-- > u \-\ v == inflate "21" [u,v]
-- 
inflate :: (Perm a, Perm b) => b -> [a] -> a
inflate w vs = lift (\v -> I.inflate v (map toVector vs)) w


-- Generating permutations
-- -----------------------

-- | @unrankPerm u rank@ is the @rank@-th (Myrvold & Ruskey)
-- permutation of size @n@. E.g.,
-- 
-- > unrankPerm 9 88888 == "561297843"
-- 
unrankPerm :: Perm a => Int -> Integer -> a
unrankPerm n = fromVector . I.unrankPerm n

-- | @randomPerm n@ is a random permutation of size @n@.
randomPerm :: Perm a => Int -> IO a
randomPerm n = (fromVector . I.fromLehmercode) `liftM` I.randomLehmercode n

-- | All permutations of a given size. E.g.,
-- 
-- > perms 3 == ["123","213","321","132","231","312"]
-- 
perms :: Perm a => Int -> [a]
perms n = map (unrankPerm n) [0 .. product [1 .. toInteger n] - 1]


-- Sorting operators
-- -----------------

-- | One pass of stack-sort.
stackSort :: Perm a => a -> a
stackSort = lift I.stackSort

-- | One pass of bubble-sort.
bubbleSort :: Perm a => a -> a
bubbleSort = lift I.bubbleSort


-- Permutation patterns
-- --------------------

-- | All methods of the Pattern typeclass have default
-- implementations. This is because any permutation can also be seen
-- as a pattern. If you want to override the default implementation
-- you should at least define 'copiesOf'.
class Perm a => Pattern a where
    -- | @copiesOf p w@ is the list of indices of copies of the pattern
    -- @p@ in the permutation @w@. E.g.,
    -- 
    -- > copiesOf "21" "2431" == [fromList [1,2],fromList [0,3],fromList [1,3],fromList [2,3]]
    -- 
    copiesOf :: Perm b => a -> b -> [Set]
    copiesOf p w = I.copies subsets (toVector p) (toVector w)

    -- | @w `contains` p@ is a predicate determining if @w@ contains the pattern @p@.
    contains :: Perm b => b -> a -> Bool
    w `contains` p = not $ w `avoids` p

    -- | @w `avoids` p@ is a predicate determining if @w@ avoids the pattern @p@.
    avoids :: Perm b => b -> a -> Bool
    w `avoids` p = null $ copiesOf p w

    -- | @w `avoidsAll` ps@ is a predicate determining if @w@ avoids the patterns @ps@.
    avoidsAll :: Perm b => b -> [a] -> Bool
    w `avoidsAll` ps = all (w `avoids`) ps

    -- | @avoiders ps vs@ is the list of permutations in @vs@ avoiding the
    -- patterns @ps@. The default definition is
    -- 
    -- > avoiders ps = filter (`avoidsAll` ps)
    -- 
    avoiders :: Perm b => [a] -> [b] -> [b]
    avoiders ps = filter (`avoidsAll` ps)

instance Pattern StPerm where
    avoiders ps = I.avoiders subsets toVector (map toVector ps)

instance Pattern String
instance Pattern CharPerm
instance Pattern IntPerm
instance Pattern Perm2


-- | @stat p@ is the pattern @p@ when regarded as a statistic/function
-- counting copies of itself:
-- 
-- > stat p = length . copiesOf p
-- 
stat :: (Pattern a, Perm b) => a -> b -> Int
stat p = length . copiesOf p

-- | @av ps n@ is the list of permutations of @[0..n-1]@ avoiding the
-- patterns @ps@. E.g.,
-- 
-- > map (length . av ["132","321"]) [1..8] == [1,2,4,7,11,16,22,29]
-- 
av :: Pattern a => [a] -> Int -> [StPerm]
av ps = avoiders ps . sym

-- | Like 'av' but the return type is any set of permutations.
permClass :: (Pattern a, Perm b) => [a] -> Int -> [b]
permClass ps = avoiders ps . perms


-- Poset functions
-- ---------------

-- | Delete the element at a given position
del :: Perm a => Int -> a -> a
del i = lift $ I.del i

-- | The list of all single point deletions
shadow :: Perm a => [a] -> [a]
shadow ws = normalize [ del i w | w <- ws, i <- [0 .. size w - 1] ]

-- | The list of permutations that are contained in at least one of
-- the given permutaions
downset :: Perm a => [a] -> [a]
downset = normalize . concat . downset'
    where
      downset' [] = []
      downset' ws = ws : downset' (shadow ws)

-- | @ext i j w@ extends @w@ by inserting a new element of
-- (relative) size @j@ at position @i@. It should hold that
-- @0 <= i,j <= size w@.
ext :: Perm a => Int -> Int -> a -> a
ext i j = lift $ \w ->
          let (u,v) = SV.splitAt i w
              f x = if x < j then x else x+1
          in SV.concat [SV.map f u, SV.singleton j, SV.map f v]

-- | The list of all single point extensions
coshadow :: Perm a => [a] -> [a]
coshadow ws = normalize [ ext i j w | w <- ws, let n = size w, i <- [0..n], j <- [0..n] ]

-- | The set of minimal elements with respect to containment.
minima :: Pattern a => [a] -> [a]
minima [] = []
minima ws = v : minima (avoiders [v] vs)
    where
      (v:vs) = normalize ws

-- | The set of maximal elements with respect to containment.
maxima :: Pattern a => [a] -> [a]
maxima [] = []
maxima ws = v : maxima [ u | u <- vs, v `avoids` u ]
    where
      (v:vs) = reverse $ normalize ws

-- | @coeff f v@ is the coefficient of @v@ when expanding the
-- permutation statistic @f@ as a sum of permutations/patterns. See
-- Petter Brändén and Anders Claesson: Mesh patterns and the expansion
-- of permutation statistics as sums of permutation patterns, The
-- Electronic Journal of Combinatorics 18(2) (2011),
-- <http://www.combinatorics.org/ojs/index.php/eljc/article/view/v18i2p5>.
coeff :: Pattern a => (a -> Int) -> a -> Int
coeff f v = f v + sum [ (-1)^(k - j) * c * f u |
                        j <- [0 .. k-1]
                      , u <- perms j
                      , let c = length $ copiesOf u v
                      , c > 0
                      ] where k = size v


-- Left-to-right maxima and similar functions
-- ------------------------------------------

-- | The set of indices of left-to-right maxima.
lMaxima :: Perm a => a -> Set
lMaxima = I.lMaxima . toVector

-- | The set of indices of left-to-right minima.
lMinima :: Perm a => a -> Set
lMinima = I.lMaxima . I.complement . toVector

-- | The set of indices of right-to-left maxima.
rMaxima :: Perm a => a -> Set
rMaxima = I.rMaxima . toVector

-- | The set of indices of right-to-left minima.
rMinima :: Perm a => a -> Set
rMinima = I.rMaxima . I.complement . toVector


-- Components and skew components
---------------------------------

-- | The set of indices of components.
components :: Perm a => a -> Set
components = I.components . toVector

-- | The set of indices of skew components.
skewComponents :: Perm a => a -> Set
skewComponents = I.components . I.complement . toVector


-- Simple permutations
-- -------------------

-- | A predicate determining if a given permutation is simple.
simple :: Perm a => a -> Bool
simple = I.simple . toVector


-- Subsets
-- -------

-- | A set is represented by an increasing vector of non-negative
-- integers.
type Set = Vector Int

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
