{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--

module Math.Sym
    (
      Permutation(..)
    , perms
    , lift
    , lift2
    ) where

import Data.Ord
import Data.SSYT (SSYTPair (..))
import qualified Data.SSYT as Y
import Data.List
import Math.Perm (Perm)
import qualified Math.Perm as P
import qualified Math.Perm.D8 as D8


-- The permutation typeclass
-- -------------------------

-- | The class of permutations. Minimal complete definition: 'st',
-- 'act' and 'idperm'. The default implementation of 'size' can be
-- somewhat slow, so you may want to implement it as well.
class Permutation a where

    -- | The standardization map. If there is an underlying linear
    -- order on @a@ then @st@ is determined by the unique order
    -- preserving map from @[0..]@ to that order. In any case, the
    -- standardization map should be equivariant with respect to the
    -- group action defined below; i.e., it should hold that
    -- 
    -- > st (u `act` v) == u `act` st v
    -- 
    st :: a -> Perm

    -- | A (left) /group action/ of 'Perm' on @a@. As for any group
    -- action it should hold that
    -- 
    -- > (u `act` v) `act` w == u `act` (v `act` w)   &&   idperm n `act` v == v
    -- 
    -- where @v,w::a@ and @u::Perm@ are of size @n@.
    act :: Perm -> a -> a

    -- | The size of a permutation. The default implementation derived from
    -- 
    -- > size == size . st
    -- 
    -- This is not a circular definition as 'size' on 'Perm' is
    -- implemented independently. If the implementation of 'st' is
    -- slow, then it can be worth while to override the standard
    -- definiton; any implementation should, however, satisfy the
    -- identity above.
    {-# INLINE size #-}
    size :: a -> Int
    size = P.size . st

    -- | The identity permutation of the given size.
    idperm :: Int -> a

    -- | The group theoretical inverse. It should hold that
    -- 
    -- > inverse == unst . inverse . st
    -- 
    -- and this is the default implementation.
    {-# INLINE inverse #-}
    inverse :: a -> a
    inverse = unst . D8.inverse . st

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
    ordiso :: Perm -> a -> Bool
    ordiso u v = u == st v

    -- | The inverse of 'st'. It should hold that
    -- 
    -- > unst w == w `act` idperm (P.size w)
    -- 
    -- and this is the default implementation.
    unst :: Permutation a => Perm -> a
    unst w = w `act` idperm (P.size w)

instance Permutation Perm where
    st       = id
    act      = P.act
    idperm   = P.idperm
    inverse  = D8.inverse
    ordiso   = (==)
    unst     = id

-- | A String viewed as a permutation of its characters. The alphabet
-- is ordered as
-- 
-- > ['1'..'9'] ++ ['A'..'Z'] ++ ['a'..]
-- 
instance Permutation String where
    st       = P.mkPerm
    act v    = map snd . sortBy (comparing fst) . zip (P.toList (D8.inverse v))
    size     = length
    idperm n = take n $ ['1'..'9'] ++ ['A'..'Z'] ++ ['a'..]

instance Permutation SSYTPair where
    st = Y.toPerm
    unst = Y.fromPerm
    u `act` v = unst $ u `act` st v
    size (SSYTPair p _) = sum $ map length p
    idperm n = SSYTPair p p where p = [[0..n-1]]
    inverse (SSYTPair p q) = SSYTPair q p

-- | The list of all permutations of the given size.
perms :: Permutation a => Int -> [a]
perms = map unst . P.perms

-- | Lifts a function on 'Perm's to one on any permutations.
lift :: (Permutation a) => (Perm -> Perm) -> a -> a
lift f = unst . f . st

-- | Like 'lift' but for functions of two variables.
lift2 :: (Permutation a) => (Perm -> Perm -> Perm) -> a -> a -> a
lift2 f u v = unst $ f (st u) (st v)
