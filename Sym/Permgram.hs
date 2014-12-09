-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--
-- Permutation diagrams, or permutations as monads.

module Sym.Permgram
    (
    -- * Data types
      Label
    , Permgram

    -- * Accessors
    , perm
    , label
    , size

    -- * Construct permgrams
    , permgram
    , inverse
    ) where

import Data.Ord
import Data.List
import Control.Monad
import Control.Applicative
import Sym.Perm (Perm)
import qualified Sym.Perm as P
import Data.Array.Unboxed

-- | The purpose of this data type is to assign labels to the indices of
-- a given permutation.
type Label a = Array Int a

-- | A permgram consists of a permutation together with a label for each
-- index of the permutation.
data Permgram a = PGram {
      -- | The underlying permutation.
      perm  :: Perm
      -- | The assignment of labels to indices.
    , label :: Label a
    }

constituents :: Permgram a -> (Perm, [a])
constituents (PGram v f) = (v, elems f)

instance Show a => Show (Permgram a) where
    show w =
        let (v, ys) = constituents w
        in unwords ["permgram", "(" ++ show v ++ ")", show ys]

instance Eq a => Eq (Permgram a) where
    u == v = constituents u == constituents v

instance Ord a => Ord (Permgram a) where
    compare u v =
        case comparing size u v of
          EQ -> comparing constituents u v
          x  -> x

-- | Construct a permgram from an underlying permutation and a list of
-- labels.
permgram :: Perm -> [a] -> Permgram a
permgram v = PGram v . listArray (0, P.size v - 1) . cycle

-- | The inverse permgram. It's obtained by mirroring the permgram in
-- the /x=y/ diagonal.
inverse :: Permgram a -> Permgram a
inverse (PGram u f) = PGram (P.fromList v) (listArray (0,n-1) (map (f!) v))
    where
      v = map snd . sort $ zip (P.toList u) [0..] -- v = u^{-1}
      n = P.size u

-- | The size of a permgram is the size of the underlying permutation.
size :: Permgram a -> Int
size = P.size . perm

instance Functor Permgram where
    fmap f w = w { label = amap f (label w) }

instance Monad Permgram where
    return x = permgram (P.fromList [0]) [x]
    w >>= f  = joinPermgram $ fmap f w

instance Applicative Permgram where
    pure  = return
    (<*>) = ap

joinPermgram :: Permgram (Permgram a) -> Permgram a
joinPermgram w@(PGram u f) = PGram (P.fromList xs) (listArray (0,m-1) ys)
    where
      len = amap size f
      m = sum $ elems len
      n = size w
      uInverse = map snd . sort $ zip (P.toList u) [0..]
      a :: UArray Int Int
      a = listArray (0,n-1) . scanl (+) 0 $ map (len!) uInverse
      (xs, ys) = unzip $ do
        i <- [0..n-1]
        let PGram v g = f!i
        let d = a ! (u `P.unsafeAt` i)
        [ (d + v `P.unsafeAt` j, g!j) | j <- [0..len!i-1] ]
