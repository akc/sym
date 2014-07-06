{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--
-- Generating permutations: rank and unrank

module Sym.Perm
    (
      module Sym.Internal.CLongArray
    , Perm
    , emptyperm
    , one
    , idperm
    , ebb
    , mkPerm
    , rank
    , unrank
    , perms
    ) where

import Data.List
import Sym.Internal.CLongArray
import Foreign
import Foreign.C.Types
import System.IO.Unsafe

-- | A permutation is just a 'CLongArray'. By convention a permutation
-- of size @n@ is understood to be a permutation of @[0..n-1]@.
type Perm = CLongArray

-- | The unique permutation length zero.
emptyperm :: Perm
emptyperm = fromList []

-- | The unique permutation length one.
one :: Perm
one = fromList [0]

-- | The identity permutation.
idperm :: Int -> Perm
idperm n = fromList [0..n-1]

-- | The reverse of the identity permutation.
ebb :: Int -> Perm
ebb n = fromList [n-1,n-2..0]

-- | Construct a permutation from a list of elements. As opposed to
-- 'fromList' this is a safe function in the sense that the output of
-- @mkPerm xs@ is guaranteed to be a permutation of @[0..length xs-1]@.
-- E.g., @mkPerm \"baxa\" == fromList [2,0,3,1]@.
mkPerm :: Ord a => [a] -> Perm
mkPerm xs =
    let sti ys = map snd . sort $ zip ys [ 0::Int .. ]
    in fromList $ (sti . sti) xs

foreign import ccall unsafe "rank.h rank" c_rank
    :: Ptr CLong -> CLong -> IO CDouble

-- | The rank of the given permutation, where the rank is defined as
-- in [W. Myrvold and F. Ruskey, Ranking and Unranking Permutations in
-- Linear Time, Information Processing Letters, 79 (2001) 281-284].
rank :: Perm -> Integer
rank w =
    let n = fromIntegral (size w)
    in truncate . unsafeDupablePerformIO . unsafeWith w $ flip c_rank n
{-# INLINE rank #-}

foreign import ccall unsafe "rank.h unrank" c_unrank
    :: Ptr CLong -> CLong -> CDouble -> IO ()

-- | The permutation of size @n@ whose rank is @r@, where the rank
-- is defined as in [W. Myrvold and F. Ruskey, Ranking and Unranking
-- Permutations in Linear Time, Information Processing Letters, 79
-- (2001) 281-284].
unrank :: Int -> Integer -> Perm
unrank n r =
    unsafeDupablePerformIO . unsafeNew n $ \ptr ->
        c_unrank ptr (fromIntegral n) (fromIntegral r)
{-# INLINE unrank #-}

-- | All permutations of a given size.
perms :: Int -> [Perm]
perms n = map (unrank n) [0..nFac-1] where nFac = product [1..toInteger n]
