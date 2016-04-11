-- |
-- Copyright   : Anders Claesson 2013-2016
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--
-- Convenience functions for dealing with arrays of 'CLong's.

module Sym.Internal.CLongArray
    (
    -- * Data type
    CLongArray

    -- * Conversions
    , fromList
    , toList
    , slices

    -- * Accessors
    , size
    , at
    , unsafeAt
    , elemIndices

    -- * Map
    , imap
    , izipWith

    -- * Low level functions
    , unsafeNew
    , unsafeWith
    ) where

import Data.Ord
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Sym.Internal.Size
import Foreign
import Foreign.C.Types

infixl 9 `at`
infixl 9 `unsafeAt`

-- Data type
-- ---------

-- | An array of 'CLong's
newtype CLongArray = CArr (V.Vector CLong) deriving (Eq)

instance Ord CLongArray where
    compare u v =
        case comparing size u v of
          EQ -> comparing toList u v
          x  -> x

instance Size CLongArray where
    size (CArr w) = V.length w
    {-# INLINE size #-}

instance Show CLongArray where
    show w = "fromList " ++ show (toList w)


-- Conversions
-- -----------

-- | Construct an array from a list of elements.
fromList :: [Int] -> CLongArray
fromList = CArr . V.fromList . map fromIntegral

-- | The list of elements.
toList :: CLongArray -> [Int]
toList (CArr w) = map fromIntegral $ V.toList w

-- | Slice a 'CLongArray' into contiguous segments of the given
-- sizes. Each segment size must be positive and they must sum to the
-- size of the array.
slices :: [Int] -> CLongArray -> [CLongArray]
slices [] _ = []
slices (k:ks) (CArr w) = let (u,v) = V.splitAt k w in CArr u : slices ks (CArr v)


-- Accessors
-- ---------

-- | @w \`at\` i@ is the value of @w@ at @i@, where @i@ is in @[0..size w-1]@.
at :: CLongArray -> Int -> Int
at (CArr w) i =
    case (V.!?) w i of
      Nothing -> error "Sym.Internal.CLongArray.at: out of range"
      Just j  -> fromIntegral j

-- | Like 'at' but without range checking.
unsafeAt :: CLongArray -> Int -> Int
unsafeAt (CArr w) = fromIntegral . (V.!) w

-- | The indices of all elements equal to the query element, in
-- ascending order.
elemIndices :: CLong -> CLongArray -> V.Vector Int
elemIndices x (CArr w) = V.elemIndices x w


-- Map and Zip
-- -----------

-- | Apply a function to every element of an array and its index.
imap :: (Int -> CLong -> CLong) -> CLongArray -> CLongArray
imap f (CArr w) =  CArr (V.imap f w)

-- | Apply a function to corresponding pairs of elements and their (shared) index.
izipWith :: (Int -> CLong -> CLong -> CLong) -> CLongArray -> CLongArray -> CLongArray
izipWith f (CArr u) (CArr v) = CArr (V.izipWith f u v)

-- Low level functions
-- -------------------

-- | Create a new array of the given size that is initialized through
-- an IO action.
unsafeNew :: Int -> (Ptr CLong -> IO ()) -> IO CLongArray
unsafeNew n act = do
    v <- V.unsafeFreeze =<< MV.unsafeNew n
    let (ptr, _) = V.unsafeToForeignPtr0 v
    withForeignPtr ptr act
    return $ CArr (V.unsafeFromForeignPtr0 ptr n)

-- | Pass a pointer to the array to an IO action; the array may not be
-- modified through the pointer.
unsafeWith :: CLongArray -> (Ptr CLong -> IO a) -> IO a
unsafeWith (CArr w) = V.unsafeWith w
