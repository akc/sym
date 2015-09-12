{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- |
-- Copyright   : Anders Claesson 2013
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
    , slice
    , unsafeSlice

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
import Data.Serialize
import Sym.Internal.Size
import Foreign
import Foreign.C.Types
import System.IO.Unsafe

infixl 9 `at`
infixl 9 `unsafeAt`

-- Data type
-- ---------

-- | An array of 'CLong's
data CLongArray = CArr {-# UNPACK #-} !(ForeignPtr CLong) -- elements
                       {-# UNPACK #-} !Int                -- size

instance Serialize CLongArray where
    put = put . toList
    get = fmap fromList get

instance Show CLongArray where
    show w = "fromList " ++ show (toList w)

instance Eq CLongArray where
    u == v = toList u == toList v

instance Ord CLongArray where
    compare u v =
        case comparing size u v of
          EQ -> comparing toList u v
          x  -> x

instance Size CLongArray where
    size (CArr _ n) = n
    {-# INLINE size #-}


-- Conversions
-- -----------

-- | Construct an array from a list of elements.
fromList :: [Int] -> CLongArray
fromList xs = CArr p (length xs)
  where
    p = unsafePerformIO $ newForeignPtr finalizerFree =<< newArray (map fromIntegral xs)

-- | The list of elements.
toList :: CLongArray -> [Int]
toList w = map fromIntegral . unsafePerformIO . unsafeWith w $ peekArray (size w)

-- | Slice a 'CLongArray' into contiguous segments of the given
-- sizes. Each segment size must be positive and they must sum to the
-- size of the array.
slice :: [Int] -> CLongArray -> [CLongArray]
slice ks w
    | any (<=0) ks     = error "Sym.Internal.CLongArray.slice: zero or negative parts"
    | sum ks /= size w = error "Sym.Internal.CLongArray.slice: parts doesn't sum to size of array"
    | otherwise        = unsafeSlice ks w

-- | Like 'slice' but without range checking.
unsafeSlice :: [Int] -> CLongArray -> [CLongArray]
unsafeSlice parts w = unsafePerformIO . unsafeWith w $ go parts
  where
    go []     _ = return []
    go (k:ks) p = do
      vs <- go ks (advancePtr p k)
      v  <- unsafeNew k $ \q -> copyArray q p k
      return (v:vs)


-- Accessors
-- ---------

-- | @w \`at\` i@ is the value of @w@ at @i@, where @i@ is in @[0..size w-1]@.
at :: CLongArray -> Int -> Int
at w i =
    let n = size w
    in if i < 0 || i >= n
       then error $ "Sym.Internal.CLongArray.at: " ++ show i ++ " not in [0.." ++ show (n-1) ++ "]"
       else unsafeAt w i

-- | Like 'at' but without range checking.
unsafeAt :: CLongArray -> Int -> Int
unsafeAt w = fromIntegral . unsafePerformIO . unsafeWith w . flip peekElemOff

-- | The indices of all elements equal to the query element, in
-- ascending order.
elemIndices :: CLong -> CLongArray -> [Int]
elemIndices x w = unsafePerformIO $ unsafeWith w (go 0)
  where
    n = size w
    go i p
      | i >= n = return []
      | otherwise = do
          y <- peek p
          ([ i | y == x ] ++) `fmap` go (i+1) (advancePtr p 1)


-- Map and Zip
-- -----------

-- | Apply a function to every element of an array and its index.
imap :: (Int -> CLong -> CLong) -> CLongArray -> CLongArray
imap f w = unsafePerformIO . unsafeWith w $ \p -> unsafeNew n (go 0 p)
  where
    n = size w
    go i p q
      | i >= n = return ()
      | otherwise = do
          x <- peek p
          poke q (f i x)
          go (i+1) (advancePtr p 1) (advancePtr q 1)

-- | Apply a function to corresponding pairs of elements and their (shared) index.
izipWith :: (Int -> CLong -> CLong -> CLong) -> CLongArray -> CLongArray -> CLongArray
izipWith f u v =
    unsafePerformIO . unsafeWith u $ \p -> unsafeWith v $ \q -> unsafeNew n (go 0 p q)
  where
    n = min (size u) (size v)
    go i p q r
      | i >= n = return ()
      | otherwise = do
          x <- peek p
          y <- peek q
          poke r (f i x y)
          go (i+1) (advancePtr p 1) (advancePtr q 1) (advancePtr r 1)

-- Low level functions
-- -------------------

-- | Create a new array of the given size that is initialized through
-- an IO action.
unsafeNew :: Int -> (Ptr CLong -> IO ()) -> IO CLongArray
unsafeNew n act = do
  q <- newForeignPtr finalizerFree =<< mallocArray n
  withForeignPtr q act
  return $ CArr q n

-- | Pass a pointer to the array to an IO action; the array may not be
-- modified through the pointer.
unsafeWith :: CLongArray -> (Ptr CLong -> IO a) -> IO a
unsafeWith (CArr p _) = withForeignPtr p
