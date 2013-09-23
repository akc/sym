{-# LANGUAGE MagicHash, UnboxedTuples, ForeignFunctionInterface #-}

-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--
-- Convenience functions for dealing with arrays of 'CLong's.

module Data.CLongArray
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

    -- * Map
    , imap

    -- * Low level functions
    , unsafeNew
    , unsafeWith
    ) where

import Data.Ord
import Foreign
import Foreign.C.Types
import GHC.Base

infixl 9 `at`
infixl 9 `unsafeAt`

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}


-- Data type
-- ---------

-- | An array of 'CLong's
data CLongArray = CArr {-# UNPACK #-} !(ForeignPtr CLong) -- elements
                       {-# UNPACK #-} !Int                -- size

instance Show CLongArray where
    show w = "fromList " ++ show (toList w)

instance Eq CLongArray where
    u == v = toList u == toList v

instance Ord CLongArray where
    compare u v =
        case comparing size u v of
          EQ -> comparing toList u v
          x  -> x


-- Conversions
-- -----------

-- | Construct an array from a list of elements.
fromList :: [Int] -> CLongArray
fromList xs = CArr p (length xs)
    where p = inlinePerformIO $ newForeignPtr finalizerFree =<< newArray (map fromIntegral xs)
{-# INLINE fromList #-}

-- | The list of elements.
toList :: CLongArray -> [Int]
toList w = map fromIntegral . inlinePerformIO . unsafeWith w $ peekArray (size w)
{-# INLINE toList #-}

-- | Slice a 'CLongArray' into contiguous segments of the given
-- sizes. Each segment size must be positive and they must sum to the
-- size of the array.
slice :: [Int] -> CLongArray -> [CLongArray]
slice ks w
    | any (<=0) ks     = error "Data.CLongArray.slice: zero or negative parts"
    | sum ks /= size w = error "Data.CLongArray.slice: parts doesn't sum to size of array"
    | otherwise        = unsafeSlice ks w

-- | Like 'slice' but without range checking.
unsafeSlice :: [Int] -> CLongArray -> [CLongArray]
unsafeSlice parts w = inlinePerformIO . unsafeWith w $ go parts
    where
      go []     _ = return []
      go (k:ks) p = do
        vs <- go ks (advancePtr p k)
        v  <- unsafeNew k $ \q -> copyArray q p k
        return (v:vs)


-- Accessors
-- ---------

-- | The size/length of the given array.
size :: CLongArray -> Int
size (CArr _ n) = n
{-# INLINE size #-}

-- | @w \`at\` i@ is the value of @w@ at @i@, where @i@ is in @[0..size w-1]@.
at :: CLongArray -> Int -> Int
at w i =
    let n = size w
    in if (i < 0 || i >= n)
       then error $ "Data.CLongArray.at: " ++ show i ++ " not in [0.." ++ show (n-1) ++ "]"
       else unsafeAt w i
{-# INLINE at #-}

-- | Like 'at' but without range checking.
unsafeAt :: CLongArray -> Int -> Int
unsafeAt w = fromIntegral . inlinePerformIO . unsafeWith w . flip peekElemOff
{-# INLINE unsafeAt #-}


-- Map
-- ---

-- | Apply a function to every element of an array and its index.
imap :: (Int -> CLong -> CLong) -> CLongArray -> CLongArray
imap f w = inlinePerformIO . unsafeWith w $ \p -> unsafeNew n (go 0 p)
    where
      n = size w
      go i p q
        | i >= n = return ()
        | otherwise = do
            x <- peek p
            poke q (f i x)
            go (i+1) (advancePtr p 1) (advancePtr q 1)


-- Low level functions
-- -------------------

-- | Create a new array of the given size that is initialized through
-- an IO action.
unsafeNew :: Int -> (Ptr CLong -> IO ()) -> IO CLongArray
unsafeNew n act = do
  q <- newForeignPtr finalizerFree =<< mallocArray n
  withForeignPtr q act
  return $ CArr q n
{-# INLINE unsafeNew #-}

-- | Pass a pointer to the array to an IO action; the array may not be
-- modified through the pointer.
unsafeWith :: CLongArray -> (Ptr CLong -> IO a) -> IO a
unsafeWith (CArr p _) = withForeignPtr p
{-# INLINE unsafeWith #-}
