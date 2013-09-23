{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--

module Data.Perm.Internal
    (
      Set
    , normalize
    , subsets
    ) where

import Data.List
import Data.CLongArray
import Foreign
import Foreign.C.Types
import System.IO.Unsafe


-- | A set is represented by an increasing array of non-negative
-- integers.
type Set = CLongArray

-- Utils
-- -----

-- | Sort and remove duplicates.
normalize :: Ord a => [a] -> [a]
normalize = map head . group . sort


-- Bitmasks
-- --------

-- A sub-class of 'Bits' used internally. Minimal complete definiton: 'next'.
class (Bits a, Integral a) => Bitmask a where
    -- | Lexicographically, the next bitmask with the same Hamming weight.
    next :: a -> a

    -- | @ones k m@ is the set of indices whose bits are set in
    -- @m@. Default implementation:
    -- 
    -- > ones m = fromListN (popCount m) $ filter (testBit m) [0..]
    -- 
    ones :: a -> CLongArray
    ones m = fromList . take (popCount m) $ filter (testBit m) [0..]

instance Bitmask CLong where
    next = nextCLong
    ones = onesCLong

instance Bitmask Integer where
    next = nextIntegral

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
subsets n k
    | n <= 32   = map ones (bitmasks n k :: [CLong])
    | otherwise = map ones (bitmasks n k :: [Integer])

foreign import ccall unsafe "bit.h next" c_next :: CLong -> CLong

-- | Lexicographically, the next 'CLong' with the same Hamming weight.
nextCLong :: CLong -> CLong
nextCLong = c_next

foreign import ccall unsafe "bit.h ones" c_ones :: Ptr CLong -> CLong -> IO ()

-- | @onesCLong m@ gives the indices whose bits are set in @m@.
onesCLong :: CLong -> CLongArray
onesCLong m = unsafeDupablePerformIO . unsafeNew (popCount m) $ flip c_ones m

-- | Lexicographically, the next integral number with the same Hamming weight.
nextIntegral :: (Integral a, Bits a) => a -> a
nextIntegral a =
    let b = (a .|. (a - 1)) + 1
    in  b .|. ((((b .&. (-b)) `div` (a .&. (-a))) `shiftR` 1) - 1)
