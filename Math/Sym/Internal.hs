{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Math.Sym.Internal
-- Copyright   : (c) Anders Claesson 2012
-- License     : BSD-style
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- 
-- An internal module used by the sym package.
-- 
-- A Lehmercode is a vector of integers @w@ such @w!i <= length w - 1 - i@
-- for each @i@ in @[0..length w - 1]@; such a vector encodes a permutation.
-- This module implements /O(n)/ algorithms for unranking Lehmercodes and
-- permutations; the algorithms are due to W. Myrvold and F. Ruskey
-- [Ranking and Unranking Permutations in Linear Time, Information Processing
-- Letters, 79 (2001) 281-284].
-- 
-- In addition, this module implements sorting operators, the
-- symmetries in D8 acting on permutations, as well as most of the
-- common permutation statistics.

module Math.Sym.Internal
    (
      Lehmercode
    , Perm0

    -- * Lehmercodes
    , unrankLehmercode
    , fromLehmercode
    , randomLehmercode
    , lehmercodes

    -- * Permutations
    , size
    , toList
    , fromList
    , act
    , unrankPerm
    , randomPerm
    , sym
    , idperm
    , revIdperm
    , sti
    , st
    , ordiso
    , simple
    , copies
    , avoiders

    -- * Permutation symmetries
    , reverse
    , complement
    , inverse
    , rotate

    -- * Permutation statistics
    , asc     -- ascents
    , des     -- descents
    , exc     -- excedances
    , fp      -- fixed points
    , cyc     -- cycles
    , inv     -- inversions
    , maj     -- the major index
    , comaj   -- the co-major index
    , peak    -- peaks
    , vall    -- valleys
    , dasc    -- double ascents
    , ddes    -- double descents
    , lmin    -- left-to-right minima
    , lmax    -- left-to-right maxima
    , rmin    -- right-to-left minima
    , rmax    -- right-to-left maxima
    , head    -- the first element
    , last    -- the last element
    , lir     -- left-most increasing run
    , ldr     -- left-most decreasing run
    , rir     -- right-most increasing run
    , rdr     -- right-most decreasing run
    , comp    -- components
    , scomp   -- skew components
    , ep      -- rank a la Elizalde & Pak
    , dim     -- dimension
    , asc0    -- small ascents
    , des0    -- small descents

    -- * Left-to-right maxima, etc
    , lMaxima
    , rMaxima

    -- * Components
    , components

    -- * Sorting operators
    , stackSort
    , bubbleSort

    -- * Single point deletions
    , del

    -- * Bitmasks
    , onesCUInt
    , nextCUInt
    , nextIntegral
    ) where

import Prelude hiding (reverse, head, last)
import qualified Prelude (head)
import System.Random (getStdRandom, randomR)
import Control.Monad (forM_, liftM)
import Control.Monad.ST (runST)
import Data.List (group)
import Data.Bits (Bits, shiftR, (.|.), (.&.), popCount)
import qualified Data.Vector.Storable as SV
    ( Vector, toList, fromList, length, (!), thaw
    , unsafeFreeze, unsafeWith, enumFromN, enumFromStepN
    , head, last, filter, maximum, minimum, null, reverse, map
    )
import qualified Data.Vector.Storable.Mutable as MV
    ( unsafeNew, unsafeWrite, unsafeWith, unsafeSlice, swap, replicate
    )
import Foreign (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.Types (CLong(..), CInt(..), CUInt(..))
import Foreign.Marshal.Utils (toBool)

-- | A Lehmercode is a vector of integers @w@ such @w!i <= length w - 1 - i@
-- for each @i@ in @[0..length w - 1]@.
type Lehmercode = SV.Vector Int

-- | By convention, a member of @Perm0@ is a permutation of some
-- finite subset of @[0..]@.
type Perm0 = SV.Vector Int


-- Lehmercodes
-- -----------

-- | @unrankLehmercode n rank@ is the @rank@-th Lehmercode of length @n@.
unrankLehmercode :: Int -> Integer -> Lehmercode
unrankLehmercode n rank = runST $ do
  v <- MV.unsafeNew n
  iter v n rank (toInteger n)
  SV.unsafeFreeze v
    where
      {-# INLINE iter #-}
      iter _ 0 _ _ = return ()
      iter v i r m = do
        let (r',j) = quotRem r m
        MV.unsafeWrite v (n-i) (fromIntegral j)
        iter v (i-1) r' (m-1)

-- | Build a permutation from its Lehmercode.
fromLehmercode :: Lehmercode -> Perm0
fromLehmercode code = runST $ do
  let n = SV.length code
  v <- MV.unsafeNew n
  forM_ [0..n-1] $ \i -> MV.unsafeWrite v i i
  forM_ [0..n-1] $ \i -> MV.swap v i (i + (SV.!) code i)
  SV.unsafeFreeze v

-- | A random Lehmercode of the given length.
randomLehmercode :: Int -> IO Lehmercode
randomLehmercode n = unrankLehmercode n `liftM` getStdRandom (randomR (0, factorial n - 1))

-- | The list of Lehmercodes of a given length.
lehmercodes :: Int -> [Lehmercode]
lehmercodes n = map (unrankLehmercode n) [0 .. factorial n - 1]


-- Permutations
-- ------------

-- | The size of a permutation; the number of elements.
size :: Perm0 -> Int
size = SV.length

-- | The list of images of a permutation.
toList :: Perm0 -> [Int]
toList = SV.toList

-- | Make a permutation from a list of images.
fromList :: [Int] -> Perm0
fromList = SV.fromList

-- | @act u v@ is the permutation /w/ defined by /w(u(i)) = v(i)/.
act :: Perm0 -> Perm0 -> Perm0
act u v = runST $ do
  let n = SV.length u
  w <- MV.unsafeNew n
  forM_ [0..n-1] $ \i -> MV.unsafeWrite w i ((SV.!) v ((SV.!) u i))
  SV.unsafeFreeze w

factorial :: Integral a => a -> Integer
factorial = product . enumFromTo 1 . toInteger 

-- | @unrankPerm n rank@ is the @rank@-th (Myrvold & Ruskey) permutation of length @n@.
unrankPerm :: Int -> Integer -> Perm0
unrankPerm n = fromLehmercode . unrankLehmercode n

-- | A random permutation of the given length.
randomPerm :: Int -> IO Perm0
randomPerm n = fromLehmercode `liftM` randomLehmercode n

-- | @sym n@ is the list of permutations of @[0..n-1]@ (the symmetric group).
sym :: Int -> [Perm0]
sym n = map (unrankPerm n) [0 .. factorial n - 1]

-- | The identity permutation of the given length.
idperm :: Int -> Perm0
idperm = SV.enumFromN 0

-- | The reverse of the identity permutation.
revIdperm :: Int -> Perm0
revIdperm n = SV.enumFromStepN (n-1) (-1) n

-- | @sti w@ is the inverse of the standardization of @w@ (a
-- permutation on @[0..length w-1]@). E.g., @sti \<4,9,2\> ==
-- \<2,0,1\>@.
sti :: Perm0 -> Perm0
sti w = runST $ do
  let a = if SV.null w then 0 else SV.minimum w
  let b = if SV.null w then 0 else SV.maximum w
  let n = SV.length w
  v <- MV.replicate (1 + b - a) (-1)
  forM_ [0..n-1] $ \i -> MV.unsafeWrite v ((SV.!) w i - a) i
  SV.filter (>=0) `liftM` SV.unsafeFreeze v

-- | The standardization map.
st :: Perm0 -> Perm0
st = inverse . sti

foreign import ccall unsafe "ordiso.h ordiso" c_ordiso
    :: Ptr CLong -> Ptr CLong -> Ptr CLong -> CLong -> CInt

-- | @ordiso u v m@ determines whether the subword in @v@ specified by
-- @m@ is order isomorphic to @u@.
ordiso :: Perm0 -> Perm0 -> SV.Vector Int -> Bool
ordiso u v m =
    let k = fromIntegral (SV.length u)
    in  unsafePerformIO $
        SV.unsafeWith u $ \u' ->
        SV.unsafeWith v $ \v' ->
        SV.unsafeWith m $ \m' ->
        return . toBool $ c_ordiso (castPtr u') (castPtr v') (castPtr m') k

foreign import ccall unsafe "simple.h simple" c_simple
    :: Ptr CLong -> CLong -> CInt

-- | @simple w@ determines whether @w@ is simple
simple :: Perm0 -> Bool
simple w =
    let n = fromIntegral (SV.length w)
    in  unsafePerformIO $
        SV.unsafeWith w $ \w' ->
        return . toBool $ c_simple (castPtr w') n

-- | @copies subsets p w@ is the list of bitmasks that represent copies of @p@ in @w@.
copies :: (Int -> Int -> [SV.Vector Int]) -> Perm0 -> Perm0 -> [SV.Vector Int]
copies subsets p w = filter (ordiso p w) $ subsets n k
    where
      n = SV.length w
      k = SV.length p

avoiders1 :: (Int -> Int -> [SV.Vector Int]) -> (a -> Perm0) -> Perm0 -> [a] -> [a]
avoiders1 subsets f p ws =
    let ws0 = map f ws
        ws2 = zip ws0 ws
    in case group (map SV.length ws0) of
         []  -> []
         [_] -> let k = SV.length p
                    n = SV.length (Prelude.head ws0)
                in  [ v | (v0,v) <- ws2,  not $ any (ordiso p v0) (subsets n k) ]
         _   ->     [ v | (v0,v) <- ws2, null $ copies subsets p v0 ] 

-- | @avoiders subsets st ps ws@ is the list of permutations in @ws@
-- avoiding the patterns in @ps@.
avoiders :: (Int -> Int -> [SV.Vector Int]) -> (a -> Perm0) -> [Perm0] -> [a] -> [a]
avoiders _       _   []   ws = ws
avoiders subsets f (p:ps) ws = avoiders subsets f ps $ avoiders1 subsets f p ws


-- Permutation symmetries
-- ----------------------

-- | @reverse \<a_1,...,a_n\> == \<a_n,,...,a_1\>@. E.g., @reverse
-- \<9,3,7,2\> == \<2,7,3,9\>@.
reverse :: Perm0 -> Perm0
reverse = SV.reverse

-- | @complement \<a_1,...,a_n\> == \<b_1,,...,b_n\>@, where @b_i = n - a_i - 1@.
-- E.g., @complement \<3,4,0,1,2\> == \<1,0,4,3,2\>@.
complement :: Perm0 -> Perm0
complement w = SV.map (\x -> SV.length w - x - 1) w

-- | @inverse w@ is the group theoretical inverse of @w@. E.g.,
-- @inverse \<1,2,0\> == \<2,0,1\>@.
inverse :: Perm0 -> Perm0
inverse w = runST $ do
  let n = SV.length w
  v <- MV.unsafeNew n
  forM_ [0..n-1] $ \i -> MV.unsafeWrite v ((SV.!) w i) i
  SV.unsafeFreeze v

-- | The clockwise rotatation through 90 degrees. E.g.,
-- @rotate \<1,0,2\> == \<1,2,0\>@.
rotate :: Perm0 -> Perm0
rotate w = runST $ do
  let n = SV.length w
  v <- MV.unsafeNew n
  forM_ [0..n-1] $ \i -> MV.unsafeWrite v ((SV.!) w (n-1-i)) i
  SV.unsafeFreeze v


-- Permutation statistics
-- ----------------------

foreign import ccall unsafe "stat.h asc" c_asc
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h des" c_des
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h exc" c_exc
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h fp" c_fp
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h cyc" c_cyc
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h inv" c_inv
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h maj" c_maj
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h comaj" c_comaj
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h peak" c_peak
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h vall" c_vall
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h dasc" c_dasc
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h ddes" c_ddes
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h lmin" c_lmin
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h lmax" c_lmax
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h lir" c_lir
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h ldr" c_ldr
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h comp" c_comp
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h ep" c_ep
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h dim" c_dim
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h asc0" c_asc0
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h des0" c_des0
    :: Ptr CLong -> CLong -> CLong

-- Marshal a permutation statistic defined in C to on in Haskell.
stat :: (Ptr CLong -> CLong -> CLong) -> Perm0 -> Int
stat f w = unsafePerformIO $
           SV.unsafeWith w $ \ptr ->
               return . fromIntegral $ f (castPtr ptr) (fromIntegral (SV.length w))

-- | First (left-most) value of a permutation.
head :: Perm0 -> Int
head = SV.head

-- | Last (right-most) value of a permutation.
last :: Perm0 -> Int
last = SV.last

-- | The number of left-to-right minima.
rmin :: Perm0 -> Int
rmin = lmin . SV.reverse

-- | The number of left-to-right maxima.
rmax :: Perm0 -> Int
rmax = lmax . SV.reverse

-- | The right-most increasing run.
rir :: Perm0 -> Int
rir = ldr . SV.reverse

-- | The right-most decreasing run.
rdr :: Perm0 -> Int
rdr = lir . SV.reverse

-- | The number of ascents.
asc :: Perm0 -> Int
asc = stat c_asc

-- | The number of descents.
des :: Perm0 -> Int
des = stat c_des

-- | The number of inversions.
inv :: Perm0 -> Int
inv = stat c_inv

-- | The major index.
maj :: Perm0 -> Int
maj = stat c_maj

-- | The co-major index.
comaj :: Perm0 -> Int
comaj = stat c_comaj

-- | The number of peaks.
peak :: Perm0 -> Int
peak = stat c_peak

-- | The number of valleys.
vall :: Perm0 -> Int
vall = stat c_vall

-- | The number of double ascents.
dasc :: Perm0 -> Int
dasc = stat c_dasc

-- | The number of double descents.
ddes :: Perm0 -> Int
ddes = stat c_ddes

-- | The number of left-to-right minima.
lmin :: Perm0 -> Int
lmin = stat c_lmin

-- | The number of left-to-right maxima.
lmax :: Perm0 -> Int
lmax = stat c_lmax

-- | The left-most increasing run.
lir :: Perm0 -> Int
lir = stat c_lir

-- | The left-most decreasing run.
ldr :: Perm0 -> Int
ldr = stat c_ldr

-- | The number of excedances.
exc :: Perm0 -> Int
exc = stat c_exc

-- | The number of fixed points.
fp :: Perm0 -> Int
fp = stat c_fp

-- | The number of cycles.
cyc :: Perm0 -> Int
cyc = stat c_cyc

-- | The number of components.
comp :: Perm0 -> Int
comp = stat c_comp

-- | The number of skew components. 
scomp :: Perm0 -> Int
scomp = comp . complement

-- | Rank as defined by Elizalde & Pak.
ep :: Perm0 -> Int
ep = stat c_ep

-- | Dimension (largest non-fixed-point).
dim :: Perm0 -> Int
dim = stat c_dim

-- | The number of small ascents.
asc0 :: Perm0 -> Int
asc0 = stat c_asc0

-- | The number of small descents.
des0 :: Perm0 -> Int
des0 = stat c_des0


-- Left-to-right maxima, etc
-- -------------------------

-- | The set of indices of left-to-right maxima.
lMaxima :: Perm0 -> SV.Vector Int
lMaxima w = runST $ do
  v <- MV.unsafeNew n
  k <- iter v n 0 (-1)
  SV.unsafeFreeze $ MV.unsafeSlice 0 k v
    where
      n = size w
      {-# INLINE iter #-}
      iter _ 0 _ _ = return 0
      iter v i j m = do
        let m' = (SV.!) w (n-i)
        if m' > m then do
            MV.unsafeWrite v j (n-i)
            (+1) `liftM` iter v (i-1) (j+1) m'
          else
            iter v (i-1) j m

-- | The set of indices of right-to-left maxima.
rMaxima :: Perm0 -> SV.Vector Int
rMaxima w = SV.reverse . SV.map (\x -> SV.length w - x - 1) . lMaxima $ reverse w


-- Components
-- ----------

-- | The set of indices of components.
components :: Perm0 -> SV.Vector Int
components w = runST $ do
  v <- MV.unsafeNew n
  k <- iter v n 0 (-1)
  SV.unsafeFreeze $ MV.unsafeSlice 0 k v
    where
      n = size w
      {-# INLINE iter #-}
      iter _ 0 _ _ = return 0
      iter v i j m = do
        let m' = max m $ (SV.!) w (n-i)
        if m' == n-i then do
            MV.unsafeWrite v j (n-i)
            (+1) `liftM` iter v (i-1) (j+1) m'
          else
            iter v (i-1) j m'

-- Sorting operators
-- -----------------

foreign import ccall unsafe "sortop.h stacksort" c_stacksort
    :: Ptr CLong -> CLong -> IO ()

foreign import ccall unsafe "sortop.h bubblesort" c_bubblesort
    :: Ptr CLong -> CLong -> IO ()

-- Marshal a sorting operator defined in C to on in Haskell.
sortop :: (Ptr CLong -> CLong -> IO ()) -> Perm0 -> Perm0
sortop f w = unsafePerformIO $ do
               v <- SV.thaw w
               MV.unsafeWith v $ \ptr -> do
                 f (castPtr ptr) (fromIntegral (SV.length w))
                 SV.unsafeFreeze v

-- | One pass of stack-sort.
stackSort :: Perm0 -> Perm0
stackSort = sortop c_stacksort

-- | One pass of bubble-sort.
bubbleSort :: Perm0 -> Perm0
bubbleSort = sortop c_bubblesort


-- Single point deletions
-- ----------------------

-- | Delete the element at a given position
del :: Int -> Perm0 -> Perm0
del i u = runST $ do
  let n = SV.length u
  let j = (SV.!) u i
  v <- MV.unsafeNew (n-1)
  forM_ [0..i-1] $ \k -> do
            let m = (SV.!) u k
            MV.unsafeWrite v k (if m < j then m else m-1)
  forM_ [i+1..n-1] $ \k -> do
            let m = (SV.!) u k
            MV.unsafeWrite v (k-1) (if m < j then m else m-1)
  SV.unsafeFreeze v


-- Bitmasks
-- --------

foreign import ccall unsafe "bit.h next" c_next :: CUInt -> CUInt

-- | Lexicographically, the next 'CUInt' with the same Hamming weight.
nextCUInt :: CUInt -> CUInt
nextCUInt = c_next

foreign import ccall unsafe "bit.h ones" c_ones :: Ptr CUInt -> CUInt -> IO ()

-- | @onesCUInt k m@ gives the @k@ smallest indices whose bits are set in @m@.
onesCUInt :: CUInt -> SV.Vector Int
onesCUInt m = SV.map fromIntegral . unsafePerformIO $ do
                v <- MV.unsafeNew (popCount m)
                MV.unsafeWith v $ \ptr -> do
                  c_ones ptr m
                  SV.unsafeFreeze v

-- | Lexicographically, the next integral number with the same Hamming weight.
nextIntegral :: (Integral a, Bits a) => a -> a
nextIntegral a =
    let b = (a .|. (a - 1)) + 1
    in  b .|. ((((b .&. (-b)) `div` (a .&. (-a))) `shiftR` 1) - 1)
