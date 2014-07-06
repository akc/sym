{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--
-- Common permutation statistics. To avoid name clashes this module is
-- best imported @qualified@; e.g.
-- 
-- > import qualified Sym.Perm.Stat as S
-- 

module Sym.Perm.Stat 
    (
      asc         -- ascents
    , des         -- descents
    , exc         -- excedances
    , fp          -- fixed points
    , sfp         -- strong fixed points
    , cyc         -- cycles
    , inv         -- inversions
    , maj         -- the major index
    , comaj       -- the co-major index
    , peak        -- peaks
    , vall        -- valleys
    , dasc        -- double ascents
    , ddes        -- double descents
    , lmin        -- left-to-right minima
    , lmax        -- left-to-right maxima
    , rmin        -- right-to-left minima
    , rmax        -- right-to-left maxima
    , head        -- the first element
    , last        -- the last element
    , lir         -- left-most increasing run
    , ldr         -- left-most decreasing run
    , rir         -- right-most increasing run
    , rdr         -- right-most decreasing run
    , comp        -- components
    , scomp       -- skew components
    , ep          -- rank a la Elizalde & Pak
    , dim         -- dimension
    , asc0        -- small ascents
    , des0        -- small descents
    , lis         -- longest increasing subsequence
    , lds         -- longest decreasing subsequence
--    , shad        -- shadow
    ) where

import Prelude hiding (head, last)
import qualified Prelude
import Sym.Perm
import qualified Sym.Perm.SSYT as Y
import qualified Sym.Perm.D8 as D8
import Foreign.Ptr
import Foreign.C.Types
import System.IO.Unsafe

marshal :: (Ptr CLong -> CLong -> CLong) -> Perm -> Int
marshal f w =
    fromIntegral . unsafeDupablePerformIO . unsafeWith w $ \p ->
        return $ f p (fromIntegral (size w))
{-# INLINE marshal #-}

foreign import ccall unsafe "stat.h asc" c_asc
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h des" c_des
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h exc" c_exc
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h fp" c_fp
    :: Ptr CLong -> CLong -> CLong

foreign import ccall unsafe "stat.h sfp" c_sfp
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

-- | The number of ascents. An /ascent/ in @w@ is an index @i@ such
-- that @w[i] \< w[i+1]@.
asc :: Perm -> Int
asc = marshal c_asc

-- | The number of descents. A /descent/ in @w@ is an index @i@ such
-- that @w[i] > w[i+1]@.
des :: Perm -> Int
des = marshal c_des

-- | The number of /excedances/: positions @i@ such that @w[i] > i@.
exc :: Perm -> Int
exc = marshal c_exc

-- | The number of /fixed points/: positions @i@ such that @w[i] == i@.
fp :: Perm -> Int
fp = marshal c_fp

-- | The number of /strong fixed points/ (also called splitters):
-- positions @i@ such that @w[j] \< i@ for @j \< i@ and @w[j] \> i@ for @j \> i@.
sfp :: Perm -> Int
sfp = marshal c_sfp

-- | The number of /cycles/:
-- orbits of the permutation when viewed as a function.
cyc :: Perm -> Int
cyc = marshal c_cyc

-- | The number of /inversions/:
-- pairs @\(i,j\)@ such that @i \< j@ and @w[i] > w[j]@.
inv :: Perm -> Int
inv = marshal c_inv

-- | /The major index/ is the sum of descents.
maj :: Perm -> Int
maj = marshal c_maj

-- | /The co-major index/ is the sum of descents.
comaj :: Perm -> Int
comaj = marshal c_comaj

-- | The number of /peaks/:
-- positions @i@ such that @w[i-1] \< w[i]@ and @w[i] \> w[i+1]@.
peak :: Perm -> Int
peak = marshal c_peak

-- | The number of /valleys/:
-- positions @i@ such that @w[i-1] \> w[i]@ and @w[i] \< w[i+1]@.
vall :: Perm -> Int
vall = marshal c_vall

-- | The number of /double ascents/:
-- positions @i@ such that @w[i-1] \<  w[i] \< w[i+1]@.
dasc :: Perm -> Int
dasc = marshal c_dasc

-- | The number of /double descents/:
-- positions @i@ such that @w[i-1] \>  w[i] \> w[i+1]@.
ddes :: Perm -> Int
ddes = marshal c_ddes

-- | The number of /left-to-right minima/:
-- positions @i@ such that @w[i] \< w[j]@ for all @j \< i@.
lmin :: Perm -> Int
lmin = marshal c_lmin

-- | The number of /left-to-right maxima/:
-- positions @i@ such that @w[i] \> w[j]@ for all @j \< i@.
lmax :: Perm -> Int
lmax = marshal c_lmax

-- | The number of /right-to-left minima/:
-- positions @i@ such that @w[i] \< w[j]@ for all @j \> i@.
rmin :: Perm -> Int
rmin = lmin . D8.reverse

-- | The number of /right-to-left maxima/:
-- positions @i@ such that @w[i] \> w[j]@ for all @j \> i@.
rmax :: Perm -> Int
rmax = lmax . D8.reverse

-- | The first (left-most) element in the standardization. E.g.,
-- @head \"231\" = head (fromList [1,2,0]) = 1@.
head :: Perm -> Int
head w | size w > 0 = w `unsafeAt` 0
       | otherwise  = 0

-- | The last (right-most) element in the standardization. E.g.,
-- @last \"231\" = last (fromList [1,2,0]) = 0@.
last :: Perm -> Int
last w | size w > 0 = w `unsafeAt` (size w - 1)
       | otherwise  = 0

-- | Length of the left-most increasing run: largest @i@ such that
-- @w[0] \< w[1] \< ... \< w[i-1]@.
lir :: Perm -> Int
lir = marshal c_lir

-- | Length of the left-most decreasing run: largest @i@ such that
-- @w[0] \> w[1] \> ... \> w[i-1]@.
ldr :: Perm -> Int
ldr = marshal c_ldr

-- | Length of the right-most increasing run: largest @i@ such that
-- @w[n-i] \< ... \< w[n-2] \< w[n-1]@.
rir :: Perm -> Int
rir = ldr . D8.reverse

-- | Length of the right-most decreasing run: largest @i@ such that
-- @w[n-i] \> ... \> w[n-2] \> w[n-1]@.
rdr :: Perm -> Int
rdr = lir . D8.reverse

-- | The number of components. E.g., @[2,0,3,1,4,6,7,5]@ has three
-- components: @[2,0,3,1]@, @[4]@ and @[6,7,5]@.
comp :: Perm -> Int
comp = marshal c_comp

-- | The number of skew components. E.g., @[5,7,4,6,3,1,0,2]@ has three
-- skew components: @[5,7,4,6]@, @[3]@ and @[1,0,2]@.
scomp :: Perm -> Int
scomp = comp . D8.complement

-- | The rank as defined by Elizalde and Pak [Bijections for
-- refined restricted permutations, /J. Comb. Theory, Ser. A/, 2004]:
-- 
-- > maximum [ k | k <- [0..n-1], w[i] >= k for all i < k ]
-- 
ep :: Perm -> Int
ep = marshal c_ep

-- | The dimension of a permutation is defined as the largest
-- non-fixed-point, or zero if all points are fixed.
dim :: Perm -> Int
dim = marshal c_dim

-- | The number of small ascents. A /small ascent/ in @w@ is an index
-- @i@ such that @w[i] + 1 == w[i+1]@.
asc0 :: Perm -> Int
asc0 = marshal c_asc0

-- | The number of small descents. A /small descent/ in @w@ is an
-- index @i@ such that @w[i] == w[i+1] + 1@.
des0 :: Perm -> Int
des0 = marshal c_des0

-- | The longest increasing subsequence.
lis :: Perm -> Int
lis w = case Y.shape (Y.fromPerm w) of
          []    -> 0
          (x:_) -> x

-- | The longest decreasing subsequence.
lds :: Perm -> Int
lds = length . Y.recordingTableau . Y.fromPerm

-- | The size of the shadow of @w@. That is, the number of different
-- one point deletions of @w@.
-- shad :: Perm -> Int
-- shad = length . shadow . return . st
