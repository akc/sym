-- |
-- Module      : Math.Sym.Stat
-- Copyright   : (c) Anders Claesson 2012
-- License     : BSD-style
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- 
-- Common permutation statistics. Please contact the maintainer if you
-- feel that there is a statistic that is missing; even better, send a
-- patch or make a pull request.
-- 
-- To avoid name clashes this module is best imported @qualified@;
-- e.g.
-- 
-- > import qualified Math.Sym.Stat as S
-- 
-- For any permutation statistic @f@, below, it holds that @f w == f
-- (st w)@, and therefore the explanations below will be done on
-- standard permutations for convenience.

module Math.Sym.Stat 
    (
      asc         -- ascents
    , des         -- descents
    , exc         -- excedances
    , fp          -- fixed points
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
    , ep          -- rank a la Elizalde & Pak
    , dim         -- dimension
    , asc0        -- small ascents
    , des0        -- small descents
    ) where

import Prelude hiding (head, last)
import Math.Sym (Perm, toVector, st)
import Math.Sym.Internal (Perm0)
import qualified Math.Sym.Internal as I 
    ( asc, des, exc, fp, cyc, inv, maj, comaj, peak, vall, dasc, ddes
    , lmin, lmax, rmin, rmax
    , head, last, lir, ldr, rir, rdr, comp, ep, dim, asc0, des0
    )

generalize :: Perm a => (Perm0 -> b) -> a -> b
generalize f = f . toVector . st

-- | The number of ascents. An /ascent/ in @w@ is an index @i@ such
-- that @w[i] \< w[i+1]@.
asc :: Perm a => a -> Int
asc = generalize I.asc

-- | The number of descents. A /descent/ in @w@ is an index @i@ such
-- that @w[i] > w[i+1]@.
des :: Perm a => a -> Int
des = generalize I.des

-- | The number of /excedances/: positions @i@ such that @w[i] > i@.
exc :: Perm a => a -> Int
exc = generalize I.exc

-- | The number of /fixed points/: positions @i@ such that @w[i] == i@.
fp :: Perm a => a -> Int
fp = generalize I.fp

-- | The number of /cycles/: orbits of the permutation when viewed as a function.
cyc :: Perm a => a -> Int
cyc = generalize I.cyc

-- | The number of /inversions/: pairs @\(i,j\)@ such that @i \< j@ and @w[i] > w[j]@.
inv :: Perm a => a -> Int
inv = generalize I.inv

-- | /The major index/ is the sum of descents.
maj :: Perm a => a -> Int
maj = generalize I.maj

-- | /The co-major index/ is the sum of descents.
comaj :: Perm a => a -> Int
comaj = generalize I.comaj

-- | The number of /peaks/: positions @i@ such that @w[i-1] \< w[i]@ and @w[i] \> w[i+1]@.
peak :: Perm a => a -> Int
peak = generalize I.peak

-- | The number of /valleys/: positions @i@ such that @w[i-1] \> w[i]@ and @w[i] \< w[i+1]@.
vall :: Perm a => a -> Int
vall = generalize I.vall

-- | The number of /double ascents/: positions @i@ such that @w[i-1] \<  w[i] \< w[i+1]@.
dasc :: Perm a => a -> Int
dasc = generalize I.dasc

-- | The number of /double descents/: positions @i@ such that @w[i-1] \>  w[i] \> w[i+1]@.
ddes :: Perm a => a -> Int
ddes = generalize I.ddes

-- | The number of /left-to-right minima/: positions @i@ such that @w[i] \< w[j]@ for all @j \< i@.
lmin :: Perm a => a -> Int
lmin = generalize I.lmin

-- | The number of /left-to-right maxima/: positions @i@ such that @w[i] \> w[j]@ for all @j \< i@.
lmax :: Perm a => a -> Int
lmax = generalize I.lmax

-- | The number of /right-to-left minima/: positions @i@ such that @w[i] \< w[j]@ for all @j \> i@.
rmin :: Perm a => a -> Int
rmin = generalize I.rmin

-- | The number of /right-to-left maxima/: positions @i@ such that @w[i] \> w[j]@ for all @j \> i@.
rmax :: Perm a => a -> Int
rmax = generalize I.rmax

-- | The first (left-most) element in the standardization. E.g., @head \"231\" = head (fromList [1,2,0]) = 1@.
head :: Perm a => a -> Int
head = generalize I.head

-- | The last (right-most) element in the standardization. E.g., @last \"231\" = last (fromList [1,2,0]) = 0@.
last :: Perm a => a -> Int
last = generalize I.last

-- | Length of the left-most increasing run: largest @i@ such that
-- @w[0] \< w[1] \< ... \< w[i-1]@.
lir :: Perm a => a -> Int
lir = generalize I.lir

-- | Length of the left-most decreasing run: largest @i@ such that
-- @w[0] \> w[1] \> ... \> w[i-1]@.
ldr :: Perm a => a -> Int
ldr = generalize I.ldr

-- | Length of the right-most increasing run: largest @i@ such that
-- @w[n-i] \< ... \< w[n-2] \< w[n-1]@.
rir :: Perm a => a -> Int
rir = generalize I.rir

-- | Length of the right-most decreasing run: largest @i@ such that
-- @w[n-i] \> ... \> w[n-2] \> w[n-1]@.
rdr :: Perm a => a -> Int
rdr = generalize I.rdr

-- | The number of components. E.g., @[2,0,3,1,4,6,7,5]@ has three
-- components: @[2,0,3,1]@, @[4]@ and @[6,7,5]@.
comp :: Perm a => a -> Int
comp = generalize I.comp

-- | The rank as defined by Elizalde and Pak [Bijections for
-- refined restricted permutations, /J. Comb. Theory, Ser. A/, 2004]:
-- 
-- > maximum [ k | k <- [0..n-1], w[i] >= k for all i < k ]
-- 
ep :: Perm a => a -> Int
ep = generalize I.ep

-- | The dimension of a permutation is defined as the largest
-- non-fixed-point, or zero if all points are fixed.
dim :: Perm a => a -> Int
dim = generalize I.dim

-- | The number of small ascents. A /small ascent/ in @w@ is an index
-- @i@ such that @w[i] + 1 == w[i+1]@.
asc0 :: Perm a => a -> Int
asc0 = generalize I.asc0

-- | The number of small descents. A /small descent/ in @w@ is an
-- index @i@ such that @w[i] == w[i+1] + 1@.
des0 :: Perm a => a -> Int
des0 = generalize I.des0
