-- |
-- Copyright   : Anders Claesson 2017
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--
-- Common permutation statistics. Most of these are refined (list) versions of
-- those in Sym.Perm.Stat
--

module Sym.Perm.ListStat
    (
      asc         -- list of ascent
    , ascIx       -- ascent indices
    , ascTops     -- ascent tops
    , ascBots     -- ascent bottoms
    , des         -- descents
    , desIx       -- descent indices
    , desTops     -- descent tops
    , desBots     -- descent bottoms
    , exc         -- excedances
    , fp          -- fixed points
    -- , sfp         -- strong fixed points
    -- , cyc         -- cycles
    -- , inv         -- inversions
    -- , peak        -- peaks
    -- , vall        -- valleys
    -- , dasc        -- double ascents
    -- , ddes        -- double descents
    -- , lmin        -- left-to-right minima
    -- , lmax        -- left-to-right maxima
    -- , rmin        -- right-to-left minima
    -- , rmax        -- right-to-left maxima
    -- , comp        -- components
    -- , scomp       -- skew components
    -- , asc0        -- small ascents
    , des0        -- list small descents
    , des0Ix      -- indices of small descents
    , des0Tops    -- small descents tops
    , des0Bots    -- small descents bottoms
    -- , lis         -- longest increasing subsequence
    -- , lds         -- longest decreasing subsequence
    ) where

import Prelude hiding (head, last)
import Sym.Perm

asc :: Perm -> [(Int, Int, Int)]
asc w =
    [ (i,x,y)
    | (i,x,y) <- zip3 [0..] ys (drop 1 ys)
    , x < y
    ]
  where
    ys = toList w

ascIx :: Perm -> [Int]
ascIx w = [ i | (i,_,_) <- asc w ]

ascBots :: Perm -> [Int]
ascBots w = [ x | (_,x,_) <- asc w ]

ascTops :: Perm -> [Int]
ascTops w = [ y | (_,_,y) <- asc w ]

des :: Perm -> [(Int, Int, Int)]
des w =
    [ (i,x,y)
    | (i,x,y) <- zip3 [0..] ys (drop 1 ys)
    , x > y
    ]
  where
    ys = toList w

desIx :: Perm -> [Int]
desIx w = [ i | (i,_,_) <- des w ]

desBots :: Perm -> [Int]
desBots w = [ x | (_,x,_) <- des w ]

desTops :: Perm -> [Int]
desTops w = [ y | (_,_,y) <- des w ]

exc :: Perm -> [Int]
exc w = [ i | i <- [0 .. size w - 1], w `at` i > i ]

fp :: Perm -> [Int]
fp w = [ i | i <- [0 .. size w - 1], w `at` i == i ]

-- sfp :: Perm -> [Int]
-- sfp = undefined

-- cyc :: Perm -> [[Int]]
-- cyc = undefined

-- inv :: Perm -> [(Int, Int)]
-- inv = undefined

-- peak :: Perm -> [Int]
-- peak = undefined

-- vall :: Perm -> [Int]
-- vall = undefined

-- dasc :: Perm -> [Int]
-- dasc = undefined

-- ddes :: Perm -> [Int]
-- ddes = undefined

-- lmin :: Perm -> [Int]
-- lmin = undefined

-- lmax :: Perm -> [Int]
-- lmax = undefined

-- rmin :: Perm -> [Int]
-- rmin = undefined

-- rmax :: Perm -> [Int]
-- rmax = undefined

-- comp :: Perm -> [Perm]
-- comp = undefined

-- scomp :: Perm -> [Perm]
-- scomp = undefined

-- asc0 :: Perm -> [Int]
-- asc0 = undefined

des0 :: Perm -> [(Int, Int, Int)]
des0 w =
    [ (i,x,y)
    | (i,x,y) <- zip3 [0..] ys (drop 1 ys)
    , x == y + 1
    ]
  where
    ys = toList w

des0Ix :: Perm -> [Int]
des0Ix w = [ i | (i,_,_) <- des0 w ]

des0Bots :: Perm -> [Int]
des0Bots w = [ x | (_,x,_) <- des0 w ]

des0Tops :: Perm -> [Int]
des0Tops w = [ y | (_,_,y) <- des0 w ]

-- lis :: Perm -> [Int]
-- lis = undefined

-- lds :: Perm -> [Int]
-- lds = undefined
