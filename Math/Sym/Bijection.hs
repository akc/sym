-- |
-- Module      : Math.Sym.Class
-- Copyright   : (c) Anders Claesson 2012, 2013
-- License     : BSD-style
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- 
-- A permutation class is a downset in the poset of permutations
-- ordered by containment. This module provides definitions of some
-- common classes.

module Math.Sym.Bijection
    (
     simionSchmidt, simionSchmidt'
    ) where

import qualified Math.Sym.Internal as I (simionSchmidt')
import Math.Sym (Perm, lift)

-- Given a set L in lmin(Sn), this is how we construct the corresponding
-- permutation t = c1 c2 ... cn in Sn(132): For i from 1 to n,

-- * if (i, a) is in L let ci = a; otherwise,

-- * let cj be the smallest letter not used that is greater than all the
--   letters used thus far.

-- Given a set L in lmin(Sn), this is how we construct the corresponding
-- permutation p = a1 a2 ... an in Sn(123): For i from 1 to n,

-- * if (i, c) is in L let ai = c; otherwise,

-- * let aj be the largest letter not used thus far.


simionSchmidt :: Perm a => a -> a
simionSchmidt = undefined

simionSchmidt' :: Perm a => a -> a
simionSchmidt' = lift I.simionSchmidt'
