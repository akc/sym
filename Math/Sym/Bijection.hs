-- |
-- Module      : Math.Sym.Bijection
-- Copyright   : (c) Anders Claesson 2013
-- License     : BSD-style
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- 
-- Bijections

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

-- | The Simion-Schmidt bijection from Av(123) onto Av(132).
simionSchmidt :: Perm a => a -> a
simionSchmidt = undefined

-- | The inverse of the Simion-Schmidt bijection. It is a function
-- from Av(132) to Av(123).
simionSchmidt' :: Perm a => a -> a
simionSchmidt' = lift I.simionSchmidt'
