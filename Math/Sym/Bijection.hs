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

import qualified Math.Sym.Internal as I (simionSchmidt, simionSchmidt')
import Math.Sym (Perm, lift)

-- | The Simion-Schmidt bijection from Av(123) onto Av(132).
simionSchmidt :: Perm a => a -> a
simionSchmidt = lift I.simionSchmidt

-- | The inverse of the Simion-Schmidt bijection. It is a function
-- from Av(132) to Av(123).
simionSchmidt' :: Perm a => a -> a
simionSchmidt' = lift I.simionSchmidt'
