-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--
-- Sum, skew sum, etc
-- 

module Sym.Perm.Constructions
    (
      (/+/)
    , (\-\)
    , directSum
    , skewSum
    , inflate
    ) where

import Foreign
import System.IO.Unsafe
import Control.Monad
import Sym.Perm
import qualified Sym.Permgram as G
import qualified Sym.Perm.D8 as D8

infixl 6 /+/
infixl 6 \-\

-- | The /direct sum/ of two permutations.
(/+/) :: Perm -> Perm -> Perm
(/+/) u v =
   let k  = size u
       l  = size v
       v' = imap (\_ x -> x + fromIntegral k) v
   in unsafeDupablePerformIO . unsafeNew (k+l) $ \p ->
       let q = advancePtr p k
       in unsafeWith u  $ \uPtr ->
          unsafeWith v' $ \vPtr -> do
              copyArray p uPtr k
              copyArray q vPtr l

-- | The direct sum of a list of permutations.
directSum :: [Perm] -> Perm
directSum = foldr (/+/) emptyperm

-- | The /skew sum/ of two permutations.
(\-\) :: Perm -> Perm -> Perm
(\-\) u v = D8.complement $ D8.complement u /+/ D8.complement v

-- | The skew sum of a list of permutations.
skewSum :: [Perm] -> Perm
skewSum = foldr (\-\) emptyperm

-- | @inflate w vs@ is the /inflation/ of @w@ by @vs@. It is the
-- permutation of length @sum (map size vs)@ obtained by replacing
-- each entry @w!i@ by an interval that is order isomorphic to @vs!i@
-- in such a way that the intervals are order isomorphic to @w@. In
-- particular,
-- 
-- > u /+/ v == inflate (mkPerm "12") [u,v]
-- > u \-\ v == inflate (mkPerm "21") [u,v]
-- 
inflate :: Perm -> [Perm] -> Perm
inflate w = G.perm . join . G.permgram w . map (`G.permgram` [()])
