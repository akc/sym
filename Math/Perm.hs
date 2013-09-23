-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--
-- A meta module collecting all Perm-modules, except those that are best
-- imported \"qualified\".

module Math.Perm (module P) where

import Data.Perm                 as P
import Math.Perm.Class           as P
import Math.Perm.Component       as P
import Math.Perm.Constructions   as P
import Math.Perm.Bijection       as P
import Math.Perm.Group           as P
import Math.Perm.Pattern         as P
import Math.Perm.Simple          as P
import Math.Perm.Sort            as P
