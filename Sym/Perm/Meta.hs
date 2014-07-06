-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--
-- A meta module collecting all Perm-modules, except those that are best
-- imported \"qualified\".

module Sym.Perm.Meta (module P) where

import Sym.Perm                 as P
import Sym.Perm.Class           as P
import Sym.Perm.Component       as P
import Sym.Perm.Constructions   as P
import Sym.Perm.Bijection       as P
import Sym.Perm.Group           as P
import Sym.Perm.Pattern         as P
import Sym.Perm.Simple          as P
import Sym.Perm.Sort            as P
