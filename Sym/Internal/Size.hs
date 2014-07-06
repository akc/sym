module Sym.Internal.Size (Size (..)) where

import qualified Data.Set as Set

class Size a where
    size :: a -> Int

instance Size [a] where
    size = length

instance Size (Set.Set a) where
    size = Set.size

instance Size a => Size (Maybe a) where
    size Nothing  = 0
    size (Just x) = size x
