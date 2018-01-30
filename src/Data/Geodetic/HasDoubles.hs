module Data.Geodetic.HasDoubles(
  HasDoubles(..)
) where

import Control.Lens (Traversal')

class HasDoubles a where
  doubles ::
    Traversal' a Double

instance HasDoubles Double where
  doubles =
    id
