{-# LANGUAGE TemplateHaskell #-}

module Data.Geodetic.XY(
  XY(..)
, HasXY(..)
) where

import Control.Lens.TH (makeClassy)
import Data.Geodetic.HasDoubles(HasDoubles(doubles))

data XY =
  XY {
    _x ::
      Double
  , _y ::
      Double
  }
  deriving (Eq, Ord, Show)

makeClassy ''XY

instance HasDoubles XY where
  doubles f (XY x_ y_) =
    XY <$>
      f x_ <*>
      f y_
