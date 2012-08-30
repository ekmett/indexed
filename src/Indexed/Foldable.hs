{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Indexed.Foldable
  ( IFoldable(..)
  ) where

import Indexed.Types
import Indexed.Monoid

class IFoldable f where
  ifoldMap :: IMonoid m => (a ~> m) -> f a ~> m
