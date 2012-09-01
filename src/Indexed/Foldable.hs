{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module Indexed.Foldable
  ( IFoldable(..)
  , IIFoldable(..)
  ) where

import Data.Monoid
import Indexed.Types
import Indexed.Monoid

class IFoldable (f :: (x -> *) -> y -> *) where
  ifoldMap :: forall (a :: x -> *) (i :: y) (m :: *). Monoid m => (forall (j :: x). a j -> m) -> f a i -> m

class IFoldable f => IIFoldable f where
  iifoldMap :: IMonoid m => (a ~> m) -> f a ~> m
