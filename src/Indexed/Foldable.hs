{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Indexed.Foldable
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Indexed foldable containers
-----------------------------------------------------------------------------
module Indexed.Foldable
  ( IFoldable(..)
  , IIFoldable(..)
  ) where

import Data.Monoid
import Indexed.Types
import Indexed.Monoid

-- | An foldable indexed container that can be folded with a monoid.
class IFoldable (f :: (x -> *) -> y -> *) where
  ifoldMap :: forall (a :: x -> *) (i :: y) (m :: *). Monoid m => (forall (j :: x). a j -> m) -> f a i -> m

-- | An foldable indexed container that can be folded with an /indexed/ monoid (or category)
class IFoldable f => IIFoldable f where
  iifoldMap :: IMonoid m => (a ~> m) -> f a ~> m
  ifoldCat :: Cat k => (forall i j. a '(i,j) -> k i j) -> f a '(x,y) -> k x y
  ifoldCat f = unwrapIMonoid . iifoldMap (wrapIMonoid . f)
