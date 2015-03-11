{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Indexed.Traversable
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Indexed Traversable Functors
-----------------------------------------------------------------------------
module Indexed.Traversable
  ( ITraversable(..)
  ) where

import Control.Applicative
import Indexed.Functor
import Indexed.Foldable

-- | Indexed Traversable Functor
class (IFunctor t, IFoldable t) => ITraversable t where
  itraverse :: Applicative f => (forall x. a x -> f (b x)) -> t a y -> f (t b y)
