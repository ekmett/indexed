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

-- | Traversable Functor (between indexed categories)
class (IFunctor t, IFoldable t) => ITraversable t where
  itraverse :: Applicative f => (forall (x :: j). a x -> f (b x)) -> t a y -> f (t b y)

-- TODO: A traversable endofunctor on an indexed category, where you can traverse it with an IApplicative or IMonad
