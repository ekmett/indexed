{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
module Indexed.Traversable
  ( ITraversable(..)
  ) where

import Control.Applicative
-- import Indexed.Types
import Indexed.Functor
import Indexed.Foldable

-- | Traversable Functor (between indexed categories)
class (IFunctor t, IFoldable t) => ITraversable t where
  itraverse :: Applicative f => (forall (x :: j). a x -> f (b x)) -> t a y -> f (t b y)

-- TODO: A traversable endofunctor on an indexed category, where you can traverse it with an IApplicative or IMonad
