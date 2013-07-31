{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Indexed.Product
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Products of indexed functors
-----------------------------------------------------------------------------
module Indexed.Product
  ( (*)(..)
  , ifst
  , isnd
  ) where

import Control.Applicative
import Data.Monoid
import Indexed.Functor
-- import Indexed.Types
import Indexed.Foldable
import Indexed.Traversable
import Indexed.Monoid

-- | Indexed functor product
data (*) :: ((x -> *) -> y -> *) -> ((x -> *) -> y -> *) -> (x -> *) -> y -> * where
  (:*) :: f a i -> g a i -> (f * g) a i

ifst :: (f * g) a i -> f a i
ifst (a :* _) = a

isnd :: (f * g) a i -> g a i
isnd (_ :* b) = b

instance (IFunctor f, IFunctor g) => IFunctor (f * g) where
  imap f (a :* b) = imap f a :* imap f b

instance (IFoldable f, IFoldable g) => IFoldable (f * g) where
  ifoldMap f (a :* b) = ifoldMap f a <> ifoldMap f b

instance (ITraversable f, ITraversable g) => ITraversable (f * g) where
  itraverse f (a :* b) = (:*) <$> itraverse f a <*> itraverse f b

instance (IApplicative f, IApplicative g) => IApplicative (f * g) where
  ireturn a = ireturn a :* ireturn a
  (af :* bf) /*/ (aa :* ba) = (af /*/ aa) :* (bf /*/ ba)

instance (IMonad f, IMonad g) => IMonad (f * g) where
  ibind f (a :* b) = ibind (ifst . f) a :* ibind (isnd . f) b

-- | Foldable product
data (&) :: (((i,i) -> *) -> (j,j) -> *) -> (((i,i) -> *) -> (j,j) -> *) -> ((i,i) -> *) -> (j,j) -> * where
  (:&) :: f a '(x,y) -> g a '(y,z) -> (f & g) a '(x,z)

instance (IFunctor f, IFunctor g) => IFunctor (f & g) where
  imap f (a :& b) = imap f a :& imap f b

instance (IFoldable f, IFoldable g) => IFoldable (f & g) where
  ifoldMap f (a :& b) = ifoldMap f a <> ifoldMap f b

instance (IIFoldable f, IIFoldable g) => IIFoldable (f & g) where
  iifoldMap f (a :& b) = iifoldMap f a >< iifoldMap f b
