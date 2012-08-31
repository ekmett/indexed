{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Indexed.Product
  ( (*)(..)
  , ifst
  , isnd
  ) where

import Indexed.Functor
import Indexed.Types
import Indexed.Foldable
import Indexed.Monoid

data (f * g) a i = f a i :* g a i

ifst :: (f * g) a ~> f a
ifst (a :* _) = a

isnd :: (f * g) a ~> g a
isnd (_ :* b) = b

instance (IFunctor f, IFunctor g) => IFunctor (f * g) where
  imap f (a :* b) = imap f a :* imap f b

-- instance (IFoldable f, IFoldable g) => IFoldable (f * g) where
--  ifoldMap f (a :* b) = f a >< f b

-- instance ITraversable f, ITraversabe g) = ITraversable (f * g) where

-- isntance (IMonad f, IMonad g) => IMonad (f * g) where

data (&) :: (((k,k) -> *) -> (k,k) -> *) -> (((k,k) -> *) -> (k,k) -> *) -> ((k,k) -> *) -> (k,k) -> * where
  (:&) :: (Fst ij ~ Fst ik, Snd ij ~ Fst jk, Snd jk ~ Snd ik) => f a ij -> g jk -> (f & g) a ik

instance (IFunctor f, IFunctor g) => IFunctor (f & g) where
  imap f (a :& b) = imap f a :& imap f b

instance (IFoldable f, IFoldable g) => IFoldable (f & g) where
  ifoldMap f (a :& b) = ifoldMap f a >< ifoldMap f b
