{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
module Indexed.Monad
  ( IMonad(..)
  , (>~>)
  , (<~<)
  , (?>=)
  , (!>=)
  , ireturnAt
  ) where

import Indexed.Types
import Indexed.Functor

class IFunctor m => IMonad m where
  ireturn :: a ~> m a
  ibind   :: (a ~> m b) -> m a ~> m b
  ijoin   :: m (m a) ~> m a
  ijoin   = ibind id
  ibind f = ijoin . imap f

(>~>) :: IMonad m => (a ~> m b) -> (b ~> m c) -> a ~> m c
f >~> g = ibind g . f

(<~<) :: IMonad m => (b ~> m c) -> (a ~> m b) -> a ~> m c
f <~< g = ibind f . g

(?>=) :: IMonad m => m a i -> (a ~> m b) -> m b i
m ?>= f = ibind f m

(!>=) :: IMonad m => m (At a j) i -> (a -> m b j) -> m b i
m !>= f = m ?>= \ (At a) -> f a

ireturnAt :: IMonad m => a -> m (At a i) i
ireturnAt a = ireturn (At a)
