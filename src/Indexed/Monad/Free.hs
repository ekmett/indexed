{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Indexed.Monad.Free
  ( Church(..)
  , Free(..)
  ) where

import Indexed.Types
import Indexed.Functor
import Indexed.Monad

class IMonad m => IMonadFree f m | m -> f where
  ifree :: f (m a) ~> m a

data Free f a i where -- :: ((i -> *) -> i -> *) -> (i -> *) -> i -> *
  Return :: a i -> Free f a i
  Free   :: f (Free f a) i -> Free f a i

instance IFunctor f => IFunctor (Free f) where
  imap f (Return a) = Return (f a)
  imap f (Free as)  = Free (imap (imap f) as)

instance IFunctor f => IMonad (Free f) where
  ireturn = Return
  ibind f (Return a) = f a
  ibind f (Free as) = Free (imap (ibind f) as)

instance IFunctor f => IMonadFree f (Free f) where
  ifree = Free

-- | Free (Coyoneda f)
newtype Church
  (f :: (k -> *) -> k -> *)
  (a :: k -> *)
  (i :: k) = Church { runChurch :: forall (r :: k -> *). (a ~> r) -> (f r ~> r) -> r i }

instance IFunctor (Church f) where
  imap f (Church m) = Church $ \kp -> m (kp . f)

instance IMonad (Church f) where
  ireturn a = Church $ \k _ -> k a
  ibind f (Church m) = Church $ \k kf -> m (\a -> runChurch (f a) k kf) kf

instance IFunctor f => IMonadFree f (Church f) where
  ifree as = Church $ \k kf -> kf (imap (\ m -> runChurch m k kf) as)
