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
  , improve
  , IMonadFree(..)
  ) where

import Control.Applicative
import Indexed.Types
import Indexed.Functor
import Indexed.Foldable
import Indexed.Traversable

class IMonad m => IMonadFree f m | m -> f where
  ifree :: f (m a) ~> m a

data Free f a i where
  Return :: a i -> Free f a i
  Free   :: f (Free f a) i -> Free f a i

instance IFunctor f => IFunctor (Free f) where
  imap f (Return a) = Return (f a)
  imap f (Free as)  = Free (imap (imap f) as)

instance IFunctor f => IApplicative (Free f) where
  ireturn = Return

instance IFoldable f => IFoldable (Free f) where
  ifoldMap f (Return a) = f a
  ifoldMap f (Free as) = ifoldMap (ifoldMap f) as

instance ITraversable f => ITraversable (Free f) where
  itraverse f (Return a) = Return <$> f a
  itraverse f (Free as)  = Free <$> itraverse (itraverse f) as

instance IFunctor f => IMonad (Free f) where
  ibind f (Return a) = f a
  ibind f (Free as) = Free (imap (ibind f) as)

instance IFunctor f => IMonadFree f (Free f) where
  ifree = Free

-- | A CPS'd Free Monad, Church-encoded.
newtype Church f a i = Church { runChurch :: forall r. (a ~> r) -> (f r ~> r) -> r i }

instance IFunctor (Church f) where
  imap f m = Church $ \kp -> runChurch m (kp . f)

instance IFoldable f => IFoldable (Church f) where
  ifoldMap f m = lower (runChurch m (Lift . f) (Lift . ifoldMap lower))

instance ITraversable f => ITraversable (Church f) where
  itraverse f m = unbox (runChurch m (\a -> Box (ireturn <$> f a)) (Box . fmap ifree . itraverse unbox))

instance IApplicative (Church f) where
  ireturn a = Church $ \k _ -> k a

instance IMonad (Church f) where
  ibind f m = Church $ \k kf -> runChurch m (\a -> runChurch (f a) k kf) kf

instance IFunctor f => IMonadFree f (Church f) where
  ifree as = Church $ \k kf -> kf (imap (\ m -> runChurch m k kf) as)

-- | Go 'Free'.
improve :: Church f a ~> Free f a
improve m = runChurch m Return Free

-- helpers for folding and traversing
newtype Lift a i = Lift { lower :: a }
newtype Box f a i = Box { unbox :: f (a i) }
