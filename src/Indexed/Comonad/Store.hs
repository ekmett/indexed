{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE DataKinds #-}
module Indexed.Comonad.Store
  ( Store(..)
  , IComonadStore(..)
  ) where

import Indexed.Types
import Indexed.Functor

class IComonad w => IComonadStore (s :: v -> *) (w :: (u -> *) -> u -> *) | w -> s v u where
  ipos    :: w a ~> s
  ipeek   :: Lim s -> w a ~> a
  iseek   :: Lim s -> w a ~> w a
  idelay  :: Lim s -> w a ~> w a
  ipeeks  :: (s ~> s) -> w a ~> a
  iseeks  :: (s ~> s) -> w a ~> w a
  idelays :: (s ~> s) -> w a ~> w a
  ipeek s   = ipeeks (const s)
  iseek s   = iseeks (const s)
  idelay s  = iextend (ipeek s)
  idelays f = iextend (ipeeks f)

data Store s a i = Store (s ~> a) (s i)

instance IFunctor (Store s) where
  imap f (Store g s) = Store (f . g) s

instance IComonad (Store s) where
  iextract (Store f s) = f s
  iduplicate (Store f s) = Store (Store f) s

instance IComonadStore s (Store s) where
  ipos      (Store _ s) = s
  ipeek s   (Store f _) = f s
  ipeeks g  (Store f s) = f (g s)
  iseek s   (Store f _) = Store f s
  iseeks g  (Store f s) = Store f (g s)
  idelay t  (Store f s) = Store (f . const t) s
  idelays g (Store f s) = Store (f . g) s

-- The classic 2 parameter store comonad can be had with
-- Store Identity (Coat a j) i


{-
newtype St s ij = St { runSt :: s (Snd ij) }
newtype Stored s a i = Stored { runStored :: forall j. s j -> a '(i,j) }

instance IFunctor (Stored s) where
  imap f (Stored k) = Stored (f . k)

-- data StoreT (s :: y -> *) (w :: (x -> *) -> x -> *) (a :: (x,y) -> *) (i :: (x,y))
data StoreT s w a i
  = StoreT (w (Stored s a) (Fst i)) (s (Snd i))

instance IFunctor w => IFunctor (StoreT s w) where
  imap f (StoreT w s) = StoreT (imap (imap f) w) s

instance IComonad w => IComonad (StoreT s w) where
  iextract (StoreT w s) = case iextract w of
    Stored f -> herp (f s)
  iduplicate (StoreT w s) = StoreT (iextend (\w' -> Stored (StoreT w')) w) s

instance IComonad w => IComonadStore (St s) (StoreT s w) where
  ipos (StoreT _ s) = St s
  -- ipeek s (StoreT w _) = herp (runStored (iextract w) (runSt s))
  -- ipeeks f (StoreT w s) = runStored (iextract w) (runSt (f (St s)))
-}
