{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Indexed.Comonad.Store
  ( Store(..)
  , IComonadStore(..)
  ) where

import Indexed.Types
import Indexed.Functor

class IComonad w => IComonadStore s w | w -> s where
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

{-
  -- These are not Cokleisli
  ipeek :: s j -> w a i -> a j
  ipeeks :: (s i -> s j) -> w a i -> a j
-}


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
-- Store Identity (At a j) i
