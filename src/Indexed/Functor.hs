{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Indexed.Functor
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Poly-kindedly indexed functors, monads, and comonads
-----------------------------------------------------------------------------
module Indexed.Functor
  ( IFunctor(..)
  , (/$/)
  , imapAt
  , imapCoat
  , IApplicative(..)
  , IMonad(..)
  , (>~>)
  , (<~<)
  , (?>=)
  , (!>=)
  , iliftM
  , ireturnAt
  , ireturnCoat
  , IComonad(..)
  , (~>~)
  , (~<~)
  , (?=>)
  , (!=>)
  , iextractAt
  , iextractCoat
  , iliftW
  ) where

import Indexed.Types

infixl 4 /$/, /$, /*/, /*, */
infixl 1 ?>=, !>=, ?=>
infixr 1 >~>, <~<, ~>~, ~<~

-- | A 'Functor' between indexed categories.
class IFunctor f where
  imap :: (a ~> b) -> f a ~> f b

  (/$) :: Lim b -> f a ~> f b
  b /$ f = imap (const b) f

(/$/) :: IFunctor f => (a ~> b) -> f a ~> f b
(/$/) = imap

imapAt :: IFunctor f => (a -> b) -> f (At a k) ~> f (At b k)
imapAt f = imap (\(At a) -> At (f a))
{-# INLINE imapAt #-}

imapCoat :: IFunctor f => (a -> b) -> f (Coat a k) ~> f (Coat b k)
imapCoat f = imap $ \ c -> Coat $ case c of Coat a -> f a
{-# INLINE imapCoat #-}

class IFunctor f => IApplicative f where
  ireturn :: a ~> f a

  (/*/) :: f (At (a -> b) j) i -> f (At a k) j -> f (At b k) i
  default (/*/) :: IMonad f => f (At (a -> b) j) i -> f (At a k) j -> f (At b k) i
  mf /*/ ma = mf !>= \f -> ma !>= \a -> ireturnAt (f a)

  (/*) :: f (At a j) i -> f (At b k) j -> f (At a k) i
  ma /* mb = imapAt const ma /*/ mb

  (*/) :: f (At a j) i -> f (At b k) j -> f (At b k) i
  ma */ mb = imapAt (const id) ma /*/ mb

ireturnAt :: IApplicative m => a -> m (At a i) i
ireturnAt a = ireturn (At a)

ireturnCoat :: IApplicative m => a -> m (Coat a i) j
ireturnCoat a = ireturn (Coat a)

class IApplicative m => IMonad m where
  ibind   :: (a ~> m b) -> m a ~> m b
  ijoin   :: m (m a) ~> m a

  ijoin   = ibind id
  ibind f = ijoin . imap f

(>~>) :: IMonad m => (a ~> m b) -> (b ~> m c) -> a ~> m c
f >~> g = ibind g . f

(<~<) :: IMonad m => (b ~> m c) -> (a ~> m b) -> a ~> m c
f <~< g = ibind f . g

-- @
-- m '?>=' 'ireturn' ≡ m
-- 'ireturn' a '?>=' f ≡ f a
-- (m '?>=' f) '?>=' g ≡ m '?>=' \x -> f x '?>=' g
-- @
(?>=) :: IMonad m => m a i -> (a ~> m b) -> m b i
m ?>= f = ibind f m

(!>=) :: IMonad (m :: (x -> *) -> x -> *) => m (At a j) i -> (a -> m b j) -> m b i
m !>= f = m ?>= \ (At a) -> f a

iliftM :: IMonad m => (a ~> b) -> m a ~> m b
iliftM f = ibind (ireturn . f)

class IFunctor w => IComonad w where
  iextract :: w a ~> a
  iextend :: (w a ~> b) -> w a ~> w b
  iduplicate :: w a ~> w (w a)
  iduplicate = iextend id
  iextend f = imap f . iduplicate

(~>~) :: IComonad w => (w a ~> b) -> (w b ~> c) -> w a ~> c
f ~>~ g = g . iextend f

(~<~) :: IComonad w => (w b ~> c) -> (w a ~> b) -> w a ~> c
f ~<~ g = f . iextend g

-- @
-- w '?=>' 'iextract' ≡ w
-- 'iextract' (w '?=>' f) ≡ f w
-- (w '?=>' f) '?=>' g ≡ w '?=>' \x -> f (x '?=>' g)
-- @
(?=>) :: IComonad w => w a i -> (w a ~> b) -> w b i
w ?=> f = iextend f w

(!=>) :: IComonad w => w a i -> (w a j -> b) -> w (Coat b j) i
w !=> f = w ?=> \u -> Coat (f u)

iextractAt :: IComonad w => w (At a i) j -> a
iextractAt = key . iextract

iextractCoat :: IComonad w => w (Coat a i) i -> a
iextractCoat = uncoat . iextract

iliftW :: IComonad w => (a ~> b) -> w a ~> w b
iliftW f = iextend (f . iextract)
