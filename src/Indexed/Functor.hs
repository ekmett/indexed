{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}

module Indexed.Functor
  ( IFunctor(..)
  , (>$<)
  , imapAt
  , IApplicative(..)
  , IMonad(..)
  , (>~>)
  , (<~<)
  , (?>=)
  , (!>=)
  , ireturnAt
  , IComonad(..)
  , (~>~)
  , (~<~)
  , (?=>)
--  , (!=>)
  , iextractAt
  ) where

import Indexed.Types

class IFunctor (f :: (k -> *) -> k -> *) where
  imap :: (a ~> b) -> f a ~> f b
  default imap :: IMonad f => (a ~> b) -> f a ~> f b
  imap f = ibind (ireturn . f)

  (>$) :: (forall i. b i) -> f a ~> f b
  b >$ f = imap (const b) f

infixl 4 >$<, >$
(>$<) :: IFunctor f => (a ~> b) -> f a ~> f b
(>$<) = imap

imapAt :: IFunctor f => (a -> b) -> f (At a j) ~> f (At b j)
imapAt f = imap (\(At a) -> At (f a))
{-# INLINE imapAt #-}

infixl 4 >*<, >*, *<

class IFunctor f => IApplicative (f :: (k -> *) -> k -> *) where
  ipure :: a -> f (At a i) i
  default ipure :: IMonad f => a -> f (At a i) i
  ipure = ireturnAt

  (>*<) :: f (At (a -> b) j) i -> f (At a k) j -> f (At b k) i
  default (>*<) :: IMonad f => f (At (a -> b) j) i -> f (At a k) j -> f (At b k) i
  mf >*< ma = mf !>= \f -> ma !>= \a -> ireturnAt (f a)

  (>*) :: f (At a j) i -> f (At b k) j -> f (At a k) i
  ma >* mb = imapAt const ma >*< mb

  (*<) :: f (At a j) i -> f (At b k) j -> f (At b k) i
  ma *< mb = imapAt (const id) ma >*< mb

class IFunctor m => IMonad (m :: (k -> *) -> k -> *) where
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

class IFunctor w => IComonad (w :: (k -> *) -> k -> *) | w -> k where
  iextract :: w a ~> a
  iextend :: (w a ~> b) -> w a ~> w b
  iduplicate :: w a ~> w (w a)
  iduplicate = iextend id
  iextend f = imap f . iduplicate

(~>~) :: IComonad w => (w a ~> b) -> (w b ~> c) -> w a ~> c
f ~>~ g = g . iextend f

(~<~) :: IComonad w => (w b ~> c) -> (w a ~> b) -> w a ~> c
f ~<~ g = f . iextend g

(?=>) :: IComonad w => w a i -> (w a ~> b) -> w b i
w ?=> f = iextend f w

-- (!=>) :: IComonad w => w a i -> (w a j -> b) -> w (At b j) i
-- w !=> f = w ?=> (At . f)

iextractAt :: IComonad w => w (At a i) i -> a
iextractAt = key . iextract
