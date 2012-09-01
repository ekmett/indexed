{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Indexed.Thrist
  ( Thrist(..)
  , List
  , Unitary
  ) where

import Indexed.Functor
import Indexed.Monoid
import Indexed.Foldable
import Indexed.Traversable
import Indexed.Types
import Unsafe.Coerce

infixr 5 :-

data Thrist :: ((i,i) -> *) -> (i,i) -> * where
  Nil  :: Thrist a '(i,i)
  (:-) :: a '(i,j) -> Thrist a '(j,k) -> Thrist a '(i,k)

instance IFunctor Thrist where
  imap _ Nil = Nil
  imap f (r :- rs) = f r :- imap f rs

(+++) :: Thrist a '(i,j) -> Thrist a '(j,k) -> Thrist a '(i,k)
Nil       +++ bs = bs
(a :- as) +++ bs = a :- (as +++ bs)

-- instance IApplicative Thrist

instance IMonad Thrist where
  ireturn a = herp (derp a :- Nil)
  ibind _ Nil = Nil
  ibind f (a :- as) = herp (derp (f a) +++ derp (ibind f as))

type Unitary k a = k (At a '( '(), '())) '( '(), '())

type List a = Unitary Thrist a

instance IMonoid (Thrist a) where
  imempty = Nil
  as >< bs = herp (derp as +++ derp bs)

instance IFoldable Thrist where
  ifoldMap _ Nil = imempty
  ifoldMap f (a :- as) = herp (derp (f a) >< derp (ifoldMap f as))

instance ITraversable Thrist where
  imapM _ Nil = ireturn Nil
  -- imapM f (a :- as) = f (herp a) ?>= \b -> undefined -- imapM f as ?>= \bs -> ireturn (b :- bs)
