{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Indexed.Thrist
  ( Thrist(..)
  , List
  , Unitary
  ) where

import Indexed.Functor
import Indexed.Monoid
import Indexed.Foldable
import Indexed.Applicative
import Indexed.Monad
import Indexed.Traversable
import Indexed.Types

infixr 5 :-
-- A Thrist.
data Thrist :: ((i,i) -> *) -> (i,i) -> * where
  Nil :: Thrist g '(i,i)
  (:-) :: g '(i,j) -> Thrist g '(j,k) -> Thrist g '(i,k)

instance IFunctor Thrist where
  imap _ Nil = Nil
  imap f (r :- rs) = f r :- imap f rs

instance IApplicative Thrist where
  -- ipure a          = At a :- Nil
  --Nil       >*< _  = Nil
  --(f :- fs) >*< as = imap f as >< (fs >*< as)

instance IMonad Thrist where
  -- ireturn a = a :- Nil
  ibind _ Nil = Nil
  ibind f (a :- as) = f a >< ibind f as

type Unitary k a = k (At a '((),())) '((),())

type List a = Unitary Thrist a

instance IMonoid (Thrist a) where
  imempty = Nil
  Nil      >< bs = bs
  (a :- as) >< bs = a :- (as >< bs)

instance IFoldable Thrist where
  ifoldMap _ Nil = imempty
  ifoldMap f (a :- as) = f a >< ifoldMap f as

--instance ITraversable Thrist where
--  imapM f Nil = ireturn Nil
