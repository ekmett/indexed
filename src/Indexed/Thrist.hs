{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Indexed.Thrist
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Thrists are lists parameterized to form an indexed path.
--
-- <http://omega.googlecode.com/files/Thrist-draft-2011-11-20.pdf>
-----------------------------------------------------------------------------
module Indexed.Thrist
  ( Thrist(..)
  , List
  , Unitary
  ) where

import Control.Applicative
import Data.Monoid
import Indexed.Functor
import Indexed.Monoid
import Indexed.Foldable
import Indexed.Traversable
import Indexed.Types

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

instance IApplicative Thrist where
  ireturn a = herp (derp a :- Nil)

instance IMonad Thrist where
  ibind _ Nil = Nil
  ibind f (a :- as) = herp (derp (f a) +++ derp (ibind f as))

type Unitary k a = k (At a '( '(), '())) '( '(), '())

-- | Lists are simply unindexed thrists.
type List a = Unitary Thrist a

instance IMonoid (Thrist a) where
  imempty = Nil
  as >< bs = herp (derp as +++ derp bs)

instance IFoldable Thrist where
  ifoldMap _ Nil = mempty
  ifoldMap f (a :- as) = f a <> ifoldMap f as

instance IIFoldable Thrist where
  iifoldMap _ Nil = imempty
  iifoldMap f (a :- as) = herp (derp (f a) >< derp (iifoldMap f as))

instance ITraversable Thrist where
  itraverse _ Nil = pure Nil
  itraverse f (a :- as) = (:-) <$> f a <*> itraverse f as
