{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Indexed.Comonad
  ( IComonad(..)
  , (~>~)
  , (~<~)
  , (?=>)
--  , (!=>)
  , iextractAt
  ) where

import Indexed.Types
import Indexed.Functor

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

(?=>) :: IComonad w => w a i -> (w a ~> b) -> w b i
w ?=> f = iextend f w

-- (!=>) :: IComonad w => w a i -> (w a j -> b) -> w (At b j) i
-- w !=> f = w ?=> (At . f)

iextractAt :: IComonad w => w (At a i) i -> a
iextractAt = key . iextract
