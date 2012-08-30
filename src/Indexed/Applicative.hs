{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Indexed.Applicative
  ( IApplicative(..)
  ) where

import Indexed.Types
import Indexed.Functor

infixl 4 >*<, >*, *<

class IFunctor f => IApplicative f where
  ipure :: a -> f (At a i) i
  (>*<) :: f (At (a -> b) j) i -> f (At a k) j -> f (At b k) i

  (>*) :: f (At a j) i -> f (At b k) j -> f (At a k) i
  ma >* mb = imapAt const ma >*< mb

  (*<) :: f (At a j) i -> f (At b k) j -> f (At b k) i
  ma *< mb = imapAt (const id) ma >*< mb
