{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Indexed.Functor
  ( IFunctor(..)
  , (>$<)
  ) where

import Indexed.Types

class IFunctor f where
  imap :: (a ~> b) -> f a ~> f b
  (>$) :: (forall i. b i) -> f a ~> f b
  b >$ f = imap (const b) f

infixl 4 >$<, >$
(>$<) :: IFunctor f => (a ~> b) -> f a ~> f b
(>$<) = imap
