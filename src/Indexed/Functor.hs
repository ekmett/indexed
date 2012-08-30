{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module Indexed.Functor
  ( IFunctor(..)
  , (>$<)
  , imapAt
  ) where

import Indexed.Types

class IFunctor f where -- (f :: (k -> *) -> k -> *) where
  imap :: (a ~> b) -> f a ~> f b
  (>$) :: (forall i. b i) -> f a ~> f b
  b >$ f = imap (const b) f

infixl 4 >$<, >$
(>$<) :: IFunctor f => (a ~> b) -> f a ~> f b
(>$<) = imap

imapAt :: IFunctor f => (a -> b) -> f (At a j) ~> f (At b j)
imapAt f = imap (\(At a) -> At (f a))
{-# INLINE imapAt #-}
