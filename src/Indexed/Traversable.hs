{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Indexed.Traversable
  ( ITraversable(..)
  ) where

import Indexed.Types
import Indexed.Functor

-- | Minimum definition ('imapM' /and/ 'itraverse') or ('isequence' /and/ 'isequenceA').
class IFunctor t => ITraversable t where
  imapM :: IMonad m => (a ~> m b) -> t a ~> m (t b)
  imapM f = isequence . imap f

  itraverse :: IApplicative f => (a ~> f b) -> t a ~> f (t b)
  itraverse f = isequenceA . imap f

  isequence :: IMonad m => t (m a) ~> m (t a)
  isequence = imapM id

  isequenceA :: IApplicative f => t (f a) ~> f (t a)
  isequenceA = itraverse id
