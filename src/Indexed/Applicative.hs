{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
module Indexed.Applicative
  ( IApplicative(..)
  ) where

import Indexed.Types
import Indexed.Functor

infixl 4 >*<, >*, *<

class IFunctor f => IApplicative f where
  ipure :: a -> f (At a i) i
  (>*<) :: f (At (a -> b) j) i -> f (At a k) j -> f (At a i) k

  (>*) :: f (At a j) i -> f (At b k) j -> f (At a i) k
--  m >* n = (\(At a) _ -> At a) >$< m >*< n

  (*<) :: f (At a j) i -> f (At b k) j -> f (At b i) k
--  m *< n = (\ _ (At b) -> At b) >$< m >*< n


-- iliftA fails
