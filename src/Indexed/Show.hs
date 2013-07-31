{-# LANGUAGE PolyKinds #-}
module Indexed.Show where

import Control.Applicative
import Data.Proxy

class IShow f where
  ishowsPrec :: Int -> f a -> ShowS

instance IShow Proxy where
  ishowsPrec _ Proxy = showString "Proxy"

instance Show m => IShow (Const m) where
  ishowsPrec d (Const m) = showParen (d > 10) $
    showString "Const " . showsPrec 11 m
