{-# LANGUAGE PolyKinds, Rank2Types, TypeFamilies, DefaultSignatures, GADTs #-}
module Indexed.Ord
  ( IEq(..)
  , IOrd(..)
  ) where

class IEq f where
  ieq :: f a -> f b -> ((a ~ b) => r) -> r -> r
  default ieq :: IOrd f => f a -> f b -> ((a ~ b) => r) -> r -> r
  ieq fa fb e ne = icompare fa fb ne e ne

class IEq f => IOrd f where
  icompare :: f a -> f b -> r -> ((a ~ b) => r) -> r -> r
