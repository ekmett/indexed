{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Indexed.Types
-- Copyright   :  (C) 2012-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Common types that are useful for working with indexed functors
-----------------------------------------------------------------------------
module Indexed.Types
  (
  -- * Natural Transformations
    (~>)()
  , (:~>)(Nat,($$))
  -- * Limits
  , Lim
  , Limit(..)
  -- * Product Kind Projections
  , Fst, Snd
  , herp, derp
  -- * Atkey
  , At(At), key
  , Atkey
  -- * Coatkey
  , Coat(Coat), uncoat
  , Coatkey
  -- * Type Equality
  , module Data.Type.Equality
  -- * Type Application
  , ($)()
  ) where

import Control.Category
import Data.Data as Data
import Data.Monoid
import Data.Type.Equality
import Prelude hiding (id,(.))
import Unsafe.Coerce

-------------------------------------------------------------------------------
-- Natural Transformations
-------------------------------------------------------------------------------

infixr 0 ~>
-- | A natural transformation from @f@ to @g@
type f ~> g = forall x. f x -> g x

infixr 0 :~>, $$
-- | A natural transformation suitable for storing in a container.
newtype f :~> g = Nat { ($$) :: f ~> g }
  deriving Typeable

instance f ~ g => Monoid (f :~> g) where
  mempty = Nat id
  mappend (Nat f) (Nat g) = Nat (f . g)

-- | A limit.
type Lim (f :: i -> *) = forall (x :: i). f x

-- | A limit suitable for storing in a container.
newtype Limit f = Limit { unlimit :: Lim f }
  deriving Typeable

-------------------------------------------------------------------------------
-- Product Kind Projections
-------------------------------------------------------------------------------

-- | Extract the first type from a lifted tuple of types.
type family Fst (p :: (a,b)) :: a
type instance Fst '(a,b) = a

-- | Extract the second type from a lifted tuple of types.
type family Snd (p :: (a,b)) :: b
type instance Snd '(a,b) = b

-------------------------------------------------------------------------------
-- Derpendent Types
-------------------------------------------------------------------------------

-- | Derpendency projection. (Work around).
--
-- This is only truly legal on GHC 7.10+ where 'GHC.Prim.Any' has been demoted to a type family.
herp :: p '(Fst ij,Snd ij) -> p ij
herp = unsafeCoerce

-- | Derpendency injection. (Work around)
--
-- This is only truly legal on GHC 7.10+ where 'GHC.Prim.Any' has been demoted to a type family.
derp :: p ij -> p '(Fst ij,Snd ij)
derp = unsafeCoerce

-------------------------------------------------------------------------------
-- Atkey
-------------------------------------------------------------------------------

data At :: * -> k -> k -> * where 
  At :: a -> At a k k
 deriving Typeable

instance Show a => Show (At a i j) where
  showsPrec d (At a) = showParen (d > 10) $
    showString "At " . showsPrec 11 a

instance (Read a, i ~ j) => Read (At a i j) where
  readsPrec d = readParen (d > 10) $ \r -> do
    ("At", s) <- lex r
    (k, t) <- readsPrec 11 s
    return (At k, t)

instance Eq a => Eq (At a i j) where
  At m == At n = m == n

instance Ord a => Ord (At a i j) where
  At m `compare` At n = compare m n

-- | Project out the value
key :: At a i j -> a
key (At a) = a

instance Monoid m => Category (At m) where
  id          = At mempty
  At m . At n = At (m <> n)

instance (Monoid m, i ~ j) => Monoid (At m i j) where
  mempty              = At mempty
  At m `mappend` At n = At (mappend m n)

-- | Type alias for indexed monads, functors, etc. in Bob Atkey's style.
type Atkey f i j a = f (At a j) i

instance (Data a, Typeable i, i ~ j) => Data (At a i j) where
  gfoldl f z (At a) = z At `f` a
  toConstr _ = atConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z At)
    _ -> error "gunfold"
  dataTypeOf _ = atDataType

atConstr :: Constr
atConstr = mkConstr atDataType "At" [] Data.Prefix
{-# NOINLINE atConstr #-}

atDataType :: DataType
atDataType = mkDataType "Indexed.Types.At" [atConstr]
{-# NOINLINE atDataType #-}

-------------------------------------------------------------------------------
-- Co-At
-------------------------------------------------------------------------------

newtype Coat a i j = Coat (i ~ j => a)
  deriving Typeable

instance (Show a, i ~ j) => Show (Coat a i j) where
  showsPrec d (Coat a) = showParen (d > 10) $
    showString "Coat " . showsPrec 11 a

instance Read a => Read (Coat a i j) where
  readsPrec d = readParen (d > 10) $ \r -> do
    ("Coat", s) <- lex r
    (k :: a, t) <- readsPrec 11 s
    return (Coat k, t)

instance (Eq a, i ~ j) => Eq (Coat a i j) where
  Coat m == Coat n = m == n

instance (Ord a, i ~ j) => Ord (Coat a i j) where
  Coat m `compare` Coat n = compare m n

instance Monoid m => Monoid (Coat m i j) where
  mempty      = Coat mempty
  mappend m n = Coat (case m of
    Coat a -> case n of
      Coat b -> mappend a b)

uncoat :: Coat a i i -> a
uncoat (Coat a) = a

-- | Type alias for indexed monads, functors, etc. in Bob Atkey's style.
type Coatkey f i j a = f (Coat a j) i

instance (Data a, Typeable i, i ~ j) => Data (Coat a i j) where
  gfoldl f z (Coat a) = z (\x -> Coat x) `f` a
  toConstr _ = coatConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z (\(x :: a) -> Coat x))
    _ -> error "gunfold"
  dataTypeOf _ = coatDataType

coatConstr :: Constr
coatConstr = mkConstr coatDataType "Coat" [] Data.Prefix
{-# NOINLINE coatConstr #-}

coatDataType :: DataType
coatDataType = mkDataType "Indexed.Types.Coat" [coatConstr]
{-# NOINLINE coatDataType #-}

-------------------------------------------------------------------------------
-- ($)
-------------------------------------------------------------------------------

infixr 0 $
-- | A type level version of @('$')@, useful to avoid parentheses
type ($) a = a

