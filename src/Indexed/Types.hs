{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  -- * Type Equality
  , (==)(Refl)
  , symm
  -- * Type Equality
  , ($)()
  ) where

import Control.Category
import Data.Data as Data
import Data.Monoid
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

instance (f ~ g) => Monoid (f :~> g) where
  mempty = Nat id
  mappend (Nat f) (Nat g) = Nat (f . g)

instance (Typeable1 f, Typeable1 g) => Typeable (f :~> g) where
  typeOf _ = mkTyConApp natTyCon [typeOf1 (undefined :: f a), typeOf1 (undefined :: g a)]

natTyCon :: TyCon
natTyCon = mkTyCon3 "indexed" "Indexed.Types" "(:~>)"
{-# NOINLINE natTyCon #-}

-- | A limit.
type Lim f = forall x. f x

-- | A limit suitable for storing in a container.
newtype Limit f = Limit { unlimit :: Lim f }


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

-- | Derpendency projection. (Work around)
herp :: (Fst ij ~ i, Snd ij ~ j) => p '(i,j) -> p ij
herp = unsafeCoerce

-- | Derpendency injection. (Work around)
derp :: (Fst ij ~ i, Snd ij ~ j) => p ij -> p '(i,j)
derp = unsafeCoerce

-------------------------------------------------------------------------------
-- Atkey
-------------------------------------------------------------------------------

data At :: * -> k -> k -> * where
  At :: a -> At a k k

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

instance Typeable3 At where
  typeOf3 _ = mkTyConApp atTyCon []

atTyCon :: TyCon
atTyCon = mkTyCon3 "indexed" "Indexed.Types" "At"
{-# NOINLINE atTyCon #-}

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
-- Type Equality
-------------------------------------------------------------------------------

infix 4 ==

-- | A witness of type equality
data a == b where
  Refl :: a == a

instance Category (==) where
  id = Refl
  Refl . Refl = Refl

-- | ('==') is symmetric.
symm :: a == b -> b == a
symm Refl = Refl

instance Show (a == b) where
  showsPrec _ Refl = showString "Refl"

instance Eq (a == b) where
  Refl == Refl = True

instance Ord (a == b) where
  Refl `compare` Refl = EQ

instance (a ~ b) => Read (a == b) where
  readsPrec d = readParen (d > 10) (\r -> [(Refl,s) | ("Refl",s) <- lex r ])

instance (a ~ b) => Monoid (a == b) where
  mempty = Refl
  mappend Refl Refl = Refl

instance Typeable2 (==) where
  typeOf2 _ = mkTyConApp eqTyCon []

eqTyCon :: TyCon
eqTyCon = mkTyCon3 "kinds" "Type.Eq" "(==)"
{-# NOINLINE eqTyCon #-}

instance (Data a, a ~ b) => Data (a == b) where
  gfoldl _ z Refl = z Refl
  toConstr _ = reflConstr
  gunfold _ z c = case constrIndex c of
    1 -> z Refl
    _ -> error "gunfold"
  dataTypeOf _ = eqDataType
  dataCast1 f = gcast1 f
  dataCast2 f = gcast2 f

reflConstr :: Constr
reflConstr = mkConstr eqDataType "Refl" [] Data.Prefix
{-# NOINLINE reflConstr #-}

eqDataType :: DataType
eqDataType = mkDataType "Type.Eq.(==)" [reflConstr]
{-# NOINLINE eqDataType #-}

-------------------------------------------------------------------------------
-- ($)
-------------------------------------------------------------------------------

infixr 0 $
-- | A type level version of @($)@, useful to avoid parentheses
type ($) a = a

