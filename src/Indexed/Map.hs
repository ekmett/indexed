{-# LANGUAGE PolyKinds, Rank2Types, TypeFamilies, DefaultSignatures, BangPatterns, GADTs, ScopedTypeVariables #-}
module Indexed.Map
  ( IMap(..)
  , lookup
  , null
  , size
  , empty
  , singleton
  , toList
  , KV(..)
  ) where

import Control.Category
import Indexed.Show
import Indexed.Ord
import Prelude hiding ((.), id, lookup, null)

data IMap k v where
  Bin :: {-# UNPACK #-} !Int -> !(k a) -> v a -> !(IMap k v) -> !(IMap k v) -> IMap k v
  Tip :: IMap k v

lookup :: forall k v a. IOrd k => k a -> IMap k v -> Maybe (v a)
lookup = go
  where
    go :: k a -> IMap k v -> Maybe (v a)
    go !_ !Tip             = Nothing
    go !k (Bin _ kx x l r) = icompare k kx (go k l) (Just x) (go k r)
{-# INLINEABLE lookup #-}

null :: IMap k v -> Bool
null Tip = True
null Bin{} = False
{-# INLINE null #-}

size :: IMap k v -> Int
size Tip              = 0
size (Bin sz _ _ _ _) = sz
{-# INLINE size #-}

empty :: IMap k v
empty = Tip
{-# INLINE empty #-}

singleton :: k a -> v a -> IMap k v
singleton k x = Bin 1 k x Tip Tip
{-# INLINE singleton #-}

-- a key value pair
data KV k v where
  KV :: k a -> v a -> KV k v

instance (IEq k, IEq v) => Eq (KV k v) where
  KV ka va == KV kb vb = ieq ka kb (ieq va vb True False) False

instance (IOrd k, IOrd v) => Ord (KV k v) where
  compare (KV ka va) (KV kb vb) = icompare ka kb LT (icompare va vb LT EQ GT) GT

instance (IShow k, IShow v) => Show (KV k v) where
  showsPrec d (KV ka va) = showParen (d > 10) $
    showString "KV " . ishowsPrec 11 ka . showChar ' ' . ishowsPrec 11 va

instance (IShow k, IShow v) => Show (IMap k v) where
  showsPrec d m = showParen (d > 10) $
    showString "fromList " . showsPrec 11 (toList m)

toList :: IMap k v -> [KV k v]
toList x0 = go x0 [] where
  go !(Bin _ kx x l r) xs = go l (KV kx x : go r xs)
  go !Tip xs = xs
{-# INLINEABLE toList #-}
