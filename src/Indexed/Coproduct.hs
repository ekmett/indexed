{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Indexed.Coproduct
  ( (+)(InL,InR)
  ) where

import Control.Applicative
import Indexed.Foldable
import Indexed.Functor
import Indexed.Traversable

infixr 6 +

data (f + g) a i
  = InL (f a i)
  | InR (g a i)

instance (IFunctor f, IFunctor g) => IFunctor (f + g) where
  imap h (InL as) = InL (imap h as)
  imap h (InR bs) = InR (imap h bs)

instance (IFoldable f, IFoldable g) => IFoldable (f + g) where
  ifoldMap f (InL as) = ifoldMap f as
  ifoldMap f (InR bs) = ifoldMap f bs

instance (IIFoldable f, IIFoldable g) => IIFoldable (f + g) where
  iifoldMap f (InL as) = iifoldMap f as
  iifoldMap f (InR bs) = iifoldMap f bs

instance (ITraversable f, ITraversable g) => ITraversable (f + g) where
  itraverse f (InL as) = InL <$> itraverse f as
  itraverse f (InR bs) = InR <$> itraverse f bs

instance (IComonad f, IComonad g) => IComonad (f + g) where
  iextract (InL as) = iextract as
  iextract (InR bs) = iextract bs
  iextend f (InL as) = InL (iextend (f . InL) as)
  iextend f (InR bs) = InR (iextend (f . InR) bs)
