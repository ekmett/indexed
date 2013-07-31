{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Indexed.Command
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Indexed.Command
  ( (>>)(..)
  ) where

import Indexed.Types
import Indexed.Functor

infixr 8 >>
infixr 1 :&

data (p >> q) r i = p i :& (q ~> r)

instance IFunctor (p >> q) where
  imap h (p :& k) = p :& (h . k)
