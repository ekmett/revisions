{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Revision.Merge
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  type families, GADTs, unboxed tuples, rank-2 types
--
----------------------------------------------------------------------------
module Control.Concurrent.Revision.Merge
  ( MergeStrategy(..)
  , forkWins
  , mainWins
  -- * Using merge strategies
  , mergeWith
  , liftMergeStrategy
  , HasMergeStrategy(..)
  ) where

import Control.Applicative
import Data.Default
import Data.Monoid (Sum(..))

-- | A user-specified merge strategy
data MergeStrategy a
  = Merge2 (a -> a -> a)      -- ^ 2 way merge between fork and main
  | Merge3 (a -> a -> a -> a) -- ^ 3 way merge using base, fork, main

forkWins :: MergeStrategy a
forkWins = Merge2 $ \a _ -> a

mainWins :: MergeStrategy a
mainWins = Merge2 $ \_ b -> b

-- | > def = ForkWins
instance Default (MergeStrategy a) where
  def = forkWins

mergeWith :: MergeStrategy a -> a -> a -> a -> a
mergeWith (Merge2 m) _ e r = m   e r
mergeWith (Merge3 m) p e r = m p e r

liftMergeStrategy :: Applicative f => MergeStrategy a -> MergeStrategy (f a)
liftMergeStrategy ms = case ms of
  Merge2 m -> Merge2 (liftA2 m)
  Merge3 m -> Merge3 (liftA3 m)

class HasMergeStrategy a where
  mergeStrategy :: MergeStrategy a
  mergeStrategy = forkWins

instance HasMergeStrategy ()

instance HasMergeStrategy b => HasMergeStrategy (a -> b) where
  mergeStrategy = liftMergeStrategy mergeStrategy

instance (HasMergeStrategy a, HasMergeStrategy b) => HasMergeStrategy (a,b) where
  mergeStrategy = case (mergeStrategy, mergeStrategy) of
    (Merge2 l, Merge2 r) -> Merge2 $ \(fl, fr) (ml, mr) -> (l fl ml, r fr mr)
    (l, r) -> Merge3 $ \(pl,pr) (fl,fr) (ml,mr) -> (mergeWith l pl fl ml, mergeWith r pr fr mr)

instance Num a => HasMergeStrategy (Sum a) where
  mergeStrategy = Merge3 $ \(Sum p) (Sum f) (Sum m) -> Sum (f - p + m)
