{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Revision.Class
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  type families, GADTs, unboxed tuples, rank-2 types
--
-- A spark-based deterministic fork/join computation monad with
-- revision-controlled variables.
----------------------------------------------------------------------------
module Control.Concurrent.Revision.Class
  ( MonadRev(..)
  -- * Custom versioning
  , defEq
  , VersionDef(..)
  , ForkDef(..)
  ) where

import Control.Applicative
import Control.Monad.Ref
import Control.Concurrent.Revision.Merge
import Control.Concurrent.Revision.Task
import Data.Default

class (Applicative m, MonadTask m) => MonadRev m where
  -- |
  --
  -- > import Data.Default
  --
  -- > x <- versioned def 0
  -- > foo <- versioned def (\x y -> x)
  -- > total <- versioned defEq { merging = ThreeWay $ \p f m -> f + m - p } 0
  versioned :: VersionDef a -> a -> m (Ref m a)

-- | A user-specified forking strategy. When a versioned variable is accessed after a fork we use this strategy to
-- adjust the value of the variable as seen by read.
data ForkDef a
  = Fork (a -> a) -- ^ After a fork @Fork f@ replaces the old value @a@ with @f a@
  | BlindFork a   -- ^ After a fork @BlindFork b@ replaces any old value with @b@, reducing read conflicts

-- | > def = Fork id
instance Default (ForkDef a) where
  def = Fork id

data VersionDef a = VersionDef
  { merging :: MergeStrategy a -- ^ merge strategy to use when both branches have performed an edit
  , forking :: ForkDef a       -- ^ what to do with this variable on the first read within a given branch
  , equating :: a -> a -> Bool -- ^ A user-supplied equality test used to recovering sharing despite intervening changes
  }

-- | By default the fork wins merges, we don't change the variable after forking,
-- and we do not try to recover if there are any intervening changes to the versioned variable
instance Default (VersionDef a) where
  def = VersionDef def def (\_ _ -> False)

defEq :: Eq a => VersionDef a
defEq = def { equating = (==) }
