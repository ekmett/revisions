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
--
----------------------------------------------------------------------------
module Control.Concurrent.Revision.Class
  ( MonadVer(..)
  -- * Custom versioning
  , defEq
  , Policy(..)
  , ForkDef(..)
  ) where

import Control.Applicative
import Control.Monad.Ref
import Control.Concurrent.Revision.Merge
import Control.Concurrent.Revision.Task
import Data.Default
import Control.Monad.Trans.Reader
import Control.Monad.Trans.List
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.Error

class (Applicative m, MonadTask m) => MonadVer m where
  type Ver m :: * -> *
  ver           :: Policy m a -> a -> m (Ver m a)
  readVer         :: Ver m a -> m a
  writeVer        :: Ver m a -> a -> m ()
  modifyVer       :: Ver m a -> (a -> a) -> m ()
  atomicModifyVer :: Ver m a -> (a -> (a, b)) -> m b

  default ver :: (MonadTrans t, MonadVer n, m ~ t n) => Policy a -> a -> Ver n a
  ver d a = lift $ ver d a

  default readVer :: (MonadTrans t, MonadVer n, m ~ t n) => Ver n a -> t n a
  readVer v = lift $ readVer v

  default writeVer :: (MonadTrans t, MonadVer n, m ~ t n) => Ver n a -> a -> t n a
  writeVer v a = lift $ writeVer v a

  default modifyVer :: (MonadTrans t, MonadVer n, m ~ t n) => Ver n a -> (a -> a) -> t n ()
  modifyVer v f = lift $ modifyVer v f

  default atomicModifyVer :: (MonadTrans t, MonadVer n, m ~ t n) => Ver n a -> (a -> (a, b)) -> t n ()
  atomicModifyVer v f = lift $ atomicModifyVer v f

instance MonadVer m => MonadVer (IdentityT m) where
  type Ver (IdentityT m) = Ver m

instance (MonadVer m, Eq e) => MonadVer (ReaderT e m) where
  type Ver (ReaderT e m) = Ver m

instance (MonadVer m, Semigroup s) => MonadVer (Strict.StateT s m) where
  type Ver (Strict.StateT s m) = Ver m

instance (MonadVer m, Monoid w) => MonadVer (Strict.WriterT w m) where
  type Ver (Strict.WriterT w m) = Ver m

instance (MonadVer m, Eq r, Monoid w, Semigroup s) => MonadVer (Strict.RWST r w s m) where
  type Ver (Strict.RWST r w s m) = Ver m

instance (MonadVer m, Semigroup s) => MonadVer (Lazy.StateT s m) where
  type Ver (Lazy.StateT s m) = Ver m

instance (MonadVer m, Monoid w) => MonadVer (Lazy.WriterT w m) where
  type Ver (Lazy.WriterT w m) = Ver m

instance (MonadVer m, Eq r, Monoid w, Semigroup s) => MonadVer (Lazy.RWST r w s m) where
  type Ver (Lazy.RWST r w s m) = Ver m

instance (MonadVer m, Error e) => MonadVer (ErrorT e m) where
  type Ver (ErrorT e m) = Ver m

instance (MonadVer m) => MonadVer (ListT m) where
  type Ver (ListT m) = Ver m

-- | A user-specified forking policy. When a versioned variable is accessed after a fork we use this strategy to
-- adjust the value of the variable as seen by read.
data ForkPolicy a
  = Fork (a -> a) -- ^ After a fork @Fork f@ replaces the old value @a@ with @f a@
  | BlindFork a   -- ^ After a fork @BlindFork b@ replaces any old value with @b@, reducing read conflicts

-- | > def = Fork id
instance Default (ForkPolicy a) where
  def = Fork id

data Policy m a = Policy
  { merging :: MergePolicy m a -- ^ Merge strategy to use when both branches have performed an edit. Any monadic side-effects are required to be commutative
  , forking :: ForkPolicy a    -- ^ What to do with this variable on the first read within a given branch
  , equating :: a -> a -> Bool -- ^ A user-supplied equality test used to recovering sharing despite intervening changes
  }

-- | By default the fork wins merges, we don't change the variable after forking,
-- and we do not try to recover if there are any intervening changes to the versioned variable
instance MonadVer m => Default (Policy m a) where
  def = Policy def def (\_ _ -> False)

defEq :: Eq a => Policy m a
defEq = def { equating = (==) }
