{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Revision.Task
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  type families, GADTs, unboxed tuples, rank-2 types
--
-- Forking and joining computations in transformers
----------------------------------------------------------------------------
module Control.Concurrent.Revision.Task
  ( MonadTask(..)
  ) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.List
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.Error
import Control.Monad.Trans.Class
import Data.Semigroup

-- IO Task
{-
import Control.Concurrent
import Control.Exception as Exception
import Data.IVar.Simple (IVar)
import qualified Data.IVar.Simple as IVar
-}

class Monad m => MonadTask m where
  type Task m :: * -> *

  -- | Fork a computation.
  --
  -- Both computations get independent copies of the versioned variables already in scope.
  --
  -- Forking is done through the spark queue. If resources are not available at the time of the fork, and the spark queue
  -- overflows, then the computation may be forgotten until joining with it forces it to be evaluated.
  --
  -- This also means that forking under high load is all but free, and that forked tasks won't interfere with real threads
  -- when they are trying to get work done.
  fork :: m a -> m (Task m a)

  -- | Join with a previously forked computation
  --
  -- Blocks waiting for the background task to complete by forcing it, then merges all edits to versioned variables that
  -- have occurred to their least common ancestor version of the current task and the joinee.
  --
  -- By default versioned variables use a 'joinee wins' strategy which means that edits that occurred in the task we're joining
  -- with replace the current value.
  join :: Task m a -> m a

instance MonadTask m => MonadTask (ReaderT e m) where
  type Task (ReaderT e m) = Task m
  fork (ReaderT f) = ReaderT $ \ e -> fork (f e)
  join = lift . join

newtype ErrorTask e m a = ErrorTask (Task m (Either e a))
instance (MonadTask m, Error e) => MonadTask (ErrorT e m) where
  type Task (ErrorT e m) = ErrorTask e m
  fork (ErrorT m) = ErrorT $ do
     t <- fork m
     return $ Right $ ErrorTask t
  join (ErrorTask t) = ErrorT $ join t

newtype StateTask s m a = StateTask (Task m (a, s))

-- use a semigroup as a 2 way merge strategy since we don't know how to find the least common ancestor!
instance (MonadTask m, Semigroup s) => MonadTask (Strict.StateT s m) where
  type Task (Strict.StateT s m) = StateTask s m
  fork (Strict.StateT m)  = Strict.StateT $ \ s -> do
     t <- fork (m s)
     return (StateTask t, s)
  join (StateTask t) = Strict.StateT $ \ mainState -> do
     (a, forkState) <- join t
     return (a, mainState <> forkState)

instance (MonadTask m, Semigroup s) => MonadTask (Lazy.StateT s m) where
  type Task (Lazy.StateT s m) = StateTask s m
  fork (Lazy.StateT m)  = Lazy.StateT $ \ s -> do
     t <- fork (m s)
     return (StateTask t, s)
  join (StateTask t) = Lazy.StateT $ \ mainState -> do
     (a, forkState) <- join t
     return (a, mainState <> forkState)

newtype WriterTask w m a = WriterTask (Task m (a, w))
instance (MonadTask m, Monoid w) => MonadTask (Strict.WriterT w m) where
  type Task (Strict.WriterT w m) = WriterTask w m
  fork (Strict.WriterT m) = Strict.WriterT $ do
    t <- fork m
    return (WriterTask t, mempty)
  join (WriterTask t) = Strict.WriterT $ join t

instance (MonadTask m, Monoid w) => MonadTask (Lazy.WriterT w m) where
  type Task (Lazy.WriterT w m) = WriterTask w m
  fork (Lazy.WriterT m) = Lazy.WriterT $ do
    t <- fork m
    return (WriterTask t, mempty)
  join (WriterTask t) = Lazy.WriterT $ join t

newtype RWSTask w s m a = RWSTask (Task m (a, s, w))
instance (MonadTask m, Semigroup s, Monoid w) => MonadTask (Strict.RWST r w s m) where
  type Task (Strict.RWST r w s m) = RWSTask w s m
  fork (Strict.RWST m) = Strict.RWST $ \ r s -> do
    t <- fork (m r s)
    return (RWSTask t, s, mempty)
  join (RWSTask t) = Strict.RWST $ \ _ mainState -> do
    (a, forkState, w) <- join t
    return (a, mainState <> forkState, w)

instance (MonadTask m, Semigroup s, Monoid w) => MonadTask (Lazy.RWST r w s m) where
  type Task (Lazy.RWST r w s m) = RWSTask w s m
  fork (Lazy.RWST m) = Lazy.RWST $ \ r s -> do
    t <- fork (m r s)
    return (RWSTask t, s, mempty)
  join (RWSTask t) = Lazy.RWST $ \ _ mainState -> do
    (a, forkState, w) <- join t
    return (a, mainState <> forkState, w)

newtype ListTask m a = ListTask (Task m [a])
-- | Subject to the usual caveats about ListT being not quite a monad transformer
instance MonadTask m => MonadTask (ListT m) where
  type Task (ListT m) = ListTask m
  fork (ListT m) = ListT $ do
    t <- fork m
    return [ListTask t]
  join (ListTask t) = ListT $ join t

{-
data IOTask a = IOTask {-# UNPACK #-} !ThreadId {-# UNPACK #-} !(IVar (Either SomeException a))
-- | We do not currently provide a way to throw away the tasks that we backgrounded, so be careful
instance MonadTask IO where
  type Task IO = IOTask
  fork m = do
    iv <- IVar.new
    tid <- forkIO $ do
      v <- Exception.catch (liftM Right m) (return . Left)
      IVar.write iv v
    return $ IOTask tid iv
  join (IOTask _ v) = case IVar.read v of
    Left e  -> Exception.throw e
    Right a -> return a
-}
