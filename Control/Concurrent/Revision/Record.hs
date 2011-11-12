{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Revision.Record
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  undecidable instances
--
-- MonadRecord allows us to record a partially evaluated version of a
-- monadic action, while we evaluate it
--
----------------------------------------------------------------------------

module Control.Concurrent.Revision.Record
  ( MonadRecord(..)
  , record_
  ) where

import Control.Concurrent.STM
import Control.Monad.Instances ()
import Control.Monad.ST
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.List
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Text.ParserCombinators.ReadP
import Data.Monoid (Monoid)
import Data.Functor.Identity
import Data.Sequence (Seq)

{-
import Data.Key
import Data.Functor.Representable
import qualified Control.Monad.Representable.State  as Representable
import qualified Control.Monad.Representable.Reader as Representable
import Data.Functor.Representable.Trie
-}

class Monad m => MonadRecord m where
  -- | Recording a series of actions for later playback. Semantically,
  --
  -- > record m = do
  -- >   a <- m
  -- >   return (m, a)
  --
  -- except we can optimize this for many monads!
  record :: m a -> m (m a, a)
  record m = do
     a <- m
     return (m, a)

record_ :: MonadRecord m => m a -> m (m a)
record_ m = do
  ma <- record m
  return (fst ma)

-- base monads should be instances, regardless of recording efficiency
instance MonadRecord []
instance MonadRecord Seq
instance MonadRecord Identity
instance MonadRecord Maybe
instance MonadRecord (Either e)
instance MonadRecord IO
instance MonadRecord STM
instance MonadRecord (ST s)
instance MonadRecord ReadP

-- | NB: recording can't see inside of ContT, but we can elide outer layers
instance Monad m => MonadRecord (ContT e m)

instance MonadRecord m => MonadRecord (IdentityT m) where
  record (IdentityT ma) = IdentityT $ do
    (m, a) <- record ma
    return (IdentityT m, a)

-- instance Monoid w => MonadRecord ((,) w) where
--  record ma@(_, a) = (ma, a)

instance (MonadRecord m, Monoid w) => MonadRecord (Strict.WriterT w m) where
  record (Strict.WriterT f) = Strict.WriterT $ do
    (m, (a, w)) <- record f
    return ((Strict.WriterT m, a), w)

instance (MonadRecord m, Monoid w) => MonadRecord (Lazy.WriterT w m) where
  record (Lazy.WriterT f) = Lazy.WriterT $ do
    (m,~(a, w)) <- record f
    return ((Lazy.WriterT m, a), w)

instance (MonadRecord m, Eq e) => MonadRecord (ReaderT e m) where
  record (ReaderT f) = ReaderT $ \e -> do
    (m, a) <- record $ f e
    return (ReaderT $ \ e' -> if e == e' then m else f e', a)

instance Eq e => MonadRecord ((->) e) where
  record f e = (\ e' -> if e == e' then a else f e', a)
    where a = f e

instance (MonadRecord m, Eq s) => MonadRecord (Strict.StateT s m) where
  record (Strict.StateT f) = Strict.StateT $ \s -> do
    (m, (a, s'')) <- record $ f s
    return ((Strict.StateT $ \ s' -> if s == s' then m else f s', a), s'')

instance (MonadRecord m, Eq s) => MonadRecord (Lazy.StateT s m) where
  record (Lazy.StateT f) = Lazy.StateT $ \s -> do
    (m, ~(a, s'')) <- record $ f s
    return ((Lazy.StateT $ \ s' -> if s == s' then m else f s', a), s'')

instance (MonadRecord m, Eq r, Monoid w, Eq s) => MonadRecord (Strict.RWST r w s m)  where
  record (Strict.RWST f) = Strict.RWST $ \r s -> do
    (m, (a, s'', w)) <- record $ f r s
    return ((Strict.RWST $ \r' s' -> if r == r' && s == s' then m else f r' s', a), s'', w)

instance (MonadRecord m, Eq r, Monoid w, Eq s) => MonadRecord (Lazy.RWST r w s m)  where
  record (Lazy.RWST f) = Lazy.RWST $ \r s -> do
    (m, ~(a, s'', w)) <- record $ f r s
    return ((Lazy.RWST $ \r' s' -> if r == r' && s == s' then m else f r' s', a), s'', w)

-- | Subject to the usual limitations about ListT not actually being a monad transformer
instance MonadRecord m => MonadRecord (ListT m) where
  record (ListT f) = ListT $ do
    (m,as) <- record f
    return [ (ListT m, a) | a <- as ]

instance (MonadRecord m, Error e) => MonadRecord (ErrorT e m) where
  record (ErrorT f) = ErrorT $ do
    (m, ea) <- record $ f
    return $ do
      a <- ea
      return (ErrorT m, a)

-- exotic instances
{-
instance HasTrie e => MonadRecord ((:->:) e)

instance (MonadRecord m, Adjustable f, Representable f) => MonadRecord (Representable.ReaderT f m) where
  record (Representable.ReaderT f) = Representable.ReaderT $ tabulate $ \e -> do
    (m, a) <- record $ index f e
    return (Representable.ReaderT $ replace e m f, a)

instance (MonadRecord m, Adjustable f, Representable f) => MonadRecord (Representable.StateT f m) where
  record (Representable.StateT f) = Representable.StateT $ tabulate $ \s -> do
    (m, (a, s')) <- record $ index f s
    return ((Representable.StateT $ replace s m f, a), s')
-}
