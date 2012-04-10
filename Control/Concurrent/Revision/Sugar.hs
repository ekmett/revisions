{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Revision.Sugar
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  type families
--
----------------------------------------------------------------------------
module Control.Concurrent.Revision.Sugar
  ( (=:)
  , (%=)
  , (+=) , (*=) , (-=)
  , (//=)
  ) where

import Control.Concurrent.Revision.Monad

(=:) :: MonadRev m => Ver m a -> a -> m ()
(=:) = writeVer
{-# INLINE (=:) #-}

(%=) :: MonadRev m => Ver m a -> (a -> a) -> m ()
(%=) = modifyVer
{-# INLINE (%=) #-}

(+=),(-=),(*=) :: (MonadRef m, Num a) => Ver m a -> a -> m ()
v += b = v %= (b +)
v -= b = v %= (b -)
v *= b = v %= (b *)

(//=) :: (MonadRef m, Fractional a) => Ver m a -> a -> m ()
v //= b = v %= (b /)
