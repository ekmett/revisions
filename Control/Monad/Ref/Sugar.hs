{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Ref.Sugar
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  type families
--
----------------------------------------------------------------------------
module Control.Monad.Ref.Sugar
  ( (=:)
  , (%=)
  , (+=) , (*=) , (-=)
  , (//=)
  ) where

import Control.Monad.Ref

(=:) :: MonadRef m => Ref m a -> a -> m ()
(=:) = writeRef
{-# INLINE (=:) #-}

(%=) :: MonadRef m => Ref m a -> (a -> a) -> m ()
(%=) = modifyRef
{-# INLINE (%=) #-}

(+=),(-=),(*=) :: (MonadRef m, Num a) => Ref m a -> a -> m ()
v += b = v %= (b +)
v -= b = v %= (b -)
v *= b = v %= (b *)

(//=) :: (MonadRef m, Fractional a) => Ref m a -> a -> m ()
v //= b = v %= (b /)
