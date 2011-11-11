-- |
-- Module      : Data.IVar.Simple
-- Copyright   : (c) 2008, 2009 Bertram Felgenhauer
--               (c) 2011 Edward Kmett
-- License     : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : ghc
--
-- 'IVar's are write-once variables.
--
-- Similarily to 'MVar's, 'IVar's can be either empty or filled. Once filled,
-- they keep their value indefinitely - they are immutable.
--
-- Reading from an empty 'IVar' will block until the 'IVar' is filled. Because
-- the value read will never change, this is a pure computation.
--
-- Edit: BlockedIndefinitely => BlockedIndefinitelyOnSTM
module Data.IVar.Simple
  ( IVar
  , new
  , newFull
  , read
  , tryRead
  , write
  , tryWrite
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import System.IO.Unsafe
import Prelude hiding (read)

-- | A write-once (/immutable/) Variable
data IVar a = IVar (MVar ()) (MVar a) a

-- | Creates a new, empty 'IVar'.
new :: IO (IVar a)
new = do
    lock <- newMVar ()
    trans <- newEmptyMVar
    let {-# NOINLINE value #-}
        value = unsafePerformIO $ takeMVar trans
    return (IVar lock trans value)

-- | Create a new filled 'IVar'.
--
-- This is slightly cheaper than creating a new 'IVar' and then writing to it.
newFull :: a -> IO (IVar a)
newFull value = do
    lock <- newEmptyMVar
    return (IVar lock (error "unused MVar") value)

-- | Returns the value of an 'IVar'.
--
-- The evaluation will block until a value is written to the 'IVar' if there
-- is no value yet.
read :: IVar a -> a
read (IVar _ _ value) = value

-- | Try to read an 'IVar'. Returns 'Nothing' if there is not value yet.
tryRead :: IVar a -> IO (Maybe a)
tryRead (IVar lock _ value) = do
    empty <- isEmptyMVar lock
    if empty then return (Just value) else return Nothing

-- | Writes a value to an 'IVar'. Raises a 'BlockedIndefinitelyOnMVar' exception if
-- it fails.
write :: IVar a -> a -> IO ()
write ivar value = do
    result <- tryWrite ivar value
    when (not result) $ throwIO BlockedIndefinitelyOnMVar
-- Note: It would be easier to block forever when the IVar is full. However,
-- the thread would likely not be garbage collected then.

-- | Writes a value to an 'IVar'. Returns 'True' if successful.
tryWrite :: IVar a -> a -> IO Bool
tryWrite (IVar lock trans _) value = mask_ $ do
    a <- tryTakeMVar lock
    case a of
        Just _  -> putMVar trans value >> return True
        Nothing -> return False
