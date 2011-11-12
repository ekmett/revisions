-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Revision
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
module Control.Concurrent.Revision
  ( module Control.Concurrent.Revision.Class
  , module Control.Concurrent.Revision.Merge
  , module Control.Concurrent.Revision.Monad
  , module Control.Concurrent.Revision.Record
  , module Control.Concurrent.Revision.Task
  , module Control.Monad.Ref
  , module Control.Monad.Ref.Sugar
  ) where

import Control.Concurrent.Revision.Class
import Control.Concurrent.Revision.Merge
import Control.Concurrent.Revision.Monad
import Control.Concurrent.Revision.Record
import Control.Concurrent.Revision.Task
import Control.Monad.Ref
import Control.Monad.Ref.Sugar
