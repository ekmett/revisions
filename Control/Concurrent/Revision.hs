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
-- Module      :  Control.Concurrent.Revision
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  type families, GADTs, unboxed tuples, rank-2 types
--
----------------------------------------------------------------------------
module Control.Concurrent.Revision
  (
  -- * Version Control
    Rev
  , runRev
  , runRevWith
  -- * Fork/Join parallelism
  , Task
  , fork
  , join
  -- * Versioned variables
  , Versioned
  -- * Custom versioning
  , version -- version def { mergeWith = Merge (....) }
  , VersionDef(..) -- options for version
  , MergeDef(..)
  , ForkDef(..)
  -- * Ref sugar
  , (=:)
  , (%=)
  , (+=) , (*=) , (-=)
  , (//=)
  ) where

import Control.Applicative
import Control.Concurrent.Supply
import Control.Concurrent.Speculation
import Control.Concurrent.Speculation.Class
import Control.Monad (MonadPlus(..))
import Control.Monad.Error (Error(..), MonadError(..))
import Control.Monad.Ref
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Default
import GHC.IO (unsafeDupablePerformIO)
import GHC.Conc (par)
import GHC.Types (Int(I#))
import Unsafe.Coerce (unsafeCoerce)

type Depth = Int

newtype Rev e s a = Rev { unrev :: forall r.
  (a -> Supply -> IntSet -> IntMap Write -> Depth -> History -> r) ->
  (e -> r) ->
  Supply -> IntSet -> IntMap Write -> Depth -> History -> r }

runRev :: (forall s. Rev String s a) -> a
runRev = runRevWith error
{-# INLINE runRev #-}

runRevWith :: (e -> a) -> (forall s. Rev e s a) -> a
runRevWith err (Rev g)
  = g (\a _ _ _ _ _ -> a) err
      (unsafeDupablePerformIO newSupply)
      mempty mempty 0 Nil
{-# INLINE runRevWith #-}

instance Functor (Rev e s) where
  fmap f (Rev m) = Rev $ \k kf -> m (k . f) kf

instance Applicative (Rev e s) where
  pure a = Rev $ \k _ -> k a
  Rev mf <*> Rev ma = Rev $ \k kf -> mf (\f -> ma (\a -> k (f a)) kf) kf
  -- (<*>) = ap

instance Error e => Alternative (Rev e s) where
  empty = Rev $ \_ kf _ _ _ _ _ -> kf noMsg
  Rev m <|> Rev n = Rev $ \ks kf s r w d h -> m ks (\_ -> n ks kf s r w d h) s r w d h

instance Error e => Monad (Rev e s) where
  return a = Rev $ \k _ -> k a
  Rev m >>= f = Rev $ \k kf -> m (\a -> unrev (f a) k kf) kf
  fail s = Rev $ \_ kf _ _ _ _ _ -> kf (strMsg s)

instance Error e => MonadError e (Rev e s) where
  throwError e = Rev $ \_ kf _ _ _ _ _ -> kf e
  catchError (Rev m) f = Rev $ \ks kf s r w d h -> m ks (\e -> unrev (f e) ks kf s r w d h) s r w d h

instance Error e => MonadPlus (Rev e s) where
  mzero = empty
  mplus = (<|>)

instance Error e => MonadSpec (Rev e s) where
  specByM  f g a = Rev (\k _ s r w d h -> specBy  f g (\ga -> k ga s r w d h) a)
  specByM' f g a = Rev (\k _ s r w d h -> specBy' f g (\ga -> k ga s r w d h) a)

data Task e s a
  = Task a !Supply !IntSet !(IntMap Write) {-# UNPACK #-} !Depth !History
  | FailedTask e

instance Functor (Task e s) where
  fmap f (Task a s r w d h) = Task (f a) s r w d h
  fmap _ (FailedTask e) = FailedTask e

fork :: Rev e s a -> Rev e s (Task e s a)
fork (Rev g) = Rev $ \k _ s r w d h -> case freshId# s of
  (# i, s' #) -> case splitSupply# s' of
    (# sl, sr #) -> let
        !h' = consS (I# i) (Segment r w) h
        !d' = d + 1
        t = g Task FailedTask sr mempty mempty d' h'
      in t `par` k t sl mempty mempty d' h'
{-# INLINE fork #-}

join :: Task e s a -> Rev e s a
join (FailedTask e) = Rev $ \_ kf _ _ _ _ _ -> kf e
join (Task a _ r' w' d' h') = Rev $ \ k _ s r w d h -> case freshId# s of
  (# i, s' #) ->
    k a s' mempty mempty (max d d' + 1) $
    joinH (Segment r w) h (Segment r' w') h' (consS (I# i))
{-# INLINE join #-}

data MergeDef a
  = Merge (a -> a -> a -> a) -- 3 way merge, base, joinee, joiner
  | JoineeMerge -- the joinee (forked process) wins
  | JoinerMerge -- the joiner (thread which issued join) wins

instance Default (MergeDef a) where
  def = JoineeMerge

data ForkDef a
  = Fork (a -> a)
  | BlindFork a

instance Default (ForkDef a) where
  def = Fork id

data Versioned s a = Versioned {-# UNPACK #-} !Int {-# UNPACK #-} !Depth {-# UNPACK #-} !(VersionDef a) a

data VersionDef a = VersionDef
  { merging :: MergeDef a      -- merge strategy to use when both branches have performed an edit
  , forking :: ForkDef a       -- what to do with this variable on the first read within a given branch
  , matching :: a -> a -> Bool -- recovering sharing despite changes
  }

instance Default (VersionDef a) where
  def = VersionDef def def (\_ _ -> False)

-- | Usage
--
-- > <- version def 0
-- > total <- version def { merging = Merge $ \o j m -> j + m - o } 0
version :: VersionDef a -> a -> Rev e s (Versioned s a)
version o a = Rev $ \k _ s r w d -> case freshId# s of
  (# i, s' #) -> k (Versioned (I# i) d o a) s' r w d
{-# INLINE version #-}

instance Error e => MonadRef (Rev e s) where
  type Ref (Rev e s) = Versioned s
  newRef = version def
  readRef (Versioned i d o a) = Rev $ \k _ s r w dh h -> case vlookup i w of
    Just b -> k b s r w dh h
    Nothing
      | dh <= d -> k a s r w dh h
      | otherwise -> case forking o of
        BlindFork b -> k b s r w dh h
        Fork ff     -> k (ff $ fromMaybe a $ vlookup i $ writes $ summary h) s (IntSet.insert i r) w dh h
  writeRef (Versioned i d o a) x = Rev $ \k _ s r w dh h -> case merging o of
    JoineeMerge -> k () s r (IntMap.insert i (JoineeWrite x) w) dh h
    JoinerMerge -> k () s r (IntMap.insert i (JoinerWrite x) w) dh h
    Merge m -> case vlookup i w of
      Just b -> k () s r (IntMap.insert i (MergeWrite m x b) w) dh h
      Nothing
        | dh <= d   -> k () s r (IntMap.insert i (MergeWrite m x a) w) dh h
        | otherwise -> k () s r (IntMap.insert i (MergeWrite m x (case forking o of
          BlindFork b -> b
          Fork ff     -> ff $ fromMaybe a $ vlookup i $ writes $ summary h)) w) dh h
  modifyRef v f = do
    a <- readRef v
    writeRef v (f a)

instance Error e => MonadAtomicRef (Rev e s) where
  atomicModifyRef v f = do
    a <- readRef v
    let (b,c) = f a
    writeRef v b
    return c

-- internal
vlookup :: Int -> IntMap Write -> Maybe a
vlookup i m = case IntMap.lookup i m of
  Just (MergeWrite _ a _) -> Just (unsafeCoerce a)
  Just (JoineeWrite a)    -> Just (unsafeCoerce a)
  Just (JoinerWrite a)    -> Just (unsafeCoerce a)
  Nothing                 -> Nothing
{-# INLINE vlookup #-}

data Write where
  MergeWrite  :: (a -> a -> a -> a) -> a -> a -> Write
  JoineeWrite :: a -> Write
  JoinerWrite :: a -> Write

isMergeWrite :: Write -> Bool
isMergeWrite (MergeWrite _ _ _) = True
isMergeWrite _ = False
{-# INLINE isMergeWrite #-}

chainWrites :: Write -> Write -> Write
chainWrites (MergeWrite mf a _) (MergeWrite _ _ o) = MergeWrite mf a (unsafeCoerce o)
chainWrites a _ = a
{-# INLINE chainWrites #-}

-- a basic block worth of actions, no fork, no joins
data Segment = Segment
  { _reads  :: !IntSet
  , writes :: !(IntMap Write)
  }

-- | Sequential composition of segments
instance Monoid Segment where
  Segment r w `mappend` Segment r' w' = Segment
    (IntSet.union (r IntSet.\\ IntMap.keysSet w') r')
    (IntMap.unionWith chainWrites w w')
  mempty = Segment mempty mempty

type BranchId = Int
type Weight = Int
type Length = Int
type Summary = Segment

-- | A complete binary tree of segments with summaries
data Tree
  = Bin {-# UNPACK #-} !BranchId {-# UNPACK #-} !Summary {-# UNPACK #-} !Segment !Tree !Tree
  | Tip {-# UNPACK #-} !BranchId {-# UNPACK #-} !Segment

-- | A skew binomial random access list of segments with summaries
data History
  = Cons {-# UNPACK #-} !Length {-# UNPACK #-} !Weight {-# UNPACK #-} !BranchId {-# UNPACK #-} !Summary !Summary !Tree !History
  | Nil

class Segmented t where
  summary :: t -> Summary
  branchId :: t -> BranchId

instance Segmented Tree where
  summary (Tip _ s) = s
  summary (Bin _ s _ _ _) = s
  branchId (Tip i _) = i
  branchId (Bin i _ _ _ _) = i

instance Segmented History where
  summary Nil = mempty
  summary (Cons _ _ _ s _ _ _) = s
  branchId Nil = -1
  branchId (Cons _ _ b _ _ _ _) = b

consS :: BranchId -> Segment -> History -> History
consS i s (Cons l w _ u ttl tl (Cons _ w2 _ _ ttr tr rs))
  | w == w2
  , !w3 <- w * 2 + 1
  , ttt <- s `mappend` ttl `mappend` ttr
  = Cons (l + 1) w3 i (mappend s u) ttt (Bin i ttt s tl tr) rs
consS i s xs@(Cons l _ _ u _ _ _) = Cons (l + 1) 1 i (mappend s u) s (Tip i s) xs
consS i s Nil                     = Cons 1       1 i s             s (Tip i s) Nil

len :: History -> Int
len Nil = 0
len (Cons l _ _ _ _ _ _) = l
{-# INLINE len #-}

consT :: Weight -> Tree -> History -> History
consT w t h = Cons (len h + w) w (branchId t) (summary t `mappend` summary h) (summary t) t h
{-# INLINE consT #-}

keep :: Length -> History -> (Summary -> History -> r) -> r
keep l h k
  | l == len h = k mempty h
  | otherwise  = keep' mempty l h k
{-# INLINE keep #-}

-- keep a history of a given length, collecting a summary of what was skipped
keep' :: Summary -> Length -> History -> (Summary -> History -> r) -> r
keep' acc n h@(Cons l w _ _ ts t xs) k
  | n == l    = k acc h
  | otherwise = case compare n (l - w) of
    GT -> keepT acc (n - l + w) w t xs k
    EQ -> k (mappend acc ts) xs
    LT -> keep' (mappend acc ts) (n - w) xs k
keep' acc _ Nil k = k acc Nil

keepT :: Summary -> Length -> Weight -> Tree -> History -> (Summary -> History -> r) -> r
keepT acc _ _ (Tip _ s) h k = k (mappend acc s) h
keepT acc n w (Bin _ _ a l r) h k = case compare n w2 of
    LT              -> keepT (acc `mappend` a `mappend` summary l) n w2 r h k
    EQ              -> k (acc `mappend` a `mappend` summary l) (consT w2 r h)
    GT | n == w - 1 -> k (acc `mappend` a) (consT w2 l (consT w2 r h))
       | otherwise  -> keepT (acc `mappend` a) (n - w2) w2 l (consT w2 r h) k
  where w2 = div w 2

-- trim history to a common shape then search for the least common ancestor
joinH :: Segment -> History -> Segment -> History -> (Segment -> History -> r) -> r
joinH sl hl sr hr k = case compare ll lr of
  LT -> keep ll hr $ \r hr' -> joinH' sl hl (sr `mappend` r) hr' k
  EQ -> joinH' sl hl sr hr k
  GT -> keep lr hl $ \l hl' -> joinH' (sl `mappend` l) hl' sr hr k
  where
    ll = len hl
    lr = len hr
{-# INLINE joinH #-}

commonH :: History -> History -> Bool
commonH Nil Nil = True
commonH (Cons _ _ bl _ _ _ _) (Cons _ _ br _ _ _ _) = bl == br
commonH _ _ = False -- crash?
{-# INLINE commonH #-}

joinH' :: Segment -> History -> Segment -> History -> (Segment -> History -> r) -> r
joinH' sl Nil sr Nil k = k (joinS sl sr) Nil
joinH' sl h@(Cons _ w bl _ stl tl ls) sr (Cons _ _ br _ str tr rs) k
  | bl == br      = k (joinS sl sr) h
  | commonH ls rs = joinT w sl tl sr tr ls k
  | otherwise     = joinH' (sl `mappend` stl) ls (sr  `mappend` str) rs k
joinH' _ _ _ _ _ = error "joinH': misaligned History"

commonT :: Tree -> Tree -> Bool
commonT (Tip i _)       (Tip j _)       = i == j
commonT (Bin i _ _ _ _) (Bin j _ _ _ _) = i == j
commonT _ _ = error "commonT: misaligned History"
{-# INLINE commonT #-}

joinT :: Weight -> Segment -> Tree -> Segment -> Tree -> History -> (Segment -> History -> r) -> r
joinT _ al (Tip i l) ar (Tip j r) h k
  | i == j    = k (joinS (al `mappend` l) (ar `mappend` r)) h
  | otherwise = k (joinS al ar) (consS i l h)
joinT w al (Bin _ _ l ll lr) ar (Bin _ _ r rl rr) h k
  | commonT ll rl = k (joinS (al `mappend` summary ll `mappend` summary lr) (ar `mappend` summary rl `mappend` summary rr)) h
  | commonT lr rr = joinT w2 (al `mappend` l) ll (ar `mappend` r) rl (consT w2 lr h) k
  | otherwise     = joinT w2 (al `mappend` l `mappend` summary ll) lr (ar `mappend` r `mappend` summary rl) rr h k
  where w2 = div w 2
joinT _ _ _ _ _ _ _ = error "joinT: misaligned history"

joinS :: Segment -> Segment -> Segment
joinS (Segment rl wl) (Segment rr wr) = Segment (IntSet.union rm (IntSet.union rl rr)) (IntMap.unionWith mergeWrites wl wr)
  where
    rm = IntMap.keysSet (IntMap.filter isMergeWrite (IntMap.intersection wl wr))
    mergeWrites (MergeWrite f l o) (MergeWrite _ r _) = MergeWrite f (f o l (unsafeCoerce r)) o
    mergeWrites (JoinerWrite a) (JoinerWrite _) = JoinerWrite a
    mergeWrites (JoineeWrite _) (JoineeWrite b) = JoineeWrite b
    mergeWrites _ _ = error "joinS: inconsistently versioned variable"

-- Sugar

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
