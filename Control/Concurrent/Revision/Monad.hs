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
-- Module      :  Control.Concurrent.Revision.Monad
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
module Control.Concurrent.Revision.Monad
  (
  -- * Version Control
    Rev
  , runRev
  , runRevWith
  ) where

import Control.Applicative
import Control.Concurrent.Supply
import Control.Concurrent.Speculation
import Control.Concurrent.Speculation.Class
import Control.Concurrent.Revision.Class
import Control.Concurrent.Revision.Merge
import Control.Concurrent.Revision.Task
import Control.Monad (MonadPlus(..))
import Control.Monad.Error (Error(..), MonadError(..))
import Control.Monad.Ref
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import Data.Hashable
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Default
import GHC.IO (unsafeDupablePerformIO)
import GHC.Conc (par, pseq)
import Unsafe.Coerce (unsafeCoerce)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- | The revision control monad.
-- @Rev e s a@ represents a computation that will yield a result @a@ or fail with an error of type @e@. The @s@ 
-- is used in an ST-like fashion to keep us from entangling our versioned variables.
newtype Rev e s a = Rev { unrev :: forall r.
  (a -> Supply -> IntSet -> IntMap Write -> Depth -> History -> r) ->
  (e -> r) ->
  Supply -> IntSet -> IntMap Write -> Depth -> History -> r }

-- | Run a revision controlled fork/join computation, ultimately invoking 'error' for any error that occurs.
--
-- The resulting error is deterministically chosen if caused by 'fail', 'empty', 'mzero' or 'throwError'.
--
-- > runRev = runRevWith error
runRev :: (forall s. Rev String s a) -> a
runRev = runRevWith error
{-# INLINE runRev #-}

-- | Run a revision controlled fork/join computation with a user supplied error handler. 
--
-- The resulting error is deterministically chosen if caused by 'fail', 'empty', 'mzero' or 'throwError'.
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

data RevTask e s a
  = Task a !Supply !IntSet !(IntMap Write) {-# UNPACK #-} !Depth !History
  | FailedTask e

instance Functor (RevTask e s) where
  fmap f (Task a s r w d h) = Task (f a) s r w d h
  fmap _ (FailedTask e) = FailedTask e

instance Error e => MonadRef (Rev e s) where
  type Ref (Rev e s) = Versioned s

  newRef = versioned def
  {-# INLINE newRef #-}

  readRef (Versioned i d (VersionDef _ fd _) a) = case fd of
    BlindFork b -> Rev $ \k _ s r w dh -> k (fromMaybe (if dh <= d then a else b) (vlookup i w)) s r w dh
    Fork ff     -> Rev $ \k _ s r w dh h -> case vlookup i w of
      Just b  -> k b s r w dh h
      Nothing
        | dh > d    -> k (ff $ fromMaybe a $ vlookup i $ writes $ summary h) s (IntSet.insert i r) w dh h -- fork the var
        | otherwise -> k a s r w dh h -- fresh
  {-# INLINE readRef #-}

  writeRef (Versioned i d (VersionDef md _ _) a) x = case md of
    Merge2 m   -> Rev $ \k _ s r w -> k () s r (IntMap.insert i (Write2 m x) w)
    Merge3 m -> Rev $ \k _ s r w dh h ->
        let k' y = k () s r (IntMap.insert i (Write3 m x y) w) dh h
        in case vlookup i w of
          Just b -> k' b
          Nothing | dh <= d                   -> k' a
                  | !wh <- writes (summary h) -> k' (fromMaybe a (vlookup i wh))
  {-# INLINE writeRef #-}

  modifyRef v f = do
    a <- readRef v
    writeRef v (f a)
  {-# INLINE modifyRef #-}

instance Error e => MonadAtomicRef (Rev e s) where
  atomicModifyRef v f = do
    a <- readRef v
    let (b,c) = f a
    writeRef v b
    return c
  {-# INLINE atomicModifyRef #-}

instance Error e => MonadTask (Rev e s) where
  type Task (Rev e s) = RevTask e s
  fork (Rev g) = Rev $ \k _ s r w d h -> case freshId s of
    (i, s') 
      | h' <- consS i (Segment r w) h
      , d' <- d + 1
      , (sl, sr) <- splitSupply s' -> pseq h' $ pseq d' $
        let t = g Task FailedTask sr mempty mempty d' h' in
        t `par` k t sl mempty mempty d' h'
  {-# INLINE fork #-}

  join (FailedTask e) = Rev $ \_ kf _ _ _ _ _ -> kf e
  join (Task a _ r' w' d' h') = Rev $ \ k _ s r w d h -> case freshId s of
    ( i, s' ) ->
      k a s' mempty mempty (max d d' + 1) $ joinH (Segment r w) h (Segment r' w') h' (consS i)
  {-# INLINE join #-}

-- | A revision-controlled variable
--
-- NB: Most of the operations on Versioned come from 'MonadRef'.

data Versioned s a = Versioned {-# UNPACK #-} !Int {-# UNPACK #-} !Depth {-# UNPACK #-} !(VersionDef a) a

instance Eq (Versioned s a) where
  Versioned i _ _ _ == Versioned j _ _ _ = i == j

instance Hashable (Versioned s a) where
  hashWithSalt s (Versioned i _ _ _) = hashWithSalt s i

instance Error e => MonadRev (Rev e s) where
  versioned o a = Rev $ \k _ s r w d -> case freshId s of
    (i, s') -> k (Versioned i d o a) s' r w d
  {-# INLINE versioned #-}

-- internal
vlookup :: Int -> IntMap Write -> Maybe a
vlookup i m = case IntMap.lookup i m of
  Just (Write3 _ a _) -> Just (unsafeCoerce a)
  Just (Write2 _ a)   -> Just (unsafeCoerce a)
  Nothing                 -> Nothing
{-# INLINE vlookup #-}

data Write where
  Write2 :: (a -> a -> a) -> a -> Write
  Write3 :: (a -> a -> a -> a) -> a -> a -> Write

threeWay :: Write -> Bool
threeWay (Write3 _ _ _) = True
threeWay _ = False
{-# INLINE threeWay #-}

chainWrites :: Write -> Write -> Write
chainWrites (Write3 mf a _) (Write3 _ _ o) = Write3 mf a (unsafeCoerce o)
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

type Branch = Int
type Weight = Int
type Length = Int
type Depth = Int
type Summary = Segment

-- | A complete binary tree of segments with summaries
data Tree
  = Bin {-# UNPACK #-} !Branch {-# UNPACK #-} !Summary {-# UNPACK #-} !Segment !Tree !Tree
  | Tip {-# UNPACK #-} !Branch {-# UNPACK #-} !Segment

-- | A skew binomial random access list of segments with summaries
data History
  = Cons {-# UNPACK #-} !Length {-# UNPACK #-} !Weight {-# UNPACK #-} !Branch {-# UNPACK #-} !Summary !Summary !Tree !History
  | Nil

class Segmented t where
  summary :: t -> Summary
  branchId :: t -> Branch

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

consS :: Branch -> Segment -> History -> History
consS i s (Cons l w _ u ttl tl (Cons _ w2 _ _ ttr tr rs))
  | w == w2
  , !w3 <- w * 2 + 1
  , ttt <- s <> ttl <> ttr
  = Cons (l + 1) w3 i (mappend s u) ttt (Bin i ttt s tl tr) rs
consS i s xs@(Cons l _ _ u _ _ _) = Cons (l + 1) 1 i (mappend s u) s (Tip i s) xs
consS i s Nil                     = Cons 1       1 i s             s (Tip i s) Nil

len :: History -> Int
len Nil = 0
len (Cons l _ _ _ _ _ _) = l
{-# INLINE len #-}

consT :: Weight -> Tree -> History -> History
consT w t h = Cons (len h + w) w (branchId t) (summary t <> summary h) (summary t) t h
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
    LT              -> keepT (acc <> a <> summary l) n w2 r h k
    EQ              -> k (acc <> a <> summary l) (consT w2 r h)
    GT | n == w - 1 -> k (acc <> a) (consT w2 l (consT w2 r h))
       | otherwise  -> keepT (acc <> a) (n - w2) w2 l (consT w2 r h) k
  where w2 = div w 2

-- trim history to a common shape then search for the least common ancestor
joinH :: Segment -> History -> Segment -> History -> (Segment -> History -> r) -> r
joinH sl hl sr hr k = case compare ll lr of
  LT -> keep ll hr $ \r hr' -> joinH' sl hl (sr <> r) hr' k
  EQ -> joinH' sl hl sr hr k
  GT -> keep lr hl $ \l hl' -> joinH' (sl <> l) hl' sr hr k
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
  | otherwise     = joinH' (sl <> stl) ls (sr  <> str) rs k
joinH' _ _ _ _ _ = error "joinH': misaligned History"

commonT :: Tree -> Tree -> Bool
commonT (Tip i _)       (Tip j _)       = i == j
commonT (Bin i _ _ _ _) (Bin j _ _ _ _) = i == j
commonT _ _ = error "commonT: misaligned History"
{-# INLINE commonT #-}

joinT :: Weight -> Segment -> Tree -> Segment -> Tree -> History -> (Segment -> History -> r) -> r
joinT _ al (Tip i l) ar (Tip j r) h k
  | i == j    = k (joinS (al <> l) (ar <> r)) h
  | otherwise = k (joinS al ar) (consS i l h)
joinT w al (Bin _ _ l ll lr) ar (Bin _ _ r rl rr) h k
  | commonT ll rl = k (joinS (al <> summary ll <> summary lr) (ar <> summary rl <> summary rr)) h
  | commonT lr rr = joinT w2 (al <> l) ll (ar <> r) rl (consT w2 lr h) k
  | otherwise     = joinT w2 (al <> l <> summary ll) lr (ar <> r <> summary rl) rr h k
  where w2 = div w 2
joinT _ _ _ _ _ _ _ = error "joinT: misaligned history"

joinS :: Segment -> Segment -> Segment
joinS (Segment rl wl) (Segment rr wr) = Segment (IntSet.union rm $ IntSet.union rl rr) $ IntMap.unionWith mergeWrites wl wr
  where
    rm = IntMap.keysSet $ IntMap.filter threeWay $ IntMap.intersection wl wr
    mergeWrites (Write3 f l o) (Write3 _ r _) = Write3 f (f o l (unsafeCoerce r)) o
    mergeWrites (Write2 f a)   (Write2 _ b)   = Write2 f (f a (unsafeCoerce b))
    mergeWrites _ _ = error "joinS: inconsistently versioned variable"
