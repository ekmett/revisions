{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
-- {-# LANGUAGE Trustworthy #-}
module Control.Concurrent.Revision
  (
  -- * Version Control
    Rev
  , runRev
  -- * Fork/Join parallelism
  , fork
  , join
  -- * Versioned variables
  , Versioned
  , vcreate
  , vread
  , (=:), (+=), (*=), (-=), (//=), (%=)
  -- * Customized merging
  , vcreateM
  -- * Customized forking
  , vcreateMF
  , Fork(..)
  ) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Control.Applicative
import Control.Concurrent.Supply
import GHC.IO (unsafeDupablePerformIO)
import GHC.Prim (Any, par#)
import Unsafe.Coerce (unsafeCoerce)
import GHC.Types (Int(I#))


vlookup :: Int -> IntMap Any -> Maybe a
vlookup i m = unsafeCoerce (IntMap.lookup i m)
{-# INLINE vlookup #-}

vinsert :: Int -> a -> IntMap Any -> IntMap Any
vinsert i a = IntMap.insert i (unsafeCoerce a)
{-# INLINE vinsert #-}

-- a basic block worth of actions, no fork, no joins
data Seg = Seg
  { _reads  :: !IntSet
  , _writes :: !(IntMap Any)
  }

instance Monoid Seg where
  Seg r w `mappend` Seg r' w' = Seg (IntSet.union (r IntSet.\\ IntMap.keysSet w') r') (IntMap.union w w')
  mempty = Seg mempty mempty

type BranchId = Int
type Weight = Int
type Length = Int
type Summary = Seg

-- | A complete binary tree of segments with summaries
data Tree
  = Bin {-# UNPACK #-} !BranchId {-# UNPACK #-} !Summary {-# UNPACK #-} !Seg !Tree !Tree
  | Tip {-# UNPACK #-} !BranchId {-# UNPACK #-} !Seg

-- | A skew binomial random access list of segments with summaries
data History
  = Cons {-# UNPACK #-} !Length {-# UNPACK #-} !Weight {-# UNPACK #-} !BranchId {-# UNPACK #-} !Summary !Summary !Tree !History
  | Nil

class Segmented t where
  summary :: t -> Summary
  branchId :: t -> BranchId

instance Segmented Tree where
  summary (Tip _ s) = s
  branchId (Tip i _) = i

instance Segmented History where
  summary Nil = mempty
  summary (Cons _ _ _ s _ _ _) = s
  branchId Nil = -1
  branchId (Cons _ _ b _ _ _ _) = b

consS :: BranchId -> Seg -> History -> History
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


consT :: Weight -> Tree -> History -> History
consT w t h = Cons (len h + w) w (branchId t) (summary t `mappend` summary h) (summary t) t h

type Merges = IntMap (Any -> Any -> Any -> Any)

keep :: Length -> History -> (# Summary, History #)
keep l h
  | l == len h = (# mempty, h #)
  | otherwise  = keep' mempty l h

-- keep a history of a given length, collecting a summary of what was skipped
keep' :: Summary -> Length -> History -> (# Summary, History #)
keep' acc n h@(Cons l w i _ ts t xs)
  | n == l    = (# acc, h #)
  | otherwise = case compare n (l - w) of
    GT -> keepT acc (n - l + w) w t xs
    EQ -> (# mappend acc ts, xs #)
    LT -> keep' (mappend acc ts) (n - w) xs
keep' acc _ Nil = (# acc, Nil #)

keepT :: Summary -> Length -> Weight -> Tree -> History -> (# Summary, History #)
keepT acc n _ (Tip b s) h
  | n > 0     = (# acc, consS b s h #)
  | otherwise = (# mappend acc s, h #)
keepTree acc n w (Bin b s a l r) h = case compare n w2 of
    LT              -> keepT (acc `mappend` a `mappend` summary l) n w2 r h
    EQ              -> (# acc `mappend` a `mappend` summary l, consT w2 r h #)
    GT | n == w - 1 -> (# acc `mappend` a, consT w2 l (consT w2 r h) #)
       | otherwise  -> keepT (acc `mappend` a) (n - w2) w2 l (consT w2 r h)
  where w2 = div w 2

-- trim history to a common shape then search for the least common ancestor
joinH :: Merges -> Seg -> History -> Seg -> History -> (# Seg, History #)
joinH mm sl hl sr hr = case compare ll lr of
  LT | (# r, hr' #) <- keep ll hr -> joinH' mm sl hl (sr `mappend` r) hr'
  EQ -> joinH' mm sl hl sr hr
  GT | (# l, hl' #) <- keep lr hl -> joinH' mm (sl `mappend` l) hl' sr hr
  where 
    ll = len hl
    lr = len hr

commonH :: History -> History -> Bool
commonH Nil Nil = True
commonH (Cons _ _ bl _ _ _ _) (Cons _ _ br _ _ _ _) = bl == br
commonH _ _ = False -- crash?

joinH' :: Merges -> Seg -> History -> Seg -> History -> (# Seg, History #)
joinH' mm sl Nil sr Nil = joinS mm sl sr Nil
joinH' mm sl h@(Cons ll w bl _ stl tl ls)
          sr   (Cons lr _ br _ str tr rs)
  | bl == br      = joinS mm sl sr h
  | commonH ls rs = joinT w mm sl tl sr tr ls
  | otherwise     = joinH' mm (sl `mappend` stl) ls (sr  `mappend` str) rs

commonT :: Tree -> Tree -> Bool
commonT (Tip i _)       (Tip j _)       = i == j
commonT (Bin i _ _ _ _) (Bin j _ _ _ _) = i == j
commonT _ _ = False -- crash?

joinT :: Weight -> Merges -> Seg -> Tree -> Seg -> Tree -> History -> (# Seg, History #)
joinT _ mm al (Tip i l) ar (Tip j r) h
  | i == j    = joinS mm (al `mappend` l) (ar `mappend` r) h
  | otherwise = joinS mm al ar (consS i l h)
joinT w mm al (Bin _ sl l ll lr) ar (Bin _ sr r rl rr) h
  | commonT ll rl = joinS mm (al `mappend` summary ll `mappend` summary lr) (ar `mappend` summary rl `mappend` summary rr) h
  | commonT lr rr = joinT w2 mm (al `mappend` l) ll (ar `mappend` r) rl (consT w2 lr h)
  | otherwise     = joinT w2 mm (al `mappend` l `mappend` summary ll) lr (ar `mappend` r `mappend` summary rl) rr h
  where w2 = div w 2

joinS :: Merges -> Seg -> Seg -> History -> (# Seg, History #)
joinS mm l r h = (# joinS' mm l r h , h #)

joinS' :: Merges -> Seg -> Seg -> History -> Seg
joinS' mm (Seg rl wl) (Seg rr wr) h = Seg (IntSet.union rl rr) (IntMap.unionWithKey merge wl wr)
  where
    merge k l r = case IntMap.lookup k mm of
      Just merge -> undefined
      Nothing -> undefined
      -- TODO: finish

newtype Rev s a = Rev { unRev :: Supply -> Merges -> Seg -> History -> (# a, Supply, Merges, Seg, History #) }

runRev :: (forall s. Rev s a) -> a
runRev (Rev g) = case g (unsafeDupablePerformIO newSupply) mempty mempty Nil of
  (# a, _, _, _, _ #) -> a

instance Functor (Rev s) where
  fmap f (Rev k) = Rev $ \s mm c h -> case k s mm c h of
    (# a, s', mm', c', h' #) -> (# f a, s', mm', c', h' #)

instance Applicative (Rev s) where
  pure a = Rev (# a,,,, #)
  Rev mf <*> Rev ma = Rev $ \s mm c h -> case mf s mm c h of
    (# f, s', mm', c', h' #) -> case ma s' mm' c' h' of
       (# a, s'', mm'', c'', h'' #) -> (# f a, s'', mm'', c'', h'' #)

instance Monad (Rev s) where
  return a = Rev (# a,,,, #)
  Rev g >>= f = Rev $ \s mm c h -> case g s mm c h of
    (# a, s', mm', c', h' #) -> unRev (f a) s' mm' c' h'

data Task s a = Task a !Supply !Merges !Seg !History

instance Functor (Task s) where
  fmap f (Task a mm s c h) = Task (f a) mm s c h

fork :: Rev s a -> Rev s (Task s a)
fork (Rev g) = Rev $ \s mm c h -> case freshId# s of
  (# i, s' #) -> case splitSupply# s' of
    (# sl, sr #) -> let
        !h' = consS (I# i) c h
        t = case g sr mm mempty h' of
              (# a, sr', mm', c', h'' #) -> Task a sr' mm' c' h''
      in case par# t of _ -> (# t, sl, mm, mempty, h' #)

join :: Task s a -> Rev s a
join (Task a s' mm' c' h') = Rev $ \s mm c h ->
   let !mm'' = IntMap.union mm mm'
   in case joinH mm'' c h c' h' of
     (# c'', h'' #) -> (# a, s, mm'', c'', h'' #)

-- we hold the merge, reinitializer and INITIAL value in the versioned variables themselves
-- this way any variable disposed of after initialization but without being written to
-- won't lurk in the written sets forever.

data Fork a
  = Fork (a -> a)
  | BlindFork a

-- TODO: change the way the merge and fork functions work so we can know whether or not we use the old values
data Versioned s a = Versioned {-# UNPACK #-} !Int a (Fork a)

vcreateMF :: Maybe (a -> a -> a -> a) -> Fork a -> a -> Rev s (Versioned s a)
vcreateMF om f a = Rev $ \s mm c h -> case freshId# s of
  (# i#, s' #) | i <- I# i# -> case om of
     Just m | !mm' <- IntMap.insert i (unsafeCoerce m) mm -> (# Versioned i a f, s', mm', c, h #)
     Nothing                                              -> (# Versioned i a f, s', mm,  c, h #)

vcreateM :: (a -> a -> a -> a) -> a -> Rev s (Versioned s a)
vcreateM m a = vcreateMF (Just m) (Fork id) a
{-# INLINE vcreateM #-}

vcreate :: a -> Rev s (Versioned s a)
vcreate a = vcreateMF Nothing (Fork id) a
{-# INLINE vcreate #-}

vread :: Versioned s a -> Rev s a
vread (Versioned i a f) = Rev $ \s mm c@(Seg r w) h -> case vlookup i w of
  Just b -> (# b, s, mm, c, h #)
  Nothing -> case h of
    Cons _ _ b (Seg _ w') _ _ _ | b > i -> case f of
      BlindFork bf -> (# bf, s, mm, c, h #)
      Fork ff      -> let !c' = Seg (IntSet.insert i r) w in (# ff $ fromMaybe a $ vlookup i w', s, mm, c', h #)
    _ -> let !c' = Seg (IntSet.insert i r) w in (# a, s, mm, c', h #)

(=:) :: Versioned s a -> a -> Rev s ()
Versioned i _ _ =: a = Rev $ \s mm (Seg r w) h -> (# (), s, mm, Seg r (vinsert i a w), h #)
{-# INLINE (=:) #-}

(%=) :: Versioned s a -> (a -> a) -> Rev s ()
v %= f = do
  x <- vread v
  v =: f x

(+=) :: Num a => Versioned s a -> a -> Rev s ()
(-=) :: Num a => Versioned s a -> a -> Rev s ()
(*=) :: Num a => Versioned s a -> a -> Rev s ()
v += b = v %= (b +)
v -= b = v %= (b -)
v *= b = v %= (b *)
(//=) :: Fractional a => Versioned s a -> a -> Rev s ()
v //= b = v %= (b /)
