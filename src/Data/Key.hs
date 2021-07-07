{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE CPP               #-}
module Data.Key (
  -- * Keys
    Key

  -- * Keyed functors
  , Keyed(..)
  , (<#$>) -- :: Keyed f => (Key f -> a -> b) -> f a -> f b
  , keyed -- :: Keyed f => f a -> f (Key f, a)

  -- * Zippable functors
  , Zip(..)

  -- * Zipping keyed functors
  , ZipWithKey(..)

  -- * Indexable functors
  , Indexable(..)
  , (!)

  -- * Safe Lookup
  , Lookup(..)
  , lookupDefault

  -- * Adjustable
  , Adjustable(..)

  -- * FoldableWithKey
  , FoldableWithKey(..)
  , foldrWithKey' -- :: FoldableWithKey t => (Key t -> a -> b -> b) -> b -> t a -> b
  , foldlWithKey' -- :: FoldableWithKey t => (b -> Key t -> a -> b) -> b -> t a -> b
  , foldrWithKeyM -- :: (FoldableWithKey t, Monad m) => (Key t -> a -> b -> m b) -> b -> t a -> m b
  , foldlWithKeyM -- :: (FoldableWithKey t, Monad m) => (b -> Key t -> a -> m b) -> b -> t a -> m b
  , traverseWithKey_ -- :: (FoldableWithKey t, Applicative f) => (Key t -> a -> f b) -> t a -> f ()
  , forWithKey_ -- :: (FoldableWithKey t, Applicative f) => t a -> (Key t -> a -> f b) -> f ()
  , mapWithKeyM_ -- :: (FoldableWithKey t, Monad m) => (Key t -> a -> m b) -> t a -> m ()
  , forWithKeyM_ -- :: (FoldableWithKey t, Monad m) => t a -> (Key t -> a -> m b) -> m ()
  , concatMapWithKey -- :: FoldableWithKey t => (Key t -> a -> [b]) -> t a -> [b]
  , anyWithKey -- :: FoldableWithKey t => (Key t -> a -> Bool) -> t a -> Bool
  , allWithKey -- :: FoldableWithKey t => (Key t -> a -> Bool) -> t a -> Bool
  , findWithKey -- :: FoldableWithKey t => (Key t -> a -> Bool) -> t a -> Maybe a

  -- * FoldableWithKey1
  , FoldableWithKey1(..)
  , traverseWithKey1_ -- :: (FoldableWithKey1 t, Apply f) => (Key t -> a -> f b) -> t a -> f ()
  , forWithKey1_ -- :: (FoldableWithKey1 t, Apply f) => t a -> (Key t -> a -> f b) -> f ()
  , foldMapWithKeyDefault1 -- :: (FoldableWithKey1, Monoid m) => (Key t -> a -> m) -> t a -> m

  -- * TraversableWithKey
  , TraversableWithKey(..)
  , forWithKey -- :: (TraversableWithKey t, Applicative f) => t a -> (Key t -> a -> f b) -> f (t b)
  , forWithKeyM -- :: (TraversableWithKey t, Monad m) => t a -> (Key t -> a -> m b) -> m (t b)
  , mapAccumWithKeyL -- :: TraversableWithKey t => (Key t -> a -> b -> (a, c)) -> t a -> (a, t c)
  , mapAccumWithKeyR -- :: TraversableWithKey t => (Key t -> a -> b -> (a, c)) -> t a -> (a, t c)
  , mapWithKeyDefault -- :: TraversableWithKey t => (Key t -> a -> b) -> t a -> t b
  , foldMapWithKeyDefault -- :: (TraversableWithKey t, Monoid m) => (Key t -> a -> m) -> t a -> m

  -- * TraversableWithKey1
  , TraversableWithKey1(..)
  , foldMapWithKey1Default -- :: (TraversableWithKey1 t, Semigroup m) => (Key t -> a -> m) -> t a -> m
  ) where

import Control.Applicative
import Control.Comonad.Trans.Traced
import Control.Monad.Free
import Control.Comonad.Cofree
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import qualified Data.Array as Array
import Data.Array (Array)
import Data.Functor.Identity
import Data.Functor.Bind
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Product
import qualified Data.Functor.Sum as Functor
import Data.Foldable
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Ix hiding (index)
import Data.Map (Map)
import qualified Data.Map as Map

#ifdef MIN_VERSION_base_orphans
import Data.Orphans ()
#endif
import Data.Proxy
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust, listToMaybe)
import qualified Data.Monoid as Monoid
import Data.Semigroup hiding (Product)
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Sequence (Seq, ViewL(EmptyL), viewl, (|>))
import qualified Data.Sequence as Seq
import Data.Tagged
import Data.Traversable
import Data.Tree
import qualified Data.List as List
import Data.Void
import GHC.Generics
import Prelude hiding (lookup, zip, zipWith)

-- TODO: half of the functions manipulating Cofree and Free build the keys in the wrong order

type family Key (f :: * -> *)
type instance Key (Cofree f) = Seq (Key f)
type instance Key (Free f) = Seq (Key f)
type instance Key Tree = Seq Int
type instance Key NonEmpty = Int
type instance Key U1 = Void
type instance Key V1 = Void
type instance Key Par1 = ()
type instance Key Proxy = Void
type instance Key (Tagged a) = ()
type instance Key (Const e) = Void
type instance Key (Constant e) = Void
type instance Key (g :.: f) = (Key g, Key f)
type instance Key (f :*: g) = Either (Key f) (Key g)
type instance Key (f :+: g) = Either (Key f) (Key g)
type instance Key (Rec1 f) = Key f
type instance Key (M1 i c f) = Key f
type instance Key (K1 i c) = Void

-- * Keyed
class Functor f => Keyed f where
  mapWithKey :: (Key f -> a -> b) -> f a -> f b

instance Keyed f => Keyed (Free f) where
  mapWithKey f (Pure a) = Pure (f Seq.empty a)
  mapWithKey f (Free as) = Free (mapWithKey (mapWithKey . fmap f . flip (|>)) as)

instance Keyed f => Keyed (Cofree f) where
  mapWithKey f (a :< as) = f Seq.empty a :< mapWithKey (mapWithKey . fmap f . flip (|>)) as

instance Keyed Tree where
  mapWithKey f (Node a as) = Node (f Seq.empty a) (mapWithKey (mapWithKey . fmap f . flip (|>)) as)

instance Keyed U1 where
  mapWithKey _ U1 = U1

instance Keyed V1 where
  mapWithKey _ v = v `seq` undefined

instance Keyed Par1 where
  mapWithKey q = fmap (q ())

instance Keyed (K1 i c) where
  mapWithKey _ (K1 c) = K1 c

instance Keyed (Tagged a) where
  mapWithKey q (Tagged a) = Tagged (q () a)

instance Keyed Proxy where
  mapWithKey _ Proxy = Proxy

instance Keyed (Const e) where
  mapWithKey _ (Const a) = Const a

instance Keyed (Constant e) where
  mapWithKey _ (Constant a) = Constant a

instance Keyed f => Keyed (M1 i c f) where
  mapWithKey q (M1 f) = M1 (mapWithKey q f)

instance Keyed f => Keyed (Rec1 f) where
  mapWithKey q (Rec1 f) = Rec1 (mapWithKey q f)

instance (Keyed g, Keyed f) => Keyed (f :*: g) where
  mapWithKey q (fa :*: ga) = mapWithKey (q . Left) fa :*: mapWithKey (q . Right) ga

instance (Keyed g, Keyed f) => Keyed (f :+: g) where
  mapWithKey q (L1 fa) = L1 (mapWithKey (q . Left) fa)
  mapWithKey q (R1 ga) = R1 (mapWithKey (q . Right) ga)

instance (Keyed g, Keyed f) => Keyed (g :.: f) where
  mapWithKey q = inComp (mapWithKey (mapWithKey . fmap q . (,)))

#if 0
mapWithKey :: (Key (g :.: f) -> a -> b) -> (g :.: f) a -> (g :.: f) b
           :: ((Key g, Key f) -> a -> b) -> (g :.: f) a -> (g :.: f) b

mapWithKey q
  = \ (Comp1 gfa) -> Comp1 (mapWithKey (\ gk -> mapWithKey (\ fk a -> q (gk, fk) a)) gfa)
  = inComp $ mapWithKey (\ gk -> mapWithKey (\ fk a -> q (gk, fk) a))
  = inComp $ mapWithKey (\ gk -> mapWithKey (\ fk -> q (gk, fk)))
  = inComp $ mapWithKey (\ gk -> mapWithKey (q . (gk,)))
  = inComp $ mapWithKey (\ gk -> mapWithKey . (q .) $ (gk,))
  = inComp $ mapWithKey (\ gk -> mapWithKey . (q .) $ (,) gk)
  = inComp (mapWithKey (mapWithKey . fmap q . (,)))

q   :: ((Key g, Key f) -> a -> b)
gfa :: g (f a)
gk  :: Key g
fk  :: Key f
#endif

-- |
--
-- Laws:
--
-- @
-- 'fmap' 'fst' ('zip' u u) = u
-- 'fmap' 'snd' ('zip' u u) = u
-- 'zip' ('fmap' 'fst' u) ('fmap' 'snd' u) = u
-- 'zip' ('flip' (,)) x y = 'zip' y x
-- @
class Functor f => Zip f where
  zipWith :: (a -> b -> c) -> f a -> f b -> f c
  zipWith f a b = uncurry f <$> zip a b

  zip :: f a -> f b -> f (a, b)
  zip = zipWith (,)

  -- zip-like 'ap'
  zap :: f (a -> b) -> f a -> f b
  zap = zipWith id

#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL zipWith | zip #-}
#endif

instance Zip f => Zip (Cofree f) where
  zipWith f (a :< as) (b :< bs) = f a b :< zipWith (zipWith f) as bs

instance Zip Tree where
  zipWith f (Node a as) (Node b bs) = Node (f a b) (zipWith (zipWith f) as bs)

instance Zip Proxy where
  zipWith = liftA2

instance Zip (Tagged a) where
  zipWith = liftA2

instance Zip U1 where
  zipWith = liftA2

instance Zip V1 where
  zipWith _ v = v `seq` undefined

instance Zip Par1 where
  zipWith = liftA2

instance (Zip f, Zip g) => Zip (f :*: g) where
  zipWith h (fa :*: ga) (fa' :*: ga') =
    zipWith h fa fa' :*: zipWith h ga ga'

instance (Zip f, Zip g) => Zip (g :.: f) where
  zipWith = inComp2 . zipWith . zipWith

instance Zip f => Zip (Rec1 f) where
  zipWith f (Rec1 a) (Rec1 b) = Rec1 (zipWith f a b)

instance Zip f => Zip (M1 i c f) where
  zipWith f (M1 a) (M1 b) = M1 (zipWith f a b)

-- | Add post- and pre-processing
(<--) :: (b -> b') -> (a' -> a) -> ((a -> b) -> (a' -> b'))
(h <-- f) g = h . g . f

-- | Apply a unary function within the 'Comp1' constructor.
inComp :: (g (f a) -> g' (f' a')) -> ((g :.: f) a -> (g' :.: f') a')
inComp = Comp1 <-- unComp1

-- | Apply a binary function within the 'Comp1' constructor.
inComp2 :: (  g (f a)   -> g' (f' a')     -> g'' (f'' a''))
        -> ((g :.: f) a -> (g' :.: f') a' -> (g'' :.: f'') a'')
inComp2 = inComp <-- unComp1

class (Keyed f, Zip f) => ZipWithKey f where
  zipWithKey :: (Key f -> a -> b -> c) -> f a -> f b -> f c
  zipWithKey f = zap . mapWithKey f

  zapWithKey :: f (Key f -> a -> b) -> f a -> f b
  zapWithKey = zipWithKey (\k f -> f k)

instance ZipWithKey f => ZipWithKey (Cofree f) where
  zipWithKey f (a :< as) (b :< bs) = f Seq.empty a b :< zipWithKey (zipWithKey . fmap f . flip (|>)) as bs

instance ZipWithKey Tree where
  zipWithKey f (Node a as) (Node b bs) = f Seq.empty a b `Node` zipWithKey (zipWithKey . fmap f . flip (|>)) as bs

instance ZipWithKey (Tagged a) where
  zipWithKey f = zipWith  (f ())

instance ZipWithKey Proxy where
  zipWithKey _ _ _ = Proxy

instance ZipWithKey U1 where
  zipWithKey _ _ _ = U1

instance ZipWithKey V1 where
  zipWithKey _ u v = u `seq` v `seq` undefined

instance ZipWithKey Par1 where
  zipWithKey f (Par1 a) (Par1 b) = Par1 (f () a b)

instance ZipWithKey f => ZipWithKey (Rec1 f) where
  zipWithKey f (Rec1 a) (Rec1 b) = Rec1 (zipWithKey f a b)

instance ZipWithKey f => ZipWithKey (M1 i c f) where
  zipWithKey f (M1 a) (M1 b) = M1 (zipWithKey f a b)

instance (ZipWithKey f, ZipWithKey g) => ZipWithKey (f :*: g) where
  zipWithKey f (as :*: bs) (cs :*: ds) = zipWithKey (f . Left) as cs :*: zipWithKey (f . Right) bs ds

instance (ZipWithKey f, ZipWithKey g) => ZipWithKey (g :.: f) where
  zipWithKey f (Comp1 xs) (Comp1 ys) = Comp1 $ zipWithKey (\a -> zipWithKey (\b -> f (a,b))) xs ys

infixl 4 <#$>

(<#$>) :: Keyed f => (Key f -> a -> b) -> f a -> f b
(<#$>) = mapWithKey
{-# INLINE (<#$>) #-}

keyed :: Keyed f => f a -> f (Key f, a)
keyed = mapWithKey (,)
{-# INLINE keyed #-}

-- * Indexable

class Lookup f => Indexable f where
  index :: f a -> Key f -> a

instance Indexable f => Indexable (Cofree f) where
  index (a :< as) key = case viewl key of
      EmptyL -> a
      k Seq.:< ks -> index (index as k) ks

instance Indexable (Tagged a) where
  index (Tagged a) () = a

instance Indexable Proxy where
  index Proxy = absurd

instance Indexable (Const e) where
  index _ = absurd

instance Indexable (Constant e) where
  index _ = absurd

instance Indexable Tree where
  index (Node a as) key = case viewl key of
      EmptyL -> a
      k Seq.:< ks -> index (index as k) ks

instance Indexable U1 where
  index U1 = absurd

instance Indexable Par1 where
  index (Par1 a) () = a

instance Indexable f => Indexable (Rec1 f) where
  index (Rec1 f) a = index f a

instance Indexable f => Indexable (M1 i c f) where
  index (M1 f) a = index f a

instance Indexable (K1 i c) where
  index _ = absurd

instance (Indexable g, Indexable f) =>
         Indexable (f :*: g) where
  index (fa :*: _) (Left  fk) = fa ! fk
  index (_ :*: ga) (Right gk) = ga ! gk

instance (Indexable g, Indexable f) =>
         Indexable (g :.: f) where
  index (Comp1 gfa) (gk,fk) = gfa ! gk ! fk

(!) :: Indexable f => f a -> Key f -> a
(!) = index

-- * Lookup

class Lookup f where
  lookup :: Key f -> f a -> Maybe a

instance Lookup f => Lookup (Cofree f) where
  lookup key (a :< as) = case viewl key of
    EmptyL -> Just a
    k Seq.:< ks -> lookup k as >>= lookup ks

instance Lookup (Tagged a) where
  lookup () (Tagged a) = Just a

instance Lookup Proxy where
  lookup _ _ = Nothing

instance Lookup (Const e) where
  lookup _ _ = Nothing

instance Lookup (Constant e) where
  lookup _ _ = Nothing

instance Lookup Tree where
  lookup key (Node a as) = case viewl key of
    EmptyL -> Just a
    k Seq.:< ks -> lookup k as >>= lookup ks

instance Lookup f => Lookup (Free f) where
  lookup key (Pure a)
    | Seq.null key = Just a
    | otherwise = Nothing
  lookup key (Free as) = case viewl key of
    k Seq.:< ks -> lookup k as >>= lookup ks
    _ -> Nothing

instance Lookup U1 where
  lookup _ _ = Nothing

instance Lookup Par1 where
  lookup = lookupDefault

instance Lookup f => Lookup (Rec1 f) where
  lookup k (Rec1 f) = lookup k f

instance Lookup f => Lookup (M1 i c f) where
  lookup k (M1 f) = lookup k f

instance Lookup (K1 i c) where
  lookup _ _ = Nothing

instance (Indexable g, Indexable f) => Lookup (f :*: g) where
  lookup = lookupDefault

instance (Indexable g, Indexable f) => Lookup (g :.: f) where
  lookup = lookupDefault

lookupDefault :: Indexable f => Key f -> f a -> Maybe a
lookupDefault k t = Just (index t k)

-- * Adjustable

class Functor f => Adjustable f where
  adjust :: (a -> a) -> Key f -> f a -> f a

  replace :: Key f -> a -> f a -> f a
  replace k v = adjust (const v) k

instance Adjustable f => Adjustable (Free f) where
  adjust f key as@(Pure a)
    | Seq.null key = Pure $ f a
    | otherwise = as
  adjust f key aas@(Free as) = case viewl key of
    k Seq.:< ks -> Free $ adjust (adjust f ks) k as
    _           -> aas

instance Adjustable f => Adjustable (Cofree f) where
  adjust f key (a :< as) = case viewl key of
    k Seq.:< ks -> a   :< adjust (adjust f ks) k as
    _           -> f a :< as

instance Adjustable Tree where
  adjust f key (Node a as) = case viewl key of
    k Seq.:< ks -> a   `Node` adjust (adjust f ks) k as
    _           -> f a `Node` as

instance Adjustable (Tagged a) where
  adjust f _ (Tagged a) = Tagged (f a)
  replace _ a _ = Tagged a

instance Adjustable Proxy where
  adjust _ _ _ = Proxy
  replace _ _ _ = Proxy

instance Adjustable (Const e) where
  adjust _ _ x = x
  replace _ _ x = x

instance Adjustable (Constant e) where
  adjust _ _ x = x
  replace _ _ x = x

instance Adjustable U1 where
  adjust _ _ _ = U1
  replace _ _ _ = U1

instance Adjustable Par1 where
  adjust h () = fmap h
  replace _ a _ = Par1 a

instance Adjustable f => Adjustable (Rec1 f) where
  adjust f k (Rec1 a) = Rec1 (adjust f k a)
  replace k a (Rec1 b) = Rec1 (replace k a b)

instance Adjustable f => Adjustable (M1 i c f) where
  adjust f k (M1 a) = M1 (adjust f k a)
  replace k a (M1 b) = M1 (replace k a b)

instance Adjustable (K1 i c) where
  adjust _ _ x = x
  replace _ _ x = x

instance (Adjustable f, Adjustable g) => Adjustable (f :+: g) where
  adjust h (Left a) (L1 fa) = L1 (adjust h a fa)
  adjust h (Right b) (R1 fb) = R1 (adjust h b fb)
  adjust _ _ x = x
  replace (Left a) v (L1 fa) = L1 (replace a v fa)
  replace (Right b) v (R1 fb) = R1 (replace b v fb)
  replace _ _ x = x

instance (Adjustable f, Adjustable g) => Adjustable (f :*: g) where
  adjust h (Left  fk) (fa :*: ga) = adjust h fk fa :*: ga
  adjust h (Right gk) (fa :*: ga) = fa :*: adjust h gk ga
  replace (Left  fk) a (fa :*: ga) = replace fk a fa :*: ga
  replace (Right gk) a (fa :*: ga) = fa :*: replace gk a ga

instance (Adjustable f, Adjustable g) => Adjustable (g :.: f) where
  adjust h (gk,fk) = inComp (adjust (adjust h fk) gk)
  replace (gk,fk) a = inComp (adjust (replace fk a) gk)

-- * FoldableWithKey

class Foldable t => FoldableWithKey t where
  toKeyedList :: t a -> [(Key t, a)]
  toKeyedList = foldrWithKey (\k v t -> (k,v):t) []

  foldMapWithKey :: Monoid m => (Key t -> a -> m) -> t a -> m
  foldMapWithKey f = foldrWithKey (\k v -> mappend (f k v)) mempty

  foldrWithKey :: (Key t -> a -> b -> b) -> b -> t a -> b
  foldrWithKey f z t = appEndo (foldMapWithKey (\k v -> Endo (f k v)) t) z

  foldlWithKey :: (b -> Key t -> a -> b) -> b -> t a -> b
  foldlWithKey f z t = appEndo (getDual (foldMapWithKey (\k a -> Dual (Endo (\b -> f b k a))) t)) z

#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL foldMapWithKey | foldrWithKey #-}
#endif

instance FoldableWithKey f => FoldableWithKey (Free f) where
  foldMapWithKey f (Pure a) = f Seq.empty a
  foldMapWithKey f (Free as) = foldMapWithKey (foldMapWithKey . fmap f . flip (|>)) as

instance FoldableWithKey f => FoldableWithKey (Cofree f) where
  foldMapWithKey f (a :< as) = f Seq.empty a `mappend` foldMapWithKey (foldMapWithKey . fmap f . flip (|>)) as

instance FoldableWithKey (Tagged a) where
  foldMapWithKey f (Tagged a) = f () a

instance FoldableWithKey Proxy where
  foldMapWithKey _ _ = mempty

instance FoldableWithKey (Const e) where
  foldMapWithKey _ _ = mempty

instance FoldableWithKey (Constant e) where
  foldMapWithKey _ _ = mempty

instance FoldableWithKey Tree where
  foldMapWithKey f (Node a as) = f Seq.empty a `mappend` foldMapWithKey (foldMapWithKey . fmap f . flip (|>)) as

instance FoldableWithKey Par1 where
  foldMapWithKey f (Par1 a) = f () a

instance (FoldableWithKey f, FoldableWithKey g) => FoldableWithKey (f :*: g) where
  foldMapWithKey f (a :*: b) = foldMapWithKey (f . Left) a `mappend` foldMapWithKey (f . Right) b

instance (FoldableWithKey f, FoldableWithKey g) => FoldableWithKey (f :+: g) where
  foldMapWithKey f (L1 a) = foldMapWithKey (f . Left) a
  foldMapWithKey f (R1 a) = foldMapWithKey (f . Right) a

instance FoldableWithKey U1 where
  foldMapWithKey _ _ = mempty

instance FoldableWithKey V1 where
  foldMapWithKey _ v = v `seq` undefined

instance FoldableWithKey (K1 i c) where
  foldMapWithKey _ _ = mempty

instance FoldableWithKey f => FoldableWithKey (M1 i c f) where
  foldMapWithKey f (M1 a) = foldMapWithKey f a

instance FoldableWithKey f => FoldableWithKey (Rec1 f) where
  foldMapWithKey f (Rec1 a) = foldMapWithKey f a

foldrWithKey' :: FoldableWithKey t => (Key t -> a -> b -> b) -> b -> t a -> b
foldrWithKey' f z0 xs = foldlWithKey f' id xs z0
  where f' k key x z = k $! f key x z
{-# INLINE foldrWithKey' #-}

foldlWithKey' :: FoldableWithKey t => (b -> Key t -> a -> b) -> b -> t a -> b
foldlWithKey' f z0 xs = foldrWithKey f' id xs z0
  where f' key x k z = k $! f z key x
{-# INLINE foldlWithKey' #-}

foldrWithKeyM :: (FoldableWithKey t, Monad m) => (Key t -> a -> b -> m b) -> b -> t a -> m b
foldrWithKeyM f z0 xs = foldlWithKey f' return xs z0
  where f' k key x z = f key x z >>= k
{-# INLINE foldrWithKeyM #-}

foldlWithKeyM :: (FoldableWithKey t, Monad m) => (b -> Key t -> a -> m b) -> b -> t a -> m b
foldlWithKeyM f z0 xs = foldrWithKey f' return xs z0
  where f' key x k z = f z key x >>= k
{-# INLINE foldlWithKeyM #-}

traverseWithKey_ :: (FoldableWithKey t, Applicative f) => (Key t -> a -> f b) -> t a -> f ()
traverseWithKey_ f = foldrWithKey (fmap (*>) . f) (pure ())
{-# INLINE traverseWithKey_ #-}

forWithKey_ :: (FoldableWithKey t, Applicative f) => t a -> (Key t -> a -> f b) -> f ()
forWithKey_ = flip traverseWithKey_
{-# INLINE forWithKey_ #-}

mapWithKeyM_ :: (FoldableWithKey t, Monad m) => (Key t -> a -> m b) -> t a -> m ()
mapWithKeyM_ f = foldrWithKey (fmap (>>) . f) (return ())
{-# INLINE mapWithKeyM_ #-}

forWithKeyM_ :: (FoldableWithKey t, Monad m) => t a -> (Key t -> a -> m b) -> m ()
forWithKeyM_ = flip mapWithKeyM_
{-# INLINE forWithKeyM_ #-}

concatMapWithKey :: FoldableWithKey t => (Key t -> a -> [b]) -> t a -> [b]
concatMapWithKey = foldMapWithKey
{-# INLINE concatMapWithKey #-}

anyWithKey :: FoldableWithKey t => (Key t -> a -> Bool) -> t a -> Bool
anyWithKey p = getAny . foldMapWithKey (fmap Any . p)
{-# INLINE anyWithKey #-}

allWithKey :: FoldableWithKey t => (Key t -> a -> Bool) -> t a -> Bool
allWithKey p = getAll . foldMapWithKey (fmap All . p)
{-# INLINE allWithKey #-}

findWithKey :: FoldableWithKey t => (Key t -> a -> Bool) -> t a -> Maybe a
findWithKey p = Monoid.getFirst . foldMapWithKey (\k x -> Monoid.First (if p k x then Just x else Nothing) )
{-# INLINE findWithKey #-}

-- * FoldableWithKey1

class (Foldable1 t, FoldableWithKey t) => FoldableWithKey1 t where
  foldMapWithKey1 :: Semigroup m => (Key t -> a -> m) -> t a -> m

  toKeyedNonEmptyList :: t a -> NonEmpty (Key t, a)
  toKeyedNonEmptyList = foldMapWithKey1 $ \k v -> pure (k,v)

-- TODO
--instance Foldable f => Foldable1 (Cofree f) where
--  foldMap1 f (a :< as) = appEndo (getDual . foldMap (Dual . diff . foldMap1 f)) (f a)

instance FoldableWithKey1 f => FoldableWithKey1 (Cofree f) where
  foldMapWithKey1 f (a :< as) = f Seq.empty a <> foldMapWithKey1 (foldMapWithKey1 . fmap f . flip (|>)) as

instance FoldableWithKey1 Tree where
  foldMapWithKey1 f (Node a []) = f Seq.empty a
  foldMapWithKey1 f (Node a (x:xs)) = f Seq.empty a <> foldMapWithKey1 (foldMapWithKey1 . fmap f . flip (|>)) (x:|xs)

instance FoldableWithKey1 f => FoldableWithKey1 (Free f) where
  foldMapWithKey1 f (Pure a) = f Seq.empty a
  foldMapWithKey1 f (Free as) = foldMapWithKey1 (foldMapWithKey1 . fmap f . flip (|>)) as

instance FoldableWithKey1 (Tagged a) where
  foldMapWithKey1 f (Tagged a) = f () a

instance (FoldableWithKey1 f, FoldableWithKey1 g) => FoldableWithKey1 (f :*: g) where
  foldMapWithKey1 f (a :*: b) = foldMapWithKey1 (f . Left) a <> foldMapWithKey1 (f . Right) b

instance (FoldableWithKey1 f, FoldableWithKey1 g) => FoldableWithKey1 (f :+: g) where
  foldMapWithKey1 f (L1 a) = foldMapWithKey1 (f . Left) a
  foldMapWithKey1 f (R1 a) = foldMapWithKey1 (f . Right) a

instance FoldableWithKey1 V1 where
  foldMapWithKey1 _ v = v `seq` undefined

instance FoldableWithKey1 Par1 where
  foldMapWithKey1 f (Par1 a) = f () a

instance FoldableWithKey1 f => FoldableWithKey1 (M1 i c f) where
  foldMapWithKey1 f (M1 a) = foldMapWithKey1 f a

instance FoldableWithKey1 f => FoldableWithKey1 (Rec1 f) where
  foldMapWithKey1 f (Rec1 a) = foldMapWithKey1 f a

newtype Act f a = Act { getAct :: f a }

instance Apply f => Semigroup (Act f a) where
  Act a <> Act b = Act (a .> b)

instance Functor f => Functor (Act f) where
  fmap f (Act a) = Act (f <$> a)
  b <$ Act a = Act (b <$ a)

traverseWithKey1_ :: (FoldableWithKey1 t, Apply f) => (Key t -> a -> f b) -> t a -> f ()
traverseWithKey1_ f = (<$) () . getAct . foldMapWithKey1 (fmap Act . f)
{-# INLINE traverseWithKey1_ #-}

forWithKey1_ :: (FoldableWithKey1 t, Apply f) => t a -> (Key t -> a -> f b) -> f ()
forWithKey1_ = flip traverseWithKey1_
{-# INLINE forWithKey1_ #-}

foldMapWithKeyDefault1 :: (FoldableWithKey1 t, Monoid m) => (Key t -> a -> m) -> t a -> m
foldMapWithKeyDefault1 f = unwrapMonoid . foldMapWithKey (fmap WrapMonoid . f)
{-# INLINE foldMapWithKeyDefault1 #-}

-- * TraversableWithKey

class (Keyed t, FoldableWithKey t, Traversable t) => TraversableWithKey t where
  traverseWithKey :: Applicative f => (Key t -> a -> f b) -> t a -> f (t b)

  mapWithKeyM :: Monad m => (Key t -> a -> m b) -> t a -> m (t b)
  mapWithKeyM f = unwrapMonad . traverseWithKey (fmap WrapMonad . f)

instance TraversableWithKey (Tagged a) where
  traverseWithKey f (Tagged a) = Tagged <$> f () a

instance TraversableWithKey Proxy where
  traverseWithKey _ _ = pure Proxy

instance TraversableWithKey (Const e) where
  traverseWithKey _ (Const a) = pure (Const a)

instance TraversableWithKey (Constant e) where
  traverseWithKey _ (Constant a) = pure (Constant a)

instance TraversableWithKey f => TraversableWithKey (Cofree f) where
  traverseWithKey f (a :< as) = (:<) <$> f Seq.empty a <*> traverseWithKey (traverseWithKey . fmap f . flip (|>)) as

instance TraversableWithKey Tree where
  traverseWithKey f (Node a as) = Node <$> f Seq.empty a <*> traverseWithKey (traverseWithKey . fmap f . flip (|>)) as

instance TraversableWithKey f => TraversableWithKey (Free f) where
  traverseWithKey f (Pure a) = Pure <$> f Seq.empty a
  traverseWithKey f (Free as) = Free <$> traverseWithKey (traverseWithKey . fmap f . flip (|>)) as

instance (TraversableWithKey f, TraversableWithKey g) => TraversableWithKey (f :*: g) where
  traverseWithKey f (a :*: b) = (:*:) <$> traverseWithKey (f . Left) a <*> traverseWithKey (f . Right) b

instance (TraversableWithKey f, TraversableWithKey g) => TraversableWithKey (f :+: g) where
  traverseWithKey f (L1 as) = L1 <$> traverseWithKey (f . Left) as
  traverseWithKey f (R1 bs) = R1 <$> traverseWithKey (f . Right) bs

instance TraversableWithKey Par1 where
  traverseWithKey f (Par1 a) = Par1 <$> f () a

instance TraversableWithKey U1 where
  traverseWithKey _ U1 = pure U1

instance TraversableWithKey V1 where
  traverseWithKey _ v = v `seq` undefined

instance TraversableWithKey (K1 i c) where
  traverseWithKey _ (K1 p) = pure (K1 p)

instance TraversableWithKey f => TraversableWithKey (Rec1 f) where
  traverseWithKey f (Rec1 a) = Rec1 <$> traverseWithKey f a

instance TraversableWithKey f => TraversableWithKey (M1 i c f) where
  traverseWithKey f (M1 a) = M1 <$> traverseWithKey f a

forWithKey :: (TraversableWithKey t, Applicative f) => t a -> (Key t -> a -> f b) -> f (t b)
forWithKey = flip traverseWithKey
{-# INLINE forWithKey #-}

forWithKeyM :: (TraversableWithKey t, Monad m) => t a -> (Key t -> a -> m b) -> m (t b)
forWithKeyM = flip mapWithKeyM
{-# INLINE forWithKeyM #-}

-- left-to-right state transformer
newtype StateL s a = StateL { runStateL :: s -> (s, a) }

instance Functor (StateL s) where
  fmap f (StateL k) = StateL $ \ s ->
    let (s', v) = k s in (s', f v)

instance Applicative (StateL s) where
  pure x = StateL (\ s -> (s, x))
  StateL kf <*> StateL kv = StateL $ \ s ->
    let (s', f) = kf s
        (s'', v) = kv s'
    in  (s'', f v)

-- |The 'mapAccumWithKeyL' function behaves like a combination of 'mapWithKey'
-- and 'foldlWithKey'; it applies a function to each element of a structure,
-- passing an accumulating parameter from left to right, and returning
-- a final value of this accumulator together with the new structure.
mapAccumWithKeyL :: TraversableWithKey t => (Key t -> a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumWithKeyL f s t = runStateL (traverseWithKey (\k b -> StateL (\a -> f k a b)) t) s
{-# INLINE mapAccumWithKeyL #-}

-- right-to-left state transformer
newtype StateR s a = StateR { runStateR :: s -> (s, a) }

instance Functor (StateR s) where
  fmap f (StateR k) = StateR $ \ s ->
    let (s', v) = k s in (s', f v)

instance Applicative (StateR s) where
  pure x = StateR (\ s -> (s, x))
  StateR kf <*> StateR kv = StateR $ \ s ->
    let (s', v) = kv s
        (s'', f) = kf s'
    in (s'', f v)

-- |The 'mapAccumWithKeyR' function behaves like a combination of 'mapWithKey'
-- and 'foldrWithKey'; it applies a function to each element of a structure,
-- passing an accumulating parameter from right to left, and returning
-- a final value of this accumulator together with the new structure.
mapAccumWithKeyR :: TraversableWithKey t => (Key t -> a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumWithKeyR f s t = runStateR (traverseWithKey (\k b -> StateR (\a -> f k a b)) t) s
{-# INLINE mapAccumWithKeyR #-}

mapWithKeyDefault :: TraversableWithKey t => (Key t -> a -> b) -> t a -> t b
mapWithKeyDefault f = runIdentity . traverseWithKey (fmap Identity . f)
{-# INLINE mapWithKeyDefault #-}

-- | This function may be used as a value for `Data.Foldable.foldMapWithKey`
-- in a `FoldableWithKey` instance.
foldMapWithKeyDefault :: (TraversableWithKey t, Monoid m) => (Key t -> a -> m) -> t a -> m
foldMapWithKeyDefault f = getConst . traverseWithKey (fmap Const . f)
{-# INLINE foldMapWithKeyDefault #-}

-- * TraversableWithKey1
class (Traversable1 t, FoldableWithKey1 t, TraversableWithKey t) => TraversableWithKey1 t where
  traverseWithKey1 :: Apply f => (Key t -> a -> f b) -> t a -> f (t b)

instance TraversableWithKey1 (Tagged a) where
  traverseWithKey1 f (Tagged a) = Tagged <$> f () a

-- instance TraversableWithKey f => TraversableWithKey1 (Cofree f) where
instance TraversableWithKey1 f => TraversableWithKey1 (Cofree f) where
  traverseWithKey1 f (a :< as) = (:<) <$> f Seq.empty a <.> traverseWithKey1 (traverseWithKey1 . fmap f . flip (|>)) as

instance TraversableWithKey1 Tree where
  traverseWithKey1 f (Node a []) = (`Node`[]) <$> f Seq.empty a
  traverseWithKey1 f (Node a (x:xs)) = (\b (y:|ys) -> Node b (y:ys)) <$> f Seq.empty a <.> traverseWithKey1 (traverseWithKey1 . fmap f . flip (|>)) (x:|xs)

instance TraversableWithKey1 f => TraversableWithKey1 (Free f) where
  traverseWithKey1 f (Pure a) = Pure <$> f Seq.empty a
  traverseWithKey1 f (Free as) = Free <$> traverseWithKey1 (traverseWithKey1 . fmap f . flip (|>)) as

instance TraversableWithKey1 Par1 where
  traverseWithKey1 f (Par1 a) = Par1 <$> f () a

instance TraversableWithKey1 f => TraversableWithKey1 (Rec1 f) where
  traverseWithKey1 f (Rec1 a) = Rec1 <$> traverseWithKey1 f a

instance TraversableWithKey1 f => TraversableWithKey1 (M1 i c f) where
  traverseWithKey1 f (M1 a) = M1 <$> traverseWithKey1 f a

instance TraversableWithKey1 V1 where
  traverseWithKey1 _ v = v `seq` undefined

instance (TraversableWithKey1 f, TraversableWithKey1 g) => TraversableWithKey1 (f :*: g) where
  traverseWithKey1 f (a :*: b) = (:*:) <$> traverseWithKey1 (f . Left) a <.> traverseWithKey1 (f . Right) b

instance (TraversableWithKey1 f, TraversableWithKey1 g) => TraversableWithKey1 (f :+: g) where
  traverseWithKey1 f (L1 as) = L1 <$> traverseWithKey1 (f . Left) as
  traverseWithKey1 f (R1 bs) = R1 <$> traverseWithKey1 (f . Right) bs

foldMapWithKey1Default :: (TraversableWithKey1 t, Semigroup m) => (Key t -> a -> m) -> t a -> m
foldMapWithKey1Default f = getConst . traverseWithKey1 (\k -> Const . f k)
{-# INLINE foldMapWithKey1Default #-}

-- * Instances

type instance Key Identity = ()

instance Indexable Identity where
  index (Identity a) _ = a

instance Lookup Identity where
  lookup _ (Identity a) = Just a

instance Adjustable Identity where
  adjust f _ (Identity a) = Identity (f a)
  replace _ b _ = Identity b

instance Zip Identity where
  zipWith f (Identity a) (Identity b) = Identity (f a b)

instance ZipWithKey Identity where
  zipWithKey f (Identity a) (Identity b) = Identity (f () a b)

instance Keyed Identity where
  mapWithKey f = Identity . f () . runIdentity

instance FoldableWithKey Identity where
  foldrWithKey f z (Identity a) = f () a z

instance FoldableWithKey1 Identity where
  foldMapWithKey1 f (Identity a) = f () a

instance TraversableWithKey Identity where
  traverseWithKey f (Identity a) = Identity <$> f () a

instance TraversableWithKey1 Identity where
  traverseWithKey1 f (Identity a) = Identity <$> f () a

type instance Key (IdentityT m) = Key m

instance Indexable m => Indexable (IdentityT m) where
  index (IdentityT m) i = index m i

instance Lookup m => Lookup (IdentityT m) where
  lookup i (IdentityT m) = lookup i m

instance Zip m => Zip (IdentityT m) where
  zipWith f (IdentityT m) (IdentityT n) = IdentityT (zipWith f m n)

instance ZipWithKey m => ZipWithKey (IdentityT m) where
  zipWithKey f (IdentityT m) (IdentityT n) = IdentityT (zipWithKey f m n)

instance Keyed m => Keyed (IdentityT m) where
  mapWithKey f = IdentityT . mapWithKey f . runIdentityT

instance FoldableWithKey m => FoldableWithKey (IdentityT m) where
  foldrWithKey f z (IdentityT m) = foldrWithKey f z m

instance FoldableWithKey1 m => FoldableWithKey1 (IdentityT m) where
  foldMapWithKey1 f (IdentityT m) = foldMapWithKey1 f m

instance TraversableWithKey m => TraversableWithKey (IdentityT m) where
  traverseWithKey f (IdentityT a) = IdentityT <$> traverseWithKey f a

instance TraversableWithKey1 m => TraversableWithKey1 (IdentityT m) where
  traverseWithKey1 f (IdentityT a) = IdentityT <$> traverseWithKey1 f a

type instance Key ((->)a) = a

instance Keyed ((->)a) where
  mapWithKey = (<*>)

instance Zip ((->)a) where
  zipWith f g h a = f (g a) (h a)

instance ZipWithKey ((->)a) where
  zipWithKey f g h a = f a (g a) (h a)

instance Indexable ((->)a) where
  index = id

instance Lookup ((->)a) where
  lookup i f = Just (f i)

type instance Key (ReaderT e m) = (e, Key m)

instance Zip m => Zip (ReaderT e m) where
  zipWith f (ReaderT m) (ReaderT n) = ReaderT $ \a ->
    zipWith f (m a) (n a)

instance ZipWithKey m => ZipWithKey (ReaderT e m) where
  zipWithKey f (ReaderT m) (ReaderT n) = ReaderT $ \a ->
    zipWithKey (f . (,) a) (m a) (n a)

instance Keyed m => Keyed (ReaderT e m) where
  mapWithKey f (ReaderT m) = ReaderT $ \k -> mapWithKey (f . (,) k) (m k)

instance Indexable m => Indexable (ReaderT e m) where
  index (ReaderT f) (e,k) = index (f e) k

instance Lookup m => Lookup (ReaderT e m) where
  lookup (e,k) (ReaderT f) = lookup k (f e)

type instance Key (TracedT s w) = (s, Key w)

instance Zip w => Zip (TracedT s w) where
  zipWith f (TracedT u) (TracedT v) = TracedT $
    zipWith (\a b s -> f (a s) (b s)) u v

instance ZipWithKey w => ZipWithKey (TracedT s w) where
  zipWithKey f (TracedT u) (TracedT v) = TracedT $
    zipWithKey (\k a b s -> f (s, k) (a s) (b s)) u v

instance Keyed w => Keyed (TracedT s w) where
  mapWithKey f = TracedT . mapWithKey (\k' g k -> f (k, k') (g k)) . runTracedT

instance Indexable w => Indexable (TracedT s w) where
  index (TracedT w) (e,k) = index w k e

instance Lookup w => Lookup (TracedT s w) where
  lookup (e,k) (TracedT w) = ($ e) <$> lookup k w

type instance Key IntMap = Int

instance Zip IntMap where
  zipWith = IntMap.intersectionWith

instance ZipWithKey IntMap where
  zipWithKey = IntMap.intersectionWithKey

instance Keyed IntMap where
  mapWithKey = IntMap.mapWithKey

instance FoldableWithKey IntMap where
#if MIN_VERSION_containers(0,5,0)
  foldrWithKey = IntMap.foldrWithKey
#else
  foldrWithKey = IntMap.foldWithKey
#endif

instance TraversableWithKey IntMap where
  traverseWithKey f = fmap IntMap.fromDistinctAscList . traverse (\(k, v) -> (,) k <$> f k v) . IntMap.toAscList

instance Indexable IntMap where
  index = (IntMap.!)

instance Lookup IntMap where
  lookup = IntMap.lookup

instance Adjustable IntMap where
  adjust = IntMap.adjust

type instance Key (Compose f g) = (Key f, Key g)

instance (Zip f, Zip g) => Zip (Compose f g) where
  zipWith f (Compose a) (Compose b) = Compose $ zipWith (zipWith f) a b

instance (ZipWithKey f, ZipWithKey g) => ZipWithKey (Compose f g) where
  zipWithKey f (Compose a) (Compose b) = Compose $
    zipWithKey (zipWithKey . fmap f . (,)) a b

instance (Keyed f, Keyed g) => Keyed (Compose f g) where
  mapWithKey f = Compose . mapWithKey (\k -> mapWithKey (f . (,) k)) . getCompose

instance (Indexable f, Indexable g) => Indexable (Compose f g) where
  index (Compose fg) (i,j) = index (index fg i) j

instance (Lookup f, Lookup g) => Lookup (Compose f g) where
  lookup (i,j) (Compose fg) = lookup i fg >>= lookup j

instance (FoldableWithKey f, FoldableWithKey m) => FoldableWithKey (Compose f m) where
  foldMapWithKey f = foldMapWithKey (\k -> foldMapWithKey (f . (,) k)) . getCompose

instance (FoldableWithKey1 f, FoldableWithKey1 m) => FoldableWithKey1 (Compose f m) where
  foldMapWithKey1 f = foldMapWithKey1 (\k -> foldMapWithKey1 (f . (,) k)) . getCompose

instance (TraversableWithKey f, TraversableWithKey m) => TraversableWithKey (Compose f m) where
  traverseWithKey f = fmap Compose . traverseWithKey (\k -> traverseWithKey (f . (,) k)) . getCompose

instance (TraversableWithKey1 f, TraversableWithKey1 m) => TraversableWithKey1 (Compose f m) where
  traverseWithKey1 f = fmap Compose . traverseWithKey1 (\k -> traverseWithKey1 (f . (,) k)) . getCompose

type instance Key [] = Int

instance Zip [] where
  zip = List.zip
  zipWith = List.zipWith

instance ZipWithKey [] where
  zipWithKey f = go 0 where
    go _ [] _ = []
    go _ _ [] = []
    go n (x:xs) (y:ys) = n' `seq` f n x y : go n' xs ys
      where n' = n + 1

instance Keyed [] where
  mapWithKey f xs0 = go xs0 0 where
    go [] _ = []
    go (x:xs) n = f n x : (go xs $! (n + 1))

instance FoldableWithKey [] where
  foldrWithKey f z0 xs0 = go z0 xs0 0 where
    go z [] _ = z
    go z (x:xs) n = f n x (go z xs $! (n + 1))

instance TraversableWithKey [] where
  traverseWithKey f xs0 = go xs0 0 where
    go [] _ = pure []
    go (x:xs) n = (:) <$> f n x <*> (go xs $! (n + 1))

instance Indexable [] where
  index = (!!)

instance Lookup [] where
  lookup = fmap listToMaybe . drop

instance Adjustable [] where
  adjust f 0 (x:xs) = f x : xs
  adjust _ _ [] = []
  adjust f n (x:xs) = n' `seq` x : adjust f n' xs where n' = n - 1

type instance Key ZipList = Int

instance Zip ZipList where
  zip       (ZipList xs) (ZipList ys) = ZipList (zip xs ys)
  zipWith f (ZipList xs) (ZipList ys) = ZipList (zipWith f xs ys)

instance ZipWithKey ZipList where
  zipWithKey f (ZipList xs) (ZipList ys) = ZipList (zipWithKey f xs ys)

instance Keyed ZipList where
  mapWithKey f = ZipList . mapWithKey f . getZipList

instance FoldableWithKey ZipList where
  foldrWithKey f z = foldrWithKey f z . getZipList

instance TraversableWithKey ZipList where
  traverseWithKey f = fmap ZipList . traverseWithKey f . getZipList

instance Indexable ZipList where
  index (ZipList xs) i = index xs i

instance Lookup ZipList where
  lookup i = lookup i . getZipList

instance Adjustable ZipList where
  adjust f i = ZipList . adjust f i . getZipList

instance Zip NonEmpty where
  zipWith = NonEmpty.zipWith

instance ZipWithKey NonEmpty where
  zipWithKey f (a:|as) (b:|bs) = f 0 a b :| zipWithKey (f . (+1)) as bs

instance Keyed NonEmpty where
  mapWithKey f (a:|as) = f 0 a :| mapWithKey (f . (+1)) as

instance FoldableWithKey NonEmpty where
  foldrWithKey f z (x:|xs) = f 0 x (foldrWithKey (f . (+1)) z xs)

instance TraversableWithKey NonEmpty where
  traverseWithKey f (x :| xs) = (:|) <$> f 0 x <*> traverseWithKey (f . (+1)) xs

instance Indexable NonEmpty where
  index (x:|_) 0 = x
  index (_:|xs) i = xs !! (i - 1)

instance Lookup NonEmpty where
  lookup 0 (x:|_) = Just x
  lookup n (_:|xs) = lookup (n - 1) xs

instance Adjustable NonEmpty where
  adjust f 0 (x:|xs) = f x :| xs
  adjust f n (x:|xs) = x :| adjust f (n - 1) xs

instance FoldableWithKey1 NonEmpty where
  foldMapWithKey1 f (x:|[]) = f 0 x
  foldMapWithKey1 f (x:|(y:ys)) = f 0 x <> foldMapWithKey1 (f . (+1)) (y:|ys) -- TODO optimize

  toKeyedNonEmptyList = keyed

instance TraversableWithKey1 NonEmpty where
  traverseWithKey1 f (x:|[]) = (:|[]) <$> f 0 x
  traverseWithKey1 f (x:|(y:ys)) = (\w (z:|zs) -> w :| (z:zs)) <$> f 0 x <.> traverseWithKey1 (f . (+1)) (y :| ys)

type instance Key Seq = Int

instance Indexable Seq where
  index = Seq.index

instance Lookup Seq where
  lookup i s =
#if MIN_VERSION_containers(0,5,8)
    Seq.lookup i s
#else
    case viewl (Seq.drop i s) of
      EmptyL -> Nothing
      a Seq.:< _ -> Just a
#endif

instance Zip Seq where
  zip = Seq.zip
  zipWith = Seq.zipWith

instance ZipWithKey Seq where
  zipWithKey f a b = Seq.zipWith id (Seq.mapWithIndex f a) b

instance Adjustable Seq where

  adjust f i xs =
#if MIN_VERSION_containers(0,5,8)
    Seq.adjust' f i xs -- Use the prefered strict version when available
#else
    -- Otherwise use a custom adjustment in place of the inefficient Seq.adjust
    case i `lookup` xs of
      Nothing -> xs
      Just x  -> let !x' = f x
                 in  Seq.update i x' xs
#endif

instance Keyed Seq where
  mapWithKey = Seq.mapWithIndex

instance FoldableWithKey Seq where
  foldrWithKey = Seq.foldrWithIndex
  foldlWithKey = Seq.foldlWithIndex
#if MIN_VERSION_containers(0,5,8)
  foldMapWithKey = Seq.foldMapWithIndex
#endif

instance TraversableWithKey Seq where
  traverseWithKey f =
#if MIN_VERSION_containers(0,5,8)
    Seq.traverseWithIndex f
#else
    fmap Seq.fromList . traverseWithKey f . toList
#endif

type instance Key (Map k) = k

instance Ord k => Zip (Map k) where
  zipWith = Map.intersectionWith

instance Ord k => ZipWithKey (Map k) where
  zipWithKey = Map.intersectionWithKey

instance Keyed (Map k) where
  mapWithKey = Map.mapWithKey

instance Ord k => Indexable (Map k) where
  index = (Map.!)

instance Ord k => Lookup (Map k) where
  lookup = Map.lookup

instance FoldableWithKey (Map k) where
  foldrWithKey = Map.foldrWithKey

instance TraversableWithKey (Map k) where
  traverseWithKey f = fmap Map.fromDistinctAscList . traverse (\(k, v) -> (,) k <$> f k v) . Map.toAscList

instance Ord k => Adjustable (Map k) where
  adjust = Map.adjust

type instance Key (Array i) = i

instance Ix i => Keyed (Array i) where
  mapWithKey f arr = Array.listArray (Array.bounds arr) $ map (uncurry f) $ Array.assocs arr

-- a pleasant fiction
instance Ix i => Indexable (Array i) where
  index = (Array.!)

instance Ix i => Lookup (Array i) where
  lookup i arr
    | inRange (Array.bounds arr) i = Just (arr Array.! i)
    | otherwise = Nothing

instance Ix i => FoldableWithKey (Array i) where
  foldrWithKey f z = Prelude.foldr (uncurry f) z . Array.assocs

instance Ix i => TraversableWithKey (Array i) where
  traverseWithKey f arr = Array.listArray (Array.bounds arr) <$> traverse (uncurry f) (Array.assocs arr)

instance Ix i => Adjustable (Array i) where
  adjust f i arr  = arr Array.// [(i, f (arr Array.! i))]
  replace i b arr = arr Array.// [(i, b)]

type instance Key (Functor.Sum f g) = Either (Key f) (Key g)

instance (Keyed f, Keyed g) => Keyed (Functor.Sum f g) where
  mapWithKey f (Functor.InL a) = Functor.InL (mapWithKey (f . Left)  a)
  mapWithKey f (Functor.InR b) = Functor.InR (mapWithKey (f . Right) b)

instance (Indexable f, Indexable g) => Indexable (Functor.Sum f g) where
  index (Functor.InL a) (Left  x) = index a x
  index (Functor.InL _) (Right _) = error "InL indexed with a Right key"
  index (Functor.InR b) (Right y) = index b y
  index (Functor.InR _) (Left  _) = error "InR indexed with a Left key"

instance (Lookup f, Lookup g) => Lookup (Functor.Sum f g) where
  lookup (Left  x) (Functor.InL a) = lookup x a
  lookup (Right y) (Functor.InR b) = lookup y b
  lookup _         _               = Nothing

instance (Adjustable f, Adjustable g) => Adjustable (Functor.Sum f g) where
  adjust f (Left  x) (Functor.InL a) = Functor.InL (adjust f x a)
  adjust f (Right y) (Functor.InR b) = Functor.InR (adjust f y b)
  adjust _ _         x               = x

  replace (Left  x) v (Functor.InL a) = Functor.InL (replace x v a)
  replace (Right y) v (Functor.InR b) = Functor.InR (replace y v b)
  replace _         _ x               = x

instance (FoldableWithKey f, FoldableWithKey g) => FoldableWithKey (Functor.Sum f g) where
  foldMapWithKey f (Functor.InL a) = foldMapWithKey (f . Left)  a
  foldMapWithKey f (Functor.InR b) = foldMapWithKey (f . Right) b

instance (FoldableWithKey1 f, FoldableWithKey1 g) => FoldableWithKey1 (Functor.Sum f g) where
  foldMapWithKey1 f (Functor.InL a) = foldMapWithKey1 (f . Left)  a
  foldMapWithKey1 f (Functor.InR b) = foldMapWithKey1 (f . Right) b

instance (TraversableWithKey f, TraversableWithKey g) => TraversableWithKey (Functor.Sum f g) where
  traverseWithKey f (Functor.InL a) = Functor.InL <$> traverseWithKey (f . Left)  a
  traverseWithKey f (Functor.InR b) = Functor.InR <$> traverseWithKey (f . Right) b

instance (TraversableWithKey1 f, TraversableWithKey1 g) => TraversableWithKey1 (Functor.Sum f g) where
  traverseWithKey1 f (Functor.InL a) = Functor.InL <$> traverseWithKey1 (f . Left)  a
  traverseWithKey1 f (Functor.InR b) = Functor.InR <$> traverseWithKey1 (f . Right) b

type instance Key (Product f g) = Either (Key f) (Key g)

instance (Keyed f, Keyed g) => Keyed (Product f g) where
  mapWithKey f (Pair a b) = Pair (mapWithKey (f . Left) a) (mapWithKey (f . Right) b)

instance (Indexable f, Indexable g) => Indexable (Product f g) where
  index (Pair a _) (Left i)  = index a i
  index (Pair _ b) (Right j) = index b j

instance (Lookup f, Lookup g) => Lookup (Product f g) where
  lookup (Left i) (Pair a _) = lookup i a
  lookup (Right j) (Pair _ b) = lookup j b

instance (Zip f, Zip g) => Zip (Product f g) where
  zipWith f (Pair a b) (Pair c d) = Pair (zipWith f a c) (zipWith f b d)

instance (ZipWithKey f, ZipWithKey g) => ZipWithKey (Product f g) where
  zipWithKey f (Pair a b) (Pair c d) = Pair (zipWithKey (f . Left) a c) (zipWithKey (f . Right) b d)

-- interleave?
instance (FoldableWithKey f, FoldableWithKey g) => FoldableWithKey (Product f g) where
  foldMapWithKey f (Pair a b) = foldMapWithKey (f . Left) a `mappend` foldMapWithKey (f . Right) b

instance (FoldableWithKey1 f, FoldableWithKey1 g) => FoldableWithKey1 (Product f g) where
  foldMapWithKey1 f (Pair a b) = foldMapWithKey1 (f . Left) a <> foldMapWithKey1 (f . Right) b

instance (TraversableWithKey f, TraversableWithKey g) => TraversableWithKey (Product f g) where
  traverseWithKey f (Pair a b) = Pair <$> traverseWithKey (f . Left) a <*> traverseWithKey (f . Right) b

instance (TraversableWithKey1 f, TraversableWithKey1 g) => TraversableWithKey1 (Product f g) where
  traverseWithKey1 f (Pair a b) = Pair <$> traverseWithKey1 (f . Left) a <.> traverseWithKey1 (f . Right) b

instance (Adjustable f, Adjustable g) => Adjustable (Product f g) where
  adjust f (Left i) (Pair a b)  = Pair (adjust f i a) b
  adjust f (Right j) (Pair a b) = Pair a (adjust f j b)
  replace (Left i) v (Pair a b) = Pair (replace i v a) b
  replace (Right j) v (Pair a b) = Pair a (replace j v b)

type instance Key ((,) k) = k

instance Keyed ((,) k) where
  mapWithKey f (k, a) = (k, f k a)

instance FoldableWithKey ((,) k) where
  foldMapWithKey = uncurry

instance FoldableWithKey1 ((,) k) where
  foldMapWithKey1 = uncurry

instance TraversableWithKey ((,) k) where
  traverseWithKey f (k, a) = (,) k <$> f k a

instance TraversableWithKey1 ((,) k) where
  traverseWithKey1 f (k, a) = (,) k <$> f k a

type instance Key (HashMap k) = k

instance Keyed (HashMap k) where
  mapWithKey = HashMap.mapWithKey

instance (Eq k, Hashable k) => Indexable (HashMap k) where
  index = (HashMap.!)

instance (Eq k, Hashable k) => Lookup (HashMap k) where
  lookup = HashMap.lookup

instance (Eq k, Hashable k) => Zip (HashMap k) where
  zipWith = HashMap.intersectionWith

instance (Eq k, Hashable k) => ZipWithKey (HashMap k) where
  zipWithKey f a b = HashMap.foldlWithKey' go HashMap.empty a
    where
      go m k v = case lookup k b of
                   Just w -> HashMap.insert k (f k v w) m
                   _      -> m

instance FoldableWithKey (HashMap k) where
  foldrWithKey = HashMap.foldrWithKey

instance TraversableWithKey (HashMap k) where
  traverseWithKey = HashMap.traverseWithKey

type instance Key Maybe = ()

instance Keyed Maybe where
  mapWithKey f = fmap (f ())

instance Indexable Maybe where
  index = const . fromJust

instance Lookup Maybe where
  lookup _ mb = mb

instance Zip Maybe where
  zipWith f (Just a) (Just b) = Just (f a b)
  zipWith _ _        _        = error "zipWith: Nothing"

instance ZipWithKey Maybe where
  zipWithKey f = zipWith (f ())

instance FoldableWithKey Maybe where
  foldMapWithKey f = foldMap (f ())

instance TraversableWithKey Maybe where
  traverseWithKey f = traverse (f ())
