{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
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
import Data.Tree
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust, listToMaybe)
import qualified Data.Monoid as Monoid
import Data.Semigroup hiding (Product)
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Sequence (Seq, ViewL(EmptyL), viewl, (|>))
import qualified Data.Sequence as Seq
import Data.Traversable
import qualified Data.List as List
import Prelude hiding (lookup, zip, zipWith)

-- TODO: half of the functions manipulating Cofree and Free build the keys in the wrong order

type family Key (f :: * -> *)
type instance Key (Cofree f) = Seq (Key f)
type instance Key (Free f) = Seq (Key f)
type instance Key Tree = Seq Int
type instance Key NonEmpty = Int

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

class Functor f => Zip f where
  zipWith :: (a -> b -> c) -> f a -> f b -> f c
  zipWith f a b = uncurry f <$> zip a b

  zip :: f a -> f b -> f (a, b)
  zip = zipWith (,)

  -- zip-like 'ap'
  zap :: f (a -> b) -> f a -> f b
  zap = zipWith id

instance Zip f => Zip (Cofree f) where
  zipWith f (a :< as) (b :< bs) = f a b :< zipWith (zipWith f) as bs

instance Zip Tree where
  zipWith f (Node a as) (Node b bs) = Node (f a b) (zipWith (zipWith f) as bs)

class (Keyed f, Zip f) => ZipWithKey f where
  zipWithKey :: (Key f -> a -> b -> c) -> f a -> f b -> f c
  zipWithKey f = zap . mapWithKey f

  zapWithKey :: f (Key f -> a -> b) -> f a -> f b
  zapWithKey = zipWithKey (\k f -> f k)

instance ZipWithKey f => ZipWithKey (Cofree f) where
  zipWithKey f (a :< as) (b :< bs) = f Seq.empty a b :< zipWithKey (zipWithKey . fmap f . flip (|>)) as bs

instance ZipWithKey Tree where
  zipWithKey f (Node a as) (Node b bs) = f Seq.empty a b `Node` zipWithKey (zipWithKey . fmap f . flip (|>)) as bs

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

instance Indexable Tree where
  index (Node a as) key = case viewl key of
      EmptyL -> a
      k Seq.:< ks -> index (index as k) ks

(!) :: Indexable f => f a -> Key f -> a
(!) = index

-- * Lookup

class Lookup f where
  lookup :: Key f -> f a -> Maybe a

instance Lookup f => Lookup (Cofree f) where
  lookup key (a :< as) = case viewl key of
    EmptyL -> Just a
    k Seq.:< ks -> lookup k as >>= lookup ks

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

instance FoldableWithKey f => FoldableWithKey (Free f) where
  foldMapWithKey f (Pure a) = f Seq.empty a
  foldMapWithKey f (Free as) = foldMapWithKey (foldMapWithKey . fmap f . flip (|>)) as

instance FoldableWithKey f => FoldableWithKey (Cofree f) where
  foldMapWithKey f (a :< as) = f Seq.empty a `mappend` foldMapWithKey (foldMapWithKey . fmap f . flip (|>)) as

instance FoldableWithKey Tree where
  foldMapWithKey f (Node a as) = f Seq.empty a `mappend` foldMapWithKey (foldMapWithKey . fmap f . flip (|>)) as

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

instance TraversableWithKey f => TraversableWithKey (Cofree f) where
  traverseWithKey f (a :< as) = (:<) <$> f Seq.empty a <*> traverseWithKey (traverseWithKey . fmap f . flip (|>)) as

instance TraversableWithKey Tree where
  traverseWithKey f (Node a as) = Node <$> f Seq.empty a <*> traverseWithKey (traverseWithKey . fmap f . flip (|>)) as

instance TraversableWithKey f => TraversableWithKey (Free f) where
  traverseWithKey f (Pure a) = Pure <$> f Seq.empty a
  traverseWithKey f (Free as) = Free <$> traverseWithKey (traverseWithKey . fmap f . flip (|>)) as

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

-- instance TraversableWithKey f => TraversableWithKey1 (Cofree f) where
instance TraversableWithKey1 f => TraversableWithKey1 (Cofree f) where
  traverseWithKey1 f (a :< as) = (:<) <$> f Seq.empty a <.> traverseWithKey1 (traverseWithKey1 . fmap f . flip (|>)) as

instance TraversableWithKey1 Tree where
  traverseWithKey1 f (Node a []) = (`Node`[]) <$> f Seq.empty a
  traverseWithKey1 f (Node a (x:xs)) = (\b (y:|ys) -> Node b (y:ys)) <$> f Seq.empty a <.> traverseWithKey1 (traverseWithKey1 . fmap f . flip (|>)) (x:|xs)

instance TraversableWithKey1 f => TraversableWithKey1 (Free f) where
  traverseWithKey1 f (Pure a) = Pure <$> f Seq.empty a
  traverseWithKey1 f (Free as) = Free <$> traverseWithKey1 (traverseWithKey1 . fmap f . flip (|>)) as

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
  foldrWithKey = IntMap.foldWithKey

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

instance TraversableWithKey1 NonEmpty where
  traverseWithKey1 f (x:|[]) = (:|[]) <$> f 0 x
  traverseWithKey1 f (x:|(y:ys)) = (\w (z:|zs) -> w :| (z:zs)) <$> f 0 x <.> traverseWithKey1 (f . (+1)) (y :| ys)

type instance Key Seq = Int

instance Indexable Seq where
  index = Seq.index

instance Lookup Seq where
  lookup i s = case viewl (Seq.drop i s) of
    EmptyL -> Nothing
    a Seq.:< _ -> Just a

instance Zip Seq where
  zip = Seq.zip
  zipWith = Seq.zipWith

instance ZipWithKey Seq where
  zipWithKey f a b = Seq.zipWith id (Seq.mapWithIndex f a) b

instance Adjustable Seq where
  adjust = Seq.adjust

instance Keyed Seq where
  mapWithKey = Seq.mapWithIndex

instance FoldableWithKey Seq where
  foldrWithKey = Seq.foldrWithIndex

instance TraversableWithKey Seq where
  traverseWithKey f = fmap Seq.fromList . traverseWithKey f . toList

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

type instance Key (Functor.Sum f g) = (Key f, Key g)

instance (Indexable f, Indexable g) => Indexable (Functor.Sum f g) where
  index (Functor.InL a) (x,_) = index a x
  index (Functor.InR b) (_,y) = index b y

instance (Lookup f, Lookup g) => Lookup (Functor.Sum f g) where
  lookup (x, _) (Functor.InL a) = lookup x a
  lookup (_, y) (Functor.InR b) = lookup y b

instance (Adjustable f, Adjustable g) => Adjustable (Functor.Sum f g) where
  adjust f (x,_) (Functor.InL a) = Functor.InL (adjust f x a)
  adjust f (_,y) (Functor.InR b) = Functor.InR (adjust f y b)

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
