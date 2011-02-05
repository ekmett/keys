{-# LANGUAGE TypeFamilies #-}
module Data.Key (
  -- * Keys
    Key

  -- * Keyed functors
  , Keyed(..)
  , (<#$>) -- :: Keyed f => (Key f -> a -> b) -> f a -> f b
  , keyed -- :: Keyed f => f a -> f (Key f, a)
  , apWithKey

  -- * Indexableing
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

  -- * TraverableWithKey1
  , TraversableWithKey1(..)
  , foldMapWithKey1Default -- :: (TraversableWithKey1 t, Semigroup m) => (Key t -> a -> m) -> t a -> m
  , module Data.Foldable
  , module Data.Traversable
  , module Data.Semigroup
  , module Data.Semigroup.Foldable
  , module Data.Semigroup.Traversable
  ) where

import Control.Applicative
import Control.Comonad.Trans.Traced
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import qualified Data.Array as Array
import Data.Array (Array)
-- import qualified Data.Array.IArray as IArray
-- import qualified Data.Array.Unboxed hiding (index)
-- import Data.Array.Unboxed (UArray)
import Data.Functor.Identity
import Data.Functor.Bind
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Coproduct
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Ix hiding (index)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Monoid as Monoid hiding (Product)
import Data.Semigroup hiding (Product)
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Sequence (Seq, ViewL(..), viewl)
import qualified Data.Sequence as Seq
import Data.Traversable
import Prelude hiding (lookup)

type family Key (f :: * -> *) 

-- * Keyed
class Functor f => Keyed f where
  mapWithKey :: (Key f -> a -> b) -> f a -> f b

infixl 4 <#$>

(<#$>) :: Keyed f => (Key f -> a -> b) -> f a -> f b
(<#$>) = mapWithKey
{-# INLINE (<#$>) #-}

keyed :: Keyed f => f a -> f (Key f, a)
keyed = mapWithKey (,)
{-# INLINE keyed #-}

apWithKey :: (Keyed f, Applicative f) => f (Key f -> a -> b) -> f a -> f b
apWithKey ff fa = mapWithKey (\k f -> f k) ff <*> fa
{-# INLINE apWithKey #-}

-- * Indexable

class Indexable f where
  index :: f a -> Key f -> a

(!) :: Indexable f => f a -> Key f -> a
(!) = index

-- * Lookup

class Lookup f where
  lookup :: Key f -> f a -> Maybe a

lookupDefault :: Indexable f => Key f -> f a -> Maybe a
lookupDefault k t = Just (index t k)

-- * Adjustable

class Functor f => Adjustable f where
  adjust :: (a -> a) -> Key f -> f a -> f a

  replace :: Key f -> a -> f a -> f a
  replace k v = adjust (const v) k

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

-- * TraverableWithKey1
class (Traversable1 t, FoldableWithKey1 t, TraversableWithKey t) => TraversableWithKey1 t where
  traverseWithKey1 :: Apply f => (Key t -> a -> f b) -> t a -> f (t b)

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

instance Indexable ((->)a) where
  index = id
  
instance Lookup ((->)a) where
  lookup i f = Just (f i)

type instance Key (ReaderT e m) = (e, Key m)

instance Keyed m => Keyed (ReaderT e m) where
  mapWithKey f (ReaderT m) = ReaderT $ \k -> mapWithKey (f . (,) k) (m k)

instance Indexable m => Indexable (ReaderT e m) where
  index (ReaderT f) (e,k) = index (f e) k

instance Lookup m => Lookup (ReaderT e m) where
  lookup (e,k) (ReaderT f) = lookup k (f e)

type instance Key (TracedT s w) = (s, Key w)

instance Keyed w => Keyed (TracedT s w) where
  mapWithKey f = TracedT . mapWithKey (\k' g k -> f (k, k') (g k)) . runTracedT 

instance Indexable w => Indexable (TracedT s w) where
  index (TracedT w) (e,k) = index w k e 

instance Lookup w => Lookup (TracedT s w) where
  lookup (e,k) (TracedT w) = ($ e) <$> lookup k w
  
type instance Key IntMap = Int

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

instance Keyed [] where
  mapWithKey f0 xs0 = go f0 xs0 0 where
    go _ [] _ = []
    go f (x:xs) n = f n x : (go f xs $! n)

instance FoldableWithKey [] where
  foldrWithKey f0 z0 xs0 = go f0 z0 xs0 0 where
    go _ z [] _ = z
    go f z (x:xs) n = f n x (go f z xs $! n)

instance TraversableWithKey [] where
  traverseWithKey f0 xs0 = go f0 xs0 0 where
    go _ [] _ = pure []
    go f (x:xs) n = (:) <$> f n x <*> (go f xs $! (n + 1))

instance Indexable [] where
  index = (!!)

instance Lookup [] where
  lookup = fmap listToMaybe . drop

instance Adjustable [] where
  adjust f 0 (x:xs) = f x : xs
  adjust _ _ [] = []
  adjust f n (x:xs) = n' `seq` x : adjust f n' xs where n' = n - 1

type instance Key Seq = Int

instance Indexable Seq where
  index = Seq.index

instance Lookup Seq where
  lookup i s = case viewl (Seq.drop i s) of
    EmptyL -> Nothing
    a :< _ -> Just a

instance Adjustable Seq where
  adjust = Seq.adjust

instance Keyed Seq where
  mapWithKey = Seq.mapWithIndex

instance FoldableWithKey Seq where
  foldrWithKey = Seq.foldrWithIndex

instance TraversableWithKey Seq where
  traverseWithKey f = fmap Seq.fromList . traverseWithKey f . toList

type instance Key (Map k) = k

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
  
{-
type instance Key (UArray i) = i

instance Ix i => Indexable (UArray i) where
  index = (IArray.!)

instance Ix i => Lookup (UArray i) where
  lookup i arr
    | inRange (IArray.bounds arr) i = Just (arr IArray.! i)
    | otherwise = Nothing

instance Ix i => FoldableWithKey (UArray i) where
  foldrWithKey f z = Prelude.foldr (uncurry f) z . IArray.assocs
-}

type instance Key (Coproduct f g) = (Key f, Key g)

instance (Indexable f, Indexable g) => Indexable (Coproduct f g) where
  index (Coproduct (Left a)) (x,_) = index a x 
  index (Coproduct (Right b)) (_,y) = index b y

instance (Lookup f, Lookup g) => Lookup (Coproduct f g) where
  lookup (x, _) (Coproduct (Left a)) = lookup x a
  lookup (_, y) (Coproduct (Right b)) = lookup y b

instance (Adjustable f, Adjustable g) => Adjustable (Coproduct f g) where
  adjust f (x,_) (Coproduct (Left a)) = Coproduct (Left (adjust f x a))
  adjust f (_,y) (Coproduct (Right b)) = Coproduct (Right (adjust f y b))

type instance Key (Product f g) = Either (Key f) (Key g)

instance (Keyed f, Keyed g) => Keyed (Product f g) where
  mapWithKey f (Pair a b) = Pair (mapWithKey (f . Left) a) (mapWithKey (f . Right) b)

instance (Indexable f, Indexable g) => Indexable (Product f g) where
  index (Pair a _) (Left i)  = index a i 
  index (Pair _ b) (Right j) = index b j

instance (Lookup f, Lookup g) => Lookup (Product f g) where
  lookup (Left i) (Pair a _) = lookup i a
  lookup (Right j) (Pair _ b) = lookup j b

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

