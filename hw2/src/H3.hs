{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module H3 where

import           Control.Applicative (Applicative(..), (<$>), (<*>), pure)
import           Data.Foldable       (Foldable(..))
import           Control.Monad       ((=<<))
import           Data.Functor        (Functor(..))
import           Data.Traversable    (Traversable(..))
import           H3Classes
import           Prelude             (Monoid(..), ($), (.), (=<<), const, flip, fmap, id, undefined)

-- class Functor f where
--    fmap :: (a -> b) -> f a -> f b
--     {-# LAWS
--         1. fmap id         ≡ id
--         2. fmap f . fmap g ≡ fmap (f . g)
--     #-}
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

--     {-# LAWS
--         1. m >>= return    ≡ m
--         2. return a >>= f  ≡ f a
--         3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
--     #-}
class MonadFish m where
  returnFish :: a -> m a
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

--     {-# LAWS
--         1. f >=> returnFish ≡ f
--         1. (f >=> returnFish) a ≡ f a
--         2. (returnFish >=> f) a ≡ f a
--         3. (f >=> g) >=> h ≡ f >=> (g >=> h)
--     #-}
class MonadJoin m where
  returnJoin :: a -> m a
  join :: m (m a) -> m a

--     {-# LAWS
--         1. join . returnJoin      ≡ id
--         2. join . fmap returnJoin ≡ id
--         3. join . fmap join       ≡ join . join
--         4* join . fmap (fmap f)   ≡ fmap f . join
--     #-}
infixl 1 >=>

-- instance Monad m => MonadFish m where
--    returnFish = return
--    f >=> g = \x -> f x >>= g
--  1. (returnFish >=> f) a = (returnFish a) >>= f = return a >>= f = f a

instance Monad m => MonadJoin m where
  returnJoin = return
  join = (=<<) id
--  1. join . returnJoin = join (returnJoin a) = join (return a) = id >>= return a = id a

-- instance MonadFish m => Monad m where
--    return = returnFish
--    (>>=) = flip (id >=>)
--    1. (m a) >>= return    =  (id >=> (m a)) return = return (m a) = m a

-- instance MonadFish m => MonadJoin m where
--    returnJoin = returnFish
--    join  = (id >=> id)
--    1. join . returnJoin      = join (returnFish a) = (id >=> id) (returnFish a) = id a
instance (Functor m, MonadJoin m) => Monad m where
  return = returnJoin
  m >>= f = join $ fmap f m

--  1. m >>= return    = join $ fmap return m = (join . fmap return) m = id m = m
instance (Functor m, MonadJoin m) => MonadFish m where
  returnFish = returnJoin
  m >=> f = join . fmap f . m

--  1. f >=> returnFish = join . fmap f . returnJoin  = id f = f
-- Functor instance
instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f r)

--         1. fmap id         ≡ id
instance Functor (Const a) where
  fmap _ (Const a) = Const a

instance Functor (Pair a) where
  fmap f (Pair (x, y)) = Pair (x, f y)

--         1. fmap id  (Pair (x,y)) = Pair (x, f y) = id (Pair (x, y))
instance Functor Identity where
  fmap f (Identity id) = Identity (f id)

instance Functor (Either a) where
  fmap _ (Left e) = Left e
  fmap f (Right r) = Right (f r)

--class Applicative f where
--  pure :: a -> f a
--  (<*>) :: f (a -> b) -> f a -> f b
--Applicative Laws
--1) pure id <*> v = v
--2) pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--3) pure f <*> pure x = pure (f x)
--4) u <*> pure y = pure ($ y) <*> u
instance Monoid a => Applicative (Const a) where
  pure _ = Const mempty
  (Const f) <*> (Const a) = Const $ f `mappend` a

instance Monoid a => Applicative (Pair a) where
  pure x = Pair (mempty, x)
  Pair (_, f) <*> p = fmap f p

instance Applicative Identity where
  pure = Identity
  Identity f <*> x = fmap f x
  --1) pure id <*> v = fmap id x = x

instance Applicative (Either a) where
  pure = Right
  Left e <*> _ = Left e
  Right f <*> r = fmap f r

instance Applicative Tree where
  pure x = Node x Leaf Leaf
  (Node fs l1 r1) <*> (Node xs l2 r2) = Node (fs xs) (l1 <*> l2) (r1 <*> r2)
  _ <*> _ = Leaf

-- Foldable 
instance Foldable Tree where
  foldMap f Leaf = mempty
  foldMap f (Node v l r) = foldMap f l `mappend` f v `mappend` foldMap f r

instance Foldable (Const a) where
  foldMap _ _ = mempty

instance Foldable (Pair a) where
  foldr f z (Pair (_, b)) = f b z

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Foldable (Either a) where
  foldMap f (Left e) = mempty
  foldMap f (Right x) = f x

instance Traversable Tree where
  traverse f Leaf = pure Leaf
  traverse f (Node v l r) = Node <$> f v <*> traverse f l <*> traverse f r

instance Traversable (Pair a) where
  traverse f (Pair (a, b)) = pure (Pair . (,) a) <*> f b

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Traversable (Const a) where
  traverse _ (Const x) = pure $ Const x

instance Traversable (Either a) where
  traverse _ (Left e) = pure $ Left e
  traverse f (Right r) = Right <$> f r
