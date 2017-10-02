module H5 where

import           Data.Semigroup
import           Data.Maybe (fromMaybe)

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = fromMaybe [] . mconcat

data NonEmpty a =
  a :| [a]

instance Semigroup (NonEmpty a) where
  (a :| as) <> (b :| bs) = a :| (as ++ b : bs)

newtype Identity a = Identity { runIdentity :: a }

instance Semigroup a => Semigroup (Identity a) where
  l <> r = Identity (runIdentity l <> runIdentity r)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity x) (Identity y) = Identity (mappend x y)
