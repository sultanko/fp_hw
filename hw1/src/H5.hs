module H5 where

import           Data.Semigroup
import           Data.Maybe
import           Data.Function

maybeConcat :: [Maybe [a]]->[a]
maybeConcat = fromMaybe [] . mconcat

data NonEmpty a = a :| [a]

instance Semigroup (NonEmpty a) where
  (a :| as) <> ~(b :| bs) = a :| (as ++ b : bs)

newtype Identity a = Identity { runIdentity :: a}

instance Monoid a => Monoid (Identity a) where 
  mempty = Identity {runIdentity = id}
  mappend (Identity x) (Identity y) = Identity (mappend x y)
