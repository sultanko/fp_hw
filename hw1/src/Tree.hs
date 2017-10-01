module Tree where

import           TreePrinters (Tree (..))

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _ = False

size :: Tree a -> Int
size Leaf = 0
size (Node n l r) = 1 + (size l) + (size r)

find :: (Ord a) => a -> Tree a -> Maybe a
find _ Leaf = Nothing
find x (Node n left right)
    | n == x    = Just n
    | x < n     = find x left
    | otherwise = find x right

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node n left right)
    | n == x    = Node n left right
    | x < n     = Node n (insert x left) right
    | otherwise = Node n left (insert x right)

toList :: Tree a -> [a]
toList Leaf         = []
toList (Node x l r) = toList l ++ [x] ++ toList r

fromList :: (Ord a) => [a] -> Tree a
fromList l = foldr insert Leaf l

instance Ord a => Monoid (Tree a) where
    mempty = Leaf
    mappend t1 t2 = foldr T.insert t1 t2

instance Foldable Tree where
    foldMap f Leaf = mempty
    foldMap f (Node v l r) = foldMap f l `mappend` f v `mappend` foldMap f r
