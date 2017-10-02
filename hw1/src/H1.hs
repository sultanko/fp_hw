module H1 where

import           Data.List(elem, sort)


order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) = (\[x1, x2, x3] -> (x1, x2, x3)) (sort [x, y, z])

highestBit :: Int -> (Int, Int)
highestBit 0 = error "Zero"
highestBit 1 = (1, 0)
highestBit n = (2 * l, r + 1) where
  (l, r) = highestBit (n `div` 2)

smartReplicate :: [Int] -> [Int]
smartReplicate [] = []
smartReplicate xs = foldr (\x -> (++) (replicate x x)) [] xs

contains :: Eq a => a -> [[a]] -> [[a]]
contains _ [] = []
contains e (x:xs) =
  let res = contains e xs
  in if e `elem` x
       then x : res
       else res

-- testContains = [[1, 2, 3], [3, 4, 5], [1, 3], [3, 5], [2, 4], [2], [1]]
-- testOrder3 = [(3, 4, 5), (3, 5, 4), (5, 4, 3), (5, 3, 4), (4, 5, 3), (4, 3, 5), (3, 3, 4), (4, 3, 3), (3, 4, 3), (3, 3, 3)]
