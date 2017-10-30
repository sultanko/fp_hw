module H2 where

import           Data.List (delete)

bin :: Int -> [[Int]]
bin 0 = [[]]
bin n = bin (n - 1) >>= \ls -> [ls ++ [0], ls ++ [1]]

incr :: Int -> [Int]
incr a = a : incr (a + 1)

genAbsent :: Int -> [Int] -> [Int]
genAbsent n lst = take (n - xs) (incr (xs + 1))
  where
    xs =
      case lst of
        [] -> 0
        _ -> last lst

combinations :: Int -> Int -> [[Int]]
combinations _ 0 = [[]]
combinations n k =
  combinations n (k - 1) >>= \ls -> genAbsent n ls >>= \gs -> [ls ++ [gs]]

permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations xs = xs >>= \x -> permutations (delete x xs) >>= \ls -> [x : ls]
