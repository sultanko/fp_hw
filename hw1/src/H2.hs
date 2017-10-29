module H2 where

import           System.Random (newStdGen, randomRs)

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt 0 (x:xs) = (Just x, xs)
removeAt n (x:xs) = (res, x : l) where (res, l) = removeAt (n - 1) xs

collectEvery :: Int -> [a] -> ([a], [a])
collectEvery _ [] = ([], [])
collectEvery n l = (init (fst splitL) ++ fst ans, curSnd)
  where
    splitL = splitAt n l
    ans = collectEvery n $ snd splitL
    curSnd =
      if length l < n
        then snd ans
        else (l !! (n - 1)) : snd ans

stringSum :: String -> Int
stringSum s = sum (map read (words s))

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort ls) (mergeSort rs)
  where
    (ls, rs) = splitOnHalf xs

splitOnHalf :: [Int] -> ([Int], [Int])
splitOnHalf [] = ([], [])
splitOnHalf (x:xs) = (x : ls, rs)
  where
    (ls, rs) = splitOnHalf2 xs

splitOnHalf2 :: [Int] -> ([Int], [Int])
splitOnHalf2 [] = ([], [])
splitOnHalf2 (x:xs) = (ls, x : rs)
  where
    (ls, rs) = splitOnHalf xs

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) =
  if x < y
    then x : merge xs (y : ys)
    else y : merge (x : xs) ys

-- tests = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030"
--         , " 123 456 789 "]
