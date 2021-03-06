module H4 where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d =
  foldr
    (\c s@(x:xs) ->
       if c == d
         then [] : s
         else (c : x) : xs)
    [[]]

joinWith :: a -> [[a]] -> [a]
joinWith _ [] = []
joinWith c l = tail $ foldr (\x acc -> (c : x) ++ acc) [] l
