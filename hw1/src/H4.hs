module H4 where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d =
  foldr
    (\c s@(x:xs) ->
       if c == d
         then [] : s
         else (c : x) : xs)
    [[]]
