import Data.List

-- Initial attempt
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) =
    if x == y
      then compress (x:xs)
      else x:(compress (y:xs))

-- After learning about group
compress' :: Eq a => [a] -> [a]
compress' = map head . group
