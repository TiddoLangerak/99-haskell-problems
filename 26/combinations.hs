import Data.List

combinations :: Eq a => Int -> [a] -> [[a]]
combinations l xs
  | l > length xs = error "List is too small for given group size"
  | l == length xs = [xs]
  | otherwise = nub $ concat $ map (combinations l) $ map (`removeAt` xs) $ [0..last]
                where last = length xs - 1


removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n + 1) xs
