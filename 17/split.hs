split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs 0 = ([], xs)
split xs n = (h ++ [y], ys)
    where
      (h, y:ys) = split xs (n - 1)
