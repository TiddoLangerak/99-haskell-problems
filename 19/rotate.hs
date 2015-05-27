rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate xs@(x:xt) n
  | n > 0 = rotate (xt ++ [x]) (n - 1)
  -- Rotating to the right can be implemented as a full rotation to the left minus n steps
  | otherwise = rotate xs (length xs + n)
