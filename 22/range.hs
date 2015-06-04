range :: Int -> Int -> [Int]
range x y
  | x > y = error "End value should be greater than or equal to start value"
  | x == y = [x]
  | otherwise = x:(range (x+1) y)
