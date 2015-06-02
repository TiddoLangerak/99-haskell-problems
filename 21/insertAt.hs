insertAt :: a -> [a] -> Int -> [a]
insertAt x xs 1 = (x:xs)
insertAt x [] n = error "Index out of bounds"
insertAt x (y:ys) n
  | n > 1 = (y:(insertAt x ys (n - 1)))
  | otherwise = error "Insertion index must be equal or greater than 1"
