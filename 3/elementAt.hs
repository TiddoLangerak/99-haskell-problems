-- initial solution
elementAt :: Int -> [a] -> a
elementAt _ [] = error "Cannot get element from empty list"
elementAt 1 (x:_) = x
elementAt n (x:xs) = elementAt (n - 1) xs

