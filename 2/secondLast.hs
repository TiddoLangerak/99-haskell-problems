secondLast :: [a] -> a
secondLast [] = error "Cannot get second last from empty list"
secondLast [x] = error "Cannot get second last from list with just one item"
secondLast (x:_:[]) = x
secondLast (_:x:xs) = secondLast (x:xs)
