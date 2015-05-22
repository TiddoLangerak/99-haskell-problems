--Initial attempt
secondLast :: [a] -> a
secondLast [] = error "Cannot get second last from empty list"
secondLast [x] = error "Cannot get second last from list with just one item"
secondLast (x:_:[]) = x
secondLast (_:x:xs) = secondLast (x:xs)

--This is how it's supposed to be done
secondLast' :: [a] -> a
secondLast [] = error "Cannot get second last from empty list"
secondLast [x] = error "Cannot get second last from list with just one item"
secondLast' = last . init
