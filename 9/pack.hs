-- initial solution
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:ys)
    | x == head (head packed) = (x:(head packed)):(tail packed)
		| otherwise = [x]:packed
		where packed = pack ys

-- Learning about span
pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = (x:repeats): pack' rest
    where(repeats, rest) = span (==x) xs
