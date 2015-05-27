duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = [x, x] ++ (duplicate xs)

duplicate' :: [a] -> [a]
duplicate' = concatMap (\x -> [x, x])

--Same, but with monads:
duplicate'' :: [a] -> [a]
duplicate'' xs = xs >>= (\x -> [x, x])
