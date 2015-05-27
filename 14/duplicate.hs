duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = [x, x] ++ (duplicate xs)

duplicate' :: [a] -> [a]
duplicate' = concatMap (\x -> [x, x])
