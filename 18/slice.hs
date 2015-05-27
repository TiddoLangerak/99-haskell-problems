slice :: [a] -> Int -> Int -> [a]
slice xs start end = drop (start - 1) $ take end xs
