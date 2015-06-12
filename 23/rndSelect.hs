import System.Random

rndSelect :: [a] -> Int -> IO [a]
rndSelect _ 0 = return []
rndSelect [] _ = error "Number of items requested is larger than list"
rndSelect xs c = randomRIO (0, length xs - 1)
  >>= (\n ->
    rndSelect (removeAt xs n) (c - 1)
      >>= (\ys -> return ((xs !! n) : ys))
  )
  -- >>= return . (!!) xs

removeAt :: [a] -> Int -> [a]
removeAt xs n
  | length xs <= n || n < 0 = error "Index out of bounds"
  | otherwise = let (ys, zs) = splitAt n xs
                    in ys ++ (tail zs)
