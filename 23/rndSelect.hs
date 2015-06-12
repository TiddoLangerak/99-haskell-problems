import System.Random

rndSelect :: [a] -> Int -> IO [a]
rndSelect _ 0 = return []
rndSelect [] _ = error "Number of items requested is larger than list"
rndSelect xs c = randomRIO (0, length xs - 1)
  >>= (\n ->
    let element = xs !! n
        in rndSelect (removeAt xs n) (c - 1)
           >>= return . (:) element
  )

-- After learning that the RandomGen versions can be implemented without monads
rndSelect' :: RandomGen g => [a] -> Int -> g -> ([a], g)
rndSelect' _ 0 gen = ([], gen)
rndSelect' [] _ _ = error "Number of items requested is larger than list"
rndSelect' xs n gen = ((xs !! i) : rest, gen'')
                    where (i, gen') = randomR (0, length xs - 1) gen
                          (rest, gen'') = (rndSelect' (removeAt xs i) (n - 1) gen')

rndSelectIO' :: [a] -> Int -> IO [a]
rndSelectIO' xs n = getStdRandom $ rndSelect' xs n

removeAt :: [a] -> Int -> [a]
removeAt xs n
  | length xs <= n || n < 0 = error "Index out of bounds"
  | otherwise = let (ys, zs) = splitAt n xs
                    in ys ++ (tail zs)

