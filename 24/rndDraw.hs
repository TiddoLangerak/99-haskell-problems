import System.Random

rndDraw :: RandomGen g => Int -> Int -> g -> ([Int], g)
rndDraw c n g
  | c > n = error "Cannot draw more random numbers than set size"
  | otherwise = rndSelect [1..n] c g

rndDrawIO :: Int -> Int -> IO [Int]
rndDrawIO c n = getStdRandom $ rndDraw c n

-- Copied from 23
rndSelect :: RandomGen g => [a] -> Int -> g -> ([a], g)
rndSelect _ 0 gen = ([], gen)
rndSelect [] _ _ = error "Number of items requested is larger than list"
rndSelect xs n gen = ((xs !! i) : rest, gen'')
                    where (i, gen') = randomR (0, length xs - 1) gen
                          (rest, gen'') = (rndSelect (removeAt xs i) (n - 1) gen')

rndSelectIO :: [a] -> Int -> IO [a]
rndSelectIO xs n = getStdRandom $ rndSelect xs n

removeAt :: [a] -> Int -> [a]
removeAt xs n
  | length xs <= n || n < 0 = error "Index out of bounds"
  | otherwise = let (ys, zs) = splitAt n xs
                    in ys ++ (tail zs)


