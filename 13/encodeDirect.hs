data Encoded a = Single a | Multiple Int a
  deriving (Show)

encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect = foldr encodeHelper []
  where
    encodeHelper :: Eq a => a -> [Encoded a] -> [Encoded a]
    encodeHelper a [] = [Single a]
    encodeHelper a ((Single x):xs) =
      if x == a
        then ((Multiple 2 x):xs)
        else ((Single a):(Single x):xs)
    encodeHelper a ((Multiple n x):xs) =
      if x == a
        then ((Multiple (n+1) x):xs)
        else ((Single a):(Multiple n x):xs)
