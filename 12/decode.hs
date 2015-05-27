import Data.List

data Encoded a = Single a | Multiple Int a
  deriving (Show)

decode :: [Encoded a] -> [a]
decode [] = []
decode ((Single x):xs) = (x:(decode xs))
decode ((Multiple c x):xs) = take c (repeat x) ++ (decode xs)
