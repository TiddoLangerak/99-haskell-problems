import Data.List

data Encoded a = Single a | Multiple Int a
  deriving (Show)

decode :: [Encoded a] -> [a]
decode [] = []
decode ((Single x):xs) = (x:(decode xs))
decode ((Multiple c x):xs) = replicate c x ++ (decode xs)


-- Improved
decode' :: [Encoded a] -> [a]
decode' = concatMap decodeEncoded
  where
    decodeEncoded (Single x) = [x]
    decodeEncoded (Multiple c x) = replicate c x
