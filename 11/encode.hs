import Data.List

data Encoded a = Single a | Multiple Int a
  deriving (Show)

encode :: Eq a => [a] -> [Encoded a]
encode = map (\x -> toEncoded (length x) (head x)) . group

toEncoded :: Int -> a -> Encoded a
toEncoded 1 = Single
toEncoded x = Multiple x
