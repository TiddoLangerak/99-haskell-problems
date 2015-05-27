import Data.List

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group

--Alternative
encode' :: Eq a => [a] -> [(Int, a)]
encode' x = zip (map length packed) (map head packed)
  where packed = group x


