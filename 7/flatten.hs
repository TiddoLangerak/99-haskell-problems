data NestedList a = Elem a | List [NestedList a]
-- Initial attempt

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List l) = flattenListOfLists(map flatten l)

flattenListOfLists :: [[a]] -> [a]
flattenListOfLists = foldr (++) []


-- Improvement after learning of concatMap
flatten' :: NestedList a -> [a]
flatten' (Elem a) = [a]
flatten' (List l) = concatMap flatten l
