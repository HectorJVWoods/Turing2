module MapSet where


type Map a b = (a, b)
newtype MapSet a b = MapSet [(a, b)] deriving (Show, Eq)

emptyMapSet :: MapSet a b
emptyMapSet = MapSet []

singleton :: (a, b) -> MapSet a b
singleton x = MapSet [x]

isEmpty :: MapSet a b -> Bool
isEmpty (MapSet []) = True
isEmpty _           = False

member :: Eq a => a -> MapSet a b -> Bool
member x (MapSet xs) = x `elem` map fst xs


getBFromA :: Eq a => a -> MapSet a b -> Maybe b
getBFromA x (MapSet xs) | null result = Nothing
                        | otherwise   = Just $ snd $ head result
                       where result = filter (\(a, _) -> a == x) xs
                       
getAFromB :: Eq b => b -> MapSet a b -> Maybe a
getAFromB x (MapSet xs) | null result = Nothing
                        | otherwise   = Just $ fst $ head result
                       where result = filter (\(_, b) -> b == x) xs
                       
                       
insertToMapSet :: Eq a => (a, b) -> MapSet a b -> MapSet a b
insertToMapSet x (MapSet xs) | member (fst x) (MapSet xs) = MapSet xs
                             | otherwise                  = MapSet (x:xs)
                             
toList :: MapSet a b -> [(a, b)]
toList (MapSet xs) = xs

getAs :: MapSet a b -> [a]
getAs (MapSet xs) = map fst xs

getBs :: MapSet a b -> [b]
getBs (MapSet xs) = map snd xs                             