module Set(Set, insertToSet, setFromList, emptySet, mapOverSet, listFromSet, isInSet, setToList) where

newtype Set a = Set [a] deriving (Show, Eq)



-- | Create a new empty set
emptySet :: Set a
emptySet = Set []

-- | Check if an element is in a set
member :: Eq a => a -> Set a -> Bool
member x (Set xs) = x `elem` xs

-- | Add an element to a set
insertToSet :: Eq a => a -> Set a -> Set a
insertToSet x (Set xs) | member x (Set xs) = Set xs
                       | otherwise         = Set (x:xs)
                  
-- | Convert a list to a set, has the effect of removing duplicates.             
setFromList :: (Eq a) => [a] -> Set a
setFromList = foldr insertToSet emptySet

setToList :: Set a -> [a]
setToList (Set xs) = xs

listFromSet :: Set a -> [a]
listFromSet (Set xs) = xs

mapOverSet :: (a -> b) -> Set a -> Set b
mapOverSet f (Set xs) = Set $ map f xs

isInSet :: Eq a => a -> Set a -> Bool
isInSet x (Set xs) = x `elem` xs
