module Set(Set, insertToSet, setFromList, emptySet, 
           mapOverSet, listFromSet, isInSet, setToList, setMap, setUnion, setRemoveElementsInOtherSet) where

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

setMap :: (a -> b) -> Set a -> Set b
setMap f (Set xs) = Set $ map f xs

setUnion :: Eq a => Set a -> Set a -> Set a
setUnion xs (Set (y:ys)) = setUnion (insertToSet y xs) (Set ys)

setConjunction :: Eq a => Set a -> Set a -> Set a
setConjunction xs (Set (y:ys)) | isInSet y xs = setConjunction (insertToSet y xs) (Set ys)
                               | otherwise    = setConjunction xs (Set ys)

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet f (Set xs) = Set $ filter f xs

removeFromSet :: Eq a => a -> Set a -> Set a
removeFromSet x (Set xs) = filterSet (/= x) (Set xs)

setRemoveElementsInOtherSet :: Eq a => Set a -> Set a -> Set a
setRemoveElementsInOtherSet xs (Set (y:ys)) | isInSet y xs = setRemoveElementsInOtherSet (removeFromSet y xs) (Set ys)
                                            | otherwise    = setRemoveElementsInOtherSet xs (Set ys)