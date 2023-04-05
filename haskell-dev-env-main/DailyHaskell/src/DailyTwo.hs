-- Lucy Kien
-- Daily Two Homework
-- Programming Languages 

module DailyTwo where
    

    --every4th consumes a list of integers
    --Produces a list of integers that takes every 4th element from the list
    every4th :: [a] -> [a]
    every4th [] = []
    every4th (_:_:_:w:xs) = w : every4th xs
    every4th _ = []

    --tupleDotQuoitent consumes two lists of integers
    --Produces the dot product of the two lists of integers
    tupleDotQuotient :: [Double] -> [Double] -> Double
    tupleDotQuotient [] [] = 0
    tupleDotQuotient _ [] = error "Second input list is empty"
    tupleDotQuotient [] _ = error "First input list is empty"
    tupleDotQuotient (x:xs) (y:ys)
        | length xs /= length ys = error "Input lists have different lengths"
        | otherwise = x / y + tupleDotQuotient xs ys



    --appendToEach consumes a string and a list of strings
    --Produces a new list of strings with the appended string
    appendToEach :: String -> [String] -> [String]
    appendToEach _ [] = []
    appendToEach suffix (x:xs) = (x ++ suffix) : appendToEach suffix xs

    --toSetList consumes a list of generic elements
    --Produces a new list of generic elements in set representation
    toSetList :: Eq a => [a] -> [a]
    toSetList [] = []
    toSetList (x:xs) = x : toSetList (removeAll x xs)

    --helper method for toSetList that removes all the alike variables
    --Consumes a list of generic elements
    --Produces the new list in set representation
    removeAll :: Eq a => a -> [a] -> [a]
    removeAll _ [] = []
    removeAll y (x:xs)
        | x == y    = removeAll y xs
        | otherwise = x : removeAll y xs
    
    --tupleDotQuotient Extra credit consumes two lists of integers
    --Produces a dot product of the two integers
    --Uses foldr
    tupleDotQuotientExtra :: [Double] -> [Double] -> Double
    tupleDotQuotientExtra xs ys = dotProduct / normX / normY
        where
            dotProduct = foldr (\(x, y) acc -> x * y + acc) 0 (zip xs ys)
            normX = sqrt (foldr (\x acc -> x * x + acc) 0 xs)
            normY = sqrt (foldr (\y acc -> y * y + acc) 0 ys)




