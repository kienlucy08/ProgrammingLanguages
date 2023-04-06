--Lucy Kien
--Daily Three Homework
--Programming Languages

module DailyThree where

--removeAllExcept :: Eq a => a -> [a] -> [a]. 
--Consumes a generic type and a list of generic types
--Returns a list of elements of the element not removed
removeAllExcept :: Eq a => a -> [a] -> [a]
removeAllExcept [] = []
removeAllExcept x (y:ys)
where
    | x == y    = y : removeAllExcept x ys
    | otherwise = removeAllExcept x ys

--countOccurrences :: Eq a => a -> [a] -> Int. This function counts how many times a given element occurs in the given list. For example, countOccurrences 'a' ['a', 'b', 'a', 'c'] will produce 2 and countOccurrences 1 [2, 4, 5, 2] would produce 0.
--substitute :: Eq a => a -> a -> [a] -> [a]. Replace all occurrences of the first argument with the second argument in the list and return the new list. For example, substitute 3 4 [1, 2, 3, 4] will give you [1, 2, 4, 4]