--Lucy Kien
--Daily Five
--Programming Languages

module DailyFive where

    import Data.Char (isLower)

    --multPairs that consumes a list of pairs of integers
    --Produces the product of the pair in a list of integers
    multPairs :: [(Int, Int)] -> [Int]
    multPairs xs = map (\(x, y) -> x * y) xs

    --squareList that consumes a list of Integers as input
    --produces a new list of pairs of Integers.
    --The resulting pairs should be the original Integer and the square of that integer.
    squareList :: [Int] -> [(Int, Int)]
    squareList xs = map (\x -> (x, x*x)) xs

    --findLowercase that consumes a list of Strings 
    --produces a list of Bool. 
    --An entry in the produced list should be True if the corresponding String starts with a lower case character and False otherwise.
    findLowercase :: [String] -> [Bool]
    findLowercase xs = map (\x -> isLower (head x)) xs


