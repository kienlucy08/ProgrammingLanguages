-- Lucy Kien
-- Daily Six
-- Programming Languages

module DailySix where


    --shorterThan which consumes a number and a list of words. 
    --The function should produce a list of the words whose length is shorter than or equal to the given number.
    shorterThan :: Int -> [String] -> [String]
    shorterThan n = filter (\word -> length word <= n)

    --removeMultiples which consumes a number and a list of numbers. 
    --The function should produce a list where the multiples of the given number have been removed. 
    --For example: removeMultiples 5 [3,5,10,9, 15] should produce [3,9] because 5, 10, 15 are multiples of 5.
    removeMultiples :: Int -> [Int] -> [Int]
    removeMultiples n xs = filter (\x -> x `mod` n /= 0) xs

    --onlyJust which consumes a list of Maybe a 
    --produces a list where all values of Nothing have been eliminated. 
    --For example: onlyJust [Nothing, Just 5, Nothing, Just 10] should produce [Just 5, Just 10]
    onlyJust :: [Maybe a] -> [a]
    onlyJust xs = filter isJust xs >>= fromJust
        where
            isJust :: Maybe a -> Bool
            isJust (Just _) = True
            isJust Nothing = False

            fromJust :: Maybe a -> [a]
            fromJust (Just x) = [x]
            fromJust Nothing = []




