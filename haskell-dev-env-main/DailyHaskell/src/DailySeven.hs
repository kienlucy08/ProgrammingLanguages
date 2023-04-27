--Lucy Kien
--Daily Seven
--Programming Lanaguages

module DailySeven where

    --findLongest which consumes a list of words (strings). 
    --The function should produce the first word whose length is longer than or equal to the length of the words in the given list. 
    --The function should return the empty string if the given list is empty.
    findLongest :: [String] -> String
    findLongest [] = ""
    findLongest (x:xs) = foldl (\acc x -> if length x >= length acc then x else acc) x xs

    --anyLarger which consumes an Integer and a list of Integers. 
    --The function should produce True if any of the Integers in the list are equal to or larger than the given Integer and False otherwise. 
    --For example anyLarger 5 [1,2,3,4] should produce False and anyLarger 5 [1,3,5,2] should produce True.
    anyLarger :: Integer -> [Integer] -> Bool
    anyLarger _ [] = False
    anyLarger n xs = foldr (\x acc -> x >= n || acc) False xs

    --Write a function called allNames which consumes a list of tuples which each contain a first name and last name of a person. 
    --The function should produce a single string which contains all of the first names and last names separated by commas. 
    --For example: allNames [("kermit", "the frog"), ("bugs", "bunny")] should produce "kermit the frog, bugs bunny".
    allNames :: [(String, String)] -> String
    allNames [] = ""
    allNames names = foldr (\(f, l) acc -> f ++ " " ++ l ++ ", " ++ acc) "" names
