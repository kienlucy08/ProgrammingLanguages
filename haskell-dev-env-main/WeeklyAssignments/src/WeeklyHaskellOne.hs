--Lucy Kien
--Weekly Assignment 1
--Programming Languages

module WeeklyHaskellOne where

    import Data.Char (ord, chr, isAscii)

    --removeChar consumes a single character and a string. 
    --Produces a new string with all instances of the character removed
    removeChar :: Char -> String -> String
    removeChar c = foldr (\x acc -> if x == c then acc else x:acc) ""

    --removeWhitespace consumes a string.
    -- Produces a new string with all spaces, tabs, new line characters, and carriage returns removed.
    removeWhitespace :: String -> String
    removeWhitespace = removeCarriageReturn . removeNewLine . removeTab . removeSpace
        where
        removeSpace = removeChar ' '
        removeTab = removeChar '\t'
        removeNewLine = removeChar '\n'
        removeCarriageReturn = removeChar '\r'
    
    --removePunctuation consumes a string. 
    --Produces a new string with all commas, periods, parentheses, square brackets, and curly brackets removed.
    removePunctuation :: String -> String
    removePunctuation = removeCommas . removePeriods . removeParaentheses . removeSquare . removeCurly
        where
            removeCommas = removeChar ','
            removePeriods = removeChar '.'
            removeParaentheses = removeChar '(' . removeChar ')'
            removeSquare  = removeChar '[' . removeChar ']'
            removeCurly = removeChar '{' . removeChar '}'

    --charsToAscii consumes a string. 
    --Produces a new list containing the ASCII values of the characters in the given string.
    charsToAscii :: String -> [Int]
    charsToAscii = map fromEnum

    --asciiToChars consumes a list of integers. 
    --Produces a new list of characters created from the ASCII values.
    asciiToChars :: [Int] -> String
    asciiToChars = map toEnum

    --shiftInts consumes an integer (the shift value) and a list of integers. 
    --Produces a new list of integers where each value in the given list has been increased by the shift value.
    shiftInts :: Int -> [Int] -> [Int]
    shiftInts shift = map (\x -> (x + shift) `mod` 128)



    --shiftMessage consumes an integer (the shift value) and a string (the message).
    --Produces a new string which is the encrypted message where each character has been shifted by the shift value in the ASCII encoding. 
    shiftMessage :: Int -> String -> String
    shiftMessage shift message = map (shiftChar shift) message

    -- helper method for the shift message method
    --consumes an integer (shift value) and a char
    --produces a new char which is encrypted by the shifted value
    shiftChar :: Int -> Char -> Char
    shiftChar shift c = if isAscii c then chr $ (ord c + shift - 32) `mod` 95 + 32 else c






