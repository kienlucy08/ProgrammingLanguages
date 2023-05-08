--Lucy Kien
--Daily Nine
--Programming Languages

module DailyNine where

    import Data.Maybe

    --Find smallest value in a list
    --consumes a list
    --produces nothing if list is empty and the minimum value in the list other wise
    findSmallest :: (Ord a) => [a] -> Maybe a
    findSmallest [] = Nothing
    findSmallest xs = Just (minimum xs)

    --allTrue function which returns just true when all elements in the list are true
    --consumes a list of boolean values
    --produces a Maybe value, just true, just false, or nothing
    allTrue :: [Bool] -> Maybe Bool
    allTrue [] = Nothing
    allTrue xs
        | all id xs = Just True
        | otherwise = Just False

    --countAllVotes where each element in the list represents the vote of one political representative on a particular issue:
    --Nothing denotes that the representative has not yet voted, Just True denotes a vote in favour, and Just False denotes a vote against the issue.
    --consumes a list of maybe boolean values such as just true, nothing etc.
    --produces a three-tuple of integers with the number of representatives who have not yet voted, voted in favour, and voted against.
    countAllVotes :: [Maybe Bool] -> (Integer, Integer, Integer)
    countAllVotes xs = (notVoted, votedTrue, votedFalse)
      where notVoted = toInteger $ length $ filter isNothing xs
            votedTrue = toInteger $ length $ filter (== Just True) xs
            votedFalse = toInteger $ length $ filter (== Just False) xs


