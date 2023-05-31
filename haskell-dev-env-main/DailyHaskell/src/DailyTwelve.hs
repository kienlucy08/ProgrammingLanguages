--Lucy Kien
--Daily Tweleve
--Programming Languages

module DailyTwelve where

    --onlyNothing :: ( a -> Maybe b ) -> [a] -> Bool. 
    --The onlyNothing function should produce True if the first parameter is applied to the elements of the second parameter 
    --and produces Nothing for each element. Otherwise onlyNothing produce False.
    --consumes a function f and a list
    --produces a boolean
    onlyNothing :: (a -> Maybe b) -> [a] -> Bool
    onlyNothing _ [] = True
    onlyNothing f (x:xs) =
        case f x of
            Nothing -> onlyNothing f xs
            Just _  -> False

    --firstAnswer :: ( a -> Maybe b ) -> [a] -> Maybe b. 
    --The first parameter should be applied to elements of the second parameter in order until the first time it produces Just v for some v and then Just v is the result of firstAnswer.
    --If the first argument returns Nothing for all the elements of the list, then firstAnswer should return Nothing.
    --consumes a function f and a list, The first parameter should be applied to elements of the second parameter 
    --produces the first Just value if encountered else returns nothing
    firstAnswer :: (a -> Maybe b) -> [a] -> Maybe b
    firstAnswer _ [] = Nothing
    firstAnswer f (x:xs) =
        case f x of
            Nothing -> firstAnswer f xs
            justVal -> justVal

    --allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]. 
    --The first parameter should be applied to elements of the second parameter. 
    --consumes a function f and a list
    --If this produces Nothing for any element, then the result for allAnswers is Nothing. 
    --Otherwise, the first parameter applied to the elements of the second parameter will have produced Just lst1, ..., Just lstn
    --nd the result of allAnswers is Just lst, where lst is [lst1, lst2, ..., lstn]
    allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
    allAnswers _ [] = Just []
    allAnswers f (x:xs) =
        case f x of
            Nothing -> Nothing
            Just lst -> case allAnswers f xs of
                        Nothing    -> Nothing
                        Just rest  -> Just (lst ++ rest)
