--Lucy Kien
--Daily Four
--Programming Languages

module DailyFour where

    --zip3Lists that consumes three lists as arguments 
    --Produces a list of tuples. 
    --For example, zip3Lists [1, 2, 3] ['a', 'b', 'c'][4, 5, 6] would produce [(1, 'a', 4), (2, 'b', 5), (3, 'c', 6)]
    zip3Lists :: [a] -> [b] -> [c] -> [(a,b,c)]
    zip3Lists [] [] [] = []
    zip3Lists [] ys zs = []
    zip3Lists xs ys [] = []
    zip3Lists xs [] zs = []
    zip3Lists (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3Lists xs ys zs


    --unzipTriples consumes a list of triples 
    --Produces a tuple of three lists. 
    --Each of the resulting lists consists of the first elements of the triples, the second elements of the triples, and the third elements of the triples. For example, unzipTriples [ (1,2,3), (4, 5, 6), (7, 8, 9) ] should produce ( [1,4,7], [2, 5, 8], [3, 6, 9] ).
    unzipTriples :: [(a, b, c)] -> ([a], [b], [c])
    unzipTriples [] = ([], [], [])
    unzipTriples ((x, y, z) : xs) = let (xs1, ys1, zs1) = unzipTriples xs in (x : xs1, y : ys1, z : zs1)

    --mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]. 
    --Consumes 3 lists which are in sorted order 
    --and merges them and produces a final list is sorted in increasing order. 
    --For example, mergeSorted3 [2, 3, 5] [1, 8] [-1, 0, 4, 10] is [-1, 0, 1, 2, 3, 4, 5, 8, 10]. As before, don't use list comprehensions or built-in sorting functions. Hint: imagine we have just two lists: [1, 3, 5, 8] and [2, 4, 5, 7, 9], you'd compare the heads of each list, 1 and 2, and put 1 first, which you'd append to merging [3,5,8] with [2,4,5,7,9] recursively.
    mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
    mergeSorted3 [] [] [] = []
    mergeSorted3 xs [] [] = xs
    mergeSorted3 [] ys [] = ys
    mergeSorted3 [] [] zs = zs
    mergeSorted3 xs ys [] = mergeSorted2 xs ys
    mergeSorted3 xs [] zs = mergeSorted2 xs zs
    mergeSorted3 [] ys zs = mergeSorted2 ys zs
    mergeSorted3 xs ys zs = mergeSorted2 xs (mergeSorted2 ys zs)

    -- merge two lists, helper method to be used in merge sorted 3
    -- Consumes 2 lists in sorted order
    -- Produces a final list in sorted increasing order
    mergeSorted2 :: Ord a => [a] -> [a] -> [a]
    mergeSorted2 [] [] = []
    mergeSorted2 xs [] = xs
    mergeSorted2 [] ys = ys
    mergeSorted2 (x:xs) (y:ys) 
        | x <= y    = x : mergeSorted2 xs (y:ys)
        | otherwise = y : mergeSorted2 (x:xs) ys


