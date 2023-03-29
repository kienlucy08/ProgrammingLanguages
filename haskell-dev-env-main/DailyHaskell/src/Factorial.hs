--Comments
module Factorial where
    -- Factorial
    -- First Version
    -- Consume a number n, produce a number which is factorial of n
    factOne :: Integer -> Integer
    --factOne n = if PREDICATE then TRUEVALUE else FALSEVALUE
    factOne n = if n == 0 
                    then 1 
                    else n * factOne(n-1)
    --to test the file use stack build then stack ghci, then run the function factOne 5

    factTwo :: Integer -> Integer
    factTwo 0 = 1
    factTwo n = n * factTwo(n-1)