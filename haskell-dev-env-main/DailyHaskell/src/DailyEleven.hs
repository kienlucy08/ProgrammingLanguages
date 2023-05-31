-- Lucy Kien
-- Daily Eleven
-- Programming Languages

module DailyEleven where

-- allLefts :: [Either a b] -> [a]. A function that returns all the left values
-- consume a list of Either types
-- produce a list of any Left values
allLefts :: [Either a b] -> [a]
allLefts [] = []
allLefts ((Left x) : xs) = x : allLefts xs
allLefts (_ : xs) = allLefts xs

-- produceStringOrSum :: (Either String Integer) -> (Either String Integer) -> (Either String Integer).
-- A function that should produce the sum of two parameters if they are both integers or string otherwise
-- consumes two Either types.
-- produce a String if either parameter is a String
produceStringOrSum :: (Either String Integer) -> (Either String Integer) -> (Either String Integer)
produceStringOrSum (Left x) _ = Left x
produceStringOrSum _ (Left y) = Left y
produceStringOrSum (Right x) (Right y) = Right (x + y)

-- sumListOfEither :: [Either String Integer] -> (Either String Integer).
-- A function that should produce the first string in the list and if not produce the sum of all integers
-- consume a list consisting of Either Strings or Integers
-- produce the first String in the list
sumListOfEither :: [Either String Integer] -> Either String Integer
sumListOfEither [] = Right 0
sumListOfEither (x : xs) =
  case x of
    Left str -> Left str
    Right num -> case sumListOfEither xs of
      Left str' -> Left str'
      Right acc -> case acc of
        0 -> if allRights xs then Right 0 else Right num
        _ -> Right (num + acc)
  where
    allRights = all isRight
    isRight (Right _) = True
    isRight _ = False
