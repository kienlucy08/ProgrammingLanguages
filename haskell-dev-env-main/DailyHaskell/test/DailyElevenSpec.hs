module DailyElevenSpec where

import DailyEleven
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "allLefts" $ do
    it "returns an empty list when given an empty list" $
      allLefts [] `shouldBe` ([] :: [Int])
    it "returns a list of Left values when all elements are Left values" $
      allLefts [Left "apple", Left "banana", Left "cherry"] `shouldBe` ["apple", "banana", "cherry"]
    it "returns an empty list when all elements are Right values" $
      allLefts [Right 1, Right 2, Right 3] `shouldBe` ([] :: [Int])
    it "returns a list of Left values when given a mixture of Left and Right values" $
      allLefts [Right 1, Left "apple", Right 2, Left "banana", Left "cherry"] `shouldBe` ["apple", "banana", "cherry"]

  describe "produceStringOrSum" $ do
    it "returns the first String parameter when both parameters are Strings" $
      produceStringOrSum (Left "apple") (Left "banana") `shouldBe` Left "apple"
    it "returns the second String parameter when only the second parameter is a String" $
      produceStringOrSum (Right 10) (Left "cherry") `shouldBe` Left "cherry"
    it "returns the first String parameter when only the first parameter is a String" $
      produceStringOrSum (Left "apple") (Right 20) `shouldBe` Left "apple"
    it "returns the sum of two Integer parameters when both parameters are Integers" $
      produceStringOrSum (Right 10) (Right 20) `shouldBe` Right 30

  describe "sumListOfEither" $ do
    it "returns Right 0 for an empty list" $
      sumListOfEither [] `shouldBe` Right 0
    it "returns the first String in the list if there is one" $
      sumListOfEither [Left "apple", Right 10, Left "banana"] `shouldBe` Left "apple"
    it "returns the sum of all the Integers in the list if there are no Strings" $
      sumListOfEither [Right 5, Right 10, Right 15] `shouldBe` Right 0
    it "returns the first String even if there are Integers after it" $
      sumListOfEither [Right 5, Left "apple", Right 10, Right 15] `shouldBe` Left "apple"
    it "returns Right 0 if the list contains only Right values" $
      sumListOfEither [Right 5, Right 10, Right 15] `shouldBe` Right 0
