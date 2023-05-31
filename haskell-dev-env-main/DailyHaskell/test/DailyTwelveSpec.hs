module DailyTwelveSpec where

import DailyTwelve
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Only Nothing" $ do
    it "returns True for empty list" $
      onlyNothing (\_ -> Nothing) ([] :: [Int]) `shouldBe` True
    it "returns True when all elements produce Nothing" $
      onlyNothing (\_ -> Nothing) [1, 2, 3] `shouldBe` True
    it "returns False for mixed Just and Nothing" $
      onlyNothing (\x -> if even x then Just x else Nothing) [1, 2, 3, 4, 5] `shouldBe` False
    it "returns False when all elements produce Just" $
      onlyNothing (\x -> Just x) [1, 2, 3] `shouldBe` False

  describe "firstAnswer" $ do
    it "returns Nothing for empty list" $
      firstAnswer (\_ -> Nothing) ([] :: [Int]) `shouldBe` (Nothing :: Maybe Int)
    it "returns the first Just at the beginning" $
      firstAnswer (\x -> if even x then Just x else Nothing) [2, 4, 6] `shouldBe` Just 2
    it "returns the first Just in the middle" $
      firstAnswer (\x -> if even x then Just x else Nothing) [1, 3, 4, 5] `shouldBe` Just 4
    it "returns the first Just at the end" $
      firstAnswer (\x -> if even x then Just x else Nothing) [1, 3, 5, 6] `shouldBe` Just 6

  describe "allAnswers" $ do
    it "returns Just [] for empty list" $
      allAnswers (\_ -> Nothing) ([] :: [Int]) `shouldBe` (Just [] :: Maybe [Int])
    it "returns Nothing when any element produces Nothing" $
      allAnswers (\_ -> Nothing) [1, 2, 3] `shouldBe` (Nothing :: Maybe [Int])
    it "returns Nothing for mixed Just and Nothing" $
      allAnswers (\x -> if even x then Just [x] else Nothing) [1, 2, 3, 4, 5] `shouldBe` (Nothing :: Maybe [Int])
    it "returns Just [2, 4, 6] for all Justs" $
      allAnswers (\x -> Just [x]) [2, 4, 6] `shouldBe` Just [2, 4, 6]