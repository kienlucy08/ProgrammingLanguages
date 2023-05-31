module DailyTenSpec where

import DailyTen
import Data.Char
import Data.Either
import Data.Eq
import Data.Functor
import Data.Maybe
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "First Functor Law" $ do
    it "obeys the first functor law for Maybe" $ do
      let maybeValue = Just ('c', 35)
      firstFunctorLaw maybeValue `shouldBe` True
    it "obeys the first functor law for lists" $ do
      let listValue = [2, 3, 5, 7, 11]
      firstFunctorLaw listValue `shouldBe` True
    it "obeys the first functor law for Nothing" $ do
      let maybeValue = Nothing :: Maybe Int
      firstFunctorLaw maybeValue `shouldBe` True
    it "obeys the first functor law for empty lists" $ do
      let listValue = [] :: [String]
      firstFunctorLaw listValue `shouldBe` True
    it "obeys the first functor law for a single list with strings" $ do
      let listValue = ["Hello"]
      firstFunctorLaw listValue `shouldBe` True
  describe "Second Functor Law" $ do
    it "obeys the second functor law for Maybe" $ do
      let maybeValue = Just ('c', 35)
      secondFunctorLaw isAlpha fst maybeValue `shouldBe` True
    it "obeys the second functor law for lists" $ do
      let listValue = [2, 3, 5, 7, 11]
      secondFunctorLaw chr (+ 96) listValue `shouldBe` True
    it "obeys the second functor law for Maybe with two ids" $ do
      let maybeValue = Just 42
      secondFunctorLaw id id maybeValue `shouldBe` True
    it "obeys the second functor law for lists with two ids" $ do
      let listValue = [1, 2, 3]
      secondFunctorLaw id id listValue `shouldBe` True
    it "obeys the second functor law for Maybe with two constant functions" $ do
      let maybeValue = Just 'c'
      secondFunctorLaw (const 1) (const "foo") maybeValue `shouldBe` True
    it "obeys the second functor law for lists with two constant functions" $ do
      let listValue = [4, 5, 6]
      secondFunctorLaw (const 'a') (const True) listValue `shouldBe` True

  describe "Either String (Maybe Integer)" $ do
    let leftValue = Left "error" :: Either String (Maybe Integer)
        rightNothingValue = Right Nothing :: Either String (Maybe Integer)
        rightJustValue = Right (Just 42) :: Either String (Maybe Integer)
        f x = (+ 2) <$> x
        g x = (* 3) <$> x

    it "obeys the first functor law with a Left value" $ do
      firstFunctorLaw leftValue `shouldBe` True
    it "obeys the first functor law with a Right Nothing value" $ do
      firstFunctorLaw rightNothingValue `shouldBe` True
    it "obeys the first functor law with a Right Just value" $ do
      firstFunctorLaw rightJustValue `shouldBe` True
    it "obeys the second functor law with a Left value" $ do
      secondFunctorLaw f g leftValue `shouldBe` True
    it "obeys the second functor law with a Right Nothing value" $ do
      secondFunctorLaw f g rightNothingValue `shouldBe` True
    it "obeys the second functor law with a Right Just value" $ do
      secondFunctorLaw f g rightJustValue `shouldBe` True
