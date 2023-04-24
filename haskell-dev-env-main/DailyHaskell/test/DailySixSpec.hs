module DailySixSpec where
import Test.Hspec
import DailySix
import Data.Maybe

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  describe "Shorter Than Function" $ do
    it "Shorter than for a list with shorter words than 5 letters" $
        shorterThan 5 ["Hellos", "Basket", "Go", "Yes"] `shouldBe` ["Go", "Yes"]
    it "Shorter than for an empty list" $
        shorterThan 5 [] `shouldBe` []
    it "Shorter than for a list that does not contain any words shorter than 5" $
        shorterThan 5 ["Hellos", "Gallows", "Basket"] `shouldBe` []
    it "Shorter than for a list that all the words are shorter than 4" $
        shorterThan 4 ["Yes", "No", "A", "And", "The"] `shouldBe` ["Yes", "No", "A", "And", "The"]

  describe "Remove Multiples Function" $ do
    it "Remove multiples from the list with no multiples" $
        removeMultiples 9 [4, 6, 16] `shouldBe` [4, 6, 16]
    it "Remove multiples from the list that is empty" $
        removeMultiples 2 [] `shouldBe` []
    it "Remove multiples from the list with multiples" $
        removeMultiples 4 [4, 6, 16, 16, 2, 5, 6, 4] `shouldBe` [6,2,5,6]
    it "Remove multiples from the list with multiples" $
        removeMultiples 2 [2,2] `shouldBe` []

  describe "Only Just Function" $ do
    it "Only just for the list [Nothing, Just 5, Nothing, Just 10] should be [Just 5, Just 10]" $
        onlyJust [Nothing, Just 5, Nothing, Just 10] `shouldBe` [5, 10]
    it "Returns the same list when there are no 'Nothing' values" $
        onlyJust [Just 1, Just 2, Just 3] `shouldBe` [1, 2, 3]
    it "Removes all 'Nothing' values from the list" $
        onlyJust [Nothing, Just 1, Nothing, Just 2, Nothing, Just 3, Nothing] `shouldBe` [1, 2, 3]
    it "should return the original list when all values are Just" $ do
        onlyJust [Just 'a', Just 'b', Just 'c'] `shouldBe` ['a','b', 'c']
    it "should return the original list when there are different types" $ do
        onlyJust [Just 'a', Just '1', Just 'c'] `shouldBe` ['a','1', 'c']
    
    