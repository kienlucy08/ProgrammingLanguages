module DailyNineSpec where
import Test.Hspec
import DailyNine

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  describe "Find Smallest Function" $ do
    it "should return Just 1 for a list with a single element" $
        findSmallest [1] `shouldBe` Just 1
    it "should return Just -5 for a list with negative values" $
        findSmallest [3, -5, 7, 1] `shouldBe` Just (-5)
    it "should return Just 'a' for a list of characters" $
        findSmallest "hello" `shouldBe` Just 'e'
    it "should return Just 3.14 for a list of floating point numbers" $
        findSmallest [5.6, 7.2, 3.14, 9.1] `shouldBe` Just 3.14

  describe "All True Function" $ do
    it "should return Nothing for an empty list" $
        allTrue [] `shouldBe` Nothing
    it "should return Just True for a list with all True values" $
        allTrue [True, True, True] `shouldBe` Just True
    it "should return Just False for a list with all False values" $
        allTrue [False, False, False] `shouldBe` Just False
    it "should return Just False for a list with mixed True and False values" $
        allTrue [True, False, True] `shouldBe` Just False

  describe "Count All Votes Function" $ do
    it "should return (0,0,0) for an empty list" $
        countAllVotes [] `shouldBe` (0,0,0)
    it "should count one representative who has not yet voted" $
        countAllVotes [Nothing] `shouldBe` (1,0,0)
    it "should count one representative who voted in favour" $
        countAllVotes [Just True] `shouldBe` (0,1,0)
    it "should count one representative who voted against" $
        countAllVotes [Just False] `shouldBe` (0,0,1)
    it "should count representatives with mixed votes" $
        countAllVotes [Just True, Nothing, Just False, Nothing, Just True] `shouldBe` (2,2,1)