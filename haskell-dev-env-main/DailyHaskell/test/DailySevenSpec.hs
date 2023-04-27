module DailySevenSpec where
import Test.Hspec
import DailySeven

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  describe "Find Longest Function" $ do
    it "Find the longest first word of equal length" $
        findLongest ["Hellos", "Basket", "Go", "Yes"] `shouldBe` "Basket"
    it "Finds the longest word" $
        findLongest ["apple", "banana", "pear", "pineapple"] `shouldBe` "pineapple"
    it "Finds the longest in an empty list" $
        findLongest [] `shouldBe` ""
    it "Finds the first longest word in a list of length 1" $
        findLongest ["hello"] `shouldBe` "hello"

  describe "Any Larger Functions" $ do
    it "Any larger of the list 5 [1,3,5,4] should be true" $
        anyLarger 5 [1,3,5,4] `shouldBe` True
    it "Any larger of an empty list should be false" $
        anyLarger 2 [] `shouldBe` False
    it "Any larger of a list that does not have a larger value 5 [1,2,3,4]" $
        anyLarger 5 [1,2,3,4] `shouldBe` False

  describe "All Names Function" $ do
    it "All names of [('kermit', 'the frog'), ('bugs', 'bunny')] should be 'kermit the frog, bugs bunny, " $
        allNames [("kermit", "the frog"), ("bugs", "bunny")] `shouldBe` "kermit the frog, bugs bunny, "
    it "All names for an empty list" $
        allNames [] `shouldBe` ""      
    it "All names for a single name" $
        allNames [("Dorothy", "Gale")] `shouldBe` "Dorothy Gale, "      
    it "All names for multiple names" $
        allNames [("kermit", "the frog"), ("bugs", "bunny"), ("elmer", "fudd")] `shouldBe` "kermit the frog, bugs bunny, elmer fudd, "       
    it "All names for names containing spaces" $
        allNames [("john", "smith"), ("mary", "ann"), ("jane", "doe")] `shouldBe` "john smith, mary ann, jane doe, "