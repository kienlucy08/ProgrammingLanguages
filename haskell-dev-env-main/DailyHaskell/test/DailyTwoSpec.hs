-- file: Spec.hs
module DailyTwoSpec where
import Test.Hspec
import DailyTwo

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  describe "Every 4th" $ do
    it "Return every 4th element in the list [1,2,3,4,5,6,7,8] returns [4,8]" $
        every4th [1,2,3,4,5,6,7,8] `shouldBe` [4,8]

  describe "Tuple Dot Quotient" $ do
    it "Finds the dot product of two list, [1,4,2,5] [2,7,8,3] return []" $
        tupleDotQuotient [1,4,2,5] [2,7,8,3] `shouldBe` 12.0
    
      