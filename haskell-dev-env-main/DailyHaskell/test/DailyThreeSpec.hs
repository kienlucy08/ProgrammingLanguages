-- file: Spec.hs
module DailyThreeSpec where
import Test.Hspec
import DailyThree

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  describe "Removes all except function" $ do
    it "Removes all the element except for 'a' from the list ['a','b','c','d','e','f']" $
        removeAllExcept 'a' ['a','b','c','d','e','f'] `shouldBe` ['a']