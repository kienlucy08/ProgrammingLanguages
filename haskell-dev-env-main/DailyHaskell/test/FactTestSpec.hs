-- file: Spec.hs
module FactTestSpec where
import Test.Hspec
import Factorial

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  describe "factorial" $ do
    it "produces the factorial of 0" $
      (factOne 0) `shouldBe` 1
    it "produces the factorial of 1" $
      (factOne 1) `shouldBe` 1
    it "produces the factorial of 5" $
      (factOne 5) `shouldBe` 120
    it "produces the factorial of 30" $
      (factOne 30) `shouldBe` 265252859812191058636308480000000










