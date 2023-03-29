-- file: Spec.hs
module DailyOneSpec where
import Test.Hspec
import DailyOne

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  describe "Triple Distance" $ do
    it "finds distance between 2 points in three-dimentions (0, 2, 3) (4, 2, 4) is 4.123106" $
        tripleDistance (0, 2, 3) (4, 2, 4) `shouldBe` 4.123105625617661
    it "finds distance between 2 points in three-dimentions (1, 1, 1) (1, 1, 1) is 0" $
      tripleDistance (1, 1, 1) (1, 1, 1) `shouldBe` 0
    it "finds distance between 2 points in three-dimentions (3.2, 2.3, 5.6) (3.3, 1.8, 9.4) is 3.834058" $
      tripleDistance (3.2, 2.3, 5.6) (3.3, 1.8, 9.4) `shouldBe` 3.834057902536163
    it "finds distance between 2 points in three-dimentions (1, 3, 9) (2, 4, 8) is 1.732051" $
      tripleDistance (1, 3, 9) (2, 4, 8) `shouldBe` 1.7320508075688772
  describe "Vector Scale" $ do
    it "finds the scaled vector of (0, 2) by value of 3 is (0, 6)" $
      scaleVector 3 (0,2) `shouldBe` (0,6)
    it "finds the scaled vector of (4, 10) by value of 2 is (8, 20)" $
      scaleVector 2 (4,10) `shouldBe` (8,20)
    it "finds the scaled vector of (5.6, 2.4) by value of 8.2 is (45.92, 19.68)" $
      scaleVector 8.2 (5.6,2.4) `shouldBe` (45.919999999999995,19.679999999999996)
    it "finds the scaled vector of (1.5, 1.5) by value of 5 is (7.5, 7.5)" $
      scaleVector 5 (1.5,1.5) `shouldBe` (7.5,7.5)
  describe "Daily One" $ do
    it "finds the value using the quadratic formula of 5, 3, 7, 9 is 599" $
      quadratic 5.0 3.0 7.0 9.0 `shouldBe` 599.0
    it "finds the value using the quadratic formula of 1, 1, 1, 1 is 3" $
      quadratic 1.0 1.0 1.0 1.0 `shouldBe` 3.0
    it "finds the value using the quadratic formula of 11.5, 3.2, 68.5, 1.5 is 170.425" $
      quadratic 11.5 3.2 68.5 1.5 `shouldBe` 170.425
    it "finds the value using the quadratic formula of 30, 12, 2, 3 is 84" $
      quadratic 30.0 12.0 2.0 3.0 `shouldBe` 84.0

    






