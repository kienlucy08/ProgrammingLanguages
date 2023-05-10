module WeeklyAssignmentThreeSpec where
import Test.Hspec
import WeeklyAssignmentThree

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  describe "Vec" $ do
    let v1 = Vec [1.0, 2.0, 3.0, 4.0]
        v2 = Vec [1.0, 2.0, 3.0, 4.0]
        v3 = Vec [1.0, 2.0, 3.0, 5.0]

    describe "Eq" $ do
      it "v1 == v2 should be True" $
        v1 == v2 `shouldBe` True
      it "v1 == v3 should be False" $
        v1 == v3 `shouldBe` False  
      it "v2 == v3 should be True" $
        v2 == v3 `shouldBe` False

    describe "Ord" $ do
      it "v1 < v3 should be True" $
        v1 < v3 `shouldBe` True
      it "v1 <= v2 should be True" $
        v1 <= v2 `shouldBe` True
      it "v1 > v2 should be False" $
        v1 > v2 `shouldBe` False

    describe "VecT" $ do
      let v1 = Vec [1.0, 2.0, 3.0]
          v2 = Vec [4.0, 5.0, 6.0]
          v3 = Vec [0.0, 0.0, 0.0]
          mag1 = magnitude v1
          mag2 = magnitude v2
          mag3 = magnitude v3

      it "magnitude v1 should be 3.7416573867739413" $
        mag1 `shouldBe` 3.7416573867739413
      it "magnitude v2 should be 8.774964387392123" $
        mag2 `shouldBe` 8.774964387392123
      it "magnitude v3 should be 0.0" $
        mag3 `shouldBe` 0.0


    describe "Semigroup" $ do
      let v5 = Vec [1.0, 2.0, 3.0]
          v6 = Vec [2.0, 3.0, 4.0]
          v7 = v5 <> v6

      it "v7 should be Vec [3.0,5.0,7.0]" $
        v7 `shouldBe` Vec [3.0,5.0,7.0]
    
      let v8 = Vec [1.0, 1.0, 1.0]
          v9 = Vec [2.0, 2.0, 2.0]
          v10 = Vec [3.0, 3.0, 3.0]
          v11 = v8 <> v9 <> v10

      it "v11 should be Vec [6.0,6.0,6.0]" $
        v11 `shouldBe` Vec [6.0,6.0,6.0]
    
      let v12 = Vec [0.0]
          v13 = Vec [0.0]
          v14 = v12 <> v13

      it "v14 should be Vec [0.0]" $
        v14 `shouldBe` Vec [0.0]

    describe "Monoid" $ do
      let v8 = mempty `asTypeOf` v1
          v9 = v8 <> v1

      it "v8 should be Vec [0.0,0.0,0.0,0.0]" $
          v8 `shouldBe` Vec [0.0,0.0,0.0,0.0]
      it "v9 should be  [1.0, 2.0, 3.0, 4.0]" $
          v9 `shouldBe` v1

      let v10 = Vec [1.0, 2.0, 3.0]
          v11 = v10 <> mempty

      it "v11 should be [1.0, 2.0, 3.0]" $
          v11 `shouldBe` v10
