module DailyFiveSpec where
import Test.Hspec
import DailyFive

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  describe "Multiple Pairs function" $ do
    it "Multipy pairs together: [(1,2),(2,5)] should be [2,10]" $
        multPairs [(1,2),(2,5)] `shouldBe` [2,10]
    it "Multipy pairs together: [] should be []" $
        multPairs [] `shouldBe` []
    it "Multipy pairs together: [(0,0),(1,1),(2,2)] should be [0,1,4]" $
        multPairs [(0,0),(1,1),(2,2)] `shouldBe` [0,1,4]
    it "Multipy pairs together: [(4,6),(-2,8),(3,-1)] should be [24,-16,-3]" $
        multPairs [(4,6),(-2,8),(3,-1)] `shouldBe` [24,-16,-3]

  describe "Square List function" $ do
    it "Square list of list: [4,9,16] should be [(2,4),(3,9),(4,16)]" $
        squareList [4,6,16] `shouldBe` [(4, 16),(6,36),(16,256)]
    it "Square list of list: [] should be []" $
        squareList [] `shouldBe` []
    it "Square list of list: [0,1,2] should be [(0,0),(1,1),(2,4)]" $
        squareList [0,1,2] `shouldBe` [(0,0),(1,1),(2,4)]
    it "Square list of list: [-3,4,-2] should be [(-3,9),(4,16),(-2,4)]" $
        squareList [-3,4,-2] `shouldBe` [(-3,9),(4,16),(-2,4)]

  describe "Find Lower Case function" $ do
    it "Find lower case: ['a','hEllo','YES'] should be [True, True, False]" $
        findLowercase ["a", "hEllo", "YES"] `shouldBe` [True, True, False]
    it "Find lower case: [] should be []" $
        findLowercase [] `shouldBe` []  
    it "Find lower case: ['hello', 'World'] should be [True, False]" $
        findLowercase ["hello", "World"] `shouldBe` [True, False]
    it "Find lower case: ['This', 'is', 'A', 'TEST'] should be [False, true, False, False]" $
        findLowercase ["This", "is", "A", "TEST"] `shouldBe` [False, True, False, False]
    it "Find lower case: ['abc', 'DEF', 'gHi'] should be [True, False, True]" $
        findLowercase ["abc", "DEF", "gHi"] `shouldBe` [True, False, True]



    
    

