-- file: Spec.hs
module DailyTwoSpec where
import Test.Hspec
import DailyTwo
import Control.Exception

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  describe "Every 4th" $ do
    it "Return every 4th element in the list [1,2,3,4,5,6,7,8] returns [4,8]" $
        every4th [1,2,3,4,5,6,7,8] `shouldBe` [4,8]
    it "Return every 4th element in the list ['yes','hello','the','a','sound','is','gone'] should be ['a']" $
        every4th ["yes", "hello", "the", "a", "sound", "is", "gone"] `shouldBe` ["a"]
    it "Return every 4th element in the list ['a','b','c','d','e','f','g','h'] should be ['d', 'h']" $
        every4th ['a','b','c','d','e','f','g','h'] `shouldBe` ['d', 'h']
    it "Return every 4th element in the list [33,52,124,343,222,785,334,23,5342,543,67,322,7843,29] should be [343,23,322]" $
        every4th [33,52,124,343,222,785,334,23,5342,543,67,322,7843,29] `shouldBe` [343,23,322]
    it "Return every 4th element in the list [2.3,1.1,4.5,2.3,7.7,7.7,3.2,4.2] should be [2.3,4.2]" $
        every4th [2.3,1.1,4.5,2.3,7.7,7.7,3.2,4.2] `shouldBe` [2.3,4.2]
    it "Return every 4th element in the list [0,0,2] should be []" $
        every4th [0,0,2] `shouldBe` []
    it "Return every 4th element in the list [0,0,0,0,0,0,0,0,0,0,0,0] should be [0,0,0]" $
        every4th [0,0,0,0,0,0,0,0,0,0,0,0] `shouldBe` [0,0,0]

  describe "Tuple Dot Quotient" $ do
    it "Finds the dot quotient of two lists, [1,4,2,5] [2,7,8,3] return 2.988095238095238" $
        tupleDotQuotient [1,4,2,5] [2,7,8,3] `shouldBe` 2.988095238095238
    it "Finds the dot quotient of two lists, [] [] returns 0" $
        tupleDotQuotient [] [] `shouldBe` 0.0
    it "Finds the dot quotient of two lists, [1.2,4.4] [3.2,1.8] should return 2.8194444444444446" $
        tupleDotQuotient [1.2,4.4] [3.2,1.8] `shouldBe` 2.8194444444444446
    it "Finds the dot quotient of two lists, [4.4,1.2][1.8,3.2] should return 2.8194444444444446" $
        tupleDotQuotient [4.4,1.2][1.8,3.2] `shouldBe` 2.8194444444444446
    it "Finds the dot quotient of two lists, [0,0][1] should throw an exception because the lists are different sizes" $
        evaluate(tupleDotQuotient [0,0][1]) `shouldThrow` anyException

  describe "Append to Each" $ do
    it "Append a string to the end of every string in a list: !!! to ['Hello', 'yes', 'cool'] should be ['Hello!!!', 'yes!!!', 'cool!!!']" $
        appendToEach "!!!" ["Hello", "yes", "cool"] `shouldBe`  ["Hello!!!", "yes!!!", "cool!!!"]  
    it "Append a string to the end of every string in a list: 10 to [1,2,3,4,5,6,7] should be [110,210,310,410,510,610,710]" $
        appendToEach "10" ["1","2","3","4","5","6","7"] `shouldBe`  ["110","210","310","410","510","610","710"]  
    it "Append a string to the end of every string in a list: '' to ['Hello', 'yes', 'cool'] should be ['Hello', 'yes', 'cool']" $
        appendToEach "" ["Hello", "yes", "cool"] `shouldBe`  ["Hello", "yes", "cool"]  
    it "Append a string to the end of every string in a list: ' it's me' to ['Hello', 'yes', 'cool'] should be ['Hello it's me', 'yes it's me', 'cool it's me']" $
        appendToEach " it's me" ["Hello", "yes", "cool"] `shouldBe`  ["Hello it's me", "yes it's me", "cool it's me"]  
    it "Append a string to the end of every string in a list: ! to [] should be []" $
        appendToEach "!" [] `shouldBe`  []
    it "Append a string to the end of every string in a list: " $
        appendToEach "goodbye" [""] `shouldBe` ["goodbye"]

  describe "To Set List" $ do
    it "Take a list and convert it into set representation: [1,1,4,2,7,5,4,7] should be [1,4,2,7,5]" $
        toSetList [1,1,4,2,7,5,4,7] `shouldBe` [1,4,2,7,5]
    it "Take a list and convert it into set representation: ['hello', 'yes', 'this', 'a', 'yes'] should be ['hello, 'yes', 'this', 'a']" $
        toSetList ["hello", "yes", "this", "a", "yes"] `shouldBe` ["hello", "yes", "this", "a"]
    it "Take a list and convert it into set representation: [0,0,0,0,0] should be [0]" $
        toSetList [0,0,0,0,0] `shouldBe` [0]
    it "Take a list and convert it into set representation: [2.3,1.1,4.5,2.3,7.7,7.7] should be [2.3,1.1,4.5,7.7]" $
        toSetList [2.3,1.1,4.5,2.3,7.7,7.7] `shouldBe` [2.3,1.1,4.5,7.7]
    it "Take a list and convert it into set representation: [33,52,124,343,222,785,334,23,5342,543,67,322,7843,29] should be [33,52,124,343,222,785,334,23,5342,543,67,322,7843,29]" $
        toSetList [33,52,124,343,222,785,334,23,5342,543,67,322,7843,29] `shouldBe` [33,52,124,343,222,785,334,23,5342,543,67,322,7843,29]
    it "Take a list and convert it into set representation: ['hello', 'a', 'yes', 'a'] should be ['hello','a','yes']" $
        toSetList ["hello", "a", "yes", "a"] `shouldBe` ["hello", "a", "yes"]
    it "Take a lsit and convert it into set representation: [''] should be ['']" $
        toSetList [""] `shouldBe` [""]
    
      