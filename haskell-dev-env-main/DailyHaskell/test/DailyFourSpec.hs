
module DailyFourSpec where
import Test.Hspec
import DailyFour
import Control.Exception

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  describe "Zip Three Lists Function" $ do
    it "Zips three lists together to produce a list of tuples consecutively: [1,2,3]['a','b','c'][4,5,6] should be [(1, 'a', 4), (2, 'b', 5), (3, 'c', 6)]" $
        zip3Lists [1, 2, 3] ['a', 'b', 'c'][4, 5, 6] `shouldBe`[(1, 'a', 4), (2, 'b', 5), (3, 'c', 6)]
    it "Zips three lists together to produce a list of tuples consecutively: ['foo','bar','baz']['hello','world','!']['a','b','c'] should be [('foo', 'hello', 'a'), ('bar', 'world', 'b'), ('baz', '!', 'c')]" $
        zip3Lists ["foo", "bar", "baz"] ["hello", "world", "!"] ["a", "b", "c"] `shouldBe` [("foo", "hello", "a"), ("bar", "world", "b"), ("baz", "!", "c")]
    it "Zips three lists together to produce a list of tuples consecutively: [1, 2, 3] ['foo', 'bar', 'baz'] [True, False, True] should be  [(1, 'foo', True), (2, 'bar', False), (3, 'baz', True)]" $
        zip3Lists [1, 2, 3] ["foo", "bar", "baz"] [True, False, True]`shouldBe` [(1, "foo", True), (2, "bar", False), (3, "baz", True)]
    it "Zips three lists together to produce a list of tuples consecutively: [1, 1, 1] ['a', 'a', 'a'][4, 4, 4] should be [(1, 'a', 4), (1, 'a', 4), (1, 'a', 4)]" $
        zip3Lists [1, 1, 1] ['a', 'a', 'a'][4, 4, 4] `shouldBe`[(1, 'a', 4), (1, 'a', 4), (1, 'a', 4)]
    it "Zips three empty lists together" $
        zip3Lists [""] [""] [""] `shouldBe` [("","","")]
  describe "Unzip Triples function" $ do
    it "Unzips triples into a tuple containing three lists : [(1,2,3), (4, 5, 6), (7, 8, 9)] should be ([1,4,7], [2, 5, 8], [3, 6, 9])." $
        unzipTriples [(1,2,3),(4,5,6),(7,8,9)] `shouldBe` ([1,4,7],[2,5,8],[3,6,9])
    it "Unzips triples into a tuple containing three lists :  [(1, 'a', True)]  should be  ([1], ['a'], [True]) ." $
        unzipTriples [(1, "a", True)] `shouldBe` ([1], ["a"], [True])   
    it "Unzips triples into a tuple containing three lists : [('foo', 42, 3.14), ('bar', 42, 2.71)] should be (['foo', 'bar'], [42, 42], [3.14, 2.71])." $
        unzipTriples [("foo", 42, 3.14), ("bar", 42, 2.71)] `shouldBe` (["foo", "bar"], [42, 42], [3.14, 2.71])    
    it "uUnzips triples into a tuple containing three lists : [(True, 1, 'a'), (False, 2, 'b'), (True, 3, 'c')] should be ([True, False, True], [1, 2, 3], ['a', 'b', 'c'])." $
        unzipTriples [(True, 1, 'a'), (False, 2, 'b'), (True, 3, 'c')] `shouldBe` ([True, False, True], [1, 2, 3], ['a', 'b', 'c'])
  describe "mergeSorted3 function" $ do
    it "merges three sorted lists in increasing order" $
        mergeSorted3 [2, 3, 5] [1, 8] [-1, 0, 4, 10] `shouldBe` [-1, 0, 1, 2, 3, 4, 5, 8, 10]
    it "handles empty lists" $
        mergeSorted3 [] [] [1,2,3] `shouldBe` [1,2,3]
    it "merges one empty list with two non-empty sorted lists" $
        mergeSorted3 [1, 2, 3] [] [4, 5, 6] `shouldBe` [1, 2, 3, 4, 5, 6]
    it "merges two empty lists with one non-empty sorted list" $
        mergeSorted3 [] [1, 2, 3] [] `shouldBe` [1, 2, 3]
    it "merges three sorted lists with overlapping elements" $
        mergeSorted3 [1, 2, 3, 5, 7] [3, 4, 5, 6] [5, 7, 8] `shouldBe` [1, 2, 3, 3, 4, 5, 5, 5, 6, 7, 7, 8]
    it "merges three sorted lists with duplicate elements" $
        mergeSorted3 [1, 2, 2, 3, 5, 5] [2, 3, 4, 5, 6] [4, 5, 5, 7, 8] `shouldBe` [1, 2, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 5, 6, 7, 8]
    it "merges three sorted lists with negative elements" $
        mergeSorted3 [-5, -3, -1] [-4, -2, 0] [-3, -2, -1, 1, 3] `shouldBe` [-5, -4, -3, -3, -2, -2, -1, -1, 0, 1, 3]



    
    

