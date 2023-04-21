module TriTreeSpec where
import Test.Hspec
import TriTree
import Data.List(sort)

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  describe "Search Function" $ do
    it "Search the TriTree when it is empty" $
        search 1 Empty `shouldBe` False
    it "Search the TriTree for a value in a leaf node: 'hello' in Leaf('hello') should be True" $
        search "hello" (Leaf "hello") `shouldBe` True
    it "Search the TriTree for a value in a leaf node: '3' in Leaf(5) should be False" $
        search 3 (Leaf 5) `shouldBe` False
    it "Search the TriTree for a value in a leaf node in left subtree: '1' in Node 5 10 (Leaf 1) (Leaf 7) (Leaf 15) should be True" $
        search 1 (Node 5 10 (Leaf 1) (Leaf 7) (Leaf 15)) `shouldBe` True
    it "Search the TriTree for a value in a leaf node in right subtree: '15' in Node 5 10 (Leaf 1) (Leaf 7) (Leaf 15) should be True" $
        search 15 (Node 5 10 (Leaf 1) (Leaf 7) (Leaf 15)) `shouldBe` True
    it "Search the TriTree for a value in a leaf node in the middle subtree: '7' in Node 5 10 (Leaf 1) (Leaf 7) (Leaf 15) should be True" $
        search 7 (Node 5 10 (Leaf 1) (Leaf 7) (Leaf 15)) `shouldBe` True
    it "Search the TriTree for a vlaue in a leaf node: '20' in Node 5 10 (Leaf 1) (Leaf 7) (Leaf 15) should be False" $
        search 20 (Node 5 10 (Leaf 1) (Leaf 7) (Leaf 15)) `shouldBe` False
    it "Search the TriTree for a value in a leaf node for a more complex tree: '2' in Node 5 15 (Node 1 3 (Leaf (-5)) (Leaf 2) (Empty)) (Leaf 8) (Node 20 30 (Leaf 17) (Leaf 25) (Empty)) should be True" $
        search 2 (Node 5 15 (Node 1 3 (Leaf (-5)) (Leaf 2) (Empty)) (Leaf 8) (Node 20 30 (Leaf 17) (Leaf 25) (Empty))) `shouldBe` True
    
  describe "Insert Function" $ do
    it "Inserts into an empty TriTree" $
        insert 1 Empty `shouldBe` Leaf 1  
    it "Inserts into a Leaf where x <= y" $
        insert 2 (Leaf 3) `shouldBe` Node 2 3 (Leaf 2) Empty Empty        
    it "Inserts into a Leaf where x > y" $
        insert 4 (Leaf 3) `shouldBe` Node 3 4 Empty Empty (Leaf 4)   
    it "Inserts into a Node where x <= y" $
        insert 2 (Node 3 1 (Leaf 2) Empty (Leaf 5)) `shouldBe` Node 3 1 (Leaf 2) Empty (Leaf 5)  
    it "Inserts into a Node where x <= z" $
        insert 3 (Node 2 4 (Leaf 1) Empty (Leaf 5)) `shouldBe` Node 2 4 (Leaf 1) Empty (Node 3 5 (Leaf 3) Empty Empty) 
    it "Inserts into a Node where x > z" $
        insert 6 (Node 2 4 (Leaf 1) Empty (Leaf 5)) `shouldBe` Node 2 4 (Leaf 1) Empty (Node 5 6 Empty Empty (Leaf 6))

  describe "Insert List Function" $ do
    it "should insert a list of values into an empty tree" $
        insertList [1,2,3,4,5] Empty `shouldBe` Node 4 5 (Node 3 4 (Node 2 3 (Node 1 2 (Leaf 1) Empty Empty) Empty Empty) Empty Empty) Empty Empty
    it "should insert a list of values in descending order" $
        insertList [5,4,3,2,1] Empty `shouldBe` Node 1 2 Empty Empty (Node 2 3 Empty Empty (Node 3 4 Empty Empty (Node 4 5 Empty Empty (Leaf 5))))
    it "should insert a list of values with duplicates" $
        insertList [1,2,2,3,3,3] Empty `shouldBe` Node 2 3 (Node 1 2 (Leaf 1) Empty Empty) Empty Empty
    it "should insert a list of values into a non-empty tree" $
        insertList [4,5,6] (Node 2 3 (Leaf 1) Empty (Leaf 3)) `shouldBe`  Node 2 3 (Leaf 1) Empty (Node 3 6 Empty Empty (Node 5 6 (Node 4 5 (Leaf 4) Empty Empty) Empty Empty))
    
  describe "Idential Function" $ do
    it "returns True for identical trees" $ do
      let tree1 = Node 3 2 (Leaf 1) Empty Empty
          tree2 = Node 3 2 (Leaf 1) Empty Empty
      identical tree1 tree2 `shouldBe` True     
    it "returns False for different trees" $ do
      let tree1 = Node 3 2 (Leaf 1) Empty Empty
          tree2 = Node 4 2 (Leaf 1) Empty Empty
      identical tree1 tree2 `shouldBe` False
    it "returns False for trees with different structures" $ do
      let tree1 = Node 3 2 (Leaf 1) Empty (Leaf 4)
          tree2 = Node 3 2 (Leaf 1) (Leaf 5) Empty
      identical tree1 tree2 `shouldBe` False   
    it "returns True for identical trees with multiple values" $ do
      let tree1 = Node 3 2 (Leaf 1) (Leaf 2) (Leaf 3)
          tree2 = Node 3 2 (Leaf 1) (Leaf 2) (Leaf 3)
      identical tree1 tree2 `shouldBe` True
    it "returns False for trees with different values" $ do
      let tree1 = Node 3 2 (Leaf 1) (Leaf 2) (Leaf 3)
          tree2 = Node 3 2 (Leaf 1) (Leaf 5) (Leaf 3)
      identical tree1 tree2 `shouldBe` False


  describe "Tree Map Function" $ do
    it "applies the function to each element in the tree" $ do
      let tree = Node 3 2 (Leaf 1) Empty Empty
          f x = x * 2
          expected = Node 6 4 (Leaf 2) Empty Empty
      treeMap f tree `shouldBe` expected
    it "works correctly with an empty tree" $ do
      let tree = Empty
          f x = x * 2
          expected = Empty
      treeMap f tree `shouldBe` expected   
    it "applies the function to each element in a larger tree" $ do
      let tree = Node 5 6 (Node 3 4 (Leaf 1) Empty Empty) (Leaf 7) (Node 8 9 Empty (Leaf 10) Empty)
          f x = x + 1
          expected = Node 6 7 (Node 4 5 (Leaf 2) Empty Empty) (Leaf 8) (Node 9 10 Empty (Leaf 11) Empty)
      treeMap f tree `shouldBe` expected    
    it "works correctly with a tree of strings" $ do
      let tree = Node "apple" "banana" (Leaf "orange") Empty (Leaf "pear")
          f x = "fruit: " ++ x
          expected = Node "fruit: apple" "fruit: banana" (Leaf "fruit: orange") Empty (Leaf "fruit: pear")
      treeMap f tree `shouldBe` expected

  describe "Tree Fold Pre Order Function" $ do
    it "folds over an empty tree and returns the initial value" $ do
      let tree = Empty
          f x y = x + y
          initial = 0
      treeFoldPreOrder f initial tree `shouldBe` initial  
    it "folds over a tree with a single leaf and combines it with the initial value" $ do
      let tree = Leaf 5
          f x y = x + y
          initial = 3
          expected = f initial 5
      treeFoldPreOrder f initial tree `shouldBe` expected
    it "folds over a tree with only left subtrees" $ do
      let tree = Node 2 3 (Node 1 2 (Leaf 3) Empty Empty) Empty Empty
          f x y = x + y
          initial = 0
          expected = f initial 11
      treeFoldPreOrder f initial tree `shouldBe` expected
    it "folds over a tree with only middle subtrees" $ do
      let tree = Node 2 3 Empty (Node 5 1 (Leaf 1) Empty Empty) Empty
          f x y = x + y
          initial = 0
          expected = f initial 12
      treeFoldPreOrder f initial tree `shouldBe` expected
    it "folds over a tree with only right subtrees" $ do
      let tree = Node 2 3 Empty Empty (Node 5 1 (Leaf 1) Empty Empty)
          f x y = x + y
          initial = 0
          expected = f initial 12
      treeFoldPreOrder f initial tree `shouldBe` expected
    it "folds over a tree with both left and right subtrees" $ do
      let tree = Node 3 2 (Leaf 1) (Node 2 1 (Leaf 3) Empty (Leaf 4)) (Leaf 5)
          f x y = x + y
          initial = 0
          expected = f initial 21
      treeFoldPreOrder f initial tree `shouldBe` expected

  describe "Tree Fold in Order Function" $ do
    it "folds over an empty tree and returns the initial value" $ do
      let tree = Empty
          f x y = x + y
          initial = 0
      treeFoldInOrder f initial tree `shouldBe` initial  
    it "folds over a tree with a single leaf and combines it with the initial value" $ do
      let tree = Leaf 5
          f x y = x + y
          initial = 0
          expected = f initial 5
      treeFoldInOrder f initial tree `shouldBe` expected
    it "folds over a tree with only left subtrees" $ do
      let tree = Node 2 3 (Node 1 2 (Leaf 3) Empty Empty) Empty Empty
          f x y = x + y
          initial = 0
          expected = f initial 11
      treeFoldInOrder f initial tree `shouldBe` expected
    it "folds over a tree with only middle subtrees" $ do
      let tree = Node 2 3 Empty (Node 5 1 (Leaf 1) Empty Empty) Empty
          f x y = x + y
          initial = 0
          expected = f initial 12
      treeFoldInOrder f initial tree `shouldBe` expected
    it "folds over a tree with only right subtrees" $ do
      let tree = Node 2 3 Empty Empty (Node 5 1 (Leaf 1) Empty Empty)
          f x y = x + y
          initial = 0
          expected = f initial 12
      treeFoldInOrder f initial tree `shouldBe` expected
    it "folds over a tree with both left and right subtrees" $ do
      let tree = Node 3 2 (Leaf 1) (Node 2 1 (Leaf 3) Empty (Leaf 4)) (Leaf 5)
          f x y = x + y
          initial = 0
          expected = f initial 21
      treeFoldInOrder f initial tree `shouldBe` expected

  describe "Tree Fold Post Order Function" $ do
    it "folds over the tree in post-order" $ do
      let tree = Node 3 2 (Leaf 1) Empty (Leaf 4)
          f x y = x + y
          initial = 0
          expected = f initial 10
      treeFoldPostOrder f initial tree `shouldBe` expected
    it "folds over a tree with only left subtrees in post-order traversal" $ do
      let tree = Node 5 3 (Node 2 1 (Leaf 1) Empty Empty) (Node 1 2 (Leaf 3) Empty Empty) Empty
          f x y = x + y
          initial = 0
          expected = f initial 18
      treeFoldPostOrder f initial tree `shouldBe` expected
    it "folds over a tree with both left and right subtrees in post-order traversal" $ do
      let tree = Node 3 2 (Leaf 1) (Node 2 1 (Leaf 3) Empty (Leaf 4)) (Leaf 5)
          f x y = x + y
          initial = 0
          expected = f initial 21
      treeFoldPostOrder f initial tree `shouldBe` expected
    it "folds over a tree with only one node in post-order traversal" $ do
      let tree = Leaf 5
          f x y = x + y
          intitial = 0
          expected = f intitial 5
      treeFoldPostOrder f intitial tree `shouldBe` expected
    it "folds over an empty tree in post-order traversal" $ do
      let tree = Empty
          f x y = x + y
          expected = 0
      treeFoldPostOrder f 0 tree `shouldBe` expected









