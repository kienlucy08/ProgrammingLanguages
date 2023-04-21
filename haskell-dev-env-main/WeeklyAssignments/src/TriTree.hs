-- Lucy Kien
-- Weekly Assignment Two
-- Programming Languges 

module TriTree where

    -- defining the data used
    --a tree with no values is simply the Empty tree
    --a tree with one value is a Leaf with the single value
    --a tree with two values is a Node with the two values in ascending order and three empty subtrees as children
    --a tree with more than two values is a combination of these nodes
    data TriTree a = Empty | 
                 Leaf a | 
                 Node a a (TriTree a) (TriTree a) (TriTree a) 
                 deriving (Eq,Show)


    -- search function that consumes a value and a TriTree
    -- Produces a true if the value is stored in the tree and a false otherwise
    search :: Ord a => a -> TriTree a -> Bool
    search _ Empty = False -- base case: value not found in empty tree
    search x (Leaf y) = x == y -- base case: value found in leaf node
    search x (Node a b left middle right)
        | x == a || x == b = True -- value found in current node
        | x < a = search x left -- value is in left subtree
        | b < x = search x right -- value is in right subtree
        | otherwise = search x middle -- value is in middle subtree

    -- insert function that consumes a value and a TriTree
    -- Produces a new TriTree with the given value in the correct location
    -- The Tree does not need to be balanced after insertion *
    insert :: Ord a => a -> TriTree a -> TriTree a
    insert x Empty = Leaf x
    insert x (Leaf y)
        | x == y = Leaf y
        | otherwise = insert' x y
    insert x (Node y z l m r)
        | x == y = Node y z l m r
        | x < y = Node y z (insert x l) m r
        | x > y = Node y z l m (insert x r)
        | x <= z     = Node y z l (insert x m) r
        | otherwise = Node y z l m (insert x r)

    -- Helper function for insert that helps with leaf functions
    --Consumes two values and produces a tri tree with this value inserted
    insert' :: Ord a => a -> a -> TriTree a
    insert' x y
        | x < y = Node x y (Leaf x) Empty Empty
        | x > y = Node y x Empty Empty (Leaf x)
        | otherwise = Leaf y  


    -- Insert list function that consumes a list of values and a TriTree
    -- Produces a new TriTree with the inserted list
    -- use foldr
    insertList :: Ord a => [a] -> TriTree a -> TriTree a
    insertList xs t = foldr insert t xs

    -- Identical which consumes two TriTrees
    -- Produces a boolean true if they are idential and false otherwise
    identical :: Eq a => TriTree a -> TriTree a -> Bool
    identical Empty Empty = True
    identical (Leaf x) (Leaf y) = x == y
    identical (Node x1 x2 l1 m1 r1) (Node y1 y2 l2 m2 r2) =
        x1 == y1 && x2 == y2 && identical l1 l2 && identical m1 m2 && identical r1 r2
    identical _ _ = False
    
    -- Tree map function which consumes a function: f :: a -> b, and a TriTree
    -- Produces a new TriTree which applies f to each value in the tree
    treeMap :: (a -> b) -> TriTree a -> TriTree b
    treeMap _ Empty = Empty
    treeMap f (Leaf x) = Leaf (f x)
    treeMap f (Node x1 x2 l m r) = Node (f x1) (f x2) (treeMap f l) (treeMap f m) (treeMap f r)
    
    -- Tree fold pre order which consumes a function, f :: a -> a -> a, an initial value, and a TriTree
    -- Produces the result of using f to combine values in the TriTree
    -- Note: The left-most value in the node should be processed first, then the right-most value in the node, and finally the left, middle, and right subtrees should be processed.
    treeFoldPreOrder :: (a -> a -> a) -> a -> TriTree a -> a
    treeFoldPreOrder f z Empty = z
    treeFoldPreOrder f z (Leaf x) = f z x
    treeFoldPreOrder f z (Node x y left middle right) =
        let z' = f z x
            z'' = treeFoldPreOrder f z' left
            z''' = treeFoldPreOrder f z'' right
        in f (treeFoldPreOrder f z''' middle) y

    
    -- Tree fold in order which consumes a function: f :: a -> a -> a, an initial value, and a TriTree. 
    -- The function should produce the result of using f to combine values in the TriTree. 
    -- Note: The values in the left subtree should be processed first, then the left-most value, followed by the middle subtree, and then the right-most value, and then the right subtree.
    treeFoldInOrder :: (a -> a -> a) -> a -> TriTree a -> a
    treeFoldInOrder f z Empty = z
    treeFoldInOrder f z (Leaf x) = f z x
    treeFoldInOrder f z (Node x y left middle right) =
        let z' = treeFoldInOrder f z left
            z'' = f z' x
            z''' = treeFoldInOrder f z'' middle
        in f (treeFoldInOrder f z''' right) y
    

    -- Tree fold post order which consumes a function:  f :: a -> a -> a, an initial value, and a TriTree. 
    -- Produces the result of using f to combine values in the TriTree.
    -- Note: The values in the subtrees should be processed first, followed by the left and right values stored in the node.
    treeFoldPostOrder :: (a -> a -> a) -> a -> TriTree a -> a
    treeFoldPostOrder f z Empty = z
    treeFoldPostOrder f z (Leaf x) = f z x
    treeFoldPostOrder f z (Node x y left middle right) =
        let z' = treeFoldPostOrder f z left
            z'' = treeFoldPostOrder f z' middle
            z''' = treeFoldPostOrder f z'' right
        in f (f z''' x) y







    
