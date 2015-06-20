{-
    BinarySearchTree 
    by Russell Bentley
    
    A simple Haskell module that implements a basic binary search tree.
-}

module BinarySearchTree (search, insert, delete, flatten, mergeT, buildTree) where

-- | 'BST' stands for Binary Search Tree.
-- The BST type represents that data structure.
data BST a = Nil | Node a (BST a) (BST a) 

instance Show a => Show (BST a) where
    show Nil = "Nil"
    show (Node x t1 t2) = "Node " ++ show x ++ "(" ++ show t1 ++ ", " ++ show t2 ++ ") "
    
-- | The `mergeL` function takes two sorted lists and merges them.
mergeL :: Ord a => [a] -> [a] -> [a]
mergeL [] ys = ys
mergeL xs [] = xs
mergeL (x:xs) (y:ys) = case compare x y of
                                LT -> x : mergeL xs (y:ys)
                                EQ -> x : mergeL xs ys
                                GT -> y : mergeL ys (x:xs)    
    
-- | The `search` method returns whether a given element is in a BST
search                 :: Ord a => a -> BST a -> Bool
search _ Nil            = False
search x (Node y t1 t2) = case compare x y of
                                LT -> search x t1
                                EQ -> True
                                GT -> search x t2

-- | The `insert` method puts an element into a BST
insert                 :: Ord a => a -> BST a -> BST a
insert x Nil            = Node x Nil Nil
insert x (Node y t1 t2) = case compare x y of
                                LT -> Node y (insert x t1) t2
                                EQ -> Node x t1 t2
                                GT -> Node y t1 (insert x t2)
                                
-- | The `flatten` function returns a sorted list of elements in the BST.
flatten                :: Ord a => BST a -> [a]
flatten Nil             = []
flatten (Node x t1 t2)  = flatten t1 ++ [x] ++ flatten t2

-- | The `buildTree` function takes a sorted list 
buildTree              :: Ord a => [a] -> BST a
buildTree []            = Nil
buildTree xs            = Node (head rs) (buildTree ls) (buildTree $ tail rs)
                            where 
                                len = length xs `div` 2
                                ls = take len xs
                                rs = drop len xs
                               
-- | The `merge` function returns a BST with all the elements of two
mergeT                   :: Ord a => BST a -> BST a -> BST a
mergeT Nil t1            = t1
mergeT t2 Nil            = t2
mergeT t1 t2             = buildTree $ mergeL (flatten t1) (flatten t2)

-- | The `delete` method removes an element from a tree.                         
delete                 :: Ord a => a -> BST a -> BST a
delete _ Nil            = Nil
delete x (Node y t1 t2) = case compare x y of 
                                LT -> Node y (delete x t1) t2
                                GT -> Node y t1 (delete x t2)
                                EQ -> mergeT t1 t2
                                

