{-# LANGUAGE ScopedTypeVariables #-}

module Tree where

import Classes

{-
    The binary tree implementation from
    'http://learnyouahaskell.com/making-our-own-types-and-typeclasses'.
    
    Instantiate the following classes with the tree type:
    * 'Show'
    * 'Container'
    * 'Invertible'
    
    A possible String representation of a binary tree such as
    
                    4                       4
                   / \                          2
                  2   5     might be                1
                 / \                                3
                1   3                           5
    
    The inversion should be performed DEEPLY i.e., for the elements as well.
-}

data Tree a
    = EmptyTree
    | Node a (Tree a) (Tree a)
    deriving (Read, Eq)

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False  
treeElem x (Node a left right)
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList = foldr treeInsert EmptyTree

instance Functor Tree where  
    fmap _ EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x)
                                            (fmap f leftsub)
                                            (fmap f rightsub)
instance (Show a) => Show (Tree a) where
  show EmptyTree = "Null"
  show (Node a left right) =  "(" ++ show left ++ " - " ++ show a ++ " - " ++ show right ++ ")"

instance Container Tree where
    contents EmptyTree = []
    contents (Node a left right) = (a : contents left) ++ contents right

instance Invertible (Tree a) where
    invert EmptyTree = EmptyTree
    invert (Node a left right) = Node a (invert right) (invert left)