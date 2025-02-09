module NestedList where

import Classes
import List ()

{-
    Define the 'NestedList' type, for handling lists with an arbitrary
    level of intricateness, which would be impossible to specify
    using plain Haskell lists. For instance, the String representation
    of such a nested list might be "[1, [2, 3], 4, 5]".
    
    Instantiate the following classes with the nested list type:
    * 'Show'
    * 'Functor'
    * 'Container'
    * 'Invertible'
    
    The inversion should be performed DEEPLY i.e., for the elements as well.
-}

data NestedList a = Simple [a] | List [NestedList a]

instance Show a => Show (NestedList a) where
    show (Simple x) = show x
    show (List xs) = "[" ++ helper xs ++ "]"
        where
            helper [] = ""
            helper [x] = show x
            helper (x:xs) = show x ++ ", " ++ helper xs

instance Functor NestedList where
    fmap f (Simple x) = Simple (map f x)
    fmap f (List xs) = List (map (fmap f) xs)

instance Container NestedList where
    contents (Simple x) = x
    contents (List xs) = concatMap contents xs

instance Invertible (NestedList a) where
    invert (Simple x) = Simple (reverse x)
    invert (List xs) = List (map invert (reverse xs))
