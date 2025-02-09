module Basics where  -- (10p)

{-
    1. (1p)
    Implement the 'reverse' function, which returns a given list
    in reverse order, using explicit recursion.
    Do NOT employ any higher-order functions.
-}

reverseRec1 :: [a] -> [a]
reverseRec1 [] = []
reverseRec1 (x : xs) = reverseRec1 xs ++ [x]

{-
    2. (1p)
    Same as (1), but change the direction on which the output is built.
    For example, if (1) built the list on return from recursion,
    you should now built the list when recursing forward.
-}

reverseRec2 :: [a] -> [a]
reverseRec2 l = reverseRec2Acc [] l

reverseRec2Acc :: [a] -> [a] -> [a]
reverseRec2Acc acc [] = acc
reverseRec2Acc acc (x : xs) = reverseRec2Acc (x : acc) xs 


{-
    3. (1.5p)
    Same as (1), but use a higher-order function instead.
    Do NOT employ explicit recursion.
    Make sure that your solution faithfully reflects the process from (1).
-}

reverseHO1 :: [a] -> [a]
reverseHO1 = foldr (\x acc -> acc ++ [x]) []

{-
    4. (1.5p)
    Same as (2), but use a higher-order function instead.
    Do NOT employ explicit recursion.
    Make sure that your solution faithfully reflects the process from (2).
-}

reverseHO2 :: [a] -> [a]
reverseHO2 = foldl (\acc x -> x : acc) []

{-
    5. (1p)
    Implement the power set function, which returns the set of all subsets
    of a set, using explicit recursion.
-}

powerSetRec :: [a] -> [[a]]
powerSetRec [] = [[]]
powerSetRec (x : xs) = [x : ps | ps <- powerSetRec xs] ++ powerSetRec xs

{-
    6. (1.5p)
    Same as (5), but use higher-order functions instead.
    Do NOT employ explicit recursion.
    Make sure that your solution faithfully reflects the process from (5).
-}

powerSetHO :: [a] -> [[a]]
powerSetHO [] = [[]]
powerSetHO (x : xs) = map (x:) (powerSetHO xs) ++ powerSetHO xs

{-
    7. (0.5p)
    Compute the cartesian product of two lists, using list comprehensions.
-}

cartesian2 :: [a] -> [b] -> [(a, b)]
cartesian2 l1 l2 = [(x, y) | x <- l1, y <- l2]

{-
    8. (2p)
    Similar to (7), but extend to any number of lists.
-}

cartesian :: [[a]] -> [[a]]
cartesian lists = foldr (\xs as -> [x : a | x <- xs, a <- as]) [[]] lists