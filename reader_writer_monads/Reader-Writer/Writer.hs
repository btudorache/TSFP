module Writer where

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)
{-
    The Writer monad is used for logging information during the unfolding
    of a computation.

    Defined in Control.Monad.Writer.
    See https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Writer-Lazy.html

    The variant in this activity, which is a restricted version of the one found
    in the standard library, uses Strings as log messages. Thus, the suggested
    implementation given below, where a is the result type and the String list
    contains the entire logging history.

    TODO: Replace all the undefined portions below, such that the test run
    for multipleOf3And7 gives the desired output.
-}
newtype Writer a = Writer { runWriter :: (a, [String]) }

instance Functor Writer where
  fmap = liftM

instance Applicative Writer where
  pure  = return
  (<*>) = ap

instance Monad Writer where
    return a = Writer (a, []) 

    Writer (a, l1) >>= f = 
        let Writer (b, l2) = f a
        in Writer (b, l1 ++ l2)

{-
    Logs a list of messages.
-}
tell :: [String] -> Writer ()
tell l = Writer ((), l)

{-
    Checks whether a number is a multiple of both 3 and 7.

    Examples:

    >>> runWriter $ multipleOf3And7 21
    ( True
    , [ "Testing 21"
      , "Checking if 21 is a multiple of 3"
      , "Checking if 21 is a multiple of 7"
      , "Returning result"
      ]
    )
-}
multipleOf3And7 :: Int -> Writer Bool
multipleOf3And7 n = do
    tell ["Testing " ++ show n]
    b <- multipleOf n 3
    c <- multipleOf n 7
    tell ["Returning result"]
    return $ b && c

{-
    Checks whether the first number is a multiple of the second.

    Examples:

    >>> runWriter $ multipleOf 21 3
    (True, ["Checking if 21 is a multiple of 3"])
-}
multipleOf :: Int -> Int -> Writer Bool
multipleOf n d = do
    tell ["Checking if " ++ show n ++ " is a multiple of " ++ show d]
    return $ n `mod` d == 0
