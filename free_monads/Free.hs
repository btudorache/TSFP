{-# LANGUAGE TemplateHaskell #-}
module Free where

{-
    Required packages:
    
    * free
    * mtl
    * deriving-compat

    Install using:

    cabal install --lib <package-name>
-}

import Control.Monad
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Writer
import Text.Show.Deriving

{-
    In general, a free object of a specific kind (monoid, monad, algebra etc.)
    is the least constrained such object, i.e., it satisfies only the
    definitional properties and nothing more. In addition, any other object of
    the same kind can be recovered from the free object.

    In particular, a free monad over a functor records a sequence of
    computations, without actually performing any specific computation. In other
    words, it constructs computation ASTs, using the functor's data constructors
    as AST nodes. More specifically:
    
    * return produces leaves
    * (>>=) grows the AST by replacing leaves with additional subtrees
    
    In turn, the ASTs can be interpreted within other, more specific monads
    (State, Writer etc.).

    The free monad for functor f is Free f, containing leaves of type a:

    data Free f a = Free (f (Free f a)) | Pure a

    If you ignore the (Pure a) part, you are left with a fixed-point type,
    which fills the holes in functor f's data constructors with the fixed-point
    type itself, in order to get... the fixed-point type. This is precisely
    what allows us to construct arbitrarily nested ASTs using functor f's data
    constructors as nodes.

    An analogy can be made with standard Haskell lists:

    data List a = Cons a (List a) | Null

    * Just as Null means no elements, Pure means no functor f layers
      (0 AST depth)
    * Just as Cons means an additional element, Free means an additional f layer
      (an additional level in the AST)

    instance Functor f => Functor (Free f) where
        fmap :: (a -> b) -> Free f a -> Free f b
        fmap f (Pure a) = Pure $ f a
        fmap f (Free tree) = Free $ fmap (fmap f) tree
    
    instance Functor f => Monad (Free f) where
        return :: a -> Free f a
        return = Pure

        (>>=) :: Free f a -> (a -> Free f b) -> Free f b
        Pure a >>= f = f a
        Free tree >>= f = Free $ fmap (>>= f) tree
    
    An element of type (f a) can be transformed into an elementary AST using:

    liftF :: Functor f => f a -> Free f a
    liftF fa = Free $ fmap Pure fa
-}

{-
    *** TODO 1 ***

    What known monad is the free monad for the Empty functor, Free Empty,
    equivalent to?

    Empty constructs unpopulated types, since there are no data constructors.

    R: -- Since Empty has no constructors, free monad for Empty is just Identity
-}
data Empty t
    deriving Functor
$(deriveShow1 ''Empty)

freeEmpty :: Free Empty Int
freeEmpty = do
    n <- return 1
    return $ n + 1

{-
    *** TODO 2 ***

    What known monad is the free monad for the Unit functor, Free Unit,
    equivalent to?

    Unit constructs singleton types, since t is unused.
    R: Since Unit always exists, free monad for Unit is essentially the Identity monad
-}
data Unit t = Unit
    deriving Functor
$(deriveShow1 ''Unit)

unit :: Free Unit a
unit = liftF Unit

freeUnit :: Free Unit Int
freeUnit = do
    a <- return 1
    b <- return 2  -- replace (return 2) with unit and reevaluate
    return $ a + b

{-
    *** TODO 3 ***

    What known monad is the free monad for the (a,) functor, using () for leaves,
    equivalent to?

    R: PseudoList is equivalent to Writer monoid
-}
type PseudoList a = Free ((,) a) ()

singleton :: a -> PseudoList a
singleton a = liftF (a, ())

pl1 :: PseudoList Int
pl1 = do
    singleton 1
    singleton 2
    singleton 3
  
pl2 :: PseudoList Int
pl2 = do
    singleton 5
    singleton 6
  
pl :: PseudoList Int
pl = do
    singleton 0
    pl1
    singleton 4
    pl2
    singleton 7

{-
    *** TODO 4 ***

    If we wanted to define a binary tree as a free monad, what would the
    corresponding functor be?

    Define TreeF and node.

    Use TODO 3 for inspiration.
-}
data TreeF a b = Leaft a b | Branch b b
    deriving Functor
$(deriveShow1 ''TreeF)

type Tree a = Free (TreeF a) ()

node :: a -> Tree a
node a = liftF (Leaf a ())

tree :: Tree Int
tree = do
    node 1
    node 2
    node 3

{-
    *** TODO 5 ***

    What is the behavior of the free monad for the standard list functor?

    An AST constructed using a free monad can be interpreted within another
    monad m using:

    foldFree :: Monad m => (forall x . f x -> m x) -> Free f a -> m a

    What is the difference between freeList and (foldFree id freeList)?
    The second expression interprets the AST within the standard list monad.
    
    R: foldFree id freeList would create a list of all possible (x,y) combinations
-}
freeList :: Free [] (Int, Int)
freeList = do
    x <- liftF [1, 2, 3]
    y <- liftF [4, 5]
    return (x, y)

{-
    *** TODO 6 ***

    StackASTF is the base functor for a simple stack language. Its data
    constructors correspond to available commands:

    * Push places an element on top of the stack
    * Add removes the top two elements from the stack, "adds" them (whatever
      that means) and pushes the result back onto the stack.

    (StackAST a) is a free monad for (StackASTF a) and corresponds to ASTs,
    wherein data constructors produce nodes:

    * a is the type of the elements on the stack
    * b is a whole that the free monad fills with the AST type itself.

    Define the "display" and "execute" interpretations of the AST:

    * display produces a textual description of the AST, by interpreting it
      within the (Writer [String]) monad
    * execute actually runs the computations in the AST using a stack of numeric
      values, by interpreting the AST within the (State [a]) monad.
-}
data StackASTF a b
    = Push a b
    | Add b
    deriving (Functor)
$(deriveShow1 ''StackASTF)

type StackAST a = Free (StackASTF a)

push :: a -> StackAST a ()
push a = liftF $ Push a ()

add :: StackAST a ()
add = liftF $ Add ()

program :: StackAST Int ()
program = do
    push 1
    push 2
    add

display :: Show a => StackAST a () -> Writer [String] ()
display = foldFree go
  where
    go :: Show a => StackASTF a x -> Writer [String] x
    go (Push a next) = do
        tell ["Push " ++ show a]
        return next
    go (Add next) = do
        tell ["Add"]
        return next
    
execute :: Num a => StackAST a () -> State [a] ()
execute = foldFree go
  where
    go :: Num a => StackASTF a x -> State [a] x
    go (Push n next) = do
        modify (n:)
        return next
    go (Add next) = do
        (b:a:rest) <- get
        put ((a + b):rest)
        return next

main :: IO ()
main = do
    mapM_ putStrLn $ execWriter (display program)
    print $ runState (execute program) []