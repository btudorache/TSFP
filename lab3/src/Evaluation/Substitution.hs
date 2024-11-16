module Evaluation.Substitution where

import Syntax.Expression
-- import Data.List
import Data.Set

{-|
    Returns the list of free variables in an expression.
-}
freeVars :: Expression -> [String]
freeVars expr = (toList (freeVarsSet expr))

freeVarsSet :: Expression -> Set String
freeVarsSet (Var x) = singleton x
freeVarsSet (Lambda x e) = delete x (freeVarsSet e)
freeVarsSet (Application e1 e2) = union (freeVarsSet e1) (freeVarsSet e2)

{-|
    Performs the substitution of the free occurrences of a variable within
    an expression with another expression.
-}
subst :: String      -- ^ Variable
      -> Expression  -- ^ New expression
      -> Expression  -- ^ Existing expression
      -> Expression  -- ^ Resulting expression
subst x new curr@(Var y) = if x == y then new else curr
subst x new curr@(Lambda y e)
    | x == y = curr
    | x /= y && not (elem y (freeVars new)) = (Lambda y (subst x new e))
    | x /= y && (elem y (freeVars new)) && not (elem z freeVarUnion) = (Lambda z (subst x new (subst y (Var z) e)))
    where z = y ++ "#"
          freeVarUnion = freeVars (Application new e)
subst x new curr@(Application e1 e2) = (Application (subst x new e1) (subst x new e2))