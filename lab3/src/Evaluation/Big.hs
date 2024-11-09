module Evaluation.Big where

import Syntax.Expression
import qualified Evaluation.Normal as EN
import qualified Evaluation.Applicative as EA
import qualified Data.Map as M


takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []
{-|
    Big-step evaluation of a given expression, within a given context.
    The evaluation should stop when either the value is reached,
    or the expression cannot be reduced further.
    
    The first argument is the small-step evaluation function.
-}
evalBig :: (Expression -> Context -> (Expression, Context))  -- ^ Small-stepper
        -> Expression             -- ^ Expression to be evaluated
        -> Context                -- ^ Context where the evaluation takes place
        -> (Expression, Context)  -- ^ Evaluation result,
                                  --   together with a possibly enriched context
                                  --   in case of definition
-- iterate while expr evaluation does not change
evalBig stepper e context = last $ takeWhileOneMore (\(expr, context) -> (fst (stepper expr context)) /= expr) ((e, context) : (iterate (\(expr, context) -> stepper expr context) (e, context)))                    
{-|
    Big-step evaluation of a list of expressions, starting with
    the given context and using it throughout the entire list,
    for propagating the encountered definitions.
    
    The first argument is the small-step evaluation function.
-}
evalList :: (Expression -> Context -> (Expression, Context))
         -> [Expression]
         -> Context
         -> ([Expression], Context)
evalList stepper elist context = let reduced = tail $ reverse $ foldl (\acc (expr) -> (evalBig stepper expr (snd (head acc))) : acc) [((Var "dummy", M.empty))] elist
                                     finalContext = snd $ last $ reduced
                                     evaluated = map fst reduced
                                 in (evaluated, finalContext)