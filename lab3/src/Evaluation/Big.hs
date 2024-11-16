module Evaluation.Big where

import Syntax.Expression
import qualified Evaluation.Normal as EN
import qualified Evaluation.Applicative as EA
import qualified Data.Map as M
import Control.Monad.State

-- initial impl
-- takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
-- takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

-- evalBig stepper e context = last $ takeWhileOneMore (\(expr, context) -> (fst (stepper expr context)) /= expr) (iterate (\(expr, context) -> stepper expr context) (e, context))               


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
-- evalBig stepper e context
--     | e == evalE = (evalE, evalContext)
--     | otherwise = evalBig stepper evalE evalContext
--     where 
--         (evalE, evalContext) = stepper e context
evalBig stepper e context = runState (evalBigM (state . stepper) e) context

evalBigM :: (Expression -> EN.Eval Expression)
        -> Expression
        -> EN.Eval Expression
evalBigM stepper e = do
    evaluated <- stepper e
    if evaluated == e then return evaluated else evalBigM stepper evaluated


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
-- evalList stepper elist context = let reduced = tail $ foldl (\acc (expr) -> (evalBig stepper expr (snd (head acc))) : acc) [((Var "dummy", M.empty))] elist
--                                      finalContext = snd $ last $ reduced
--                                      evaluated = map fst reduced
--                                  in (evaluated, finalContext)
evalList stepper elist context = runState (evalListM (state . stepper) elist) context


evalListM :: (Expression -> EN.Eval Expression)
        -> [Expression]
        -> EN.Eval [Expression]
evalListM = mapM . evalBigM