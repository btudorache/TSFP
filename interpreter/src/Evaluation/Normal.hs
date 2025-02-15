module Evaluation.Normal where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

type Eval a = StateT Context (ExceptT String IO) a


{-|
    Small-step normal-order evaluation of a given expression,
    within a given context.
-}

-- eval :: Expression             -- ^ Expression to be evaluated
--      -> Context                -- ^ Context where the evaluation takes place
--      -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
--                                --   enriched context, in case of definition
-- eval expr@(Var x) context = (M.findWithDefault expr x context, context)
-- eval expr@(Lambda x e) context = (expr, context)
-- eval expr@(Def x e) context = (e, (M.insert x e context))

-- -- reduce rule
-- eval expr@(Application (Lambda x e1) e2) context = (subst x e2 e1, context)
-- -- eval rule 
-- eval expr@(Application e1 e2) context = ((Application (fst (eval e1 context)) e2), context)
-- eval expr context = runStateT (evalM expr) context

evalIO :: Expression -> 
          Context -> 
          IO (Expression, Context)
evalIO exp ctx = do
    res <- runExceptT $ runStateT (evalM exp) ctx
    case res of
        Left err -> do
            putStrLn err 
            return (exp, ctx)
        Right (exp', ctx') -> return (exp', ctx')

evalM :: Expression -> Eval Expression
-- evalM expr@(Var x) = do
--     context <- get
--     return (M.findWithDefault expr x context) 
evalM (Var x) = do
    ctx <- get
    case M.lookup x ctx of
        Nothing -> throwError $ "Variable " ++ x ++ " not present in context!"
        Just exp -> liftIO (putStrLn "Found variable") >> return exp

evalM expr@(Lambda x e) = return expr

evalM expr@(Def x e) = do
    context <- get
    put (M.insert x e context)
    return expr

evalM expr@(Application (Lambda x e1) e2) = return (subst x e2 e1)

evalM expr@(Application e1 e2) = do
    evalE1 <- evalM e1
    return (Application evalE1 e2)