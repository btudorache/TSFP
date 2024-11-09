module Evaluation.Normal where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M

{-|
    Small-step normal-order evaluation of a given expression,
    within a given context.
-}
eval :: Expression             -- ^ Expression to be evaluated
     -> Context                -- ^ Context where the evaluation takes place
     -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
                               --   enriched context, in case of definition
eval expr@(Var x) context = (M.findWithDefault expr x context, context)
eval expr@(Lambda x e) context = (expr, context)
eval expr@(Def x e) context = (e, (M.insert x e context))

-- reduce rule
eval expr@(Application (Lambda x e1) e2) context = (subst x e2 e1, context)
-- eval rule 
eval expr@(Application e1 e2) context = ((Application (fst (eval e1 context)) e2), context)

