module Syntax.Expression where

data Expression
    = Var String
    | Lambda String Expression
    | Application Expression Expression
    | Def String Expression -- definition is not really an expression but doing for convenience
    deriving Show

-- instance Show Expression where
--     show (Var x) = x
--     show (Lambda x e) = "\\" ++ x ++ "." ++ show e
--     show (Application e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
--     show (Def x e) = x ++ " = " ++ show e
