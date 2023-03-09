module Normal (Term (..)) where

data Term
    = Var String
    | Ab String Term -- Abstraction
    | Ap Term Term -- Apply
    deriving (Eq)

instance Show Term where
    show (Var s) = s
    show (Ab x t) = "λ" ++ x ++ show t
    show (Ap x y) = "(" ++ show x ++ ") " ++ "(" ++ show y ++ ")"
