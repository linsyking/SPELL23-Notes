module Normal (Term (..), VarName (..)) where

data Term
    = Var VarName
    | Ab VarName Term -- Abstraction
    | Ap Term Term -- Apply
    deriving (Show, Eq)

data VarName = VarName String Int
    deriving (Eq, Ord)

instance Show VarName where
    show (VarName s i) = s ++ show i
