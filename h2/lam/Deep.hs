module Deep where
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (id)

data Term where
  Var :: VarName -> Term
  App :: Term -> Term -> Term
  Lam :: VarName -> Term -> Term
  deriving (Show, Eq)

data VarName = VarName String Int
  deriving (Eq, Ord)

instance Show VarName where
  show (VarName s i) = s ++ show i

within :: VarName -> Term -> Bool
within s t = Set.member s (cover t)

cover :: Term -> Set VarName
cover (Var x) = Set.singleton x
cover (App t u) = Set.union (cover t) (cover u)
cover (Lam x t) = Set.delete x (cover t)

-- t [ y / x ]
alpha :: VarName -> VarName -> Term -> Term
alpha x y (Var z) | x == z = Var y
alpha x y (App t u) = App (alpha x y t) (alpha x y u)
alpha x y (Lam z t) | x /= z = Lam z (alpha x y t)
alpha _ _ t = t

-- t [ s / x ]
subst :: VarName -> Term -> Term -> Term
subst x s (Var y) | x == y = s
subst x s (App t u) = App (subst x s t) (subst x s u)
subst x s (Lam y t) | within y s = subst x s $ Lam (fresh y) $ alpha y (fresh y) t where fresh (VarName v i) = VarName v (i + 1)
subst x s (Lam y t) = Lam y (subst x s t)
subst _ _ t = t

beta :: Term -> Maybe Term
beta (App (Lam x t) u) = beta $ subst x u t
beta (App t u) = do
  t' <- beta t
  return $ App t' u
beta (Lam x t) = do
  t' <- beta t
  return $ Lam x t'
beta t = Just t

id :: Term
id = Lam (VarName "x" 0) (Var (VarName "x" 0))

idapp :: Term
idapp = App id id

-- -- (\x -> x x) (\x -> x x)
-- omega :: Term
-- omega = App w w where
--   w = Lam (VarName "x" 0) (App (Var (VarName "x" 0)) (Var (VarName "x" 0)))