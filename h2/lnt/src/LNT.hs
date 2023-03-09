module LNT (Term (..), subst, beta, varNames) where

data Term
    = FreeVar String
    | BoundVar Int
    | Ab Term -- Abstraction
    | Ap Term Term -- Ap(M;N)
    deriving (Eq)

instance Show Term where
    show (FreeVar x) = x
    show (BoundVar x) = "[" ++ show x ++ "]"
    show (Ab x) = "λ" ++ show x
    show (Ap x y) = "(" ++ show x ++ ") " ++ "(" ++ show y ++ ")"

-- Find all possible bound var given layer l
substHelper :: Term -> Int -> Term -> Term
substHelper m l n =
    case m of
        FreeVar _ -> m
        BoundVar x ->
            if x == l
                then n
                else
                    if x > l
                        then BoundVar (x - 1)
                        else m
        Ab x -> Ab (substHelper x (l + 1) n)
        Ap x y -> Ap (substHelper x l n) (substHelper y l n)

-- For /\M, find all bound var for this /\ and replace them with N
subst :: Term -> Term -> Term
subst (Ab m) n = substHelper m 0 n
subst m _ = m

-- Reduction rule
beta :: Term -> Term
beta (Ab x) = Ab (beta x)
beta (Ap m n) =
    case m of
        Ab x -> subst (Ab x) n
        Ap _ _ -> Ap (beta m) n
        _ -> Ap m n
beta m = m

varNames :: Term -> [String]
varNames (FreeVar x) = [x]
varNames (BoundVar _) = []
varNames (Ab x) = varNames x
varNames (Ap x y) = varNames x ++ varNames y

testM :: Term
testM = Ab (Ap (FreeVar "z") (BoundVar 0))

testN :: Term
testN = Ab (Ab (Ap (BoundVar 1) (FreeVar "y")))

testMN :: Term
testMN = Ap testM testN

testNM :: Term
testNM = Ap testN testM
