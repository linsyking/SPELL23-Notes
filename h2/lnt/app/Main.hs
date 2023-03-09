module Main (main) where

data LNT
    = FreeVar String
    | BoundVar Int
    | Abstraction LNT
    | Ap LNT LNT -- Ap(M;N)

instance Show LNT where
    show :: LNT -> String
    show (FreeVar x) = x
    show (BoundVar x) = "[" ++ show x ++ "]"
    show (Abstraction x) = "λ" ++ show x
    show (Ap x y) = show x ++ "(" ++ show y ++ ")"

-- Find all possible bound var given layer l
substHelper :: LNT -> Int -> LNT -> LNT
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
        Abstraction x -> Abstraction (substHelper x (l + 1) n)
        Ap x y -> Ap (substHelper x l n) (substHelper y l n)

-- For /\M, find all bound var for this /\ and replace them with N
subst :: LNT -> LNT -> LNT
subst (Abstraction m) n = substHelper m 0 n
subst m _ = m

-- Reduction rule
beta :: LNT -> LNT
beta (Abstraction x) = Abstraction (beta x)
beta (Ap m n) =
    case m of
        Abstraction x -> subst (Abstraction x) n
        Ap _ _ -> Ap (beta m) n
        _ -> Ap m n
beta m = m

testM :: LNT
testM = Abstraction (Ap (FreeVar "z") (BoundVar 0))

testN :: LNT
testN = Abstraction (Abstraction (Ap (BoundVar 1) (FreeVar "y")))

testMN :: LNT
testMN = Ap testM testN

testNM :: LNT
testNM = Ap testN testM

main :: IO ()
main = putStrLn "Hi"
