module Translate (normal2nameless, nameless2normal) where

import qualified LNT as L
import qualified Normal as N

-- bind_n(x;m)
bindn :: Int -> String -> L.Term -> L.Term
bindn l x y =
    case y of
        L.FreeVar p -> if p == x then L.BoundVar l else y
        L.BoundVar _ -> y
        L.Ab p -> L.Ab (bindn (l + 1) x p)
        L.Ap p q -> L.Ap (bindn l x p) (bindn l x q)

-- LNT of /\x.M => /\M'
abx :: String -> L.Term -> L.Term
abx x m = L.Ab $ bindn 0 x m

normal2nameless :: N.Term -> L.Term
normal2nameless (N.Var x) = L.FreeVar x
normal2nameless (N.Ap x y) = L.Ap (normal2nameless x) (normal2nameless y)
normal2nameless (N.Ab x y) = abx x (normal2nameless y)

varFromNum :: String -> Int -> String
varFromNum pre n = pre ++ show n

-- Generate a unique variable name prefix from given LNT Term
uniqueNamePrefix :: L.Term -> String
uniqueNamePrefix l = replicate (maximum $ 1 : map length (L.varNames l)) 'x'

-- Record the current variable
nameless2normalHelper :: L.Term -> String -> Int -> N.Term
nameless2normalHelper l prefix n =
    case l of
        (L.FreeVar x) -> N.Var x
        (L.BoundVar x) -> N.Var (varFromNum prefix (n - x - 1))
        (L.Ap x y) -> N.Ap (nameless2normalHelper x prefix n) (nameless2normalHelper y prefix n)
        (L.Ab x) -> N.Ab (varFromNum prefix n) (nameless2normalHelper x prefix (n + 1))

nameless2normal :: L.Term -> N.Term
nameless2normal l = nameless2normalHelper l (uniqueNamePrefix l) 0

testT :: N.Term
testT = N.Ab "x" (N.Ab "x" (N.Ap (N.Var "x") (N.Var "y")))

testR :: L.Term
testR = L.Ab (L.Ap (L.BoundVar 0) (L.Ab (L.BoundVar 1)))

nameless2normalHelper2 :: L.Term -> String -> Int -> N.Term
nameless2normalHelper2 l prefix n =
    case l of
        (L.FreeVar x) -> N.Var x
        (L.BoundVar x) -> error $ "cannot find any bindings for " ++ show x
        (L.Ap x y) -> N.Ap (nameless2normalHelper x prefix n) (nameless2normalHelper y prefix n)
        (L.Ab x) -> N.Ab name (nameless2normalHelper (L.subst x (L.FreeVar name)) prefix (n + 1)) where name = varFromNum prefix n

nameless2normal2 :: L.Term -> N.Term
nameless2normal2 l = nameless2normalHelper2 l (uniqueNamePrefix l) 0
