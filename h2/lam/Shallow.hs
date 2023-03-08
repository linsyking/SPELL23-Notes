module Shallow where

data Term where
  Lam :: (Term -> Term) -> Term

app :: Term -> Term -> Term
app (Lam f) = f

-- (\x -> x x)
w :: Term
w = Lam (\x -> app x x)

-- (\x -> x x) (\x -> x x)
-- omega :: Term
-- omega = app w w
