module Translate where

import qualified LNT as L
import qualified Normal as N

normal2nameless :: N.Term -> L.Term
normal2nameless x = L.FreeVar "s"

nameless2normal :: L.Term -> N.Term
nameless2normal x = N.Var (N.VarName "x" 0)
