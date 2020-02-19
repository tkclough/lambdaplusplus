module StrictSyntax where

import qualified Syntax as E

type Name = String

data Expr
  = Lam Name Expr
  | App Expr Expr
  | Var Name
  deriving (Eq, Show)

fromExtended :: E.Expr -> Expr
fromExtended (E.Lam (i:[]) s) = Lam i (fromExtended s)
fromExtended (E.Lam (i:is) s) = Lam i (fromExtended (E.Lam is s))
fromExtended (E.App s1 s2) = App (fromExtended s1) (fromExtended s2)
fromExtended (E.Var v) = Var v