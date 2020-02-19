module Syntax where

type Name = String

data Expr
  = Lam [Name] Expr
  | App Expr Expr
  | Var Name
  deriving (Eq, Show)

data ConsoleInput
  = Reduction Expr
  | Assignment (Name, Expr)