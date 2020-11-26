{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Language where

import Data.String (IsString, fromString)

type Name = String

-- | Grammar for untyped lambda calculus
data UntypedExpr = UVar Name
                 | ULam Name UntypedExpr
                 | UApp UntypedExpr UntypedExpr
                 deriving (Eq)

-- | Untyped lambda calculus, plus functionality for typed computations
data TypedExpr a = Var Name
                 | Lam Name (TypedExpr a)
                 | App (TypedExpr a) (TypedExpr a)
                 | Typed a
                 | TypedFunc (a -> Maybe (TypedExpr a))
                 | UntypedFunc (UntypedExpr -> Maybe (TypedExpr a))


instance Show a => Show (TypedExpr a) where
    show e = paren (fst $ go e)
        where go e =  case e of
                Var y -> (y, 3)
                App e1 e2 ->
                    let p = 2
                        (s1, p1) = go e1 
                        (s2, p2) = go e2
                        s1' = if p1 >= p then s1 else paren s1
                        s2' = if p2 > p then s2 else paren s2 in
                            (s1' ++ " " ++ s2', p)
                Lam y e1 ->
                    let p = 1
                        (s1, p1) = go e1
                        s1' = if p1 >= p then s1 else paren s1 in
                            ("/" ++ y ++ "." ++ s1', p)
                Typed x -> (show x, 4)
                TypedFunc _ -> ("TypedFunc", 1)
                UntypedFunc _ -> ("UntypedFunc", 1)
              paren s = "(" ++ s ++ ")"

instance Show UntypedExpr where
    show = show . (toTyped :: UntypedExpr -> TypedExpr ())

-- | Do the usual equality for pure lambda calculus, with additional equality
-- for typed values - we assume nothing about functions, so they are always
-- considered not equal.
instance Eq a => Eq (TypedExpr a) where 
  Var x == Var y = x == y
  Lam x e1 == Lam y e2 = x == y && e1 == e2
  App e1 e2 == App e3 e4 = e1 == e3 && e2 == e4
  Typed x == Typed y = x == y
  _ == _ = False

-- Niceties for writing expressions in Haskell
instance IsString (TypedExpr a) where
  fromString s = Var s

instance IsString UntypedExpr where
  fromString s = UVar s

-- | Shorthand for application on typed expressions.
(#) :: TypedExpr a -> TypedExpr a -> TypedExpr a
(#) = App

-- | Shorthand for application on untyped expressions.
(##) :: UntypedExpr -> UntypedExpr -> UntypedExpr
(##) = UApp

-- | A program is a bunch of named expressions.
type Program a = [Assignment a]

-- | An assignment binds an expression to some name.
type Assignment a = (Name, UntypedExpr)

-- | Console inputs are either an assignment, an evaluation, or some other
-- command.
data ConsoleInput a
  = Assignment (Assignment a)
  | Evaluation Evaluation
  | Command ConsoleCommand
  deriving (Show)

-- | Show something in the console.
data ConsoleCommand
  = Show ConsoleShowable
  deriving (Show)

-- | Things that can be showed in console
data ConsoleShowable
  = Bindings
  deriving (Show)

-- | An evaluation is either untyped e.g. "> (/x.x)(/z.z)", or has some type
-- annotation e.g. "> /f x.f x as nat".
data Evaluation
  = UntypedEvaluation UntypedExpr
  | TypedEvaluation UntypedExpr Type
  deriving (Show)

-- | The supported types
data Type 
  = NatType
  | BoolType
  | ListType Type
  | PairType Type Type
  deriving (Show, Eq)


-- | This is an annoying hack to be able to evaluate expressions generically.
-- We need a type annotation because otherwise, the evaluator has no way of 
-- knowing what type we want an expression to have.
data Annotated a where
  AUntypedResult :: UntypedExpr -> Annotated UntypedExpr
  ANatResult :: Nat -> Annotated Nat
  ABoolResult :: Bool -> Annotated Bool
  AListResult :: [Annotated a] -> Annotated [Annotated a]
  APairResult :: Pair (Annotated a) (Annotated b) 
                -> Annotated (Pair (Annotated a) (Annotated b))

-- | We also need the un-annotated version of the results, because the
-- evaluator needs to support returning any type.
data TypedResult = UntypedResult UntypedExpr
                 | NatResult Nat
                 | BoolResult Bool
                 | ListResult [TypedResult]
                 | PairResult (Pair TypedResult TypedResult)
                 deriving (Show, Eq)

-- | Necessary for evaluation function
removeAnnotation :: Annotated a -> TypedResult
removeAnnotation (AUntypedResult e) = UntypedResult e
removeAnnotation (ANatResult n) = NatResult n
removeAnnotation (ABoolResult b) = BoolResult b
removeAnnotation (AListResult xs) = ListResult (map removeAnnotation xs)
removeAnnotation (APairResult (Pair (x,y))) = PairResult (Pair (removeAnnotation x, removeAnnotation y))

-- | A reminder that nats are positive numbers (may add support for ints as well).
newtype Nat = Nat Int
            deriving (Show, Eq)

-- | Tuples don't play nicely with typeclasses, so we wrap one.
newtype Pair a b = Pair (a, b)
                 deriving (Show, Eq)

-- | When we have a typed expression that we're pretty sure doesn't have any
-- actual typed component, we can cast it to any other type.
tryCast :: TypedExpr a -> Maybe (TypedExpr b)
tryCast e = toTyped <$> toUntyped e

-- | Only expressions with no typed components can be turned into untyped 
-- expressions.
toUntyped :: TypedExpr a -> Maybe UntypedExpr
toUntyped e = 
    case e of
        Var x -> Just $ UVar x
        Lam y e1 -> ULam y <$> toUntyped e1
        App e1 e2 -> UApp <$> toUntyped e1 <*> toUntyped e2
        _ -> Nothing

-- | Any untyped expression can be turned into a typed equivalent.
toTyped :: UntypedExpr -> TypedExpr a
toTyped e =
    case e of
        UVar x -> Var x
        ULam y e1 -> Lam y (toTyped e1)
        UApp e1 e2 -> App (toTyped e1) (toTyped e2)
