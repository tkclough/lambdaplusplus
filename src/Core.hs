{-# LANGUAGE LambdaCase #-}

module Core where

import           Control.Monad.State
import qualified Data.Set as S
import           Data.Set (Set)

import Language

-- type Context a = TypedExpr a -> TypedExpr a
data Context a = LamContext Name (Context a)
               | LeftAppContext (Context a) (TypedExpr a)
               | RightAppContext (TypedExpr a) (Context a)
               | Here
               deriving (Show, Eq)

context :: Context a -> TypedExpr a -> TypedExpr a
context Here = id
context (LamContext y c) = Lam y . context c
context (LeftAppContext c e2) = flip App e2 . context c
context (RightAppContext e1 c) = App e1 . context c

lamContext :: Name -> Context a -> Context a
lamContext = LamContext

leftAppContext :: TypedExpr a -> Context a -> Context a
leftAppContext e2 e1 = LeftAppContext e1 e2

rightAppContext :: TypedExpr a -> Context a -> Context a
rightAppContext = RightAppContext

type Strategy a = TypedExpr a -> Maybe (Redex a)

data Redex a =
      UntypedRedex (Context a) Name (TypedExpr a) (TypedExpr a) 
    | TypedRedex (Context a) (a -> Maybe (TypedExpr a)) a
    | MixedRedex (Context a) (UntypedExpr -> Maybe (TypedExpr a)) UntypedExpr
    -- note there's no redex with TypedExpr a applied to a, because that's accounted
    -- for by the LambdaRedex case

instance Show a => Show (Redex a) where
    show r = case r of
                UntypedRedex c x e1 e2 -> 
                    "UntypedRedex " ++ "(" ++ show c ++ ") " 
                                    ++ "(" ++ show x ++ ") "
                                    ++ "(" ++ show e1 ++ ") "
                                    ++ "(" ++ show e2 ++ ")"
                TypedRedex c _ x ->
                    "TypedRedex " ++ "(" ++ show c ++ ") " 
                                  ++ "<func> "
                                  ++ "(" ++ show x ++ ")"
                MixedRedex c _ e ->
                    "MixedRedex " ++ "(" ++ show c ++ ") " 
                                  ++ "<func> "
                                  ++ "(" ++ show e ++ ")"
composeContext :: (Context a -> Context a) -> Redex a -> Redex a
composeContext f (UntypedRedex c x y z) = UntypedRedex (f c) x y z
composeContext f (TypedRedex c x y) = TypedRedex (f c) x y
composeContext f (MixedRedex c x y) = MixedRedex (f c) x y


-- substitution code
-- | Compute the free variables in an expression
freeVars :: TypedExpr a -> Set Name
freeVars e = go S.empty e
    where go :: Set Name -> TypedExpr a -> Set Name
          go boundVars e = 
              case e of
                  Var y -> if y `S.member` boundVars 
                             then S.empty       -- this variable isn't free
                             else S.singleton y -- this variable is free
                  App e1 e2 -> go boundVars e1 `S.union` go boundVars e2
                  Lam y e1 -> let boundVars' = S.insert y boundVars in
                                    go boundVars' e1
                  _ -> S.empty

allVars :: TypedExpr a -> Set Name
allVars e = case e of
    Var y -> S.singleton y
    App e1 e2 -> S.union (allVars e1) (allVars e2)
    Lam y e1 -> S.insert y (allVars e1)
    _ -> S.empty

-- | Get an infinite list of variables free in the given list
freshVars :: [TypedExpr a] -> [Name]
freshVars es = do
    let fvs = S.unions . map allVars $ es
    -- get a candidate fresh var
    candidate <- map ((++ "_") . show) [1..]
    if candidate `S.notMember` fvs
        then return candidate -- if its not a free var return it
        else []               -- otherwise skip it


-- | Perform variable substitution
subst :: (Name, TypedExpr a) -> TypedExpr a -> TypedExpr a
subst (x,sub) e = evalState (go (x,sub) e) (freshVars [e, sub])
    where -- | Substitution subroutine with state context of current fresh
          -- variables
          go :: (Name, TypedExpr a) -> TypedExpr a -> State [Name] (TypedExpr a)
          go (x,sub) e = 
              case e of
                  Var y -> if x == y
                            then return sub     -- substitute the matching var
                            else return (Var y) -- var doesn't match
                  App e1 e2 -> 
                      -- recursively substitute, threading used variables through
                      App <$>
                        go (x,sub) e1 <*>
                        go (x,sub) e2
                  Lam y e1 ->
                      if x == y
                          -- abstraction variable matches so no substitution
                          -- will occur here
                          then return (Lam y e1)
                          else do
                            --   -- get the next fresh variable
                            --   z <- pop
                            --   -- compute e1[z/y]
                            --   e1' <- go (y,Var z) e1
                            --   -- compute e1'[sub/x]
                            --   e1'' <- go (x,sub) e1'
                            --   -- put back in the abstraction
                            --   return (Lam z e1'')
                            z <- pop
                            let sub' = subst (y,Var z) sub
                            e1' <- go (x,sub') e1
                            return (Lam y e1')

                  z -> return z
          
          -- | Get the first fresh variable on the stack, and take it off
          pop :: State [Name] Name
          pop = do
              x <- gets head
              modify tail
              return x

substs :: [(Name, TypedExpr a)] -> TypedExpr a -> TypedExpr a
substs bindings e = foldr (\b acc -> subst b acc) e bindings

normalRedex :: Strategy a
normalRedex e =
    case e of
        App (Lam y e1) e2 -> Just (UntypedRedex Here y e2 e1)
        App (TypedFunc f) (Typed x) -> Just (TypedRedex Here f x)
        -- App (UntypedFunc f) e2 -> 
        --     case normalRedex e2 of
        --         Just r -> Just (composeContext (leftAppContext e2) r)
        --         _ -> Just (MixedRedex Here f e2)
        App (UntypedFunc f) e2 -> MixedRedex Here f <$> toUntyped e2
        App e1 e2 -> 
            case normalRedex e1 of
                Just r -> Just (composeContext (leftAppContext e2) r)
                _ -> case normalRedex e2 of
                    Just r -> Just (composeContext (rightAppContext e1) r)
                    _ -> Nothing
                
        Lam y e1 ->
            case normalRedex e1 of
                Just r -> Just (composeContext (lamContext y) r)
                _ -> Nothing

        _ -> Nothing


data ReductionError a =
      TypedError (a -> Maybe (TypedExpr a)) a
    | MixedError (UntypedExpr -> Maybe (TypedExpr a)) UntypedExpr

data Reduction a =
      Error (ReductionError a)
    | Reduced (TypedExpr a)
    | NoRedex

reduceOnce :: Strategy a -> TypedExpr a -> Reduction a
reduceOnce strat e =
    case strat e of
        Just r -> case r of
            UntypedRedex cxt x sub e1 -> Reduced $ context cxt (subst (x,sub) e1)
            TypedRedex cxt f x -> 
                case f x of
                    Just fx -> Reduced $ context cxt fx
                    _ -> Error $ TypedError f x
            MixedRedex cxt f x -> 
                case f x of
                    Just fx -> Reduced $ context cxt fx
                    _ -> Error $ MixedError f x
        _ -> NoRedex

data Resolution a =
      ResolutionError (ReductionError a)
    | Result (TypedExpr a)
    | OutOfFuel (TypedExpr a)

reduce :: Strategy a -> Int -> TypedExpr a -> Resolution a
reduce strat fuel e | fuel < 0 = error "negative fuel"
                    | fuel == 0 = OutOfFuel e
                    | otherwise =
        case reduceOnce strat e of
            Error e -> ResolutionError e
            Reduced e' -> reduce strat (fuel - 1) e'
            NoRedex -> Result e

reduceWithSteps :: Strategy a -> Int -> TypedExpr a -> [Resolution a]
reduceWithSteps strat fuel e | fuel < 0 = error "negative fuel"
                             | fuel == 0 = [OutOfFuel e]
                             | otherwise = 
        case reduceOnce strat e of
            Error e -> [ResolutionError e]
            Reduced e' -> (Result e'):(reduceWithSteps strat (fuel - 1) e')
            NoRedex -> [Result e]

fromResolution :: Resolution a -> Maybe (TypedExpr a)
fromResolution (Result e) = Just e
fromResolution (OutOfFuel e) = Just e
fromResolution _ = Nothing