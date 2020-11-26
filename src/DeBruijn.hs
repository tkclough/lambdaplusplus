module DeBruijn where

import Language (UntypedExpr(..), Name)
import qualified Data.Map as M

-- | A DeBruijn expression uses integer indices rather than variable names to
-- refer to variables. An index n corresponds to the variable bound in the lambda
-- separated by n lambdas
data Expr = Lam Expr
          | App Expr Expr
          | BoundVar Int
          | UnboundVar
          deriving (Eq)


fromExpr :: UntypedExpr -> Expr
fromExpr = go 0 M.empty
    where go :: Int -> M.Map Name Int -> UntypedExpr -> Expr
          go depth depthMapping e =
            case e of
                ULam v e1 -> 
                    Lam $ go (depth + 1) (M.insert v depth depthMapping) e1
                UApp e1 e2 ->
                    App (go (depth + 1) depthMapping e1)
                        (go (depth + 1) depthMapping e2)
                UVar v -> case M.lookup v depthMapping of
                    Just d -> BoundVar $ depth - d
                    Nothing -> UnboundVar

-- | Two expressions are alpha equivalent when their DeBruijn represenations
-- are equivalent
alphaEquivalent :: UntypedExpr -> UntypedExpr -> Bool
alphaEquivalent s1 s2 = fromExpr s1 == fromExpr s2