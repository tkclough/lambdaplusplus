module Compile where
    
import Language
import Core
-- import Core (freeVars,substitute,substitutes)
import Prims (y)
import           Data.List                      ( partition
                                                , intercalate
                                                )
import           Data.Graph                     ( Graph
                                                , Vertex
                                                )
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import qualified Data.Array                    as A
import qualified Data.Graph                    as G
import qualified Data.Tree                     as T
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import Control.Monad.State ()
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , isJust
                                                )

-- | From bindings, build graph of which functions depend on which.
buildDependencyGraph
  :: Map Name (TypedExpr a)
  -> (Graph, Vertex -> (Name, Name, [Name]), Name -> Maybe Vertex)
buildDependencyGraph bindings =
  let -- compute free variables for each name, some of which refer to other functions
      fvs    = M.map freeVars bindings :: Map Name (Set Name)

      edgeList =
          catMaybes
            . filter isJust
            . map snd
            . M.toList
            . M.mapWithKey (f fvs)
            $ fvs
  in  G.graphFromEdges edgeList

 where
  -- | Get the dependencies of a name, returning nothing if the named function doesn't exist
  f :: Map Name (Set Name) -> Name -> Set Name -> Maybe (Name, Name, [Name])
  f fvs u _ = case M.lookup u fvs of 
    Nothing -> Nothing 
    Just dep -> Just (u, u, S.toList dep)

-- | Partition terms into groups of those that refer to each other, and terms
-- that don't have mutual recursion.
mutuallyRecursiveTerms :: Graph -> ([[Vertex]], [Vertex])
mutuallyRecursiveTerms gr =
  let sccs = map T.flatten $ G.scc gr
      (mutrec, notmutrec) = 
          partition (\scc -> length scc > 1 || ((head scc) `elem` (gr A.! (head scc)))) $
          sccs
  in  (mutrec, concat notmutrec)

-- | Transform a recursive function defined in terms of itself into a function
-- that takes itself as an argument, and wrap in in a fixed point combinator.
transformRecursiveTerm :: Name -> TypedExpr a -> TypedExpr a
transformRecursiveTerm v s = App (toTyped y) $ Lam v (subst (v,Var v) s)

-- | Transform a group of mutually recursive functions into a number of functions
-- that refer to a bundled function that includes all the mutually recursive
-- functions.
transformMutuallyRecursiveTerms :: [(Name, TypedExpr a)] -> [(Name, TypedExpr a)]
transformMutuallyRecursiveTerms bindings =
  let -- the new function will be the concatenation of every name with underscores between
    bundleName     = intercalate "_" (map fst bindings)

    -- for mutually recursive function i, the corresponding selector will be
    -- /s1 s2...sn -> si
    selectors      = mkSelectors (length bindings)

    -- pair each name with its corresponding selector
    nameToSelector = M.fromList $ zip (map fst bindings) selectors

    -- replace each call to a mutually recursive function with a call to bundle on the respective function
    -- i.e. a call fi will be replaced with bundle si
    bindings'      = map
      (\(_, s) -> substs (M.toList (M.map (App bundle) nameToSelector)) s)
      bindings

    -- bundle all the functions f1,...,fn into a single function 
    -- /s.s f1'...fn' where fi' is the substituted version of fi
    bundle = App (toTyped y) . Lam "s" $ foldl (\acc m -> App acc m) (Var "s") bindings'

    -- the ith function will be defined as bundle si
    bindings'' = M.map (\s -> App bundle s) nameToSelector
  in
    (bundleName, bundle) : (M.toList bindings'')

 where
  mkSelectors :: Int -> [TypedExpr a]
  mkSelectors n =
    let names            = map (('s' :) . show) [0 .. n]
        selectorTemplate = \v -> foldr (\u acc -> Lam u acc) (Var v) names
    in  map selectorTemplate names

-- | Replace function calls with their definition.
expand :: [Name] -> Map Name (TypedExpr a) -> Map Name (TypedExpr a)
expand order bindings = foldl go bindings order
 where
  go :: Map Name (TypedExpr a) -> Name -> Map Name (TypedExpr a)
  go bindings v =
    let val = fromJust $ M.lookup v bindings
    in  M.map (\m -> subst (v,val) m) bindings

expandNew :: Map Name UntypedExpr -> UntypedExpr -> Maybe UntypedExpr
expandNew bindings e = toUntyped $ foldr go (toTyped e) (M.assocs . M.map toTyped $ bindings)
    where go :: (Name, TypedExpr a) -> TypedExpr a -> TypedExpr a
          go (k,v) e = subst (k,v) e

-- | Build up an entry point by finding an evaluation order and expanding terms.
compile :: Map Name (TypedExpr a) -> Map Name (TypedExpr a)
compile bindings =
  let (gr, f, g)                = buildDependencyGraph bindings

      -- transform recursive functions into pure form
      (allRec, _) = mutuallyRecursiveTerms gr
      (mutRec, oneRec) = partition ((> 1) . length) allRec

      recursive' =
          map (\(x, y) -> (x, transformRecursiveTerm x y))
            . map (\(x, y) -> (x, fromJust y))
            . filter (\(_, x) -> isJust x)
            . map (\name -> (name, M.lookup name bindings))
            . map (\i -> (\(x, _, _) -> x) $ f i)
            $ concat oneRec

      -- update bindings
      bindings' =
          foldr (\(x, y) bindings -> M.insert x y bindings) bindings recursive'

      -- transform mutually recursive functions into pure form
      mutRec' =
          concatMap transformMutuallyRecursiveTerms
            . map (\exprs -> map (\(x, y) -> (x, fromJust y)) exprs)
            . filter (\exprs -> all (isJust . snd) exprs)
            . map (\names -> map (\name -> (name, M.lookup name bindings')) names)
            . map (\is -> map (\i -> (\(x, _, _) -> x) $ f i) is)
            $ mutRec

      bindings'' =
          foldr (\(x, y) bindings -> M.insert x y bindings) bindings' mutRec'

      -- build new dependency graph with mutually recursive functions
      (gr', f', g') = buildDependencyGraph bindings''

      -- compute evaluation order by toposort
      order         = G.topSort gr'
      order'        = map (\i -> (\(x, _, _) -> x) $ f i) order

      -- expand terms
      bindings'''   = expand order' bindings''

  in  bindings'''

