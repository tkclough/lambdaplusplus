module Lib where

import           StrictSyntax
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import           Data.List                      ( partition
                                                , intercalate
                                                )
import           Data.Graph                     ( Graph
                                                , Vertex
                                                )
import qualified Data.Array                    as A
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Graph                    as G
import qualified Data.Tree                     as T
import           Parser                         ( parseExpr
                                                , parseDeclarations
                                                )
import           Control.Monad.State
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           Debug.Trace

type RedexPair = (Expr -> Expr, Expr)
type Strategy = Expr -> Maybe RedexPair

-- | Fixed point combinator.
y :: Expr
y = Lam
  "g"
  (App (Lam "x" (App (Var "g") (App (Var "x") (Var "x"))))
       (Lam "x" (App (Var "g") (App (Var "x") (Var "x"))))
  )

-- | Substitute a number of names into an expression
substitutes :: Map Name Expr -> Expr -> Expr
substitutes = go S.empty
 where
  go :: Set Name -> Map Name Expr -> Expr -> Expr
  go bound decls s = case s of
    Var v -> if v `S.member` bound
      then Var v
      else if v `M.member` decls then fromJust . M.lookup v $ decls else Var v
    App s1 s2 -> App (go bound decls s1) (go bound decls s2)
    Lam v  s1 -> Lam v (go (S.insert v bound) decls s1)

-- `substitute v r s` means replace all occurrences of v in s with r
substitute :: Name -> Expr -> Expr -> Expr
substitute k v e = case e of
  Var u     -> if k == u then v else Var u
  App e1 e2 -> App (substitute k v e1) (substitute k v e2)
  Lam u  e1 -> if k == u
    then Lam u e1 -- k is a bound var in e1
    else Lam u (substitute k v e1) -- k is free in e1

-- | Compute free variables for an expression
freeVars :: Expr -> Set Name
freeVars = go S.empty
 where
  go :: Set Name -> Expr -> Set Name
  go bound s = case s of
    App s1 s2 -> go bound s1 `S.union` go bound s2
    Lam v  s1 -> go (S.insert v bound) s1
    Var v     -> if S.member v bound then S.empty else S.singleton v

format :: Expr -> String
format s = case s of
  Lam v  s  -> "(" ++ "/" ++ v ++ "." ++ format s ++ ")"
  App s1 s2 -> "(" ++ format s1 ++ " " ++ format s2 ++ ")"
  Var v     -> v

parseStrictExpr :: String -> Either String Expr
parseStrictExpr = fmap fromExtended . parseExpr

-- leftmost outermost redex
normalNextRedex :: Strategy
normalNextRedex s = case s of
  App (Lam _ _) s2 -> Just (id, s)
  App s1        s2 -> case normalNextRedex s1 of
    Just (f, r) -> Just (\r' -> App (f r') s2, r)
    Nothing     -> case normalNextRedex s2 of
      Just (f, r) -> Just (\r' -> App s1 (f r'), r)
      Nothing     -> Nothing
  Lam v s1 -> case normalNextRedex s1 of
    Just (f, r) -> Just (\r' -> Lam v (f r'), r)
    Nothing     -> Nothing
  _ -> Nothing

reduceOnce :: Strategy -> Expr -> Either Expr Expr
reduceOnce strat s = case strat s of
  Nothing                   -> Left s
  Just (f, App (Lam v r) t) -> Right . f . substitute v t $ r

reduceExpr :: Strategy -> Expr -> Expr
reduceExpr strat s = case reduceOnce strat s of
  Left  s' -> s'
  Right s' -> reduceExpr strat s'

reduceExprWithSteps :: Strategy -> Expr -> [Expr]
reduceExprWithSteps strat s = case reduceOnce strat s of
  Left  s' -> [s]
  Right s' -> s : (reduceExprWithSteps strat s')

reduceNormal :: Expr -> Expr
reduceNormal = reduceExpr normalNextRedex

testBindings :: String -> IO (Either String (Map Name Expr))
testBindings fd = do
  fileContents <- readFile fd
  let decls = parseDeclarations fileContents
  return (M.map fromExtended <$> decls)

-- | From bindings, build graph of which functions depend on which.
buildDependencyGraph
  :: Map Name Expr
  -> (Graph, Vertex -> (Name, Vertex, [Vertex]), Vertex -> Maybe Vertex)
buildDependencyGraph bindings =
  let -- map each name to sequential integers
      labels = M.fromList $ zip (M.keys bindings) [0 ..]

      -- compute free variables for each name, some of which refer to other functions
      fvs    = M.map freeVars bindings :: Map Name (Set Name)

      edgeList =
          map fromJust
            . filter isJust
            . map snd
            . M.toList
            . M.mapWithKey (f labels)
            $ fvs
  in  G.graphFromEdges edgeList

 where
    -- | Get the dependencies of a name, returning nothing if the named function doesn't exist
  f :: Map Name Vertex -> Name -> Set Name -> Maybe (Name, Vertex, [Vertex])
  f labels u vs = case M.lookup u labels of
    Nothing -> Nothing
    Just u' ->
      let maybeOutVertices = S.map (\v -> M.lookup v labels) vs
          outVertices      = S.map fromJust . S.filter isJust $ maybeOutVertices
      in  Just (u, u', S.toList outVertices)


-- | Partition terms into those that refer to themselves and those that don't.
recursiveTerms :: Graph -> ([Vertex], [Vertex])
recursiveTerms gr =
  (\(x, y) -> (concatMap snd x, concatMap snd y))
    . partition (\(u, vs) -> u `elem` vs)
    $ A.assocs gr

-- | Partition terms into groups of those that refer to each other, and terms
-- that don't have mutual recursion.
mutuallyRecursiveTerms :: Graph -> ([[Vertex]], [Vertex])
mutuallyRecursiveTerms gr =
  let (mutrec, notmutrec) =
          partition ((> 1) . length) . map T.flatten $ G.scc gr
  in  (mutrec, join notmutrec)

-- | Transform a recursive function defined in terms of itself into a function
-- that takes itself as an argument, and wrap in in a fixed point combinator.
transformRecursiveTerm :: Name -> Expr -> Expr
transformRecursiveTerm v s = App y $ Lam v (substitute v s (Var v))

-- | Transform a group of mutually recursive functions into a number of functions
-- that refer to a bundled function that includes all the mutually recursive
-- functions.
transformMutuallyRecursiveTerms :: [(Name, Expr)] -> [(Name, Expr)]
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
      (\(_, s) -> substitutes (M.map (\s -> App bundle s) nameToSelector) s)
      bindings

    -- bundle all the functions f1,...,fn into a single function 
    -- /s.s f1'...fn' where fi' is the substituted version of fi
    bundle = App y . Lam "s" $ foldl (\acc m -> App acc m) (Var "s") bindings'

    -- the ith function will be defined as bundle si
    bindings'' = M.map (\s -> App bundle s) nameToSelector
  in
    (bundleName, bundle) : (M.toList bindings'')

 where
  mkSelectors :: Int -> [Expr]
  mkSelectors n =
    let names            = map (('s' :) . show) [0 .. n]
        selectorTemplate = \v -> foldr (\u acc -> Lam u acc) (Var v) names
    in  map selectorTemplate names

-- | Replace function calls with their definition.
expand :: [Name] -> Map Name Expr -> Map Name Expr
expand order bindings = foldl go bindings order
 where
  go :: Map Name Expr -> Name -> Map Name Expr
  go bindings v =
    let val = fromJust $ M.lookup v bindings
    in  M.map (\m -> substitute v val m) bindings


-- | Build up an entry point by finding an evaluation order and expanding terms.
compile :: Name -> Map Name Expr -> Maybe Expr
compile entryPoint bindings =
  let (gr, f, g)                = buildDependencyGraph bindings

      -- transform recursive functions into pure form
      (recursive, nonRecursive) = recursiveTerms gr
      recursive' =
          map (\(x, y) -> (x, transformRecursiveTerm x y))
            . map (\(x, y) -> (x, fromJust y))
            . filter (\(_, x) -> isJust x)
            . map (\name -> (name, M.lookup name bindings))
            . map (\i -> (\(x, _, _) -> x) $ f i)
            $ recursive

      -- update bindings
      bindings' =
          foldr (\(x, y) bindings -> M.insert x y bindings) bindings recursive'

      -- transform mutually recursive functions into pure form
      (mutRec, nonMutRec) = mutuallyRecursiveTerms gr
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
  in  M.lookup entryPoint bindings'''
