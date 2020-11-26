import           Test.Tasty
import           Test.Tasty.HUnit
import           DeBruijn (alphaEquivalent)
import           Parser                         ( parseExpr, parseProgram )
import           Core
import           Language
import Compile ( buildDependencyGraph, mutuallyRecursiveTerms )
import qualified Prims                         as P
import qualified Data.Set                      as S
import qualified Data.Graph                    as G
import qualified Data.Map                      as M
import qualified Data.Array                    as A
import           Data.Map                       ( Map )
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                , isNothing
                                                )
import           Data.Either                    ( isRight
                                                , isLeft
                                                )

main :: IO ()
main = defaultMain $ testGroup
  "Lib"
  [ testParseExpr 
  , testSubst
  , testNormalRedex
  , testFreeVars
  -- , testSubstitute
  , testMutuallyRecursiveTerms
  , testBuildDependencyGraph
  -- , testNormalNextRedex
  -- , testReduceOnce
  -- , testReduceNormal
  ]

testParseExpr :: TestTree
testParseExpr = testGroup
  "parseExpr"
  [ testCase "Simple" $ do
      let s = "/x.x"
          expected = ULam "x" (UVar "x")
          eitherActual = parseExpr s
      assertBool "Parsing should succeed" $ isRight eitherActual
      let Right actual = eitherActual
      assertBool "Expected matches actual" $ alphaEquivalent expected actual
  
  , testCase "App higher precedence" $ do
      let s = "/x.x y"
          expected = ULam "x" (UApp (UVar "x") (UVar "y"))
          eitherActual = parseExpr s
      assertBool "Parsing should succeed" $ isRight eitherActual
      let Right actual = eitherActual
      assertBool "Expected matches actual" $ alphaEquivalent expected actual
    ]

testSubst :: TestTree
testSubst = testGroup
  "parseExpr"
  [ testCase "Simple" $ do
      let e = Var "x"
          v = "x"
          sub = Var "y"
          e' = subst (v,sub) e :: TypedExpr ()
      e' @?= Var "y"

  , testCase "Inside different abstraction" $ do
      let e = Lam "z" (Var "x")
          v = "x"
          sub = Var "y"
          e' = subst (v,sub) e :: TypedExpr ()
      e' @?= Lam "z" (Var "y")

  , testCase "Inside same abstraction" $ do
      let e = Lam "x" (Var "x")
          v = "x"
          sub = Var "y"
          e' = subst (v,sub) e :: TypedExpr ()
      e' @?= Lam "x" (Var "x")

  , testCase "Nested abstraction" $ do
      let e = Lam "w" (App (Var "x") (Lam "z" (Var "x")))
          v = "x"
          sub = Var "y"
          e' = subst (v,sub) e :: TypedExpr ()
      e' @?= Lam "w" (App (Var "y") (Lam "z" (Var "y")))
  ]

testNormalRedex :: TestTree
testNormalRedex = testGroup
  "normalRedex"
  [ testCase "Simple redex" $ do
      let e = App (Lam "x" (Var "x")) (Var "y")
          r = normalRedex e :: Maybe (Redex ())

      case r of
        Just (UntypedRedex cxt x sub e1) -> do
          x @?= "x"
          sub @?= Var "y"
          e1 @?= Var "x"
          context cxt (App (Lam x e1) sub) @?= e 
        _ -> fail "No redex"

  , testCase "Inside lambda" $ do
      let e = Lam "z" (App (Lam "x" (Var "x")) (Var "y"))
          r = normalRedex e :: Maybe (Redex ())

      case r of
        Just (UntypedRedex cxt x sub e1) -> do
          x @?= "x"
          sub @?= Var "y"
          e1 @?= Var "x"
          context cxt (App (Lam x e1) sub) @?= e 
        _ -> fail "No redex"

  , testCase "No redex" $ do
      let e = App (Var "x") (Lam "z" (App (Var "z") (Var "y")))
          r = normalRedex e :: Maybe (Redex ())
        
      case r of
        Nothing -> return ()
        _ -> fail "Shouldn't have a redex"
  ]

testFreeVars :: TestTree
testFreeVars = testGroup
  "freeVars"
  [ testCase "No free var" $ do
    let s   = Lam "x" (Var "x")
        fvs = freeVars s
    S.size fvs @?= 0
  , testCase "One free var" $ do
    let s   = Lam "x" (App (Var "y") (Var "x"))
        fvs = freeVars s
    S.size fvs @?= 1
    assertBool "y should be in free vars of /x.yx" ("y" `S.member` fvs)
  ]

-- testReduceNormal :: TestTree
-- testReduceNormal = testGroup
--   "reduceNormal"
--   [ testCase "zeroPlusTwoIsTwo" $ do
--     let s = P.add # P.zero # P.two
--         expected = P.two
--         s' = reduceNormal s
--     assertBool "reduced should be equivalent" $
--       (alphaEquivalent s' expected)
--   , testCase "onePlusTwoIsThree" $ do
--     let s = P.add # P.one # P.two
--         expected = P.three
--         s' = reduceNormal s
--     assertBool "reduced should be equivalent" $
--       (alphaEquivalent s' expected)
--   , testCase "tAndTIsT" $ do
--     let s = P.and # P.t # P.t
--         expected = P.t
--         s' = reduceNormal s
--     assertBool "reduced should be equivalent" $ 
--       (alphaEquivalent s' expected)
--   ]

testMutuallyRecursiveTerms :: TestTree
testMutuallyRecursiveTerms = testGroup
  "mutuallyRecursiveTerms"
  [ testCase "Cycle" $ do
    let (gr, _, _) = G.graphFromEdges
          [("f", 0, [1]), ("g", 1, [2]), ("h", 2, [3]), ("i", 3, [0])]
        (mutRec, nonMutRec) = mutuallyRecursiveTerms gr
    length nonMutRec @?= 0
    length mutRec @?= 1
    length (head mutRec) @?= 4
    S.fromList (head mutRec) @?= S.fromList [0, 1, 2, 3]
  , testCase "Three SCCs" $ do
    let (gr, _, _) = G.graphFromEdges
          [ ("f", 0, [1])
          , ("g", 1, [0])
          , ("h", 2, [3])
          , ("i", 3, [1, 2])
          , ("j", 4, [])
          ]
        (mutRec, nonMutRec) = mutuallyRecursiveTerms gr
    length nonMutRec @?= 1
    length mutRec @?= 2
    let [scc1, scc2] = mutRec
    length scc1 @?= 2
    length scc2 @?= 2
  ]

readBindings :: String -> IO (Either String (Map Name (TypedExpr a)))
readBindings fd = do
    contents <- readFile fd
    let prog = parseProgram fd contents 
    return $ M.map toTyped . M.fromList <$> prog

testBuildDependencyGraph :: TestTree
testBuildDependencyGraph = testGroup
  "buildDependencyGraph"
  [ testCase "simple.lpp" $ do
    eitherBindings <- readBindings "test/simple.lpp"
    case eitherBindings of
      Left  err      -> assertFailure $ "failed parsing: " ++ err
      Right bindings -> do
        let (gr, _, vertexFromName) = buildDependencyGraph bindings
            vertices = mapM vertexFromName ["T", "F", "or", "main"]
        length gr @?= 4
        assertBool "vertices must exist" (isJust vertices)
        let [_T, _F, _or, _main] = fromJust vertices
        (gr A.! _T) @?= []
        (gr A.! _F) @?= []
        (gr A.! _or) @?= [_T]
        S.fromList (gr A.! _main) @?= S.fromList [_T, _F, _or]
  , testCase "rec.lpp" $ do
    eitherBindings <- readBindings "test/rec.lpp"
    case eitherBindings of
      Left  err      -> assertFailure $ "failed parsing: " ++ err
      Right bindings -> do
        let (gr, _, vertexFromName) = buildDependencyGraph bindings
            vertices                = mapM vertexFromName ["fib", "main"]
        length gr @?= 2
        assertBool "vertices must exist" (isJust vertices)
        let [_fib, _main] = fromJust vertices
        (gr A.! _fib) @?= [_fib]
        (gr A.! _main) @?= [_fib]
  , testCase "mutrec.lpp" $ do
    eitherBindings <- readBindings "test/mutrec.lpp"
    case eitherBindings of
      Left  err      -> assertFailure $ "failed parsing: " ++ err
      Right bindings -> do
        let (gr, _, vertexFromName) = buildDependencyGraph bindings
            vertices = mapM vertexFromName ["isEven", "isOdd", "main"]
        length gr @?= 3
        assertBool "vertices must exist" (isJust vertices)
        let [_isEven, _isOdd, _main] = fromJust vertices
        (gr A.! _isEven) @?= [_isOdd]
        (gr A.! _isOdd) @?= [_isEven]
        (gr A.! _main) @?= [_isEven]
  ]