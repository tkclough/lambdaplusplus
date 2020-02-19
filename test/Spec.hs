import           Test.Tasty
import           Test.Tasty.HUnit
import           Lib
import           StrictSyntax
import qualified Data.Set                      as S
import qualified Data.Graph                    as G

main :: IO ()
main = defaultMain
  $ testGroup "Lib" [testFreeVars, testSubstitute, testRecursiveTerms]

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

testSubstitute :: TestTree
testSubstitute = testGroup
  "substitute"
  [ testCase "Simple" $ do
    let s       = Lam "x" (App (Var "x") (Var "y"))
        subexpr = Var "z"
        s'      = substitute "y" subexpr s
    s' @?= Lam "x" (App (Var "x") (Var "z"))
  , testCase "Substitute on bound variable" $ do
    let s       = Lam "x" (Var "x")
        subexpr = Var "z"
        s'      = substitute "x" subexpr s
    s' @?= Lam "x" (Var "x")
  ]

testRecursiveTerms :: TestTree
testRecursiveTerms = testGroup
  "recursiveTerms"
  [ testCase "One recursive, one not" $ do
      let (gr, _, _) = G.graphFromEdges [("f", 0, [0]), ("g", 1, [0])]
          (recTerms, nonrecTerms) = recursiveTerms gr
      recTerms @?= [0]
      nonrecTerms @?= [1]
  ]

applyingRedexIsIdentity :: Strategy -> Expr -> Bool
applyingRedexIsIdentity strat s = case strat s of
  Nothing -> True
  Just (f, s') -> f s' == s