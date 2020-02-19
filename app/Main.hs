{-# LANGUAGE TemplateHaskell #-}

module Main where

import Lib
import StrictSyntax (fromExtended)
import Parser (parseDeclarations)
import System.Environment (getArgs)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as M
import Debug.Trace (traceShow)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Expected a file"
    file:_ -> do
      contents <- readFile file
      let eitherDecls = parseDeclarations contents
      case eitherDecls of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right decls' -> do
          print decls'
          let strictDecls = M.map fromExtended decls'
              expanded = traceShow strictDecls $ compile "main" strictDecls
          if isJust expanded
            then do let reduced = reduceNormal $ fromJust expanded
                    putStrLn . format $ reduced
            else putStrLn "reduction failed"


{-
compile :: Map Name Expr -> Name -> Expr
compile decls entry = 
  let fvs = M.map freeVars decls
      asList = M.toList fvs
      lnodes = zip [0..] $ map fst asList
      ledges = concatMap (\((name, fv), (_, i)) -> ()) (zip asList lnodes)
-}
-- Dependency Graph: DynGraph Name ()
-- Declarations: Map Name Expr

-- data Env = Env {
--   _decl :: Map Name Expr,
--   _dep :: Graph
-- }
-- makeLenses ''Env

-- main :: IO ()
-- main = runInputT defaultSettings (evalStateT loop (Env G.empty M.empty 0))
--   where 
--     popNode = do
--       x <- use node
--       node += 1
--       return x

--     loop :: StateT Env (InputT IO) ()
--     loop = do
--       input <- lift $ getInputLine "$ "
--       case input of
--         Nothing -> return ()
--         Just l -> do
--           let consoleInput = parseConsoleInput l
--           case consoleInput of
--             Right (Reduction s) -> do
--               -- convert to strict form
--               let s' = fromExtended s
--               lift . outputStrLn $ "expr: " ++ format s'
--               built <- flip buildTerm s' <$> use decl
--               lift . outputStrLn $ "substituted: " ++ format built
--               lift . outputStrLn $ "reduced: " ++ format (reduceNormal built)
--             Right (Assignment (v, s)) -> do
--               let s' = fromExtended s
--                   fvs = S.toList $ freeVars s'
--               -- make a new node for each fv and for v
--               nextNode <- popNode
--               dep %= G.insNode (nextNode, v)

--               forM_ fvs $ \fv -> do
--                 nextNode <- popNode
--                 dep %= G.insNode (nextNode, fv)
              
--               -- insert declaration
--               decl %= M.insert v s'

--               lift . outputStrLn $ "ok."
--             Left e -> lift . outputStrLn $ "error: " ++ e
--       loop
