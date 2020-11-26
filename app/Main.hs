{-# LANGUAGE TemplateHaskell #-}
module Main where

import System.Console.Haskeline
import System.Environment
import Control.Monad.State
import Data.Map (Map)
import Data.Maybe
import Data.List (intercalate)
import Data.Either
import Lens.Micro.Platform

import qualified Data.Map as M

import Parser (parseConsoleInput, parseProgram)
import Language
import Conversion
import Compile (compile, expandNew)
import Prims (builtins)

data ProgramState a = ProgramState {
  _mappings :: Map Name UntypedExpr
}
makeLenses ''ProgramState

formatResult :: TypedResult -> String 
formatResult r = case r of
    UntypedResult e -> show e
    NatResult (Nat n) -> show n
    BoolResult b -> show b
    ListResult xs -> "[" ++ intercalate ", " (map formatResult xs) ++ "]"
    PairResult (Pair (x, y)) -> "(" ++ formatResult x ++ ", " ++ formatResult y ++ ")"

main :: IO ()
main = do
  args <- getArgs
  bindings <- fromRight [] <$> case args of
    [fd] -> do
      contents <- readFile fd
      return $ parseProgram fd contents

    _ -> return $ Right []
  let (Just bindings') = traverse toUntyped $ compile (M.map toTyped $ M.fromList bindings `M.union` builtins)
  runInputT defaultSettings (evalStateT loop (ProgramState bindings'))
  where 
      loop :: StateT (ProgramState a) (InputT IO) ()
      loop = do
        minput <- lift $ getInputLine "% "
        case minput of
          Just input ->
            case parseConsoleInput input of
              Right ci -> case ci of
                Assignment (y,e) -> do
                  bindings <- gets _mappings
                  let e' = fromJust $ expandNew bindings e
                  modify (over mappings (M.insert y e'))
                  lift . outputStrLn $ show e'
                  loop

                Evaluation ev -> do
                  bindings <- gets _mappings

                  let ev' = case ev of
                              TypedEvaluation e t -> TypedEvaluation (fromJust $ expandNew bindings e) t
                              UntypedEvaluation e -> UntypedEvaluation (fromJust $ expandNew bindings e)

                  let out = evaluate 10000 ev'
                  case out of
                    Just x -> lift $ outputStrLn $ formatResult x
                    Nothing -> lift $ outputStrLn "Nothing"
                  loop

                Command (Show Bindings) -> do
                  bindings <- M.assocs <$> gets _mappings
                  forM_ bindings $ \(y,e) -> do
                    let s = y ++ " = " ++ (show e)
                    lift $ outputStrLn s

                  lift . outputStrLn $ "Total Count: " ++ show (length bindings)

                  loop
              Left err -> loop
          _ -> loop