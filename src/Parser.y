{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
    parseExpr,
    parseDeclaration,
    parseDeclarations,
    parseConsoleInput
) where

import Lexer
import Syntax
import Data.Map (Map, fromList)

import Control.Monad.Except
}

%tokentype { Token }

%token
    '/' {TokenLambda}
    '.' {TokenDot}
    '(' {TokenLParen}
    ')' {TokenRParen}
    '=' {TokenEq}
    ';' {TokenSemi}
    VAR {TokenSym $$}

%monad { Except String } { (>>=) } { return }
%error { parseError }

%name expr Expr
%name declaration Declaration
%name declarations Declarations
%name consoleInput ConsoleInput
%%
ConsoleInput : Expr             { Reduction $1 }
             | Declaration      { Assignment $1 }

Declarations : Declaration ';'              { [$1] }
             | Declaration ';' Declarations { $1:$3 }

Declaration : VAR '=' Expr      { ($1, $3) }

Expr : '/' Variables '.' Expr   { Lam $2 $4 }
     | Fact                     { $1 }

Fact : Fact Atom                { App $1 $2 }
     | Atom                     { $1 }

Atom : '(' Expr ')'             { $2 }
     | VAR                      { Var $1 } 

Variables : VAR Variables       { $1:$2 }
          | VAR                 { [$1] }

{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of input"

parseDeclarations :: String -> Either String (Map Name Expr)
parseDeclarations input = runExcept $ do
  tokenStream <- scanTokens input
  fromList <$> declarations tokenStream

parseDeclaration :: String -> Either String (Name, Expr)
parseDeclaration input = runExcept $ do
  tokenStream <- scanTokens input
  declaration tokenStream

parseExpr :: String -> Either String Expr
parseExpr input = runExcept $ do
  tokenStream <- scanTokens input
  expr tokenStream

parseConsoleInput :: String -> Either String ConsoleInput
parseConsoleInput input = runExcept $ do
  tokenStream <- scanTokens input
  consoleInput tokenStream
}