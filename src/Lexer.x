{
{-# LANGUAGE FlexibleContexts #-}

module Lexer (
    Token(..),
    scanTokens
) where

import Syntax

import Control.Monad.Except
}

%wrapper "basic"

$alpha = [a-zA-Z]
$digit = [0-9]
$eol = [\n]

tokens :-
  $eol                                               ;
  $white+                                            ; 

  "#".*                                              ; 

  [\;]                          { \s -> TokenSemi }
  [\/]                          { \s -> TokenLambda }
  [\.]                          { \s -> TokenDot }   
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  [\=]                          { \s -> TokenEq }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s } 

{
data Token
  = TokenLambda
  | TokenDot
  | TokenLParen
  | TokenRParen
  | TokenSym String
  | TokenEOF
  | TokenEq
  | TokenSemi
  deriving (Eq, Show)

scanTokens :: String -> Except String [Token]
scanTokens str = go ('\n', [], str) where
  go inp@(_,_bs,str) =
    case alexScan inp 0 of
      AlexEOF -> return []
      AlexError _ -> throwError "Invalid lexeme."
      AlexSkip inp' len -> go inp'
      AlexToken inp' len act -> do
        res <- go inp'
        let rest = act (take len str)
        return (rest : res)
}