{
{-# OPTIONS -w #-}
module Lexer where

import Prelude hiding (lex)
import Control.Monad (liftM)
import Language (Name)
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [A-Za-z]

tokens :-
  $white+                                ;
  "--".*                                 ;
  \/                                     { lex' TokenLambda }
  as                                     { lex' TokenAs }
  nat                                    { lex' TokenTypeNat }
  list                                   { lex' TokenTypeList }              
  pair                                   { lex' TokenTypePair }
  bool                                   { lex' TokenTypeBool }
  show                                   { lex' TokenShow }
  bindings                               { lex' TokenBindings }
  [$alpha $digit] [$alpha $digit \_ \']* { lex TokenName }
  \.                                     { lex' TokenDot }
  \(                                     { lex' TokenLParen }
  \)                                     { lex' TokenRParen }
  \=                                     { lex' TokenEq }
  \;                                     { lex' TokenSemi }
  \:                                     { lex' TokenColon }
{

data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving ( Show )

data TokenClass
  = TokenLambda
  | TokenName Name
  | TokenDot
  | TokenLParen
  | TokenRParen
  | TokenEq
  | TokenSemi
  | TokenAs
  | TokenEOF
  | TokenTypeNat
  | TokenTypeList
  | TokenTypePair
  | TokenTypeBool
  | TokenShow
  | TokenColon
  | TokenBindings
  deriving (Show)


unLex :: TokenClass -> String
unLex TokenLambda = "/"
unLex (TokenName x) = show x
unLex TokenDot = "."
unLex TokenLParen = "("
unLex TokenRParen = ")"
unLex TokenEq = "="
unLex TokenSemi = ";"
unLex TokenAs = "as"
unLex TokenTypeNat = "nat"
unLex TokenTypeList = "list"
unLex TokenTypePair = "pair"
unLex TokenShow = "show"
unLex TokenColon = ":"
unLex TokenBindings = "bindings"
unLex TokenEOF = "<EOF>"

alexEOF :: Alex Token
alexEOF = do
    (p,_,_,_) <- alexGetInput
    return $ Token p TokenEOF

lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}