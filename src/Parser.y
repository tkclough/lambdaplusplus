{
{-# OPTIONS -w #-}
module Parser where

import Language
import Lexer
}

%name expr Expr
%name program Program
%name consoleInput ConsoleInput
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { happyError }

%token
      '/'      { Token _ TokenLambda }
      as        { Token _ TokenAs }
      name      { Token _ (TokenName $$) }
      nat       { Token _ TokenTypeNat }
      list      { Token _ TokenTypeList }
      pair      { Token _ TokenTypePair }
      bool      { Token _ TokenTypeBool }
      show      { Token _ TokenShow }
      bindings  { Token _ TokenBindings }
      '.'       { Token _ TokenDot }
      '('       { Token _ TokenLParen }
      ')'       { Token _ TokenRParen }
      '='       { Token _ TokenEq }
      ';'       { Token _ TokenSemi }
      ':'       { Token _ TokenColon }

%%

Program : Assignment ';' Program    { $1:$3 }
        | Assignment ';'            { [$1] }

ConsoleInput : Assignment           { Assignment $1 }
             | Evaluation           { Evaluation $1 }
             | Command              { Command $1   }

Assignment : name '=' Expr          { ($1, $3) }
           | name Names '=' Expr    { ($1, foldr (\n e -> ULam n e) $4 $2)}
           
Evaluation : Expr                   { UntypedEvaluation $1 }
           | Expr as Type           { TypedEvaluation $1 $3 }

Command : ':' ConsoleCommand        { $2 }

ConsoleCommand : show ConsoleShowable { Show $2 }

ConsoleShowable : bindings            { Bindings }

Expr : '/' Names '.' Expr           { foldr (\n e -> ULam n e) $4 $2 }
     | Term                         { $1 }
    
Names : name Names                  { $1:$2 }
      | name                        { [$1] }

Term : Term Factor                  { UApp $1 $2 }
     | Factor                       { $1 }

Factor : name                       { UVar $1 }
       | '(' Expr ')'               { $2 }

Type : nat                          { NatType }
     | bool                         { BoolType }
     | list Type                    { ListType $2 }
     | pair Type Type               { PairType $2 $3 }
     | '(' Type ')'                 { $2 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseExpr :: String -> Either String UntypedExpr
parseExpr input = runAlex input expr

--parseExpr' :: String -> Either String (TypedExpr a)
--parseExpr' input = toTyped <$> parseExpr input

parseProgram :: FilePath -> String -> Either String (Program a)
parseProgram = runAlex' program

parseConsoleInput :: String -> Either String (ConsoleInput a)
parseConsoleInput input = runAlex input consoleInput
}