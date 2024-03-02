{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
-- {-# HLINT ignore "Use <$>" #-}

module Parser.Lang where

import AST
import Parser.Lib
import Parser.Literal
import Parser.Operator

import           Control.Applicative hiding (many, some)
import           Control.Monad.Combinators.Expr
import           Data.Functor (($>))
import           Data.Void
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Debug



allowedIdentiferChars :: Parser Char
allowedIdentiferChars = letterChar <|> digitChar <|> char '_' <|> char '\''

variableIdentifierP :: Parser Text
variableIdentifierP = lexemeP $ T.pack <$> ((:) <$> lowerChar <*> many allowedIdentiferChars)

identifierP :: Parser Text
identifierP = lexemeP $ T.pack <$> ((:) <$> letterChar <*> many allowedIdentiferChars)

-- TODO: add handling for polymorphic types (e.g foo: a -> a)
typeIdentifierP :: Parser Text
typeIdentifierP = upperChar *> identifierP

typeP :: Parser Type
typeP = choice
  [ symbolP "String" $> TString
  , symbolP "Int" $> TInt
  , symbolP "Float" $> TFloat
  , symbolP "Bool" $> TBool
  , parensP typeP
  , TVar . T.pack <$> some letterChar
  , TCon . T.pack <$> some letterChar
  ]

variableUsageP :: Parser Expr
variableUsageP = do
  varName <- maybeDbg "variableUsageP" variableIdentifierP
  pure $ EVar $ Variable varName Nothing

baseExprP :: Parser Expr
baseExprP = maybeDbg "baseExprP" (
            maybeDbg "baseExprP::parensP exprP"   (try $ parensP exprP)
        <|> maybeDbg "baseExprP::lambdaP"         (lambdaP)
        <|> maybeDbg "baseExprP::literalP"        (literalP)
        <|> maybeDbg "baseExprP::funApplicationP" (try funApplicationP)
        <|> maybeDbg "baseExprP::variableUsageP"  (variableUsageP)
        )

variableTypeP :: Parser (Maybe Type)
variableTypeP = optional $ (symbolP ":") *> typeP

infixOpAsPrefixP :: Parser Expr
infixOpAsPrefixP = choice [
    try leftPartialInfixOpP,
    try rightPartialInfixOpP,
    try (do
      op <- unappliedInfixOpP
      pure $ EVar $ Variable op Nothing
    )
  ]
  where
    unappliedInfixOpP = maybeDbg "infixOpAsPrefixP::parenthRawInfixOpP" (parensP rawInfixOpP)
    leftPartialInfixOpP = parensP $ do
      left <- maybeDbg "infixOpAsPrefixP::left::expr" baseExprP
      op <- maybeDbg "infixOpAsPrefixP::left::op" rawInfixOpP
      pure $ EApply (EVar $ Variable op Nothing) left
    rightPartialInfixOpP = parensP $ do
      op <- maybeDbg "infixOpAsPrefixP::right::op" rawInfixOpP
      right <- maybeDbg "infixOpAsPrefixP::right::expr" baseExprP
      pure $ ELambda [FPSimple $ Variable "__partialInfixLambdaParam0" Nothing] (EApply
                                                                                 (EApply
                                                                                   (EVar (Variable op Nothing))
                                                                                   (EVar (Variable "__partialInfixLambdaParam0" Nothing)))
                                                                                 right)
                                                                                 
-- TODO: handle type constructor and diff between type, poly, data, variable, etc
declaractionP :: Parser Expr
declaractionP = do
  varName <- maybeDbg "declaractionP::varName" identifierP
  varType <- maybeDbg "declaractionP::varType" variableTypeP
  maybeDbg "declaractionP::symbol=" (symbolP "=")
  varValue <- maybeDbg "declaractionP::varValue" exprP
  pure $ ELet (Variable varName varType) varValue

-- \(i: Int): Bool -> mod(i, 2) == 0
lambdaP :: Parser Expr
lambdaP = do
  maybeDbg "lambdaP::symbol\\" $ symbolP "\\"
  params <- maybeDbg "lambdaP::params" lambdaParamsP
  returnType <- maybeDbg "lambdaP::returnType" (symbolP ":" *> typeP)
  symbolP "->"
  body <- maybeDbg "lambdaP::body" exprP
  pure $ ELambda params body

lambdaParamsP :: Parser [FunParam]
lambdaParamsP = choice [parensP (sepBy funParamP (symbolP ",")), sepBy funParamP (symbolP ",")]
  where
    funParamP = do
      varName <- lexemeP $ T.pack <$> some letterChar
      FPSimple . Variable varName <$> variableTypeP

funApplicationP :: Parser Expr
funApplicationP = do
  fun <- maybeDbg "funApplicationP::fun" funApp'
  args <- maybeDbg "funApplicationP::args" $ some exprP
  -- args <- maybeDbg "funApplicationP::args" $ some $ try $ sc *> exprP
  pure $ foldl EApply fun args
  where
    funApp' = maybeDbg "funApp'" (
              maybeDbg "funApp'::infixOpAsPrefixP" (try infixOpAsPrefixP)
          <|> maybeDbg "funApp'::parensP exprP"    (try $ parensP exprP)
          <|> maybeDbg "funApp'::lambdaP"          (lambdaP)
          <|> maybeDbg "funApp'::literalP"         (literalP)
          <|> maybeDbg "funApp'::variableUsageP"   (variableUsageP)
          -- <|> maybeDbg "funApp'::funApplicationP"  (funApplicationP)
        )

infixExprP :: Parser Expr
infixExprP = maybeDbg "infixExprP" $ makeExprParser (maybeDbg "infixExprP::baseExprP" baseExprP) operatorTable

exprP :: Parser Expr
exprP = maybeDbg "exprP::infixExprP" infixExprP

rootExprP :: Parser Expr
rootExprP = maybeDbg "rootExprP" $ ((maybeDbg "rootExprP::declaractionP" (try declaractionP)) <|> (maybeDbg "rootExprP::exprP" exprP)) <* eol