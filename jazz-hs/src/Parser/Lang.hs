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


reservedWords :: [Text]
reservedWords = ["import", "module", "if", "else"]

allowedIdentiferChars :: Parser Char
allowedIdentiferChars = letterChar <|> digitChar <|> char '_' <|> char '\''

variableIdentifierP :: Parser Text
variableIdentifierP = lexemeP $ T.pack <$> ((:) <$> lowerChar <*> many allowedIdentiferChars)

identifierP :: Parser Text
identifierP = lexemeP $ T.pack <$> ((:) <$> letterChar <*> many allowedIdentiferChars)

-- TODO: add handling for polymorphic types (e.g foo: a -> a)
typeIdentifierP :: Parser Text
typeIdentifierP = upperChar *> identifierP

-- moduleIdentifierP :: Parser [Text]
-- moduleIdentifierP = sepBy typeIdentifierP (symbolP "/")

typeP :: Parser Type
typeP = choice
  [ symbolP "String" $> TString
  , symbolP "Int" $> TInt
  , symbolP "Float" $> TFloat
  , symbolP "Bool" $> TBool
  , try tupleTypeP
  , listTypeP
  , parensP typeP
  , TVar . T.pack <$> some letterChar
  , TCon . T.pack <$> some letterChar
  ]

tupleTypeP :: Parser Type
tupleTypeP = do
  maybeDbg "tupleTypeP::symbol(" $ symbolP "("
  elements <- maybeDbg "tupleTypeP::elements" $ sepBy typeP (symbolP ",")
  maybeDbg "tupleTypeP::symbol)" $ symbolP ")"
  pure $ TTuple elements

listTypeP :: Parser Type
listTypeP = do
  maybeDbg "listTypeP::symbol[" $ symbolP "["
  elementType <- maybeDbg "listTypeP::elementType" typeP
  maybeDbg "listTypeP::symbol]" $ symbolP "]"
  pure $ TList elementType
-- funParamTypeP :: Parser Type
-- funParamTypeP = choice
--   [ symbolP "String" $> TString
--   , symbolP "Int" $> TInt
--   , symbolP "Float" $> TFloat
--   , symbolP "Bool" $> TBool
--   , parensP typeP
--   , TVar . T.pack <$> some letterChar
--   , TCon . T.pack <$> some letterChar
--   ]

listLiteralP :: Parser Expr
listLiteralP = do
  maybeDbg "listLiteralP::symbol[" $ symbolP "["
  elements <- maybeDbg "listLiteralP::elements" $ sepBy exprP (symbolP ",")
  maybeDbg "listLiteralP::symbol]" $ symbolP "]"
  pure $ ELiteral (LList elements)

tupleLiteralP :: Parser Expr
tupleLiteralP = do
  maybeDbg "tupleLiteralP::symbol(" $ symbolP "("
  elements <- maybeDbg "tupleLiteralP::elements" $ sepBy2 exprP (symbolP ",")
  maybeDbg "tupleLiteralP::symbol)" $ symbolP ")"
  pure $ ELiteral (LTuple elements)

variableUsageP :: Parser Expr
variableUsageP = do
  varName <- maybeDbg "variableUsageP" variableIdentifierP
  pure $ EVar $ Variable varName Nothing

baseExprP :: Parser Expr
baseExprP = maybeDbg "baseExprP" (
            maybeDbg "baseExprP::parensP exprP"    (try $ parensP exprP)
        <|> maybeDbg "baseExprP::listLiteralP"     (listLiteralP)
        <|> maybeDbg "baseExprP::tupleLiteralP"    (try tupleLiteralP)
        <|> maybeDbg "baseExprP::lambdaP"          (lambdaP)
        <|> maybeDbg "baseExprP::literalP"         (literalExprP)
        <|> maybeDbg "baseExprP::funApplicationP"  (try funApplicationP)
        <|> maybeDbg "baseExprP::variableUsageP"   (variableUsageP)
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
  returnType <- maybeDbg "lambdaP::returnType" $ optional (symbolP ":" *> typeP)
  symbolP "->"
  body <- maybeDbg "lambdaP::body" exprP
  pure $ ELambda params body

lambdaParamsP :: Parser [FunParam]
lambdaParamsP = parensP (sepBy lambdaParamP (symbolP ","))

lambdaParamP :: Parser FunParam
lambdaParamP = choice [funParamSimpleP, lambdaParamPatternP]
  where
    funParamSimpleP = do
      varName <- lexemeP $ T.pack <$> some letterChar
      FPSimple . Variable varName <$> variableTypeP

lambdaParamPatternP :: Parser FunParam
lambdaParamPatternP = choice [patternLiteralP, patternTupleP, patternListP]
  where
    patternLiteralP = FPPattern . PatternLiteral <$> (maybeDbg "lambdaParamPatternP::patternLiteralP" literalP)
    patternTupleP = FPPattern . PatternTuple <$> maybeDbg "lambdaParamPatternP::patternTupleP" (parensP (sepBy lambdaParamP (symbolP ",")))
    patternListP = FPPattern . PatternList <$> bracketsP (sepBy lambdaParamP (symbolP "|"))

funApplicationP :: Parser Expr
funApplicationP = do
  fun <- maybeDbg "funApplicationP::fun" funApp'
  args <- maybeDbg "funApplicationP::args" $ some exprP
  pure $ foldl EApply fun args
  where
    funApp' = maybeDbg "funApp'" (
              maybeDbg "funApp'::infixOpAsPrefixP" (try infixOpAsPrefixP)
          <|> maybeDbg "funApp'::parensP exprP"    (try $ parensP exprP)
          <|> maybeDbg "funApp'::listLiteralP"     (listLiteralP)
          <|> maybeDbg "funApp'::tupleLiteralP"    (try tupleLiteralP)
          <|> maybeDbg "funApp'::lambdaP"          (lambdaP)
          <|> maybeDbg "funApp'::literalP"         (literalExprP)
          <|> maybeDbg "funApp'::variableUsageP"   (variableUsageP)
        )



-- moduleP :: Parser Expr
-- moduleP = do
--   maybeDbg "moduleP::symbolModule" $ symbolP "module"
--   moduleName <- maybeDbg "moduleP::moduleName" $ lexemeP $ T.pack <$> some (letterChar <|> char '/')
--   maybeDbg "moduleP::symbol{" $ symbolP "{"
--   maybeDbg "moduleP::moduleBody" $ many (maybeDbg "moduleP::moduleBody::rootExprP" rootExprP)
--   maybeDbg "moduleP::symbol}" $ symbolP "}"
--   pure $ EVar $ Variable moduleName Nothing

-- importP :: Parser Expr
-- importP = do
--   maybeDbg "importP::symbolImport" $ symbolP "import"
--   importPath <- maybeDbg "importP::importPath" $ lexemeP $ T.pack <$> some (letterChar <|> char '/')
--   pure $ TLImport importPath

infixExprP :: Parser Expr
infixExprP = maybeDbg "infixExprP" $ makeExprParser (maybeDbg "infixExprP::baseExprP" baseExprP) operatorTable

exprP :: Parser Expr
exprP = maybeDbg "exprP::infixExprP" infixExprP

rootExprP :: Parser Expr
rootExprP = maybeDbg "rootExprP" $ (maybeDbg "rootExprP::declaractionP" (try declaractionP)) <|> (maybeDbg "rootExprP::exprP" exprP)