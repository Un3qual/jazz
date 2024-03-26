{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE LambdaCase #-}
-- {-# HLINT ignore "Use <$>" #-}

module Parser.Lang where

import AST
import Parser.Lib
import Parser.Literal
import Parser.Operator

import           Control.Applicative hiding (many, some)
import           Control.Monad.Combinators.Expr
import           Data.Functor (($>))
import           Data.Maybe (fromJust)
import           Data.Void
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Debug
import           Data.Foldable (foldl')


reservedWords :: [Text]
reservedWords = ["import", "module", "if", "else", "let", "data", "as", "case"]

allowedIdentiferChars :: Parser Char
allowedIdentiferChars = letterChar <|> digitChar <|> char '_' <|> char '\''

variableIdentifierP :: Parser Text
variableIdentifierP = identifierP lowerChar

identifierP :: Parser Char -> Parser Text
identifierP firstCharP = (lexemeP . try) (p >>= ensureNotReserved)
  where
    p = T.pack <$> ((:) <$> firstCharP <*> many allowedIdentiferChars)
    ensureNotReserved x = if x `elem` reservedWords
      then fail $ "keyword " ++ T.unpack x ++ "is reserved, and cannot be an identifier"
      else pure x

-- TODO: add handling for polymorphic types (e.g foo: a -> a)
-- this should really be called typeclassIdentifierP
typeIdentifierP :: Parser Text
typeIdentifierP = identifierP upperChar

-- moduleIdentifierP :: Parser [Text]
-- moduleIdentifierP = sepBy typeIdentifierP (symbolP "/")

typeP :: Parser Type
typeP = choice
  [
    lambdaTypeP
  , symbolP "String" $> TString
  , symbolP "Int" $> TInt
  , symbolP "Float" $> TFloat
  , symbolP "Bool" $> TBool
  , try tupleTypeP
  , listTypeP
  , parensP typeP
  , TVar <$> variableIdentifierP
  -- , TCon . T.pack <$> some letterChar
  ]

lambdaTypeP :: Parser Type
lambdaTypeP = sepBy2 nonLambdaTypeP (symbolP "->") >>= \case
  [x] -> pure x
  (x:xs) -> pure $ foldl TLambda x xs
  _ -> fail "Invalid lambda type"
  where
    nonLambdaTypeP = choice
      [ symbolP "String" $> TString
      , symbolP "Int" $> TInt
      , symbolP "Float" $> TFloat
      , symbolP "Bool" $> TBool
      , try tupleTypeP
      , listTypeP
      , parensP typeP
      , TVar <$> variableIdentifierP
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
        <|> maybeDbg "baseExprP::typeSignatureP"   (try typeSignatureP)
        <|> maybeDbg "baseExprP::funApplicationP"  (try funApplicationP)
        <|> maybeDbg "baseExprP::variableUsageP"   (variableUsageP)
        )

variableTypeP :: Parser (Maybe Type)
variableTypeP = optional $ (symbolP ":") *> typeP

typeSignatureP :: Parser Expr
typeSignatureP = do
  varName <- maybeDbg "typeSignatureP::varName" variableIdentifierP
  maybeDbg "typeSignatureP::symbol::" (symbolP "::")
  typeclassConstraints <- (maybeDbg "typeSignatureP::typeclassConstraintsP" typeclassConstraintsP) <|> pure []
  varType <- maybeDbg "typeSignatureP::varType" typeP
  pure $ ETypeSignature (Variable varName Nothing) typeclassConstraints varType
  where
    typeclassConstraintsP = do
      symbolP "@"
      tcConstraints <- curlyBraceP (sepBy ((,) <$> typeIdentifierP <*> variableIdentifierP) (symbolP ","))
      symbolP ":"
      pure $ map (\(tc, var) -> Variable var (Just $ TCon tc)) tcConstraints


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
      pure $ ELambda (Just $ FPSimple $ Variable "__partialInfixLambdaParam0" Nothing) (EApply
                                                                                        (EApply
                                                                                          (EVar (Variable op Nothing))
                                                                                          (EVar (Variable "__partialInfixLambdaParam0" Nothing)))
                                                                                        right) Nothing

-- TODO: handle type constructor and diff between type, poly, data, variable, etc
declaractionP :: Parser Expr
declaractionP = do
  varName <- maybeDbg "declaractionP::varName" variableIdentifierP
  -- varType <- maybeDbg "declaractionP::varType" variableTypeP
  maybeDbg "declaractionP::symbol=" (symbolP "=")
  varValue <- maybeDbg "declaractionP::varValue" exprP
  pure $ ELet (Variable varName Nothing) varValue

-- \(i: Int): Bool -> mod(i, 2) == 0
lambdaP :: Parser Expr
lambdaP = do
  maybeDbg "lambdaP::symbol\\" $ symbolP "\\"
  params <- maybeDbg "lambdaP::params" lambdaParamsP
  returnType <- maybeDbg "lambdaP::returnType" $ optional (symbolP ":" *> typeP)
  symbolP "->"
  body <- maybeDbg "lambdaP::body" exprP
  pure $ foldr (\param acc -> ELambda param acc Nothing) body params

lambdaParamsP :: Parser [Maybe FunParam]
lambdaParamsP = parensP (sepBy (optional lambdaParamP) (symbolP ","))

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
  args <- maybeDbg "funApplicationP::args" $ some nonFunAppExprP
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
    nonFunAppExprP = maybeDbg "nonFunAppP" (
            maybeDbg "nonFunAppP::parensP exprP"    (try $ parensP exprP)
        <|> maybeDbg "nonFunAppP::listLiteralP"     (listLiteralP)
        <|> maybeDbg "nonFunAppP::tupleLiteralP"    (try tupleLiteralP)
        <|> maybeDbg "nonFunAppP::lambdaP"          (lambdaP)
        <|> maybeDbg "nonFunAppP::literalP"         (literalExprP)
        <|> maybeDbg "nonFunAppP::variableUsageP"   (variableUsageP)
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