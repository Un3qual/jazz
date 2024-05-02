{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE LambdaCase #-}

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
import           Data.Foldable (foldl')
import Data.Type.Coercion (sym)

programP :: Parser SpannedProgram
programP = do
  maybeDbg "programP::skipEol" (skipMany eol)
  maybeDbg "programP::separatedExprs" (sepEndBy (maybeDbg "programP::rootExprP" rootExprP) (maybeDbg "programP::rootSeparator" (symbolP ".")))

baseExpressionsDoNotUse :: [Parser SpannedExpr]
baseExpressionsDoNotUse = [
    (try . parensP) exprP
  , try blockP
  , try importP
  , try moduleDeclP
  , try typeclassDeclP
  , try typeclassImplP
  , try dataDeclP
  , try caseP
  , listLiteralExprP
  , try tupleLiteralExprP
  , lambdaP
  , literalExprP
  , try typeSignatureP]

baseExprP :: Parser SpannedExpr
baseExprP = choice $ baseExpressionsDoNotUse <> [try funApplicationP, try infixOpAsPrefixP, variableUsageP]

identifierP :: Parser Char -> Parser Text
identifierP firstCharP = (lexemeP . try) (p >>= ensureNotReserved)
  where
    p = T.pack <$> (try internalIdentifier <|> try normalIdentifier <|> (pure <$> char '_'))
    ensureNotReserved x = if x `elem` reservedWords
      then fail $ "keyword " ++ T.unpack x ++ "is reserved, and cannot be an identifier"
      else pure x
    normalIdentifier = (:) <$> firstCharP <*> many allowedIdentiferChars
    internalIdentifier = (:) <$> char '$' <*> ((:) <$> firstCharP <*> many allowedIdentiferChars)

variableIdentifierP :: Parser Text
variableIdentifierP = identifierP lowerChar <|> parensP rawInfixOpP

functionNameIdentiferP :: Parser Text
functionNameIdentiferP = variableIdentifierP <|> identifierP upperChar

-- TODO: add handling for polymorphic types (e.g foo: a -> a)
-- this should really be called typeclassIdentifierP
typeIdentifierP :: Parser Text
typeIdentifierP = identifierP upperChar

blockP :: Parser SpannedExpr
blockP = withSpan $ EBlock <$> curlyBraceP programP

baseTypesForParsing :: [Parser Type]
baseTypesForParsing = [
    maybeDbg "typeP::lambda" $ try lambdaTypeP
  -- , maybeDbg "typeP::String" $ symbolP "String" $> TString
  -- , maybeDbg "typeP::Int" $ try (symbolP "Int" $> TInt)
  -- , maybeDbg "typeP::Float" $ symbolP "Float" $> TFloat
  -- , maybeDbg "typeP::Bool" $ symbolP "Bool" $> TBool
  , maybeDbg "typeP::TCon" $ try constructorTypeP
  , maybeDbg "typeP::tuple" $ try tupleTypeP
  , maybeDbg "typeP::list" $ try listTypeP
  , maybeDbg "typeP::parens" $ try (parensP typeP)
  , maybeDbg "typeP::TVar" $ TVar . TV <$> variableIdentifierP
  
  ]

typeP :: Parser Type
typeP = choice baseTypesForParsing

typeParametersP :: Parser [Type]
typeParametersP = parensP (sepBy typeP (symbolP ",")) <|> pure []

lambdaTypeP :: Parser Type
lambdaTypeP = sepBy2 nonLambdaTypeP (symbolP "->") >>= \case
  [x] -> pure x
  (x:xs) -> pure $ foldl TLambda x xs
  _ -> fail "Invalid lambda type"
  where
    nonLambdaTypeP = (choice . tail) baseTypesForParsing

tupleTypeP :: Parser Type
tupleTypeP = TTuple <$> parensP (sepBy2 typeP (symbolP ","))

listTypeP :: Parser Type
listTypeP = TList <$> bracketsP typeP

variableTypeP :: Parser (Maybe Type)
variableTypeP = optional $ symbolP ":" *> typeP

constructorTypeP :: Parser Type
constructorTypeP = do
  constructorName <- typeIdentifierP
  TCon (TC constructorName) <$> typeParametersP

typeSignatureP :: Parser SpannedExpr
typeSignatureP = withSpan $ do
  varName <- maybeDbg "typeSignatureP::varName" variableIdentifierP
  maybeDbg "typeSignatureP::symbol::" (symbolP "::")
  typeclassConstraints <- try typeclassConstraintsP <|> pure []
  varType <- maybeDbg "typeSignatureP::varType" typeP
  pure $ ETypeSignature (Variable varName) typeclassConstraints varType

typeclassConstraintsP :: Parser [Type]
typeclassConstraintsP = do
  maybeDbg "typeclassConstraintsP::symbol@" $ symbolP "@"
  tcConstraints <- curlyBraceP (sepBy constructorTypeP (symbolP ","))
  symbolP ":"
  pure tcConstraints

moduleNameP :: Parser ModuleName
moduleNameP = do
  currentCtxPrefix <- optional $ symbolP "::"
  modNameRest <- sepBy1 moduleNamePartP (symbolP "::")
  case currentCtxPrefix of
    Just _ -> pure $ ModuleNameChild modNameRest
    Nothing -> pure $ ModuleNameGlobal modNameRest
  where
    moduleNamePartP :: Parser Text
    moduleNamePartP = typeIdentifierP

importP :: Parser SpannedExpr
importP = withSpan $ do
  symbolP "import"
  moduleName <- moduleNameP
  qualifier <- optional $ choice [moduleQualiferP, importQualiferP]
  pure $ EImport (ImportStatement moduleName qualifier)
  where
    moduleQualiferP = ModuleQualified <$> (symbolP "as" *> typeIdentifierP)
    importQualiferP = FunctionQualified <$> parensP (sepBy1 functionNameIdentiferP (symbolP ","))

moduleDeclP :: Parser SpannedExpr
moduleDeclP = withSpan $ do
  symbolP "module"
  moduleName <- moduleNameP
  EModule moduleName <$> blockP

typeclassDeclP :: Parser SpannedExpr
typeclassDeclP = withSpan $ do
  maybeDbg "typeclassDeclP::classSymbol" $ symbolP "class"
  classConstraints <- try typeclassConstraintsP <|> pure []
  classConstructor <- constructorTypeP
  EClass classConstraints classConstructor <$> blockP

typeclassImplP :: Parser SpannedExpr
typeclassImplP = withSpan $ do
  symbolP "impl"
  classConstraints <- typeclassConstraintsP
  className <- typeIdentifierP
  classType <- TCon (TC className) <$> typeParametersP
  EClassImpl classConstraints classType <$> blockP

dataDeclP :: Parser SpannedExpr
dataDeclP = withSpan $ do
  maybeDbg "dataDeclP::" $ symbolP "data"
  dataName <- maybeDbg "dataDeclP::dataName" typeIdentifierP
  dataParams <- maybeDbg "dataDeclP::dataParams" typeParametersP
  constructors <- maybeDbg "dataDeclP::constructors" $ curlyBraceP (sepBy constructorDeclP (symbolP ","))
  pure $ EData dataName dataParams constructors

constructorDeclP :: Parser Constructor
constructorDeclP = Constructor <$> typeIdentifierP <*> typeParametersP

caseP :: Parser SpannedExpr
caseP = withSpan $ do
  symbolP "case"
  caseExpr <- exprP
  cases <- curlyBraceP (some caseBranchP)
  if length cases < 2
    then fail "Case statements must have at least 2 branches."
    else pure $ ECase caseExpr cases

caseBranchP :: Parser (SpannedPattern, SpannedExpr)
caseBranchP = do
  symbolP "|"
  branchPattern <- patternP
  symbolP "->"
  expr <- exprP
  pure (branchPattern, expr)

-- TODO: refactor the list and tuple literal parsers into global literalP (if possible)
listLiteralP :: Parser SpannedLiteral
listLiteralP = withSpan $ LList <$> bracketsP (sepBy exprP (symbolP ","))
listLiteralExprP :: Parser SpannedExpr
listLiteralExprP = withSpan $ ELiteral <$> listLiteralP

tupleLiteralP :: Parser SpannedLiteral
tupleLiteralP = withSpan $ LTuple <$> parensP (sepBy2 exprP (symbolP ","))
tupleLiteralExprP :: Parser SpannedExpr
tupleLiteralExprP = withSpan $ ELiteral <$> tupleLiteralP

variableUsageP :: Parser SpannedExpr
variableUsageP = withSpan $ EVar . Variable <$> functionNameIdentiferP

infixOpAsPrefixP :: Parser SpannedExpr
infixOpAsPrefixP = maybeDbg "infixOpAsPrefixP" $ parensP $ choice [maybeDbg "infixOpAsPrefixP::leftPartialInfixOpP" leftPartialInfixOpP, maybeDbg "infixOpAsPrefixP::rightPartialInfixOpP" rightPartialInfixOpP]
  where
    leftPartialInfixOpP = withSpan $ do
      left <- maybeDbg "infixOpAsPrefixP::baseExprP" baseExprP
      op <- withSpan $ EVar . Variable <$> (maybeDbg "infixOpAsPrefixP::rawInfixOpP" rawInfixOpP)
      pure $ EApply op left
    rightPartialInfixOpP = withSpan $ do
      op <- withSpan $ EVar . Variable <$> (maybeDbg "leftPartialInfixOpP::rawInfixOpP" rawInfixOpP)
      ELambda
        (Just (Ann nullSpan $ FPSimple (Variable "__partialInfixLambdaParam0"))) . Ann nullSpan . EApply
          (Ann nullSpan (EApply
            op
            (Ann nullSpan $ EVar (Variable "__partialInfixLambdaParam0")))) <$> (maybeDbg "rightPartialInfixOpP::baseExprP" baseExprP)

-- TODO: handle type constructor and diff between type, poly, data, variable, etc
declarationP :: Parser SpannedExpr
declarationP = withSpan $ do
  varName <- maybeDbg "declarationP::varName" variableIdentifierP
  maybeDbg "declarationP::symbol=" (symbolP "=")
  varValue <- maybeDbg "declarationP::varValue" exprP
  pure $ ELet (Variable varName) varValue

lambdaP :: Parser SpannedExpr
lambdaP = do
  maybeDbg "regularLambdaP::symbol\\" $ symbolP "\\"
  singleLambdaHeadP

-- caseLambdaP :: Parser SpannedExpr
-- caseLambdaP = withSpan $ do
--   maybeDbg "caseLambdaP::symbol\\" $ symbolP "\\"
--   maybeDbg "caseLambdaP::symbol|" $ lookAhead (symbolP "|")
--   params <- maybeDbg "caseLambdaP::params" lambdaParamsP
--   symbolP "->"
--   body <- maybeDbg "caseLambdaP::body" $ choice [blockP, exprP]
--   pure $ foldr ELambda body params

singleLambdaHeadP :: Parser SpannedExpr
singleLambdaHeadP = do
  start <- getSourcePos
  params <- maybeDbg "singleLambdaHeadP::param" lambdaParamsP
  symbolP "->"
  body <- maybeDbg "singleLambdaHeadP::body" $ choice [blockP, exprP]
  end <- getSourcePos
  pure $ foldr (\p b -> Ann (Span start end) (ELambda p b)) body params

lambdaParamsP :: Parser [Maybe SpannedParam]
lambdaParamsP = parensP (sepBy (optional lambdaParamP) (symbolP ","))

lambdaParamP :: Parser SpannedParam
lambdaParamP = withSpan $ try (FPPattern <$> patternP) <|> (FPSimple . Variable <$> variableIdentifierP)

patternP :: Parser SpannedPattern
patternP = choice [patternWildcardP, patternLiteralP, patternTupleP, patternListP, patternConstructorP]
  where
    patternWildcardP = withSpan $ PatternWildcard <$ symbolP "_"
    patternLiteralP  = withSpan $ PatternLiteral <$> maybeDbg "patternP::patternLiteralP" literalP
    patternTupleP    = withSpan $ PatternTuple <$> maybeDbg "patternP::patternTupleP" (parensP (sepBy lambdaParamP (symbolP ",")))
    patternListP     = withSpan $ PatternList <$> bracketsP (sepBy lambdaParamP (symbolP "|"))

patternConstructorP :: Parser SpannedPattern
patternConstructorP = withSpan $ do
  consName <- maybeDbg "lambdaPatternConstructorParamP::varName" typeIdentifierP
  consParams <- parensP (sepBy lambdaParamP (symbolP ","))
  pure $ PatternConstructor consName consParams

funApplicationP :: Parser SpannedExpr
funApplicationP = do
  fun <- maybeDbg "funApplicationP::fun" funApp'
  args <- maybeDbg "funApplicationP::args" $ some funApp'
  pure $ foldl argFold fun args
  where
    funApp' = choice $ baseExpressionsDoNotUse <> [try infixOpAsPrefixP, variableUsageP]
    argFold :: SpannedExpr -> SpannedExpr -> SpannedExpr
    argFold f a@(Ann sp _) = Ann sp (EApply f a)

infixExprP :: Parser SpannedExpr
infixExprP = maybeDbg "infixExprP" $ makeExprParser (maybeDbg "infixExprP::baseExprP" baseExprP) operatorTable

exprP :: Parser SpannedExpr
exprP = maybeDbg "exprP::infixExprP" infixExprP

rootExprP :: Parser SpannedExpr
rootExprP = maybeDbg "rootExprP" $ maybeDbg "rootExprP::declarationP" (try declarationP) <|> maybeDbg "rootExprP::exprP" exprP