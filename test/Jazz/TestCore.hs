{-# LANGUAGE DataKinds #-}

-- | Phase-exact builders for tests whose contract is canonical shape rather
-- than Task 7's independently verified node identity and span metadata.
module Jazz.TestCore
  ( parseSurfaceProgramPoints,
    assertLoweredCoreEqual,
    loweredApply,
    loweredAsPattern,
    loweredBinary,
    loweredBlock,
    loweredCaseArm,
    loweredClass,
    loweredClassMethodSignature,
    loweredConsListPattern,
    loweredConstructor,
    loweredConstructorAt,
    loweredConstructorPattern,
    loweredData,
    loweredExpression,
    loweredIf,
    loweredImpl,
    loweredImplMethod,
    loweredLambda,
    loweredList,
    loweredListPattern,
    loweredLiteral,
    loweredLiteralPattern,
    loweredModule,
    loweredOperatorValue,
    loweredOrPattern,
    loweredPatternCase,
    loweredSectionLeft,
    loweredSectionRight,
    loweredSignature,
    loweredTuple,
    loweredTuplePattern,
    loweredTypeApplication,
    loweredVariable,
    loweredVariablePattern,
    loweredWildcardPattern,
    loweredImport,
    loweredLet,
  )
where

import Data.Text (Text)
import Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    CoreNode (..),
    CoreNodeId (..),
    CorePhase (Lowered),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal,
    Pattern (..),
    SignaturePayload,
    SignatureType,
    Statement (..),
  )
import Jazz.Compiler.Diagnostics (Diagnostic, SourceSpan (..), sourceSpanStart)
import Jazz.Compiler.Name (UnresolvedName)
import Jazz.Compiler.Parser (parseSurfaceProgramTokens)
import Jazz.Compiler.Parser.AST (SurfaceExpr)
import Jazz.Compiler.Parser.Lexer (Token (..), tokenize)
import Jazz.TestHarness (assertEqual)

fixtureNode :: CoreNode 'Lowered sort
fixtureNode = CoreNode (CoreNodeId 0) (SourceSpan 0 0) ()

fixtureStatementNode :: SourceSpan -> CoreNode 'Lowered sort
fixtureStatementNode spanValue = CoreNode (CoreNodeId 0) spanValue ()

loweredLiteral :: Literal -> Expr 'Lowered
loweredLiteral = ELit fixtureNode

loweredVariable :: UnresolvedName -> Expr 'Lowered
loweredVariable = EVar fixtureNode

loweredLambda :: UnresolvedName -> Expr 'Lowered -> Expr 'Lowered
loweredLambda = ELambda fixtureNode

loweredOperatorValue :: Text -> Expr 'Lowered
loweredOperatorValue = EOperatorValue fixtureNode

loweredList :: [Expr 'Lowered] -> Expr 'Lowered
loweredList = EList fixtureNode

loweredTuple :: [Expr 'Lowered] -> Expr 'Lowered
loweredTuple = ETuple fixtureNode

loweredApply :: Expr 'Lowered -> Expr 'Lowered -> Expr 'Lowered
loweredApply = EApply fixtureNode

loweredTypeApplication :: Expr 'Lowered -> SourceSpan -> SignatureType 'Lowered -> Expr 'Lowered
loweredTypeApplication = ETypeApplication fixtureNode

loweredIf :: Expr 'Lowered -> Expr 'Lowered -> Expr 'Lowered -> Expr 'Lowered
loweredIf = EIf fixtureNode

loweredPatternCase :: Expr 'Lowered -> [CaseArm 'Lowered] -> Expr 'Lowered
loweredPatternCase = EPatternCase fixtureNode

loweredBinary :: Text -> Expr 'Lowered -> Expr 'Lowered -> Expr 'Lowered
loweredBinary = EBinary fixtureNode

loweredSectionLeft :: Expr 'Lowered -> Text -> Expr 'Lowered
loweredSectionLeft = ESectionLeft fixtureNode

loweredSectionRight :: Text -> Expr 'Lowered -> Expr 'Lowered
loweredSectionRight = ESectionRight fixtureNode

loweredBlock :: [Statement 'Lowered] -> Expr 'Lowered
loweredBlock = EBlock fixtureNode

loweredWildcardPattern :: Pattern 'Lowered
loweredWildcardPattern = PWildcard fixtureNode

loweredVariablePattern :: UnresolvedName -> Pattern 'Lowered
loweredVariablePattern = PVariable fixtureNode

loweredLiteralPattern :: Literal -> Pattern 'Lowered
loweredLiteralPattern = PLiteral fixtureNode

loweredConstructorPattern :: UnresolvedName -> [Pattern 'Lowered] -> Pattern 'Lowered
loweredConstructorPattern = PConstructor fixtureNode

loweredListPattern :: [Pattern 'Lowered] -> Pattern 'Lowered
loweredListPattern = PList fixtureNode

loweredConsListPattern :: Pattern 'Lowered -> Pattern 'Lowered -> Pattern 'Lowered
loweredConsListPattern = PConsList fixtureNode

loweredTuplePattern :: [Pattern 'Lowered] -> Pattern 'Lowered
loweredTuplePattern = PTuple fixtureNode

loweredAsPattern :: UnresolvedName -> Pattern 'Lowered -> Pattern 'Lowered
loweredAsPattern = PAs fixtureNode

loweredOrPattern :: [Pattern 'Lowered] -> Pattern 'Lowered
loweredOrPattern = POr fixtureNode

loweredCaseArm :: Pattern 'Lowered -> Maybe (Expr 'Lowered) -> Expr 'Lowered -> CaseArm 'Lowered
loweredCaseArm = CaseArm fixtureNode

loweredConstructor :: UnresolvedName -> [SignatureType 'Lowered] -> DataConstructor 'Lowered
loweredConstructor = DataConstructor fixtureNode

loweredConstructorAt :: SourceSpan -> UnresolvedName -> [SignatureType 'Lowered] -> DataConstructor 'Lowered
loweredConstructorAt spanValue = DataConstructor (fixtureStatementNode spanValue)

loweredClassMethodSignature :: UnresolvedName -> SourceSpan -> SignaturePayload 'Lowered -> ClassMethodSignature 'Lowered
loweredClassMethodSignature name spanValue = ClassMethodSignature (fixtureStatementNode spanValue) name

loweredImplMethod :: UnresolvedName -> SourceSpan -> Expr 'Lowered -> ImplMethod 'Lowered
loweredImplMethod name spanValue = ImplMethod (fixtureStatementNode spanValue) name

loweredLet :: UnresolvedName -> SourceSpan -> Expr 'Lowered -> Statement 'Lowered
loweredLet name spanValue = SLet (fixtureStatementNode spanValue) name

loweredSignature :: UnresolvedName -> SourceSpan -> SignaturePayload 'Lowered -> Statement 'Lowered
loweredSignature name spanValue = SSignature (fixtureStatementNode spanValue) name

loweredData :: SourceSpan -> UnresolvedName -> [UnresolvedName] -> [DataConstructor 'Lowered] -> Statement 'Lowered
loweredData spanValue = SData (fixtureStatementNode spanValue)

loweredClass :: SourceSpan -> UnresolvedName -> [UnresolvedName] -> [ClassMethodSignature 'Lowered] -> Statement 'Lowered
loweredClass spanValue name parameters methods = SClass (fixtureStatementNode spanValue) name parameters methods [] []

loweredImpl :: SourceSpan -> UnresolvedName -> [SignatureType 'Lowered] -> [ImplMethod 'Lowered] -> Statement 'Lowered
loweredImpl spanValue name targets methods = SImpl (fixtureStatementNode spanValue) name targets methods []

loweredModule :: SourceSpan -> [Text] -> Statement 'Lowered
loweredModule spanValue = SModule (fixtureStatementNode spanValue)

loweredImport :: SourceSpan -> [Text] -> Maybe Text -> Maybe [Text] -> Statement 'Lowered
loweredImport spanValue = SImport (fixtureStatementNode spanValue)

loweredExpression :: SourceSpan -> Expr 'Lowered -> Statement 'Lowered
loweredExpression spanValue = SExpr (fixtureStatementNode spanValue)

assertLoweredCoreEqual :: Text -> Expr 'Lowered -> Expr 'Lowered -> IO ()
assertLoweredCoreEqual label expected actual =
  assertEqual label (eraseExprMetadata expected) (eraseExprMetadata actual)

eraseNodeMetadata :: CoreNode 'Lowered sort -> CoreNode 'Lowered sort
eraseNodeMetadata _ = fixtureNode

eraseExprMetadata :: Expr 'Lowered -> Expr 'Lowered
eraseExprMetadata expression =
  case expression of
    ELit node literal -> ELit (eraseNodeMetadata node) literal
    EVar node name -> EVar (eraseNodeMetadata node) name
    ELambda node parameter body -> ELambda (eraseNodeMetadata node) parameter (eraseExprMetadata body)
    EOperatorValue node symbol -> EOperatorValue (eraseNodeMetadata node) symbol
    EList node items -> EList (eraseNodeMetadata node) (map eraseExprMetadata items)
    ETuple node items -> ETuple (eraseNodeMetadata node) (map eraseExprMetadata items)
    EApply node function argument -> EApply (eraseNodeMetadata node) (eraseExprMetadata function) (eraseExprMetadata argument)
    ETypeApplication node function _ signatureType ->
      ETypeApplication (eraseNodeMetadata node) (eraseExprMetadata function) (SourceSpan 1 1) signatureType
    EIf node condition trueBranch falseBranch ->
      EIf (eraseNodeMetadata node) (eraseExprMetadata condition) (eraseExprMetadata trueBranch) (eraseExprMetadata falseBranch)
    EPatternCase node scrutinee arms ->
      EPatternCase (eraseNodeMetadata node) (eraseExprMetadata scrutinee) (map eraseCaseArmMetadata arms)
    EBinary node symbol left right ->
      EBinary (eraseNodeMetadata node) symbol (eraseExprMetadata left) (eraseExprMetadata right)
    ESectionLeft node left symbol -> ESectionLeft (eraseNodeMetadata node) (eraseExprMetadata left) symbol
    ESectionRight node symbol right -> ESectionRight (eraseNodeMetadata node) symbol (eraseExprMetadata right)
    EBlock node statements -> EBlock (eraseNodeMetadata node) (map eraseStatementMetadata statements)

erasePatternMetadata :: Pattern 'Lowered -> Pattern 'Lowered
erasePatternMetadata patternValue =
  case patternValue of
    PWildcard node -> PWildcard (eraseNodeMetadata node)
    PVariable node name -> PVariable (eraseNodeMetadata node) name
    PLiteral node literal -> PLiteral (eraseNodeMetadata node) literal
    PConstructor node name patterns -> PConstructor (eraseNodeMetadata node) name (map erasePatternMetadata patterns)
    PList node patterns -> PList (eraseNodeMetadata node) (map erasePatternMetadata patterns)
    PConsList node headPattern tailPattern ->
      PConsList (eraseNodeMetadata node) (erasePatternMetadata headPattern) (erasePatternMetadata tailPattern)
    PTuple node patterns -> PTuple (eraseNodeMetadata node) (map erasePatternMetadata patterns)
    PAs node name pattern' -> PAs (eraseNodeMetadata node) name (erasePatternMetadata pattern')
    POr node patterns -> POr (eraseNodeMetadata node) (map erasePatternMetadata patterns)

eraseCaseArmMetadata :: CaseArm 'Lowered -> CaseArm 'Lowered
eraseCaseArmMetadata (CaseArm node patternValue guard body) =
  CaseArm
    (eraseNodeMetadata node)
    (erasePatternMetadata patternValue)
    (fmap eraseExprMetadata guard)
    (eraseExprMetadata body)

eraseStatementMetadata :: Statement 'Lowered -> Statement 'Lowered
eraseStatementMetadata statement =
  case statement of
    SLet node name value -> SLet (eraseNodeMetadata node) name (eraseExprMetadata value)
    SSignature node name payload -> SSignature (eraseNodeMetadata node) name payload
    SData node name parameters constructors ->
      SData (eraseNodeMetadata node) name parameters (map eraseConstructorMetadata constructors)
    SClass node name parameters methods prerequisites defaults ->
      SClass (eraseNodeMetadata node) name parameters (map eraseClassMethodMetadata methods) prerequisites (map eraseImplMethodMetadata defaults)
    SImpl node name arguments methods prerequisites ->
      SImpl (eraseNodeMetadata node) name arguments (map eraseImplMethodMetadata methods) prerequisites
    SModule node path -> SModule (eraseNodeMetadata node) path
    SImport node path alias symbols -> SImport (eraseNodeMetadata node) path alias symbols
    SExpr node value -> SExpr (eraseNodeMetadata node) (eraseExprMetadata value)

eraseConstructorMetadata :: DataConstructor 'Lowered -> DataConstructor 'Lowered
eraseConstructorMetadata (DataConstructor node name fields) =
  DataConstructor (eraseNodeMetadata node) name fields

eraseClassMethodMetadata :: ClassMethodSignature 'Lowered -> ClassMethodSignature 'Lowered
eraseClassMethodMetadata (ClassMethodSignature node name payload) =
  ClassMethodSignature (eraseNodeMetadata node) name payload

eraseImplMethodMetadata :: ImplMethod 'Lowered -> ImplMethod 'Lowered
eraseImplMethodMetadata (ImplMethod node name body) =
  ImplMethod (eraseNodeMetadata node) name (eraseExprMetadata body)

-- | Legacy syntax fixtures compare point-only ASTs. Feed an explicitly
-- projected token stream through the real parser; production range behavior
-- is checked separately by SourceRangesSpec without this adapter.
parseSurfaceProgramPoints :: Text -> Either Diagnostic SurfaceExpr
parseSurfaceProgramPoints source = do
  tokens <- tokenize source
  parseSurfaceProgramTokens [token {tokenSpan = sourceSpanStart (tokenSpan token)} | token <- tokens]
