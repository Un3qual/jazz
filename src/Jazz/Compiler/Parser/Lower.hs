{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lowers parser-surface nodes into the smaller core AST consumed by later
-- compiler phases.
module Jazz.Compiler.Parser.Lower
  ( ModuleDeclaration (..),
    ModuleLoweringFailure (..),
    lowerSurfaceExpr,
    lowerSurfaceModuleDetailed,
    lowerSurfaceModule,
    reindexLoweredExpr,
  )
where

import Control.Monad.Trans.State.Strict (State, evalState, state)
import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    CoreNode (..),
    CoreNodeId (..),
    CorePhase (Lowered),
    CoreSort (ExpressionSort),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    Pattern (..),
    SignatureConstraint,
    SignaturePayload,
    SignatureToken,
    SignatureType,
    Statement (..),
  )
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (..),
    SourceSpan,
    mkErrorDiagnostic,
    qualifySourceSpan,
    setDiagnosticPrimarySpan,
  )
import Jazz.Compiler.ModuleExports
  ( qualifyModuleExportSelectorSpans,
  )
import Jazz.Compiler.ModuleGraph
  ( CoreModule (..),
    DeclaredImportExposure (..),
    DeclaredModuleExports (..),
    DeclaredModuleFacts (..),
    ModuleImport (..),
  )
import Jazz.Compiler.ModuleIdentity
  ( ModuleIdentity,
    mkModulePath,
    mkModuleQualifier,
    moduleIdentityPath,
    moduleIdentitySource,
    modulePathTextSegments,
    sourceFilePath,
  )
import Jazz.Compiler.Name
  ( GeneratedNameKind (..),
    Identifier,
    UnresolvedName,
    generatedName,
    identifierText,
    isOperatorBindingIdentifierText,
    mkIdentifier,
    operatorBindingNameFromIdentifier,
    qualifiedName,
    sourceName,
    splitQualifiedIdentifierText,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceClassMethodSignature (..),
    SurfaceDataConstructor (..),
    SurfaceExpr (..),
    SurfaceExprForm (..),
    SurfaceImplMethod (..),
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfacePattern (..),
    SurfacePatternForm (..),
    SurfacePatternLambdaClause (..),
    SurfaceSignatureConstraint,
    SurfaceSignaturePayload,
    SurfaceSignatureToken,
    SurfaceSignatureType,
    SurfaceStatement (..),
  )
import qualified Jazz.Compiler.TypeRepresentation as TypeRepresentation

-- | The declaration inputs retained when module validation fails. Keeping
-- these values structured lets hosted-lowering parity compare semantic inputs
-- without recovering them from rendered diagnostics.
data ModuleDeclaration = ModuleDeclaration
  { moduleDeclarationSpan :: SourceSpan,
    moduleDeclarationPath :: [Text]
  }
  deriving (Eq, Show)

-- | Failures owned specifically by module lowering, before they are rendered
-- into the compiler's shared diagnostic representation.
data ModuleLoweringFailure
  = MultipleModuleDeclarations FilePath [ModuleDeclaration]
  | ModulePathMismatch FilePath [Text] ModuleDeclaration
  | EmptyImportSymbolList FilePath SourceSpan [Text]
  deriving (Eq, Show)

-- | Validate and lower one parsed module exactly once. Module/import forms are
-- retained as graph metadata and removed from the executable core scope.
lowerSurfaceModule :: ModuleIdentity -> SurfaceExpr -> Either Diagnostic (CoreModule 'Lowered)
lowerSurfaceModule identity surfaceExpr =
  case lowerSurfaceModuleDetailed identity surfaceExpr of
    Left failure -> Left (moduleLoweringFailureDiagnostic failure)
    Right coreModule -> Right coreModule

-- | Preserve the semantic inputs for the two module-lowering failures. The
-- public compiler entry point above renders these into the existing E4005 and
-- E4006 diagnostics, so production behavior remains unchanged.
lowerSurfaceModuleDetailed :: ModuleIdentity -> SurfaceExpr -> Either ModuleLoweringFailure (CoreModule 'Lowered)
lowerSurfaceModuleDetailed identity surfaceExpr =
  {-# SCC "jazz-stage:lowering" #-}
  do
    declaredExports <- validateDeclaration
    (bodyNode, imports, executableStatements) <- runLowering lowerModuleBody
    let (qualifiedBodyNode, qualifiedStatements) = qualifyModuleBody bodyNode executableStatements
    pure
      CoreModule
        { coreModuleIdentity = identity,
          coreModuleBodyNode = qualifiedBodyNode,
          coreModuleImports = map qualifyImport imports,
          coreModuleStatements = qualifiedStatements,
          coreModuleFacts = DeclaredModuleFacts declaredExports
        }
  where
    sourcePath = sourceFilePath (moduleIdentitySource identity)
    expectedPath = NonEmpty.toList (modulePathTextSegments (moduleIdentityPath identity))
    statements =
      case surfaceExprForm surfaceExpr of
        SEBlock moduleStatements -> moduleStatements
        _ -> []

    declarations =
      [ (ModuleDeclaration spanValue modulePath, moduleExports)
      | SSModule spanValue modulePath moduleExports <- statements
      ]

    qualifyModuleBody bodyNode bodyStatements =
      ( qualifyLoweredNode sourcePath bodyNode,
        map (qualifyStatementSourceSpans sourcePath) bodyStatements
      )

    qualifyImport importDecl =
      importDecl
        { moduleImportNode = qualifyNode (moduleImportNode importDecl)
        }
      where
        qualifyNode (CoreNode nodeId spanValue facts) =
          CoreNode nodeId (qualifySourceSpan sourcePath spanValue) facts

    lowerModuleBody = do
      bodyNode <- freshNode (surfaceExprSpan surfaceExpr)
      loweredItems <- traverse lowerModuleStatement statements
      pure $ do
        items <- sequence loweredItems
        let (imports, maybeStatements) = partitionEithers items
        Right (bodyNode, imports, catMaybes maybeStatements)

    lowerModuleStatement statement =
      case statement of
        SSModule {} -> pure (Right (Right Nothing))
        SSImport spanValue modulePath alias importedSymbols -> do
          node <- freshNode spanValue
          let qualifier = mkModuleQualifier . mkIdentifier <$> alias
          pure $ do
            importedPath <-
              maybe
                (Left (EmptyImportSymbolList sourcePath spanValue modulePath))
                (Right . mkModulePath . fmap mkIdentifier)
                (NonEmpty.nonEmpty modulePath)
            exposure <-
              case importedSymbols of
                Nothing -> Right (DeclaredImportAll qualifier)
                Just symbols ->
                  maybe
                    (Left (EmptyImportSymbolList sourcePath spanValue modulePath))
                    (Right . DeclaredImportOnly qualifier . fmap mkIdentifier)
                    (NonEmpty.nonEmpty symbols)
            Right
              ( Left
                  ModuleImport
                    { moduleImportNode = node,
                      importedModule = importedPath,
                      importExposure = exposure
                    }
              )
        _ -> Right . Right . Just <$> lowerSurfaceStatement statement

    validateDeclaration =
      case declarations of
        [] -> Right Nothing
        [(declaration, declaredExportSelectors)]
          | moduleDeclarationPath declaration == expectedPath ->
              Right
                ( DeclaredModuleExports
                    (qualifySourceSpan sourcePath (moduleDeclarationSpan declaration))
                    . map (qualifyModuleExportSelectorSpans sourcePath)
                    <$> declaredExportSelectors
                )
          | otherwise ->
              Left (ModulePathMismatch sourcePath expectedPath declaration)
        declaredModules ->
          Left (MultipleModuleDeclarations sourcePath (map fst declaredModules))

moduleLoweringFailureDiagnostic :: ModuleLoweringFailure -> Diagnostic
moduleLoweringFailureDiagnostic failure =
  case failure of
    MultipleModuleDeclarations sourcePath declarations ->
      mkErrorDiagnostic
        E4005
        CompilationOrigin
        ( "multiple module declarations in '"
            <> Text.pack sourcePath
            <> "': "
            <> Text.intercalate ", " (map (renderModulePath . moduleDeclarationPath) declarations)
        )
    ModulePathMismatch sourcePath expectedPath declaration ->
      mkErrorDiagnostic
        E4006
        CompilationOrigin
        ( "module declaration mismatch at '"
            <> Text.pack sourcePath
            <> "': expected '"
            <> renderModulePath expectedPath
            <> "', found '"
            <> renderModulePath (moduleDeclarationPath declaration)
            <> "'"
        )
    EmptyImportSymbolList sourcePath spanValue modulePath ->
      setDiagnosticPrimarySpan spanValue $
        mkErrorDiagnostic
          E4010
          CompilationOrigin
          ( "invalid empty module import in '"
              <> Text.pack sourcePath
              <> "' for '"
              <> Text.intercalate "::" modulePath
              <> "'"
          )
  where
    renderModulePath = Text.intercalate "::"

qualifyExprSourceSpans :: FilePath -> Expr 'Lowered -> Expr 'Lowered
qualifyExprSourceSpans sourcePath expr =
  case expr of
    ELit node literal -> ELit (qualifyLoweredNode sourcePath node) literal
    EVar node name -> EVar (qualifyLoweredNode sourcePath node) name
    ELambda node parameter body -> ELambda (qualifyLoweredNode sourcePath node) parameter (go body)
    EOperatorValue node symbol -> EOperatorValue (qualifyLoweredNode sourcePath node) symbol
    EList node items -> EList (qualifyLoweredNode sourcePath node) (map go items)
    ETuple node items -> ETuple (qualifyLoweredNode sourcePath node) (map go items)
    EApply node function argument -> EApply (qualifyLoweredNode sourcePath node) (go function) (go argument)
    ETypeApplication node function spanValue signatureType ->
      ETypeApplication (qualifyLoweredNode sourcePath node) (go function) (qualifySpan spanValue) signatureType
    EIf node condition trueBranch falseBranch ->
      EIf (qualifyLoweredNode sourcePath node) (go condition) (go trueBranch) (go falseBranch)
    EPatternCase node scrutinee arms ->
      EPatternCase (qualifyLoweredNode sourcePath node) (go scrutinee) (map qualifyCaseArm arms)
    EBinary node symbol left right -> EBinary (qualifyLoweredNode sourcePath node) symbol (go left) (go right)
    ESectionLeft node left symbol -> ESectionLeft (qualifyLoweredNode sourcePath node) (go left) symbol
    ESectionRight node symbol right -> ESectionRight (qualifyLoweredNode sourcePath node) symbol (go right)
    EBlock node statements -> EBlock (qualifyLoweredNode sourcePath node) (map (qualifyStatementSourceSpans sourcePath) statements)
  where
    go = qualifyExprSourceSpans sourcePath
    qualifySpan = qualifySourceSpan sourcePath

    qualifyCaseArm (CaseArm node patternValue guardExpr bodyExpr) =
      CaseArm (qualifyLoweredNode sourcePath node) (qualifyPattern patternValue) (fmap go guardExpr) (go bodyExpr)

    qualifyPattern patternValue =
      case patternValue of
        PWildcard node -> PWildcard (qualifyLoweredNode sourcePath node)
        PVariable node name -> PVariable (qualifyLoweredNode sourcePath node) name
        PLiteral node literal -> PLiteral (qualifyLoweredNode sourcePath node) literal
        PConstructor node name patterns -> PConstructor (qualifyLoweredNode sourcePath node) name (map qualifyPattern patterns)
        PList node patterns -> PList (qualifyLoweredNode sourcePath node) (map qualifyPattern patterns)
        PConsList node headPattern tailPattern ->
          PConsList (qualifyLoweredNode sourcePath node) (qualifyPattern headPattern) (qualifyPattern tailPattern)
        PTuple node patterns -> PTuple (qualifyLoweredNode sourcePath node) (map qualifyPattern patterns)
        PAs node name nestedPattern -> PAs (qualifyLoweredNode sourcePath node) name (qualifyPattern nestedPattern)
        POr node alternatives -> POr (qualifyLoweredNode sourcePath node) (map qualifyPattern alternatives)

qualifyLoweredNode :: FilePath -> CoreNode 'Lowered sort -> CoreNode 'Lowered sort
qualifyLoweredNode sourcePath (CoreNode nodeId spanValue facts) =
  CoreNode nodeId (qualifySourceSpan sourcePath spanValue) facts

qualifyStatementSourceSpans :: FilePath -> Statement 'Lowered -> Statement 'Lowered
qualifyStatementSourceSpans sourcePath statement =
  case statement of
    SLet node name valueExpr -> SLet (qualifyNode node) name (go valueExpr)
    SSignature node name payload -> SSignature (qualifyNode node) name payload
    SData node name parameters constructors ->
      SData (qualifyNode node) name parameters (map qualifyDataConstructor constructors)
    SClass node name parameters methods ->
      SClass (qualifyNode node) name parameters (map qualifyClassMethod methods)
    SImpl node name arguments methods ->
      SImpl (qualifyNode node) name arguments (map qualifyImplMethod methods)
    SModule node path -> SModule (qualifyNode node) path
    SImport node path alias symbols -> SImport (qualifyNode node) path alias symbols
    SExpr node valueExpr -> SExpr (qualifyNode node) (go valueExpr)
  where
    qualifyNode = qualifyLoweredNode sourcePath
    go = qualifyExprSourceSpans sourcePath
    qualifyDataConstructor (DataConstructor node name fieldTypes) =
      DataConstructor (qualifyNode node) name fieldTypes
    qualifyClassMethod (ClassMethodSignature node name payload) =
      ClassMethodSignature (qualifyNode node) name payload
    qualifyImplMethod (ImplMethod node name bodyExpr) =
      ImplMethod (qualifyNode node) name (go bodyExpr)

-- | Convert parser-surface nodes into located lowered core. Node identities are
-- allocated in strict source pre-order and every core node retains its source
-- location.
lowerSurfaceExpr :: SurfaceExpr -> Expr 'Lowered
lowerSurfaceExpr surfaceExpr =
  {-# SCC "jazz-stage:lowering" #-}
  runLowering (lowerSurfaceExprWithoutCostCentre surfaceExpr)

type Lowering = State CoreNodeId

runLowering :: Lowering value -> value
runLowering action = evalState action (CoreNodeId 0)

freshNode :: SourceSpan -> Lowering (CoreNode 'Lowered sort)
freshNode spanValue =
  state $ \(CoreNodeId nextId) ->
    (CoreNode (CoreNodeId nextId) spanValue (), CoreNodeId (nextId + 1))

-- | Re-establish one deterministic identity space after independently lowered
-- trees are composed. IDs are allocated before descendants, matching ordinary
-- lowering's strict source pre-order while preserving every span and payload.
reindexLoweredExpr :: Expr 'Lowered -> Expr 'Lowered
reindexLoweredExpr = runLowering . reindexExpr

reindexExpr :: Expr 'Lowered -> Lowering (Expr 'Lowered)
reindexExpr expression =
  case expression of
    ELit node literal -> ELit <$> reindexNode node <*> pure literal
    EVar node name -> EVar <$> reindexNode node <*> pure name
    ELambda node parameter body ->
      ELambda <$> reindexNode node <*> pure parameter <*> reindexExpr body
    EOperatorValue node operatorSymbol ->
      EOperatorValue <$> reindexNode node <*> pure operatorSymbol
    EList node elements ->
      EList <$> reindexNode node <*> traverse reindexExpr elements
    ETuple node elements ->
      ETuple <$> reindexNode node <*> traverse reindexExpr elements
    EApply node functionExpr argumentExpr ->
      EApply <$> reindexNode node <*> reindexExpr functionExpr <*> reindexExpr argumentExpr
    ETypeApplication node functionExpr typeArgumentSpan signatureType ->
      ETypeApplication
        <$> reindexNode node
        <*> reindexExpr functionExpr
        <*> pure typeArgumentSpan
        <*> pure signatureType
    EIf node condition trueBranch falseBranch ->
      EIf
        <$> reindexNode node
        <*> reindexExpr condition
        <*> reindexExpr trueBranch
        <*> reindexExpr falseBranch
    EPatternCase node scrutinee arms ->
      EPatternCase <$> reindexNode node <*> reindexExpr scrutinee <*> traverse reindexCaseArm arms
    EBinary node operatorSymbol left right ->
      EBinary <$> reindexNode node <*> pure operatorSymbol <*> reindexExpr left <*> reindexExpr right
    ESectionLeft node left operatorSymbol ->
      ESectionLeft <$> reindexNode node <*> reindexExpr left <*> pure operatorSymbol
    ESectionRight node operatorSymbol right ->
      ESectionRight <$> reindexNode node <*> pure operatorSymbol <*> reindexExpr right
    EBlock node statements ->
      EBlock <$> reindexNode node <*> traverse reindexStatement statements

reindexNode :: CoreNode 'Lowered sort -> Lowering (CoreNode 'Lowered sort)
reindexNode (CoreNode _ spanValue ()) = freshNode spanValue

reindexCaseArm :: CaseArm 'Lowered -> Lowering (CaseArm 'Lowered)
reindexCaseArm (CaseArm node patternValue guardExpr bodyExpr) =
  CaseArm
    <$> reindexNode node
    <*> reindexPattern patternValue
    <*> traverse reindexExpr guardExpr
    <*> reindexExpr bodyExpr

reindexPattern :: Pattern 'Lowered -> Lowering (Pattern 'Lowered)
reindexPattern patternValue =
  case patternValue of
    PWildcard node -> PWildcard <$> reindexNode node
    PVariable node name -> PVariable <$> reindexNode node <*> pure name
    PLiteral node literal -> PLiteral <$> reindexNode node <*> pure literal
    PConstructor node name patterns ->
      PConstructor <$> reindexNode node <*> pure name <*> traverse reindexPattern patterns
    PList node patterns ->
      PList <$> reindexNode node <*> traverse reindexPattern patterns
    PConsList node headPattern tailPattern ->
      PConsList <$> reindexNode node <*> reindexPattern headPattern <*> reindexPattern tailPattern
    PTuple node patterns ->
      PTuple <$> reindexNode node <*> traverse reindexPattern patterns
    PAs node name nestedPattern ->
      PAs <$> reindexNode node <*> pure name <*> reindexPattern nestedPattern
    POr node alternatives ->
      POr <$> reindexNode node <*> traverse reindexPattern alternatives

reindexStatement :: Statement 'Lowered -> Lowering (Statement 'Lowered)
reindexStatement statement =
  case statement of
    SLet node name valueExpr ->
      SLet <$> reindexNode node <*> pure name <*> reindexExpr valueExpr
    SSignature node name signaturePayload ->
      SSignature <$> reindexNode node <*> pure name <*> pure signaturePayload
    SData node name parameters constructors ->
      SData
        <$> reindexNode node
        <*> pure name
        <*> pure parameters
        <*> traverse reindexDataConstructor constructors
    SClass node name parameters methods ->
      SClass
        <$> reindexNode node
        <*> pure name
        <*> pure parameters
        <*> traverse reindexClassMethod methods
    SImpl node name arguments methods ->
      SImpl
        <$> reindexNode node
        <*> pure name
        <*> pure arguments
        <*> traverse reindexImplMethod methods
    SModule node modulePath -> SModule <$> reindexNode node <*> pure modulePath
    SImport node modulePath alias symbols ->
      SImport <$> reindexNode node <*> pure modulePath <*> pure alias <*> pure symbols
    SExpr node valueExpr -> SExpr <$> reindexNode node <*> reindexExpr valueExpr

reindexDataConstructor :: DataConstructor 'Lowered -> Lowering (DataConstructor 'Lowered)
reindexDataConstructor (DataConstructor node name fieldTypes) =
  DataConstructor <$> reindexNode node <*> pure name <*> pure fieldTypes

reindexClassMethod :: ClassMethodSignature 'Lowered -> Lowering (ClassMethodSignature 'Lowered)
reindexClassMethod (ClassMethodSignature node name signaturePayload) =
  ClassMethodSignature <$> reindexNode node <*> pure name <*> pure signaturePayload

reindexImplMethod :: ImplMethod 'Lowered -> Lowering (ImplMethod 'Lowered)
reindexImplMethod (ImplMethod node name bodyExpr) =
  ImplMethod <$> reindexNode node <*> pure name <*> reindexExpr bodyExpr

lowerSurfaceExprWithoutCostCentre :: SurfaceExpr -> Lowering (Expr 'Lowered)
lowerSurfaceExprWithoutCostCentre surfaceExpr = do
  node <- freshNode (surfaceExprSpan surfaceExpr)
  case surfaceExprForm surfaceExpr of
    SELit literal -> pure (ELit node (lowerSurfaceLiteral literal))
    SEVar name -> pure (EVar node (sourceName name))
    SEQualifiedVar qualifier member ->
      pure (EVar node (qualifiedName qualifier member))
    SELambda parameters bodyExpr ->
      lowerSurfaceLambda node (surfaceExprSpan surfaceExpr) parameters bodyExpr
    SEPatternLambda clauses ->
      lowerSurfacePatternLambda node (surfaceExprSpan surfaceExpr) clauses
    SEOperatorValue operatorSymbol -> pure (EOperatorValue node operatorSymbol)
    SEList elements ->
      EList node <$> traverse lowerSurfaceExprWithoutCostCentre elements
    SETuple elements ->
      ETuple node <$> traverse lowerSurfaceExprWithoutCostCentre elements
    SEApply functionExpr argumentExpr -> do
      function <- lowerSurfaceExprWithoutCostCentre functionExpr
      argument <- lowerSurfaceExprWithoutCostCentre argumentExpr
      pure (EApply node function argument)
    SETypeApplication functionExpr spanValue signatureType -> do
      function <- lowerSurfaceExprWithoutCostCentre functionExpr
      pure (ETypeApplication node function spanValue (lowerSurfaceSignatureType signatureType))
    SEIf conditionExpr thenExpr elseExpr -> do
      condition <- lowerSurfaceExprWithoutCostCentre conditionExpr
      trueBranch <- lowerSurfaceExprWithoutCostCentre thenExpr
      falseBranch <- lowerSurfaceExprWithoutCostCentre elseExpr
      pure (EIf node condition trueBranch falseBranch)
    SECase scrutineeExpr caseArms -> do
      scrutinee <- lowerSurfaceExprWithoutCostCentre scrutineeExpr
      arms <- traverse lowerSurfaceCaseArm caseArms
      pure (EPatternCase node scrutinee arms)
    SEBinary operatorSymbol functionExpr argumentExpr
      | operatorSymbol == Text.pack "$" -> do
          function <- lowerSurfaceExprWithoutCostCentre functionExpr
          argument <- lowerSurfaceExprWithoutCostCentre argumentExpr
          pure (EApply node function argument)
    SEBinary operatorSymbol leftExpr rightExpr -> do
      left <- lowerSurfaceExprWithoutCostCentre leftExpr
      right <- lowerSurfaceExprWithoutCostCentre rightExpr
      pure (EBinary node operatorSymbol left right)
    SESectionLeft leftExpr operatorSymbol -> do
      left <- lowerSurfaceExprWithoutCostCentre leftExpr
      pure (ESectionLeft node left operatorSymbol)
    SESectionRight operatorSymbol rightExpr -> do
      right <- lowerSurfaceExprWithoutCostCentre rightExpr
      pure (ESectionRight node operatorSymbol right)
    SEBlock statements -> EBlock node <$> traverse lowerSurfaceStatement statements

lowerSurfaceLambda :: CoreNode 'Lowered 'ExpressionSort -> SourceSpan -> NonEmpty SurfaceLambdaParameter -> SurfaceExpr -> Lowering (Expr 'Lowered)
lowerSurfaceLambda firstNode lambdaSpan parameters bodyExpr =
  lowerParameters firstNode (zip [1 :: Int ..] (NonEmpty.toList parameters))
  where
    lowerParameters _ [] = lowerSurfaceExprWithoutCostCentre bodyExpr
    lowerParameters node ((parameterIndex, parameter) : rest) =
      case parameter of
        SurfaceLambdaIdentifier _ parameterName -> do
          loweredBody <-
            case rest of
              [] -> lowerSurfaceExprWithoutCostCentre bodyExpr
              _ -> freshNode lambdaSpan >>= \bodyNode -> lowerParameters bodyNode rest
          pure (ELambda node (sourceName parameterName) loweredBody)
        SurfaceLambdaPattern parameterPattern -> do
          let parameterName = generatedName (LambdaPatternArgument parameterIndex)
          caseNode <- freshNode (surfacePatternSpan parameterPattern)
          variableNode <- freshNode (surfacePatternSpan parameterPattern)
          armNode <- freshNode (surfacePatternSpan parameterPattern)
          loweredPattern <- lowerSurfacePattern parameterPattern
          loweredBody <-
            case rest of
              [] -> lowerSurfaceExprWithoutCostCentre bodyExpr
              _ -> freshNode lambdaSpan >>= \bodyNode -> lowerParameters bodyNode rest
          pure
            ( ELambda
                node
                parameterName
                ( EPatternCase
                    caseNode
                    (EVar variableNode parameterName)
                    [CaseArm armNode loweredPattern Nothing loweredBody]
                )
            )

lowerSurfacePatternLambda :: CoreNode 'Lowered 'ExpressionSort -> SourceSpan -> NonEmpty SurfacePatternLambdaClause -> Lowering (Expr 'Lowered)
lowerSurfacePatternLambda firstNode lambdaSpan clauses =
  lowerArguments firstNode argumentNames
  where
    SurfacePatternLambdaClause _ firstPatterns _ = NonEmpty.head clauses
    argumentNames = map (generatedName . LambdaPatternArgument) [1 .. NonEmpty.length firstPatterns]

    lowerArguments _ [] = lowerPatternCase
    lowerArguments node (argumentName : rest) = do
      nextNode <- freshNode lambdaSpan
      loweredBody <-
        case rest of
          [] -> lowerPatternCaseWithNode nextNode
          _ -> lowerArguments nextNode rest
      pure (ELambda node argumentName loweredBody)

    lowerPatternCase = do
      node <- freshNode lambdaSpan
      lowerPatternCaseWithNode node

    lowerPatternCaseWithNode caseNode = do
      scrutinee <- lowerScrutinee
      arms <- traverse lowerClause (NonEmpty.toList clauses)
      pure (EPatternCase caseNode scrutinee arms)

    lowerScrutinee =
      case argumentNames of
        [argumentName] -> do
          node <- freshNode lambdaSpan
          pure (EVar node argumentName)
        _ -> do
          node <- freshNode lambdaSpan
          variables <- traverse (\argumentName -> EVar <$> freshNode lambdaSpan <*> pure argumentName) argumentNames
          pure (ETuple node variables)

    lowerClause (SurfacePatternLambdaClause clauseSpan patterns bodyExpr) = do
      node <- freshNode clauseSpan
      patternValue <- lowerClausePattern clauseSpan patterns
      body <- lowerSurfaceExprWithoutCostCentre bodyExpr
      pure (CaseArm node patternValue Nothing body)

    lowerClausePattern clauseSpan patterns =
      case NonEmpty.toList patterns of
        [patternValue] -> lowerSurfacePattern patternValue
        patternValues -> do
          node <- freshNode clauseSpan
          PTuple node <$> traverse lowerSurfacePattern patternValues

-- | Lower literal syntax without changing the value domain available to later
-- semantic phases.
lowerSurfaceLiteral :: SurfaceLiteral -> Literal
lowerSurfaceLiteral literal =
  case literal of
    SLInt value -> LInt value
    SLFloat value literalSource maybeTargetType ->
      LFloat value literalSource maybeTargetType
    SLBool value -> LBool value
    SLChar value -> LChar value
    SLText value -> LText value

lowerSurfacePattern :: SurfacePattern -> Lowering (Pattern 'Lowered)
lowerSurfacePattern surfacePattern = do
  node <- freshNode (surfacePatternSpan surfacePattern)
  case surfacePatternForm surfacePattern of
    SPWildcard -> pure (PWildcard node)
    SPVariable name -> pure (PVariable node (sourceName name))
    SPLiteral literal -> pure (PLiteral node (lowerSurfaceLiteral literal))
    SPConstructor name patterns ->
      PConstructor node (sourceName name) <$> traverse lowerSurfacePattern patterns
    SPList patterns ->
      PList node <$> traverse lowerSurfacePattern patterns
    SPConsList headPattern tailPattern -> do
      headPattern' <- lowerSurfacePattern headPattern
      tailPattern' <- lowerSurfacePattern tailPattern
      pure (PConsList node headPattern' tailPattern')
    SPTuple patterns ->
      PTuple node <$> traverse lowerSurfacePattern patterns
    SPAs name patternValue -> PAs node (sourceName name) <$> lowerSurfacePattern patternValue
    SPOr patterns ->
      POr node <$> traverse lowerSurfacePattern patterns

lowerSurfaceCaseArm :: SurfaceCaseArm -> Lowering (CaseArm 'Lowered)
lowerSurfaceCaseArm (SurfaceCaseArm patternExpr guardExpr bodyExpr) = do
  node <- freshNode (surfacePatternSpan patternExpr)
  patternValue <- lowerSurfacePattern patternExpr
  guard <- traverse lowerSurfaceExprWithoutCostCentre guardExpr
  body <- lowerSurfaceExprWithoutCostCentre bodyExpr
  pure (CaseArm node patternValue guard body)

-- | Lower a parsed statement without changing its span-carrying shape.
lowerSurfaceStatement :: SurfaceStatement -> Lowering (Statement 'Lowered)
lowerSurfaceStatement surfaceStatement =
  case surfaceStatement of
    SSLet name spanValue valueExpr -> do
      node <- freshNode spanValue
      value <- lowerSurfaceExprWithoutCostCentre valueExpr
      pure (SLet node (lowerBindingName name) value)
    SSSignature name spanValue signaturePayload -> do
      node <- freshNode spanValue
      pure (SSignature node (lowerBindingName name) (lowerSurfaceSignaturePayload signaturePayload))
    SSData spanValue typeName typeParameters constructors -> do
      node <- freshNode spanValue
      loweredConstructors <- traverse (lowerSurfaceDataConstructor spanValue) constructors
      pure (SData node (sourceName typeName) (map sourceName typeParameters) loweredConstructors)
    SSClass spanValue capabilityName parameters methods -> do
      node <- freshNode spanValue
      loweredMethods <- traverse lowerSurfaceClassMethodSignature methods
      pure (SClass node (sourceName capabilityName) (map sourceName parameters) loweredMethods)
    SSImpl spanValue capabilityName arguments methods -> do
      node <- freshNode spanValue
      loweredMethods <- traverse lowerSurfaceImplMethod methods
      pure (SImpl node (sourceName capabilityName) (map lowerSurfaceSignatureType arguments) loweredMethods)
    SSModule spanValue modulePath _ -> do
      node <- freshNode spanValue
      pure (SModule node modulePath)
    SSImport spanValue modulePath alias importedSymbols -> do
      node <- freshNode spanValue
      pure (SImport node modulePath alias importedSymbols)
    SSExpr spanValue expr -> do
      node <- freshNode spanValue
      value <- lowerSurfaceExprWithoutCostCentre expr
      pure (SExpr node value)

lowerSurfaceClassMethodSignature :: SurfaceClassMethodSignature -> Lowering (ClassMethodSignature 'Lowered)
lowerSurfaceClassMethodSignature (SurfaceClassMethodSignature methodName spanValue signaturePayload) = do
  node <- freshNode spanValue
  pure (ClassMethodSignature node (sourceName methodName) (lowerSurfaceSignaturePayload signaturePayload))

lowerSurfaceImplMethod :: SurfaceImplMethod -> Lowering (ImplMethod 'Lowered)
lowerSurfaceImplMethod (SurfaceImplMethod methodName spanValue methodExpr) = do
  node <- freshNode spanValue
  body <- lowerSurfaceExprWithoutCostCentre methodExpr
  pure (ImplMethod node (sourceName methodName) body)

lowerSurfaceSignaturePayload :: SurfaceSignaturePayload -> SignaturePayload 'Lowered
lowerSurfaceSignaturePayload surfaceSignaturePayload =
  case surfaceSignaturePayload of
    TypeRepresentation.SignatureType signatureType ->
      TypeRepresentation.SignatureType (lowerSurfaceSignatureType signatureType)
    TypeRepresentation.ConstrainedSignature constraints signatureType ->
      TypeRepresentation.ConstrainedSignature
        (map lowerSurfaceSignatureConstraint constraints)
        (lowerSurfaceSignatureType signatureType)
    TypeRepresentation.UnsupportedSignature signatureTokens ->
      TypeRepresentation.UnsupportedSignature (map lowerSurfaceSignatureToken signatureTokens)

-- | Preserve structured constrained-signature payloads exactly; acceptance or
-- rejection of the constraint subset belongs to type inference.
lowerSurfaceSignatureConstraint :: SurfaceSignatureConstraint -> SignatureConstraint 'Lowered
lowerSurfaceSignatureConstraint =
  bimap lowerSurfaceSignatureName sourceName

lowerSurfaceSignatureType :: SurfaceSignatureType -> SignatureType 'Lowered
lowerSurfaceSignatureType =
  bimap lowerSurfaceSignatureName sourceName

lowerSurfaceSignatureName :: Identifier -> UnresolvedName
lowerSurfaceSignatureName name =
  case splitQualifiedIdentifierText (identifierText name) of
    Just (qualifier, member) ->
      qualifiedName (mkIdentifier qualifier) (mkIdentifier member)
    Nothing -> sourceName name

lowerSurfaceSignatureToken :: SurfaceSignatureToken -> SignatureToken 'Lowered
lowerSurfaceSignatureToken =
  fmap (sourceName . mkIdentifier)

lowerSurfaceDataConstructor :: SourceSpan -> SurfaceDataConstructor -> Lowering (DataConstructor 'Lowered)
lowerSurfaceDataConstructor declarationSpan (SurfaceDataConstructor constructorName fieldTypes) = do
  node <- freshNode declarationSpan
  pure (DataConstructor node (sourceName constructorName) (map lowerSurfaceSignatureType fieldTypes))

lowerBindingName :: Identifier -> UnresolvedName
lowerBindingName name
  | isOperatorBindingIdentifierText (identifierText name) =
      operatorBindingNameFromIdentifier name
  | otherwise = sourceName name
