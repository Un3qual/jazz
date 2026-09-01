{-# LANGUAGE OverloadedStrings #-}

-- | Lowers parser-surface nodes into the smaller core AST consumed by later
-- compiler phases.
module Jazz.Compiler.Parser.Lower
  ( ModuleDeclaration (..),
    ModuleLoweringFailure (..),
    lowerSurfaceExpr,
    lowerSurfaceModuleDetailed,
    lowerSurfaceModule,
  )
where

import Data.Bifunctor (bimap)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
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
  )
import Jazz.Compiler.ModuleExports
  ( qualifyModuleExportSelectorSpans,
  )
import Jazz.Compiler.ModuleGraph
  ( CoreModule (..),
    CoreResolvedImport (..),
    DeclaredModuleExports (..),
  )
import Jazz.Compiler.Name
  ( GeneratedNameKind (..),
    Identifier,
    Name,
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
  deriving (Eq, Show)

-- | Validate and lower one parsed module exactly once. Module/import forms are
-- retained as graph metadata and removed from the executable core scope.
lowerSurfaceModule :: FilePath -> [Text] -> SurfaceExpr -> Either Diagnostic CoreModule
lowerSurfaceModule sourcePath expectedPath surfaceExpr =
  case lowerSurfaceModuleDetailed sourcePath expectedPath surfaceExpr of
    Left failure -> Left (moduleLoweringFailureDiagnostic failure)
    Right coreModule -> Right coreModule

-- | Preserve the semantic inputs for the two module-lowering failures. The
-- public compiler entry point above renders these into the existing E4005 and
-- E4006 diagnostics, so production behavior remains unchanged.
lowerSurfaceModuleDetailed :: FilePath -> [Text] -> SurfaceExpr -> Either ModuleLoweringFailure CoreModule
lowerSurfaceModuleDetailed sourcePath expectedPath surfaceExpr =
  {-# SCC "jazz-stage:lowering" #-}
  do
    (declaredPath, declaredExports) <- validateDeclaration
    pure
      CoreModule
        { coreModuleDeclaredPath = declaredPath,
          coreModuleDeclaredExports = declaredExports,
          coreModuleImports = imports,
          coreModuleExpr = qualifyExprSourceSpans sourcePath loweredBody
        }
  where
    statements =
      case surfaceExprForm surfaceExpr of
        SEBlock moduleStatements -> moduleStatements
        _ -> []

    declarations =
      [ (ModuleDeclaration spanValue modulePath, moduleExports)
      | SSModule spanValue modulePath moduleExports <- statements
      ]

    imports =
      [ CoreResolvedImport
          { coreResolvedImportSpan = qualifySourceSpan sourcePath spanValue,
            coreResolvedImportPath = modulePath,
            coreResolvedImportAlias = alias,
            coreResolvedImportSymbols = importedSymbols
          }
      | SSImport spanValue modulePath alias importedSymbols <- statements
      ]

    executableStatements =
      [ statement
      | statement <- statements,
        case statement of
          SSModule {} -> False
          SSImport {} -> False
          _ -> True
      ]

    loweredBody =
      case surfaceExprForm surfaceExpr of
        SEBlock _ -> EBlock (map lowerSurfaceStatement executableStatements)
        _ -> lowerSurfaceExprWithoutCostCentre surfaceExpr

    validateDeclaration =
      case declarations of
        [] -> Right (Nothing, Nothing)
        [(declaration, declaredExportSelectors)]
          | moduleDeclarationPath declaration == expectedPath ->
              Right
                ( Just (moduleDeclarationPath declaration),
                  DeclaredModuleExports
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
  where
    renderModulePath = Text.intercalate "::"

qualifyExprSourceSpans :: FilePath -> Expr -> Expr
qualifyExprSourceSpans sourcePath expr =
  case expr of
    ELit literal -> ELit literal
    EVar name -> EVar name
    ELambda parameter body -> ELambda parameter (go body)
    EOperatorValue symbol -> EOperatorValue symbol
    EList items -> EList (map go items)
    ETuple items -> ETuple (map go items)
    EApply function argument -> EApply (go function) (go argument)
    ETypeApplication function spanValue signatureType -> ETypeApplication (go function) (qualifySpan spanValue) signatureType
    EIf condition trueBranch falseBranch -> EIf (go condition) (go trueBranch) (go falseBranch)
    EPatternCase scrutinee arms -> EPatternCase (go scrutinee) (map qualifyCaseArm arms)
    EBinary symbol left right -> EBinary symbol (go left) (go right)
    ESectionLeft left symbol -> ESectionLeft (go left) symbol
    ESectionRight symbol right -> ESectionRight symbol (go right)
    EBlock statements -> EBlock (map qualifyStatement statements)
  where
    go = qualifyExprSourceSpans sourcePath
    qualifySpan = qualifySourceSpan sourcePath

    qualifyCaseArm (CaseArm patternValue guardExpr bodyExpr) =
      CaseArm patternValue (fmap go guardExpr) (go bodyExpr)

    qualifyClassMethod (ClassMethodSignature name spanValue payload) =
      ClassMethodSignature name (qualifySpan spanValue) payload

    qualifyImplMethod (ImplMethod name spanValue bodyExpr) =
      ImplMethod name (qualifySpan spanValue) (go bodyExpr)

    qualifyStatement statement =
      case statement of
        SLet name spanValue valueExpr -> SLet name (qualifySpan spanValue) (go valueExpr)
        SSignature name spanValue payload -> SSignature name (qualifySpan spanValue) payload
        SData spanValue name parameters constructors -> SData (qualifySpan spanValue) name parameters constructors
        SClass spanValue name parameters methods ->
          SClass (qualifySpan spanValue) name parameters (map qualifyClassMethod methods)
        SImpl spanValue name arguments methods ->
          SImpl (qualifySpan spanValue) name arguments (map qualifyImplMethod methods)
        SModule spanValue path -> SModule (qualifySpan spanValue) path
        SImport spanValue path alias symbols -> SImport (qualifySpan spanValue) path alias symbols
        SExpr spanValue valueExpr -> SExpr (qualifySpan spanValue) (go valueExpr)

-- | Convert parser-surface nodes into core nodes. The current core AST does not
-- yet retain the surface-node locations; the phase-indexed core migration owns
-- that transition.
lowerSurfaceExpr :: SurfaceExpr -> Expr
lowerSurfaceExpr surfaceExpr =
  {-# SCC "jazz-stage:lowering" #-}
  lowerSurfaceExprWithoutCostCentre surfaceExpr

lowerSurfaceExprWithoutCostCentre :: SurfaceExpr -> Expr
lowerSurfaceExprWithoutCostCentre surfaceExpr =
  case surfaceExprForm surfaceExpr of
    SELit literal -> ELit (lowerSurfaceLiteral literal)
    SEVar name -> EVar (sourceName name)
    SEQualifiedVar qualifier member ->
      EVar (qualifiedName qualifier member)
    SELambda parameters bodyExpr ->
      lowerSurfaceLambda parameters bodyExpr
    SEPatternLambda clauses ->
      lowerSurfacePatternLambda clauses
    SEOperatorValue operatorSymbol -> EOperatorValue operatorSymbol
    SEList elements ->
      EList (map lowerSurfaceExprWithoutCostCentre elements)
    SETuple elements ->
      ETuple (map lowerSurfaceExprWithoutCostCentre elements)
    SEApply functionExpr argumentExpr ->
      EApply (lowerSurfaceExprWithoutCostCentre functionExpr) (lowerSurfaceExprWithoutCostCentre argumentExpr)
    SETypeApplication functionExpr spanValue signatureType ->
      ETypeApplication (lowerSurfaceExprWithoutCostCentre functionExpr) spanValue (lowerSurfaceSignatureType signatureType)
    SEIf conditionExpr thenExpr elseExpr ->
      EIf
        (lowerSurfaceExprWithoutCostCentre conditionExpr)
        (lowerSurfaceExprWithoutCostCentre thenExpr)
        (lowerSurfaceExprWithoutCostCentre elseExpr)
    SECase scrutineeExpr caseArms ->
      EPatternCase
        (lowerSurfaceExprWithoutCostCentre scrutineeExpr)
        (map lowerSurfaceCaseArm caseArms)
    SEBinary operatorSymbol functionExpr argumentExpr
      | operatorSymbol == Text.pack "$" ->
          EApply
            (lowerSurfaceExprWithoutCostCentre functionExpr)
            (lowerSurfaceExprWithoutCostCentre argumentExpr)
    SEBinary operatorSymbol leftExpr rightExpr ->
      EBinary
        operatorSymbol
        (lowerSurfaceExprWithoutCostCentre leftExpr)
        (lowerSurfaceExprWithoutCostCentre rightExpr)
    SESectionLeft leftExpr operatorSymbol ->
      ESectionLeft (lowerSurfaceExprWithoutCostCentre leftExpr) operatorSymbol
    SESectionRight operatorSymbol rightExpr ->
      ESectionRight operatorSymbol (lowerSurfaceExprWithoutCostCentre rightExpr)
    SEBlock statements -> EBlock (map lowerSurfaceStatement statements)

lowerSurfaceLambda :: NonEmpty SurfaceLambdaParameter -> SurfaceExpr -> Expr
lowerSurfaceLambda parameters bodyExpr =
  foldr
    lowerParameter
    (lowerSurfaceExprWithoutCostCentre bodyExpr)
    (zip [1 :: Int ..] (NonEmpty.toList parameters))
  where
    lowerParameter (_, SurfaceLambdaIdentifier _ parameterName) loweredBody =
      ELambda (sourceName parameterName) loweredBody
    lowerParameter (parameterIndex, SurfaceLambdaPattern parameterPattern) loweredBody =
      let parameterName =
            generatedName (LambdaPatternArgument parameterIndex)
       in ELambda
            parameterName
            ( EPatternCase
                (EVar parameterName)
                [CaseArm (lowerSurfacePattern parameterPattern) Nothing loweredBody]
            )

lowerSurfacePatternLambda :: NonEmpty SurfacePatternLambdaClause -> Expr
lowerSurfacePatternLambda clauses =
  foldr
    ELambda
    ( EPatternCase
        scrutinee
        (map lowerClause (NonEmpty.toList clauses))
    )
    argumentNames
  where
    SurfacePatternLambdaClause _ firstPatterns _ = NonEmpty.head clauses
    argumentNames =
      map
        (generatedName . LambdaPatternArgument)
        [1 .. NonEmpty.length firstPatterns]
    scrutinee =
      case argumentNames of
        [argumentName] -> EVar argumentName
        _ -> ETuple (map EVar argumentNames)
    lowerClause (SurfacePatternLambdaClause _ patterns bodyExpr) =
      CaseArm
        (lowerClausePattern patterns)
        Nothing
        (lowerSurfaceExprWithoutCostCentre bodyExpr)
    lowerClausePattern patterns =
      case NonEmpty.toList patterns of
        [patternValue] -> lowerSurfacePattern patternValue
        patternValues -> PTuple (map lowerSurfacePattern patternValues)

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

lowerSurfacePattern :: SurfacePattern -> Pattern
lowerSurfacePattern surfacePattern =
  case surfacePatternForm surfacePattern of
    SPWildcard -> PWildcard
    SPVariable name -> PVariable (sourceName name)
    SPLiteral literal -> PLiteral (lowerSurfaceLiteral literal)
    SPConstructor name patterns ->
      PConstructor (sourceName name) (map lowerSurfacePattern patterns)
    SPList patterns ->
      PList (map lowerSurfacePattern patterns)
    SPConsList headPattern tailPattern ->
      PConsList (lowerSurfacePattern headPattern) (lowerSurfacePattern tailPattern)
    SPTuple patterns ->
      PTuple (map lowerSurfacePattern patterns)
    SPAs name patternValue ->
      PAs (sourceName name) (lowerSurfacePattern patternValue)
    SPOr patterns ->
      POr (map lowerSurfacePattern patterns)

lowerSurfaceCaseArm :: SurfaceCaseArm -> CaseArm
lowerSurfaceCaseArm (SurfaceCaseArm patternExpr guardExpr bodyExpr) =
  CaseArm
    (lowerSurfacePattern patternExpr)
    (fmap lowerSurfaceExprWithoutCostCentre guardExpr)
    (lowerSurfaceExprWithoutCostCentre bodyExpr)

-- | Lower a parsed statement without changing its span-carrying shape.
lowerSurfaceStatement :: SurfaceStatement -> Statement
lowerSurfaceStatement surfaceStatement =
  case surfaceStatement of
    SSLet name spanValue valueExpr ->
      SLet (lowerBindingName name) spanValue (lowerSurfaceExprWithoutCostCentre valueExpr)
    SSSignature name spanValue signaturePayload ->
      SSignature (lowerBindingName name) spanValue (lowerSurfaceSignaturePayload signaturePayload)
    SSData spanValue typeName typeParameters constructors ->
      SData spanValue (sourceName typeName) (map sourceName typeParameters) (map lowerSurfaceDataConstructor constructors)
    SSClass spanValue capabilityName parameters methods ->
      SClass spanValue (sourceName capabilityName) (map sourceName parameters) (map lowerSurfaceClassMethodSignature methods)
    SSImpl spanValue capabilityName arguments methods ->
      SImpl
        spanValue
        (sourceName capabilityName)
        (map lowerSurfaceSignatureType arguments)
        (map lowerSurfaceImplMethod methods)
    SSModule spanValue modulePath _ ->
      SModule spanValue modulePath
    SSImport spanValue modulePath alias importedSymbols ->
      SImport spanValue modulePath alias importedSymbols
    SSExpr spanValue expr ->
      SExpr spanValue (lowerSurfaceExprWithoutCostCentre expr)

lowerSurfaceClassMethodSignature :: SurfaceClassMethodSignature -> ClassMethodSignature
lowerSurfaceClassMethodSignature (SurfaceClassMethodSignature methodName spanValue signaturePayload) =
  ClassMethodSignature (sourceName methodName) spanValue (lowerSurfaceSignaturePayload signaturePayload)

lowerSurfaceImplMethod :: SurfaceImplMethod -> ImplMethod
lowerSurfaceImplMethod (SurfaceImplMethod methodName spanValue methodExpr) =
  ImplMethod (sourceName methodName) spanValue (lowerSurfaceExprWithoutCostCentre methodExpr)

lowerSurfaceSignaturePayload :: SurfaceSignaturePayload -> SignaturePayload
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
lowerSurfaceSignatureConstraint :: SurfaceSignatureConstraint -> SignatureConstraint
lowerSurfaceSignatureConstraint =
  bimap lowerSurfaceSignatureName sourceName

lowerSurfaceSignatureType :: SurfaceSignatureType -> SignatureType
lowerSurfaceSignatureType =
  bimap lowerSurfaceSignatureName sourceName

lowerSurfaceSignatureName :: Identifier -> Name
lowerSurfaceSignatureName name =
  case splitQualifiedIdentifierText (identifierText name) of
    Just (qualifier, member) ->
      qualifiedName (mkIdentifier qualifier) (mkIdentifier member)
    Nothing -> sourceName name

lowerSurfaceSignatureToken :: SurfaceSignatureToken -> SignatureToken
lowerSurfaceSignatureToken =
  fmap (sourceName . mkIdentifier)

lowerSurfaceDataConstructor :: SurfaceDataConstructor -> DataConstructor
lowerSurfaceDataConstructor (SurfaceDataConstructor constructorName fieldTypes) =
  DataConstructor (sourceName constructorName) (map lowerSurfaceSignatureType fieldTypes)

lowerBindingName :: Identifier -> Name
lowerBindingName name
  | isOperatorBindingIdentifierText (identifierText name) =
      operatorBindingNameFromIdentifier name
  | otherwise = sourceName name
