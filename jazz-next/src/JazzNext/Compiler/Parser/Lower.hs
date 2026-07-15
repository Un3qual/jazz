{-# LANGUAGE OverloadedStrings #-}

-- | Lowers parser-surface nodes into the smaller core AST consumed by later
-- compiler phases.
module JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr,
    lowerSurfaceModule
  ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Data.Text (Text)
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    DataConstructorArgument (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    NumericType (..),
    Pattern (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceClassMethodSignature (..),
    SurfaceDataConstructorArgument (..),
    SurfaceDataConstructor (..),
    SurfaceExpr (..),
    SurfaceImplMethod (..),
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfaceNumericType (..),
    SurfacePattern (..),
    SurfaceSignatureConstraint (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureToken (..),
    SurfaceSignatureType (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Name
  ( GeneratedNameKind (..),
    Identifier,
    Name,
    generatedName,
    identifierText,
    isOperatorBindingIdentifierText,
    mkIdentifier,
    operatorBindingNameFromIdentifier,
    qualifiedName,
    splitQualifiedIdentifierText,
    sourceName
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (..),
    mkErrorDiagnostic,
    qualifySourceSpan
  )
import JazzNext.Compiler.DiagnosticCatalog
  ( ErrorCode (..)
  )
import JazzNext.Compiler.ModuleGraph
  ( CoreModule (..),
    DeclaredModuleExports (..),
    ResolvedImport (..)
  )

-- | Validate and lower one parsed module exactly once. Module/import forms are
-- retained as graph metadata and removed from the executable core scope.
lowerSurfaceModule :: FilePath -> [Text] -> SurfaceExpr -> Either Diagnostic CoreModule
lowerSurfaceModule sourcePath expectedPath surfaceExpr =
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
      case surfaceExpr of
        SEBlock moduleStatements -> moduleStatements
        _ -> []

    declarations =
      [ (modulePath, spanValue, moduleExports)
        | SSModule spanValue modulePath moduleExports <- statements
      ]

    imports =
      [ ResolvedImport
          { resolvedImportSpan = qualifySourceSpan sourcePath spanValue,
            resolvedImportPath = modulePath,
            resolvedImportAlias = alias,
            resolvedImportSymbols = importedSymbols
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
      case surfaceExpr of
        SEBlock _ -> EBlock (map lowerSurfaceStatement executableStatements)
        _ -> lowerSurfaceExprWithoutCostCentre surfaceExpr

    validateDeclaration =
      case declarations of
        [] -> Right (Nothing, Nothing)
        [(declaredPath, declarationSpan, declaredExportSelectors)]
          | declaredPath == expectedPath ->
              Right
                ( Just declaredPath,
                  DeclaredModuleExports
                    (qualifySourceSpan sourcePath declarationSpan)
                    <$> declaredExportSelectors
                )
          | otherwise ->
              Left
                ( mkErrorDiagnostic
                    E4006 CompilationOrigin
                    ( "module declaration mismatch at '"
                        <> Text.pack sourcePath
                        <> "': expected '"
                        <> renderModulePath expectedPath
                        <> "', found '"
                        <> renderModulePath declaredPath
                        <> "'"
                    )
                )
        declaredModules ->
          Left
            ( mkErrorDiagnostic
                E4005 CompilationOrigin
                ( "multiple module declarations in '"
                    <> Text.pack sourcePath
                    <> "': "
                    <> Text.intercalate ", " (map (renderModulePath . declaredModulePath) declaredModules)
                )
            )

    declaredModulePath (modulePath, _, _) = modulePath
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

-- | Convert parser-surface nodes into core nodes while preserving statement
-- source spans. Expression constructors like `ELit`, `EVar`, `EApply`, and
-- `EBinary` do not carry spans in the core AST, so expression-level location
-- handling stays in later phases.
lowerSurfaceExpr :: SurfaceExpr -> Expr
lowerSurfaceExpr surfaceExpr =
  {-# SCC "jazz-stage:lowering" #-}
  lowerSurfaceExprWithoutCostCentre surfaceExpr

lowerSurfaceExprWithoutCostCentre :: SurfaceExpr -> Expr
lowerSurfaceExprWithoutCostCentre surfaceExpr =
  case surfaceExpr of
    SELit literal -> ELit (lowerSurfaceLiteral literal)
    SEVar name -> EVar (sourceName name)
    SEQualifiedVar qualifier member ->
      EVar (qualifiedName qualifier member)
    SELambda parameters bodyExpr ->
      lowerSurfaceLambda parameters bodyExpr
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
    lowerParameter (_, SurfaceLambdaIdentifier parameterName) loweredBody =
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

-- | Lower literal syntax without changing the value domain available to later
-- semantic phases.
lowerSurfaceLiteral :: SurfaceLiteral -> Literal
lowerSurfaceLiteral literal =
  case literal of
    SLInt value -> LInt value
    SLFloat value literalSource maybeTargetType ->
      LFloat value literalSource (fmap lowerSurfaceNumericType maybeTargetType)
    SLBool value -> LBool value
    SLChar value -> LChar value
    SLText value -> LText value

lowerSurfacePattern :: SurfacePattern -> Pattern
lowerSurfacePattern surfacePattern =
  case surfacePattern of
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
    SPAs name pattern ->
      PAs (sourceName name) (lowerSurfacePattern pattern)
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
    SurfaceSignatureType signatureType ->
      SignatureType (lowerSurfaceSignatureType signatureType)
    SurfaceConstrainedSignature constraints signatureType ->
      ConstrainedSignature
        (map lowerSurfaceSignatureConstraint constraints)
        (lowerSurfaceSignatureType signatureType)
    SurfaceUnsupportedSignature signatureTokens ->
      UnsupportedSignature (map lowerSurfaceSignatureToken signatureTokens)

-- | Preserve structured constrained-signature payloads exactly; acceptance or
-- rejection of the constraint subset belongs to type inference.
lowerSurfaceSignatureConstraint :: SurfaceSignatureConstraint -> SignatureConstraint
lowerSurfaceSignatureConstraint (SurfaceSignatureConstraint constraintName constraintArguments) =
  SignatureConstraint
    (lowerSurfaceSignatureName constraintName)
    (map lowerSurfaceSignatureType constraintArguments)

lowerSurfaceSignatureType :: SurfaceSignatureType -> SignatureType
lowerSurfaceSignatureType surfaceSignatureType =
  case surfaceSignatureType of
    SurfaceTypeInt -> TypeInt
    SurfaceTypeFloat -> TypeFloat
    SurfaceTypeNumeric numericType -> TypeNumeric (lowerSurfaceNumericType numericType)
    SurfaceTypeBool -> TypeBool
    SurfaceTypeChar -> TypeChar
    SurfaceTypeText -> TypeText
    SurfaceTypeVariable name -> TypeVariable (sourceName name)
    SurfaceTypeName name -> TypeName (lowerSurfaceSignatureName name)
    SurfaceTypeApplication name arguments ->
      TypeApplication (lowerSurfaceSignatureName name) (map lowerSurfaceSignatureType arguments)
    SurfaceTypeList innerType ->
      TypeList (lowerSurfaceSignatureType innerType)
    SurfaceTypeTuple elementTypes ->
      TypeTuple (map lowerSurfaceSignatureType elementTypes)
    SurfaceTypeFunction argumentType resultType ->
      TypeFunction
        (lowerSurfaceSignatureType argumentType)
        (lowerSurfaceSignatureType resultType)

lowerSurfaceSignatureName :: Identifier -> Name
lowerSurfaceSignatureName name =
  case splitQualifiedIdentifierText (identifierText name) of
    Just (qualifier, member) ->
      qualifiedName (mkIdentifier qualifier) (mkIdentifier member)
    Nothing -> sourceName name

lowerSurfaceNumericType :: SurfaceNumericType -> NumericType
lowerSurfaceNumericType surfaceNumericType =
  case surfaceNumericType of
    SurfaceNumericInt8 -> NumericInt8
    SurfaceNumericInt16 -> NumericInt16
    SurfaceNumericInt32 -> NumericInt32
    SurfaceNumericInt64 -> NumericInt64
    SurfaceNumericUInt8 -> NumericUInt8
    SurfaceNumericUInt16 -> NumericUInt16
    SurfaceNumericUInt32 -> NumericUInt32
    SurfaceNumericUInt64 -> NumericUInt64
    SurfaceNumericFloat16 -> NumericFloat16
    SurfaceNumericFloat32 -> NumericFloat32
    SurfaceNumericFloat64 -> NumericFloat64

lowerSurfaceSignatureToken :: SurfaceSignatureToken -> SignatureToken
lowerSurfaceSignatureToken surfaceSignatureToken =
  case surfaceSignatureToken of
    SurfaceSignatureNameToken name -> SignatureNameToken (sourceName (mkIdentifier name))
    SurfaceSignatureIntToken value -> SignatureIntToken value
    SurfaceSignatureArrowToken -> SignatureArrowToken
    SurfaceSignatureAtToken -> SignatureAtToken
    SurfaceSignatureColonToken -> SignatureColonToken
    SurfaceSignatureLParenToken -> SignatureLParenToken
    SurfaceSignatureRParenToken -> SignatureRParenToken
    SurfaceSignatureLBraceToken -> SignatureLBraceToken
    SurfaceSignatureRBraceToken -> SignatureRBraceToken
    SurfaceSignatureLBracketToken -> SignatureLBracketToken
    SurfaceSignatureRBracketToken -> SignatureRBracketToken
    SurfaceSignatureCommaToken -> SignatureCommaToken
    SurfaceSignatureOperatorToken symbol -> SignatureOperatorToken symbol
    SurfaceSignatureOtherToken lexeme -> SignatureOtherToken lexeme

lowerSurfaceDataConstructor :: SurfaceDataConstructor -> DataConstructor
lowerSurfaceDataConstructor (SurfaceDataConstructor constructorName constructorArguments) =
  DataConstructor (sourceName constructorName) (map lowerSurfaceDataConstructorArgument constructorArguments)

lowerSurfaceDataConstructorArgument :: SurfaceDataConstructorArgument -> DataConstructorArgument
lowerSurfaceDataConstructorArgument surfaceArgument =
  case surfaceArgument of
    SurfaceDataConstructorArgumentName argumentName ->
      DataConstructorArgumentName (sourceName argumentName)
    SurfaceDataConstructorArgumentOpaque ->
      DataConstructorArgumentOpaque

lowerBindingName :: Identifier -> Name
lowerBindingName name
  | isOperatorBindingIdentifierText (identifierText name) =
      operatorBindingNameFromIdentifier name
  | otherwise = sourceName name
