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
    ConstraintSignatureType (..),
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
    SurfaceConstrainedSignatureType (..),
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
    generatedName,
    identifierText,
    isOperatorBindingIdentifierText,
    mkIdentifier,
    operatorBindingNameFromIdentifier,
    qualifiedName,
    sourceName
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    mkDiagnostic,
    qualifySourceSpan
  )
import JazzNext.Compiler.ModuleGraph
  ( CoreModule (..),
    ResolvedImport (..)
  )

-- | Validate and lower one parsed module exactly once. Module/import forms are
-- retained as graph metadata and removed from the executable core scope.
lowerSurfaceModule :: FilePath -> [Text] -> SurfaceExpr -> Either Diagnostic CoreModule
lowerSurfaceModule sourcePath expectedPath surfaceExpr = do
  declaredPath <- validateDeclaredPath
  pure
    CoreModule
      { coreModuleDeclaredPath = declaredPath,
        coreModuleImports = imports,
        coreModuleExpr = qualifyExprSourceSpans sourcePath loweredBody
      }
  where
    statements =
      case surfaceExpr of
        SEBlock moduleStatements -> moduleStatements
        _ -> []

    declarations =
      [ modulePath
        | SSModule _ modulePath <- statements
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
        _ -> lowerSurfaceExpr surfaceExpr

    validateDeclaredPath =
      case declarations of
        [] -> Right Nothing
        [declaredPath]
          | declaredPath == expectedPath -> Right (Just declaredPath)
          | otherwise ->
              Left
                ( mkDiagnostic
                    "E4006"
                    ( "module declaration mismatch at '"
                        <> Text.pack sourcePath
                        <> "': expected '"
                        <> renderModulePath expectedPath
                        <> "', found '"
                        <> renderModulePath declaredPath
                        <> "'"
                    )
                )
        declaredPaths ->
          Left
            ( mkDiagnostic
                "E4005"
                ( "multiple module declarations in '"
                    <> Text.pack sourcePath
                    <> "': "
                    <> Text.intercalate ", " (map renderModulePath declaredPaths)
                )
            )

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
    ETypeApplication function signatureType -> ETypeApplication (go function) signatureType
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
  case surfaceExpr of
    SELit literal -> ELit (lowerSurfaceLiteral literal)
    SEVar name -> EVar (sourceName name)
    SEQualifiedVar qualifier member ->
      EVar (qualifiedName qualifier member)
    SELambda parameters bodyExpr ->
      lowerSurfaceLambda parameters bodyExpr
    SEOperatorValue operatorSymbol -> EOperatorValue operatorSymbol
    SEList elements ->
      EList (map lowerSurfaceExpr elements)
    SETuple elements ->
      ETuple (map lowerSurfaceExpr elements)
    SEApply functionExpr argumentExpr ->
      EApply (lowerSurfaceExpr functionExpr) (lowerSurfaceExpr argumentExpr)
    SETypeApplication functionExpr signatureType ->
      ETypeApplication (lowerSurfaceExpr functionExpr) (lowerSurfaceSignatureType signatureType)
    SEIf conditionExpr thenExpr elseExpr ->
      EIf
        (lowerSurfaceExpr conditionExpr)
        (lowerSurfaceExpr thenExpr)
        (lowerSurfaceExpr elseExpr)
    SECase scrutineeExpr caseArms ->
      EPatternCase
        (lowerSurfaceExpr scrutineeExpr)
        (map lowerSurfaceCaseArm caseArms)
    SEBinary operatorSymbol functionExpr argumentExpr
      | operatorSymbol == Text.pack "$" ->
          EApply
            (lowerSurfaceExpr functionExpr)
            (lowerSurfaceExpr argumentExpr)
    SEBinary operatorSymbol leftExpr rightExpr ->
      EBinary
        operatorSymbol
        (lowerSurfaceExpr leftExpr)
        (lowerSurfaceExpr rightExpr)
    SESectionLeft leftExpr operatorSymbol ->
      ESectionLeft (lowerSurfaceExpr leftExpr) operatorSymbol
    SESectionRight operatorSymbol rightExpr ->
      ESectionRight operatorSymbol (lowerSurfaceExpr rightExpr)
    SEBlock statements -> EBlock (map lowerSurfaceStatement statements)

lowerSurfaceLambda :: NonEmpty SurfaceLambdaParameter -> SurfaceExpr -> Expr
lowerSurfaceLambda parameters bodyExpr =
  foldr
    lowerParameter
    (lowerSurfaceExpr bodyExpr)
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
    (fmap lowerSurfaceExpr guardExpr)
    (lowerSurfaceExpr bodyExpr)

-- | Lower a parsed statement without changing its span-carrying shape.
lowerSurfaceStatement :: SurfaceStatement -> Statement
lowerSurfaceStatement surfaceStatement =
  case surfaceStatement of
    SSLet name spanValue valueExpr ->
      SLet (lowerBindingName name) spanValue (lowerSurfaceExpr valueExpr)
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
        (map lowerSurfaceConstrainedSignatureType arguments)
        (map lowerSurfaceImplMethod methods)
    SSModule spanValue modulePath ->
      SModule spanValue modulePath
    SSImport spanValue modulePath alias importedSymbols ->
      SImport spanValue modulePath alias importedSymbols
    SSExpr spanValue expr ->
      SExpr spanValue (lowerSurfaceExpr expr)

lowerSurfaceClassMethodSignature :: SurfaceClassMethodSignature -> ClassMethodSignature
lowerSurfaceClassMethodSignature (SurfaceClassMethodSignature methodName spanValue signaturePayload) =
  ClassMethodSignature (sourceName methodName) spanValue (lowerSurfaceSignaturePayload signaturePayload)

lowerSurfaceImplMethod :: SurfaceImplMethod -> ImplMethod
lowerSurfaceImplMethod (SurfaceImplMethod methodName spanValue methodExpr) =
  ImplMethod (sourceName methodName) spanValue (lowerSurfaceExpr methodExpr)

lowerSurfaceSignaturePayload :: SurfaceSignaturePayload -> SignaturePayload
lowerSurfaceSignaturePayload surfaceSignaturePayload =
  case surfaceSignaturePayload of
    SurfaceSignatureType signatureType ->
      SignatureType (lowerSurfaceSignatureType signatureType)
    SurfaceConstrainedSignature constraints signatureType ->
      ConstrainedSignature
        (map lowerSurfaceSignatureConstraint constraints)
        (lowerSurfaceConstrainedSignatureType signatureType)
    SurfaceUnsupportedSignature signatureTokens ->
      UnsupportedSignature (map lowerSurfaceSignatureToken signatureTokens)

-- | Preserve structured constrained-signature payloads exactly; acceptance or
-- rejection of the constraint subset belongs to type inference.
lowerSurfaceSignatureConstraint :: SurfaceSignatureConstraint -> SignatureConstraint
lowerSurfaceSignatureConstraint (SurfaceSignatureConstraint constraintName constraintArguments) =
  SignatureConstraint
    (sourceName constraintName)
    (map lowerSurfaceConstrainedSignatureType constraintArguments)

lowerSurfaceConstrainedSignatureType :: SurfaceConstrainedSignatureType -> ConstraintSignatureType
lowerSurfaceConstrainedSignatureType signatureType =
  case signatureType of
    SurfaceConstrainedTypeName name ->
      ConstraintTypeName (sourceName name)
    SurfaceConstrainedTypeApplication name arguments ->
      ConstraintTypeApplication (sourceName name) (map lowerSurfaceConstrainedSignatureType arguments)
    SurfaceConstrainedTypeList innerType ->
      ConstraintTypeList (lowerSurfaceConstrainedSignatureType innerType)
    SurfaceConstrainedTypeTuple elementTypes ->
      ConstraintTypeTuple (map lowerSurfaceConstrainedSignatureType elementTypes)
    SurfaceConstrainedTypeFunction argumentType resultType ->
      ConstraintTypeFunction
        (lowerSurfaceConstrainedSignatureType argumentType)
        (lowerSurfaceConstrainedSignatureType resultType)

lowerSurfaceSignatureType :: SurfaceSignatureType -> SignatureType
lowerSurfaceSignatureType surfaceSignatureType =
  case surfaceSignatureType of
    SurfaceTypeInt -> TypeInt
    SurfaceTypeFloat -> TypeFloat
    SurfaceTypeNumeric numericType -> TypeNumeric (lowerSurfaceNumericType numericType)
    SurfaceTypeBool -> TypeBool
    SurfaceTypeList innerType ->
      TypeList (lowerSurfaceSignatureType innerType)
    SurfaceTypeTuple elementTypes ->
      TypeTuple (map lowerSurfaceSignatureType elementTypes)
    SurfaceTypeFunction argumentType resultType ->
      TypeFunction
        (lowerSurfaceSignatureType argumentType)
        (lowerSurfaceSignatureType resultType)

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

lowerBindingName name
  | isOperatorBindingIdentifierText (identifierText name) =
      operatorBindingNameFromIdentifier name
  | otherwise = sourceName name
