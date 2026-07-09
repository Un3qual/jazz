-- | Lowers parser-surface nodes into the smaller core AST consumed by later
-- compiler phases.
module JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  ) where

import qualified Data.Text as Text
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
import JazzNext.Compiler.Identifier
  ( mkIdentifier,
    mkQualifiedIdentifier,
    identifierText
  )

-- | Convert parser-surface nodes into core nodes while preserving statement
-- source spans. Expression constructors like `ELit`, `EVar`, `EApply`, and
-- `EBinary` do not carry spans in the core AST, so expression-level location
-- handling stays in later phases.
lowerSurfaceExpr :: SurfaceExpr -> Expr
lowerSurfaceExpr surfaceExpr =
  case surfaceExpr of
    SELit literal -> ELit (lowerSurfaceLiteral literal)
    SEVar name -> EVar name
    SEQualifiedVar qualifier member ->
      EVar (mkQualifiedIdentifier (identifierText qualifier) (identifierText member))
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

lowerSurfaceLambda :: [SurfaceLambdaParameter] -> SurfaceExpr -> Expr
lowerSurfaceLambda parameters bodyExpr =
  case parameters of
    [] ->
      error
        ( "internal lowering error: empty lambda parameter list in lowerSurfaceLambda for body "
            ++ show bodyExpr
        )
    _ ->
      foldr lowerParameter (lowerSurfaceExpr bodyExpr) (zip [1 :: Int ..] parameters)
  where
    lowerParameter (_, SurfaceLambdaIdentifier parameterName) loweredBody =
      ELambda parameterName loweredBody
    lowerParameter (parameterIndex, SurfaceLambdaPattern parameterPattern) loweredBody =
      let generatedName =
            mkIdentifier
              (Text.pack "$lambda_pattern_arg_" <> Text.pack (show parameterIndex))
       in ELambda
            generatedName
            ( EPatternCase
                (EVar generatedName)
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
    SPVariable name -> PVariable name
    SPLiteral literal -> PLiteral (lowerSurfaceLiteral literal)
    SPConstructor name patterns ->
      PConstructor name (map lowerSurfacePattern patterns)
    SPList patterns ->
      PList (map lowerSurfacePattern patterns)
    SPConsList headPattern tailPattern ->
      PConsList (lowerSurfacePattern headPattern) (lowerSurfacePattern tailPattern)
    SPTuple patterns ->
      PTuple (map lowerSurfacePattern patterns)
    SPAs name pattern ->
      PAs name (lowerSurfacePattern pattern)
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
      SLet name spanValue (lowerSurfaceExpr valueExpr)
    SSSignature name spanValue signaturePayload ->
      SSignature name spanValue (lowerSurfaceSignaturePayload signaturePayload)
    SSData spanValue typeName typeParameters constructors ->
      SData spanValue typeName typeParameters (map lowerSurfaceDataConstructor constructors)
    SSClass spanValue capabilityName parameters methods ->
      SClass spanValue capabilityName parameters (map lowerSurfaceClassMethodSignature methods)
    SSImpl spanValue capabilityName arguments methods ->
      SImpl
        spanValue
        capabilityName
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
  ClassMethodSignature methodName spanValue (lowerSurfaceSignaturePayload signaturePayload)

lowerSurfaceImplMethod :: SurfaceImplMethod -> ImplMethod
lowerSurfaceImplMethod (SurfaceImplMethod methodName spanValue methodExpr) =
  ImplMethod methodName spanValue (lowerSurfaceExpr methodExpr)

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
    constraintName
    (map lowerSurfaceConstrainedSignatureType constraintArguments)

lowerSurfaceConstrainedSignatureType :: SurfaceConstrainedSignatureType -> ConstraintSignatureType
lowerSurfaceConstrainedSignatureType signatureType =
  case signatureType of
    SurfaceConstrainedTypeName name ->
      ConstraintTypeName name
    SurfaceConstrainedTypeApplication name arguments ->
      ConstraintTypeApplication name (map lowerSurfaceConstrainedSignatureType arguments)
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
    SurfaceSignatureNameToken name -> SignatureNameToken name
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
  DataConstructor constructorName (map lowerSurfaceDataConstructorArgument constructorArguments)

lowerSurfaceDataConstructorArgument :: SurfaceDataConstructorArgument -> DataConstructorArgument
lowerSurfaceDataConstructorArgument surfaceArgument =
  case surfaceArgument of
    SurfaceDataConstructorArgumentName argumentName ->
      DataConstructorArgumentName argumentName
    SurfaceDataConstructorArgumentOpaque ->
      DataConstructorArgumentOpaque
