-- | Lowers parser-surface nodes into the smaller core AST consumed by later
-- compiler phases.
module JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  ) where

import JazzNext.Compiler.AST
  ( CaseArm (..),
    ConstraintSignatureType (..),
    DataConstructor (..),
    Expr (..),
    Literal (..),
    Pattern (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceConstrainedSignatureType (..),
    SurfaceDataConstructor (..),
    SurfaceExpr (..),
    SurfaceLiteral (..),
    SurfacePattern (..),
    SurfaceSignatureConstraint (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureToken (..),
    SurfaceSignatureType (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Identifier
  ( Identifier,
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
    SEApply functionExpr argumentExpr ->
      EApply (lowerSurfaceExpr functionExpr) (lowerSurfaceExpr argumentExpr)
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

lowerSurfaceLambda :: [Identifier] -> SurfaceExpr -> Expr
lowerSurfaceLambda parameters bodyExpr =
  case parameters of
    [] ->
      error
        ( "internal lowering error: empty lambda parameter list in lowerSurfaceLambda for body "
            ++ show bodyExpr
        )
    _ ->
      foldr ELambda (lowerSurfaceExpr bodyExpr) parameters

lowerSurfaceLiteral :: SurfaceLiteral -> Literal
lowerSurfaceLiteral literal =
  case literal of
    SLInt value -> LInt value
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

lowerSurfaceCaseArm :: SurfaceCaseArm -> CaseArm
lowerSurfaceCaseArm (SurfaceCaseArm patternExpr bodyExpr) =
  CaseArm
    (lowerSurfacePattern patternExpr)
    (lowerSurfaceExpr bodyExpr)

-- | Lower a parsed statement without changing its span-carrying shape.
lowerSurfaceStatement :: SurfaceStatement -> Statement
lowerSurfaceStatement surfaceStatement =
  case surfaceStatement of
    SSLet name spanValue valueExpr ->
      SLet name spanValue (lowerSurfaceExpr valueExpr)
    SSSignature name spanValue signaturePayload ->
      SSignature name spanValue (lowerSurfaceSignaturePayload signaturePayload)
    SSData spanValue typeName constructors ->
      SData spanValue typeName (map lowerSurfaceDataConstructor constructors)
    SSModule spanValue modulePath ->
      SModule spanValue modulePath
    SSImport spanValue modulePath alias importedSymbols ->
      SImport spanValue modulePath alias importedSymbols
    SSExpr spanValue expr ->
      SExpr spanValue (lowerSurfaceExpr expr)

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
    SurfaceConstrainedTypeFunction argumentType resultType ->
      ConstraintTypeFunction
        (lowerSurfaceConstrainedSignatureType argumentType)
        (lowerSurfaceConstrainedSignatureType resultType)

lowerSurfaceSignatureType :: SurfaceSignatureType -> SignatureType
lowerSurfaceSignatureType surfaceSignatureType =
  case surfaceSignatureType of
    SurfaceTypeInt -> TypeInt
    SurfaceTypeBool -> TypeBool
    SurfaceTypeList innerType ->
      TypeList (lowerSurfaceSignatureType innerType)
    SurfaceTypeFunction argumentType resultType ->
      TypeFunction
        (lowerSurfaceSignatureType argumentType)
        (lowerSurfaceSignatureType resultType)

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
lowerSurfaceDataConstructor (SurfaceDataConstructor constructorName constructorArity) =
  DataConstructor constructorName constructorArity
