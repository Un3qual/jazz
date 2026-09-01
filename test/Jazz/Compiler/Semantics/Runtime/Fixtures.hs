{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Semantics.Runtime.Fixtures
  ( caseArm,
    classMethodSignature,
    dataConstructor,
    expressionApply,
    expressionBinary,
    expressionBlock,
    expressionConstructor,
    expressionIf,
    expressionLambda,
    expressionList,
    expressionLiteral,
    expressionOperatorValue,
    expressionPatternCase,
    expressionQualifiedMethod,
    expressionSectionLeft,
    expressionSectionRight,
    expressionTuple,
    expressionTypeApplication,
    expressionVariable,
    fixtureAmbientTypeVariable,
    fixtureResolvedTypeName,
    fixtureTypeName,
    fixtureTypeVariable,
    fixtureValueName,
    implMethod,
    patternConstructor,
    patternLiteral,
    patternTuple,
    patternVariable,
    patternWildcard,
    statementClass,
    statementData,
    statementExpression,
    statementImpl,
    statementLet,
    statementSignature,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    CoreNode (..),
    CoreNodeId (..),
    CorePhase (Resolved),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal,
    Pattern (..),
    SignaturePayload,
    SignatureType,
    Statement (..),
  )
import Jazz.Compiler.Diagnostics (SourceSpan (..))
import Jazz.Compiler.Name
  ( Name (BuiltinName),
    NameNamespace (CapabilityNamespace, ConstructorNamespace, TypeNamespace, ValueNamespace),
    ResolvedName,
    ResolvedNameOrigin (CurrentModule),
    ResolvedUserName (ResolvedUserName),
    UnresolvedName,
    UserNameLike (renderUserName),
    identifierText,
    mkIdentifier,
    qualifiedMemberName,
    resolvedAmbientName,
  )
import qualified Jazz.Compiler.TypeRepresentation as TypeRepresentation

-- These phase-exact builders are for synthetic runtime unit inputs. Parser and
-- lowering contracts use real source units and independently verify node IDs.
expressionNode :: CoreNode 'Resolved sort
expressionNode = CoreNode (CoreNodeId 0) (SourceSpan 1 1) ()

statementNode :: SourceSpan -> CoreNode 'Resolved sort
statementNode spanValue = CoreNode (CoreNodeId 0) spanValue ()

resolvedName :: NameNamespace -> UnresolvedName -> ResolvedName
resolvedName namespace =
  fmap
    (\source -> ResolvedUserName CurrentModule namespace (mkIdentifier (renderUserName source)))

valueName :: UnresolvedName -> ResolvedName
valueName source
  | "__kernel_" `Text.isPrefixOf` rendered = BuiltinName (mkIdentifier rendered)
  | otherwise = resolvedName ValueNamespace source
  where
    rendered = identifierText source

typeName :: UnresolvedName -> ResolvedName
typeName = resolvedName TypeNamespace

capabilityName :: UnresolvedName -> ResolvedName
capabilityName = resolvedName CapabilityNamespace

constructorName :: UnresolvedName -> ResolvedName
constructorName = resolvedName ConstructorNamespace

expressionLiteral :: Literal -> Expr 'Resolved
expressionLiteral = ELit expressionNode

expressionVariable :: UnresolvedName -> Expr 'Resolved
expressionVariable = EVar expressionNode . valueName

expressionQualifiedMethod :: UnresolvedName -> UnresolvedName -> Expr 'Resolved
expressionQualifiedMethod capability method =
  EVar expressionNode (qualifiedMemberName (capabilityName capability) (valueName method))

expressionConstructor :: UnresolvedName -> Expr 'Resolved
expressionConstructor = EVar expressionNode . constructorName

expressionLambda :: UnresolvedName -> Expr 'Resolved -> Expr 'Resolved
expressionLambda name = ELambda expressionNode (valueName name)

expressionOperatorValue :: Text -> Expr 'Resolved
expressionOperatorValue = EOperatorValue expressionNode

expressionList :: [Expr 'Resolved] -> Expr 'Resolved
expressionList = EList expressionNode

expressionTuple :: [Expr 'Resolved] -> Expr 'Resolved
expressionTuple = ETuple expressionNode

expressionApply :: Expr 'Resolved -> Expr 'Resolved -> Expr 'Resolved
expressionApply = EApply expressionNode

expressionTypeApplication :: Expr 'Resolved -> SourceSpan -> SignatureType 'Resolved -> Expr 'Resolved
expressionTypeApplication = ETypeApplication expressionNode

expressionIf :: Expr 'Resolved -> Expr 'Resolved -> Expr 'Resolved -> Expr 'Resolved
expressionIf = EIf expressionNode

expressionPatternCase :: Expr 'Resolved -> [CaseArm 'Resolved] -> Expr 'Resolved
expressionPatternCase = EPatternCase expressionNode

expressionBinary :: Text -> Expr 'Resolved -> Expr 'Resolved -> Expr 'Resolved
expressionBinary = EBinary expressionNode

expressionSectionLeft :: Expr 'Resolved -> Text -> Expr 'Resolved
expressionSectionLeft = ESectionLeft expressionNode

expressionSectionRight :: Text -> Expr 'Resolved -> Expr 'Resolved
expressionSectionRight = ESectionRight expressionNode

expressionBlock :: [Statement 'Resolved] -> Expr 'Resolved
expressionBlock = EBlock expressionNode

patternWildcard :: Pattern 'Resolved
patternWildcard = PWildcard expressionNode

patternVariable :: UnresolvedName -> Pattern 'Resolved
patternVariable = PVariable expressionNode . valueName

patternLiteral :: Literal -> Pattern 'Resolved
patternLiteral = PLiteral expressionNode

patternTuple :: [Pattern 'Resolved] -> Pattern 'Resolved
patternTuple = PTuple expressionNode

patternConstructor :: UnresolvedName -> [Pattern 'Resolved] -> Pattern 'Resolved
patternConstructor name = PConstructor expressionNode (constructorName name)

caseArm :: Pattern 'Resolved -> Maybe (Expr 'Resolved) -> Expr 'Resolved -> CaseArm 'Resolved
caseArm = CaseArm expressionNode

dataConstructor :: UnresolvedName -> [SignatureType 'Resolved] -> DataConstructor 'Resolved
dataConstructor name = DataConstructor expressionNode (constructorName name)

classMethodSignature :: UnresolvedName -> SourceSpan -> SignaturePayload 'Resolved -> ClassMethodSignature 'Resolved
classMethodSignature name spanValue = ClassMethodSignature (statementNode spanValue) (valueName name)

implMethod :: UnresolvedName -> SourceSpan -> Expr 'Resolved -> ImplMethod 'Resolved
implMethod name spanValue = ImplMethod (statementNode spanValue) (valueName name)

statementLet :: UnresolvedName -> SourceSpan -> Expr 'Resolved -> Statement 'Resolved
statementLet name spanValue = SLet (statementNode spanValue) (valueName name)

statementSignature :: UnresolvedName -> SourceSpan -> SignaturePayload 'Resolved -> Statement 'Resolved
statementSignature name spanValue = SSignature (statementNode spanValue) (valueName name)

statementData :: SourceSpan -> UnresolvedName -> [UnresolvedName] -> [DataConstructor 'Resolved] -> Statement 'Resolved
statementData spanValue name parameters = SData (statementNode spanValue) (typeName name) (map typeName parameters)

statementClass :: SourceSpan -> UnresolvedName -> [UnresolvedName] -> [ClassMethodSignature 'Resolved] -> Statement 'Resolved
statementClass spanValue name parameters = SClass (statementNode spanValue) (capabilityName name) (map typeName parameters)

statementImpl :: SourceSpan -> UnresolvedName -> [SignatureType 'Resolved] -> [ImplMethod 'Resolved] -> Statement 'Resolved
statementImpl spanValue name = SImpl (statementNode spanValue) (capabilityName name)

statementExpression :: SourceSpan -> Expr 'Resolved -> Statement 'Resolved
statementExpression spanValue = SExpr (statementNode spanValue)

fixtureTypeVariable :: UnresolvedName -> SignatureType 'Resolved
fixtureTypeVariable = TypeRepresentation.TypeVariable . typeName

fixtureAmbientTypeVariable :: UnresolvedName -> SignatureType 'Resolved
fixtureAmbientTypeVariable =
  TypeRepresentation.TypeVariable
    . resolvedAmbientName TypeNamespace
    . mkIdentifier
    . identifierText

fixtureTypeName :: UnresolvedName -> SignatureType 'Resolved
fixtureTypeName = TypeRepresentation.TypeName . typeName

fixtureValueName :: UnresolvedName -> ResolvedName
fixtureValueName = valueName

fixtureResolvedTypeName :: UnresolvedName -> ResolvedName
fixtureResolvedTypeName = typeName
