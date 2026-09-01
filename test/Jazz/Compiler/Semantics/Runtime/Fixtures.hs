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
    expressionConstrainedAs,
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

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    CoreNode (..),
    CoreNodeId (..),
    CorePhase (Analyzed),
    CoreSort (..),
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
import Jazz.Compiler.SemanticFacts
  ( ExpressionFacts (..),
    PatternConstructorFact (PatternHasNoConstructor),
    PatternFacts (..),
    PatternRefutability (RefutablePattern),
    RuntimeObligation (ConstrainResult, InstantiateTypes),
    RuntimePlan (RuntimePlan),
    StatementDeclarationFact (ExpressionDeclaration),
    StatementFacts (..),
  )
import qualified Jazz.Compiler.TypeRepresentation as TypeRepresentation

-- These phase-exact builders are for synthetic runtime unit inputs. Parser and
-- lowering contracts use real source units and independently verify node IDs.
expressionNode :: CoreNode 'Analyzed 'ExpressionSort
expressionNode =
  CoreNode
    (CoreNodeId 0)
    (SourceSpan 1 1)
    ( ExpressionFacts
        (TypeRepresentation.SemanticVariable (TypeRepresentation.InferenceVariable 0))
        []
        []
        mempty
    )

patternNode :: CoreNode 'Analyzed 'PatternSort
patternNode =
  CoreNode
    (CoreNodeId 0)
    (SourceSpan 1 1)
    (PatternFacts Map.empty PatternHasNoConstructor RefutablePattern)

statementNode :: SourceSpan -> CoreNode 'Analyzed 'StatementSort
statementNode spanValue@(SourceSpan line column) =
  CoreNode
    (CoreNodeId (line * 1000 + column))
    spanValue
    (StatementFacts [] Map.empty ExpressionDeclaration)
statementNode spanValue@(SourceSpanIn _ line column) =
  CoreNode
    (CoreNodeId (line * 1000 + column))
    spanValue
    (StatementFacts [] Map.empty ExpressionDeclaration)

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

expressionLiteral :: Literal -> Expr 'Analyzed
expressionLiteral = ELit expressionNode

expressionVariable :: UnresolvedName -> Expr 'Analyzed
expressionVariable = EVar expressionNode . valueName

expressionQualifiedMethod :: UnresolvedName -> UnresolvedName -> Expr 'Analyzed
expressionQualifiedMethod capability method =
  EVar expressionNode (qualifiedMemberName (capabilityName capability) (valueName method))

expressionConstructor :: UnresolvedName -> Expr 'Analyzed
expressionConstructor = EVar expressionNode . constructorName

expressionConstrainedAs :: SignatureType 'Analyzed -> Expr 'Analyzed -> Expr 'Analyzed
expressionConstrainedAs resultType expression =
  mapExpressionNode
    ( \node ->
        node
          { coreNodeFacts =
              (coreNodeFacts node)
                { expressionRuntimePlan =
                    RuntimePlan (Seq.singleton (ConstrainResult (fixtureSemanticType resultType)))
                }
          }
    )
    expression

mapExpressionNode :: (CoreNode 'Analyzed 'ExpressionSort -> CoreNode 'Analyzed 'ExpressionSort) -> Expr 'Analyzed -> Expr 'Analyzed
mapExpressionNode update expression =
  case expression of
    ELit node literal -> ELit (update node) literal
    EVar node name -> EVar (update node) name
    ELambda node name body -> ELambda (update node) name body
    EOperatorValue node operator -> EOperatorValue (update node) operator
    EList node elements -> EList (update node) elements
    ETuple node elements -> ETuple (update node) elements
    EApply node function argument -> EApply (update node) function argument
    ETypeApplication node function spanValue argumentType -> ETypeApplication (update node) function spanValue argumentType
    EIf node condition whenTrue whenFalse -> EIf (update node) condition whenTrue whenFalse
    EPatternCase node scrutinee arms -> EPatternCase (update node) scrutinee arms
    EBinary node operator left right -> EBinary (update node) operator left right
    ESectionLeft node left operator -> ESectionLeft (update node) left operator
    ESectionRight node operator right -> ESectionRight (update node) operator right
    EBlock node statements -> EBlock (update node) statements

expressionLambda :: UnresolvedName -> Expr 'Analyzed -> Expr 'Analyzed
expressionLambda name = ELambda expressionNode (valueName name)

expressionOperatorValue :: Text -> Expr 'Analyzed
expressionOperatorValue = EOperatorValue expressionNode

expressionList :: [Expr 'Analyzed] -> Expr 'Analyzed
expressionList = EList expressionNode

expressionTuple :: [Expr 'Analyzed] -> Expr 'Analyzed
expressionTuple = ETuple expressionNode

expressionApply :: Expr 'Analyzed -> Expr 'Analyzed -> Expr 'Analyzed
expressionApply = EApply expressionNode

expressionTypeApplication :: Expr 'Analyzed -> SourceSpan -> SignatureType 'Analyzed -> Expr 'Analyzed
expressionTypeApplication function argumentSpan argumentType =
  ETypeApplication
    ( expressionNode
        { coreNodeFacts =
            (coreNodeFacts expressionNode)
              { expressionRuntimePlan =
                  RuntimePlan
                    (Seq.singleton (InstantiateTypes (fixtureSemanticType argumentType NonEmpty.:| [])))
              }
        }
    )
    function
    argumentSpan
    argumentType

fixtureSemanticType :: SignatureType 'Analyzed -> TypeRepresentation.SemanticType ResolvedName TypeRepresentation.InferenceVariable
fixtureSemanticType signatureType =
  case signatureType of
    TypeRepresentation.TypeInt -> TypeRepresentation.SemanticInt
    TypeRepresentation.TypeFloat -> TypeRepresentation.SemanticFloat
    TypeRepresentation.TypeNumeric numericType -> TypeRepresentation.SemanticNumeric numericType
    TypeRepresentation.TypeBool -> TypeRepresentation.SemanticBool
    TypeRepresentation.TypeChar -> TypeRepresentation.SemanticChar
    TypeRepresentation.TypeText -> TypeRepresentation.SemanticText
    TypeRepresentation.TypeVariable _ -> TypeRepresentation.SemanticVariable (TypeRepresentation.InferenceVariable 0)
    TypeRepresentation.TypeName name -> TypeRepresentation.SemanticData name []
    TypeRepresentation.TypeApplication name arguments ->
      TypeRepresentation.SemanticData name (map fixtureSemanticType arguments)
    TypeRepresentation.TypeList elementType -> TypeRepresentation.SemanticList (fixtureSemanticType elementType)
    TypeRepresentation.TypeTuple elementTypes -> TypeRepresentation.SemanticTuple (map fixtureSemanticType elementTypes)
    TypeRepresentation.TypeFunction argumentType resultType ->
      TypeRepresentation.SemanticFunction
        (fixtureSemanticType argumentType)
        (fixtureSemanticType resultType)

expressionIf :: Expr 'Analyzed -> Expr 'Analyzed -> Expr 'Analyzed -> Expr 'Analyzed
expressionIf = EIf expressionNode

expressionPatternCase :: Expr 'Analyzed -> [CaseArm 'Analyzed] -> Expr 'Analyzed
expressionPatternCase = EPatternCase expressionNode

expressionBinary :: Text -> Expr 'Analyzed -> Expr 'Analyzed -> Expr 'Analyzed
expressionBinary = EBinary expressionNode

expressionSectionLeft :: Expr 'Analyzed -> Text -> Expr 'Analyzed
expressionSectionLeft = ESectionLeft expressionNode

expressionSectionRight :: Text -> Expr 'Analyzed -> Expr 'Analyzed
expressionSectionRight = ESectionRight expressionNode

expressionBlock :: [Statement 'Analyzed] -> Expr 'Analyzed
expressionBlock = EBlock expressionNode

patternWildcard :: Pattern 'Analyzed
patternWildcard = PWildcard patternNode

patternVariable :: UnresolvedName -> Pattern 'Analyzed
patternVariable = PVariable patternNode . valueName

patternLiteral :: Literal -> Pattern 'Analyzed
patternLiteral = PLiteral patternNode

patternTuple :: [Pattern 'Analyzed] -> Pattern 'Analyzed
patternTuple = PTuple patternNode

patternConstructor :: UnresolvedName -> [Pattern 'Analyzed] -> Pattern 'Analyzed
patternConstructor name = PConstructor patternNode (constructorName name)

caseArm :: Pattern 'Analyzed -> Maybe (Expr 'Analyzed) -> Expr 'Analyzed -> CaseArm 'Analyzed
caseArm = CaseArm expressionNode

dataConstructor :: UnresolvedName -> [SignatureType 'Analyzed] -> DataConstructor 'Analyzed
dataConstructor name = DataConstructor (statementNode (SourceSpan 1 1)) (constructorName name)

classMethodSignature :: UnresolvedName -> SourceSpan -> SignaturePayload 'Analyzed -> ClassMethodSignature 'Analyzed
classMethodSignature name spanValue = ClassMethodSignature (statementNode spanValue) (valueName name)

implMethod :: UnresolvedName -> SourceSpan -> Expr 'Analyzed -> ImplMethod 'Analyzed
implMethod name spanValue = ImplMethod (statementNode spanValue) (valueName name)

statementLet :: UnresolvedName -> SourceSpan -> Expr 'Analyzed -> Statement 'Analyzed
statementLet name spanValue = SLet (statementNode spanValue) (valueName name)

statementSignature :: UnresolvedName -> SourceSpan -> SignaturePayload 'Analyzed -> Statement 'Analyzed
statementSignature name spanValue = SSignature (statementNode spanValue) (valueName name)

statementData :: SourceSpan -> UnresolvedName -> [UnresolvedName] -> [DataConstructor 'Analyzed] -> Statement 'Analyzed
statementData spanValue name parameters = SData (statementNode spanValue) (typeName name) (map typeName parameters)

statementClass :: SourceSpan -> UnresolvedName -> [UnresolvedName] -> [ClassMethodSignature 'Analyzed] -> Statement 'Analyzed
statementClass spanValue name parameters = SClass (statementNode spanValue) (capabilityName name) (map typeName parameters)

statementImpl :: SourceSpan -> UnresolvedName -> [SignatureType 'Analyzed] -> [ImplMethod 'Analyzed] -> Statement 'Analyzed
statementImpl spanValue name = SImpl (statementNode spanValue) (capabilityName name)

statementExpression :: SourceSpan -> Expr 'Analyzed -> Statement 'Analyzed
statementExpression spanValue = SExpr (statementNode spanValue)

fixtureTypeVariable :: UnresolvedName -> SignatureType 'Analyzed
fixtureTypeVariable = TypeRepresentation.TypeVariable . typeName

fixtureAmbientTypeVariable :: UnresolvedName -> SignatureType 'Analyzed
fixtureAmbientTypeVariable =
  TypeRepresentation.TypeVariable
    . resolvedAmbientName TypeNamespace
    . mkIdentifier
    . identifierText

fixtureTypeName :: UnresolvedName -> SignatureType 'Analyzed
fixtureTypeName = TypeRepresentation.TypeName . typeName

fixtureValueName :: UnresolvedName -> ResolvedName
fixtureValueName = valueName

fixtureResolvedTypeName :: UnresolvedName -> ResolvedName
fixtureResolvedTypeName = typeName
