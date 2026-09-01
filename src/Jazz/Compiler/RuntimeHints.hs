{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.RuntimeHints
  ( BindingRuntimeHintKey (..),
    bindingRuntimeHintKey,
    bindingRuntimeHintKeyInModule,
    explicitTypeApplicationRuntimeHintKeyInModule,
    projectRuntimeHints,
    projectSourceUnitRuntimeHints,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Jazz.Compiler.AST
  ( CaseArm (..),
    CoreNode (..),
    CorePhase (Analyzed, Resolved),
    CoreSort (ExpressionSort, StatementSort),
    Expr (..),
    ImplMethod (..),
    SignatureType,
    Statement (..),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan,
  )
import Jazz.Compiler.ModuleGraph
  ( CoreModule,
    CoreProgram,
    PreludeArtifact (preludeModule),
    coreModuleExpr,
    coreModulePath,
    coreProgramModules,
    coreProgramPrelude,
  )
import Jazz.Compiler.ModuleIdentity (ModulePath, modulePathTextSegments, standaloneModulePath)
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (ConstructorNamespace, TypeNamespace),
    ResolvedName,
    ResolvedUserName (..),
    mkIdentifier,
    resolvedAmbientName,
  )
import Jazz.Compiler.SemanticFacts
  ( AnalyzedScheme (..),
    ExpressionFacts (..),
    RuntimeObligation (ConstrainResult),
    RuntimePlan (..),
    StatementFacts (..),
  )
import Jazz.Compiler.SourceUnitOwnership
  ( sourceUnitOwnerRuntimePath,
    sourceUnitStatementOwners,
  )
import qualified Jazz.Compiler.TypeInference.Signature as Signature
import Jazz.Compiler.TypeRepresentation
  ( InferenceVariable,
    SemanticType (SemanticFunction),
  )

data BindingRuntimeHintKey
  = BindingRuntimeHintKey (Maybe [Text]) SourceSpan ResolvedName
  | ExplicitTypeApplicationRuntimeHintKey (Maybe [Text]) SourceSpan
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

bindingRuntimeHintKey :: ResolvedName -> SourceSpan -> BindingRuntimeHintKey
bindingRuntimeHintKey bindingName bindingSpan =
  bindingRuntimeHintKeyInModule Nothing bindingName bindingSpan

bindingRuntimeHintKeyInModule :: Maybe [Text] -> ResolvedName -> SourceSpan -> BindingRuntimeHintKey
bindingRuntimeHintKeyInModule modulePath bindingName bindingSpan =
  BindingRuntimeHintKey modulePath bindingSpan bindingName

explicitTypeApplicationRuntimeHintKeyInModule :: Maybe [Text] -> SourceSpan -> BindingRuntimeHintKey
explicitTypeApplicationRuntimeHintKeyInModule =
  ExplicitTypeApplicationRuntimeHintKey

projectRuntimeHints :: CoreProgram 'Analyzed -> Map BindingRuntimeHintKey (SignatureType 'Resolved)
projectRuntimeHints program =
  foldMap projectModule (maybePreludeModule : map Just (NonEmpty.toList (coreProgramModules program)))
  where
    maybePreludeModule = preludeModule (coreProgramPrelude program)
    projectModule Nothing = Map.empty
    projectModule (Just coreModule) = projectCoreModule coreModule

projectCoreModule :: CoreModule 'Analyzed -> Map BindingRuntimeHintKey (SignatureType 'Resolved)
projectCoreModule coreModule =
  projectAnalyzedExpr
    (Just (NonEmpty.toList (modulePathTextSegments (coreModulePath coreModule))))
    (coreModuleExpr coreModule)

-- | Temporary standalone-source bridge for the still-resolved interpreter.
-- The source-unit index is traversal context, never a semantic lookup key:
-- nested nodes inherit the path selected by their enclosing top-level statement.
projectSourceUnitRuntimeHints ::
  ModulePath ->
  Set Int ->
  Expr 'Analyzed ->
  Map BindingRuntimeHintKey (SignatureType 'Resolved)
projectSourceUnitRuntimeHints preludePath preludeStatementIndices expression =
  case expression of
    EBlock _ statements ->
      foldMap
        ( \(owner, statement) ->
            projectStatement (sourceUnitOwnerRuntimePath owner) statement
        )
        ( zip
            (sourceUnitStatementOwners standaloneModulePath preludePath preludeStatementIndices statements)
            statements
        )
    _ -> projectAnalyzedExpr Nothing expression

projectAnalyzedExpr :: Maybe [Text] -> Expr 'Analyzed -> Map BindingRuntimeHintKey (SignatureType 'Resolved)
projectAnalyzedExpr modulePath expression =
  explicitApplicationHint <> childHints
  where
    explicitApplicationHint =
      case expression of
        ETypeApplication node _ argumentSpan _ ->
          maybe
            Map.empty
            (Map.singleton (explicitTypeApplicationRuntimeHintKeyInModule modulePath argumentSpan))
            (nodeRuntimeHint node)
        _ -> Map.empty
    childHints =
      case expression of
        ELambda _ _ body -> recur body
        EList _ elements -> foldMap recur elements
        ETuple _ elements -> foldMap recur elements
        EApply _ function argument -> recur function <> recur argument
        ETypeApplication _ function _ _ -> recur function
        EIf _ condition thenExpression elseExpression -> foldMap recur [condition, thenExpression, elseExpression]
        EPatternCase _ scrutinee arms -> recur scrutinee <> foldMap projectArm arms
        EBinary _ _ left right -> recur left <> recur right
        ESectionLeft _ left _ -> recur left
        ESectionRight _ _ right -> recur right
        EBlock _ statements -> foldMap (projectStatement modulePath) statements
        _ -> Map.empty
    recur = projectAnalyzedExpr modulePath
    projectArm (CaseArm _ _ guard body) = foldMap recur guard <> recur body

projectStatement :: Maybe [Text] -> Statement 'Analyzed -> Map BindingRuntimeHintKey (SignatureType 'Resolved)
projectStatement modulePath statement =
  case statement of
    SLet node name value ->
      maybe
        Map.empty
        (Map.singleton (bindingRuntimeHintKeyInModule modulePath name (coreNodeSpan node)))
        (bindingRuntimeHint node value)
        <> projectAnalyzedExpr modulePath value
    SImpl _ _ _ methods ->
      foldMap (\(ImplMethod _ _ body) -> projectAnalyzedExpr modulePath body) methods
    SExpr _ value -> projectAnalyzedExpr modulePath value
    _ -> Map.empty

bindingRuntimeHint :: CoreNode 'Analyzed 'StatementSort -> Expr 'Analyzed -> Maybe (SignatureType 'Resolved)
bindingRuntimeHint (CoreNode _ _ facts) value = do
  runtimeType <- expressionRuntimeType value
  Signature.expressionTypeToRuntimeHint runtimeType
    <|> polymorphicFunctionTemplate facts value runtimeType

polymorphicFunctionTemplate :: StatementFacts -> Expr 'Analyzed -> SemanticType ResolvedName InferenceVariable -> Maybe (SignatureType 'Resolved)
polymorphicFunctionTemplate facts value runtimeType =
  case runtimeType of
    SemanticFunction {}
      | not (directConstructorReference value) -> do
          scheme <- statementScheme facts
          Signature.expressionTypeToRuntimeTemplate
            (runtimeTemplateVariables scheme)
            runtimeType
    _ -> Nothing

statementScheme :: StatementFacts -> Maybe AnalyzedScheme
statementScheme facts =
  case statementBinderIds facts of
    binder : _ -> Map.lookup binder (statementGeneralizedSchemes facts)
    [] -> Nothing

runtimeTemplateVariables :: AnalyzedScheme -> Map InferenceVariable ResolvedName
runtimeTemplateVariables scheme =
  Map.fromList
    [ (typeVar, resolvedAmbientName TypeNamespace (mkIdentifier ("t" <> Text.pack (show position))))
    | (position, typeVar) <- zip [0 :: Int ..] (analyzedSchemeVariables scheme)
    ]

directConstructorReference :: Expr 'Analyzed -> Bool
directConstructorReference expression =
  case expression of
    EVar _ (UserName (ResolvedUserName _ ConstructorNamespace _)) -> True
    _ -> False

nodeRuntimeHint :: CoreNode 'Analyzed 'ExpressionSort -> Maybe (SignatureType 'Resolved)
nodeRuntimeHint (CoreNode _ _ facts) =
  expressionRuntimePlanHint (expressionRuntimePlan facts)

expressionRuntimeType :: Expr 'Analyzed -> Maybe (SemanticType ResolvedName InferenceVariable)
expressionRuntimeType expression =
  case expression of
    ELit node _ -> nodeType node
    EVar node _ -> nodeType node
    ELambda node _ _ -> nodeType node
    EOperatorValue node _ -> nodeType node
    EList node _ -> nodeType node
    ETuple node _ -> nodeType node
    EApply node _ _ -> nodeType node
    ETypeApplication node _ _ _ -> nodeType node
    EIf node _ _ _ -> nodeType node
    EPatternCase node _ _ -> nodeType node
    EBinary node _ _ _ -> nodeType node
    ESectionLeft node _ _ -> nodeType node
    ESectionRight node _ _ -> nodeType node
    EBlock node _ -> nodeType node
  where
    nodeType (CoreNode _ _ facts) = runtimePlanResult (expressionRuntimePlan facts)

expressionRuntimePlanHint :: RuntimePlan -> Maybe (SignatureType 'Resolved)
expressionRuntimePlanHint plan =
  runtimePlanResult plan >>= Signature.expressionTypeToRuntimeHint

runtimePlanResult :: RuntimePlan -> Maybe (SemanticType ResolvedName InferenceVariable)
runtimePlanResult (RuntimePlan obligations) =
  case Seq.viewr obligations of
    _ Seq.:> ConstrainResult runtimeType -> Just runtimeType
    _ -> Nothing
