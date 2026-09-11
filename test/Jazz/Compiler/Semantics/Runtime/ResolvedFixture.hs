{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Synthetic runtime cases retain hand-authored type/representation facts, but
-- use production lexical resolution. Resolve once after composing a fixture.
module Jazz.Compiler.Semantics.Runtime.ResolvedFixture
  ( observeFixture,
    evaluateFixture,
    evaluateFixtureWithHost,
    resolveRuntimeFixture,
    resolveRuntimeFixtureWith,
    fixtureDeclarations,
  )
where

import Control.Monad.Trans.State.Strict (runState, state)
import Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Jazz.Compiler.AST
import Jazz.Compiler.BuiltinCatalog (lookupKernelBuiltinSymbol)
import Jazz.Compiler.CoreIdentity
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.ModuleIdentity (SourceUnitOwner (..), standaloneModulePath)
import Jazz.Compiler.Name
import Jazz.Compiler.Parser.Operator (isBuiltinOperatorSymbol)
import Jazz.Compiler.RecursiveBindings (publishResolvedCaptures, resolveLexicalScopes)
import Jazz.Compiler.Runtime (RuntimeValue)
import qualified Jazz.Compiler.Runtime as Runtime
import Jazz.Compiler.Runtime.Observation (RuntimeObservationRequest, RuntimeObservationResult)
import Jazz.Compiler.RuntimeHost (RuntimeHost)
import Jazz.Compiler.SemanticFacts
import Jazz.Compiler.SourceUnitOwnership (sourceUnitOwnerOrigin)

observeFixture :: RuntimeObservationRequest -> Expr 'Analyzed -> RuntimeObservationResult (Maybe RuntimeValue)
observeFixture request = Runtime.evaluateRuntimeExprObserved request . resolveRuntimeFixture

evaluateFixture :: Expr 'Analyzed -> Either Diagnostic (Maybe RuntimeValue)
evaluateFixture = Runtime.evaluateRuntimeExpr . resolveRuntimeFixture

evaluateFixtureWithHost :: (Monad m) => RuntimeHost m -> Expr 'Analyzed -> m (Either Diagnostic (Maybe RuntimeValue))
evaluateFixtureWithHost host = Runtime.evaluateRuntimeExprWithHost host . resolveRuntimeFixture

resolveRuntimeFixture :: Expr 'Analyzed -> Expr 'Analyzed
resolveRuntimeFixture = resolveRuntimeFixtureWith (StandaloneSourceUnit standaloneModulePath) Map.empty

resolveRuntimeFixtureWith :: SourceUnitOwner -> Map.Map ResolvedName ResolvedReference -> Expr 'Analyzed -> Expr 'Analyzed
resolveRuntimeFixtureWith _ _ fixture | coreNodeId (expressionNode fixture) /= CoreNodeId (-1) = fixture
resolveRuntimeFixtureWith owner external fixture =
  runIdentity (traverseFixture restoreExpression restorePattern restoreStatement resolved)
  where
    (bare, (_, expressions, patterns, statements)) = runState (traverseFixture allocateExpression allocatePattern allocateStatement fixture) (0, Map.empty, Map.empty, Map.empty)
    resolved = publishResolvedCaptures (resolveLexicalScopes external (Map.keysSet external) bare)
    nodeFacts index binder target =
      (emptyResolvedNodeFacts owner)
        { resolvedNodeBinder = if binder then Just (CoreBinderId (owner, index)) else Nothing,
          resolvedNodeReference = reference <$> target
        }
    reference name = case Map.lookup name external of
      Just target -> target
      Nothing -> case name of
        BuiltinName identifier -> BuiltinReference identifier
        GeneratedName (OperatorBinding symbol) | isBuiltinOperatorSymbol symbol -> BuiltinOperatorReference symbol
        UserName (ResolvedUserName origin _ identifier)
          | Just _ <- lookupKernelBuiltinSymbol (identifierText identifier) -> BuiltinReference identifier
          | [capability, method] <- Text.splitOn "::" (identifierText identifier) ->
              CapabilityMethodReference (CapabilityId (UserName (ResolvedUserName (if origin == CurrentModule then sourceUnitOwnerOrigin owner else origin) CapabilityNamespace (mkIdentifier capability)))) (mkIdentifier method)
        _ -> UnresolvedReference name
    allocateExpression binder target (CoreNode _ spanValue facts) = state $ \(next, es, ps, ss) ->
      let index = CoreNodeId next
          resolution = nodeFacts index binder target
          operatorResolution = case resolvedNodeReference (expressionResolution facts) of
            Just operatorReference@BuiltinOperatorReference {} -> resolution {resolvedNodeReference = Just operatorReference}
            _ -> resolution
       in (CoreNode index spanValue operatorResolution, (next + 1, Map.insert index facts es, ps, ss))
    allocatePattern binder target (CoreNode _ spanValue facts) = state $ \(next, es, ps, ss) ->
      let index = CoreNodeId next
       in (CoreNode index spanValue (nodeFacts index binder target), (next + 1, es, Map.insert index facts ps, ss))
    allocateStatement binder target (CoreNode _ spanValue facts) = state $ \(next, es, ps, ss) ->
      let index = CoreNodeId next
       in (CoreNode index spanValue (nodeFacts index binder target), (next + 1, es, ps, Map.insert index facts ss))
    restoreExpression _ _ (CoreNode index spanValue facts) = pure (CoreNode index spanValue ((expressions Map.! index) {expressionResolution = facts}))
    restorePattern _ _ (CoreNode index spanValue facts) = pure (CoreNode index spanValue ((patterns Map.! index) {patternResolution = facts}))
    restoreStatement _ _ (CoreNode index spanValue facts) = pure (CoreNode index spanValue ((statements Map.! index) {statementResolution = facts}))

fixtureDeclarations :: Expr 'Analyzed -> Map.Map ResolvedName ResolvedReference
fixtureDeclarations (EBlock _ statements) = Map.fromList [(name, resolvedBinderReference (statementResolution (coreNodeFacts node))) | SLet node name _ <- statements]
fixtureDeclarations _ = Map.empty

-- This test-only traversal changes node facts and identities, preserving the
-- exact semantic payloads needed to exercise runtime checking independently.
traverseFixture ::
  (Applicative f, CoreUserNameAt from ~ ResolvedUserName, CoreUserNameAt to ~ ResolvedUserName) =>
  (Bool -> Maybe ResolvedName -> CoreNode from 'ExpressionSort -> f (CoreNode to 'ExpressionSort)) ->
  (Bool -> Maybe ResolvedName -> CoreNode from 'PatternSort -> f (CoreNode to 'PatternSort)) ->
  (Bool -> Maybe ResolvedName -> CoreNode from 'StatementSort -> f (CoreNode to 'StatementSort)) ->
  Expr from ->
  f (Expr to)
traverseFixture expressionNodeVisit patternNodeVisit statementNodeVisit = expression
  where
    node = expressionNodeVisit False Nothing
    binding = expressionNodeVisit True Nothing
    reference name = expressionNodeVisit False (Just name)
    plainStatement = statementNodeVisit False Nothing
    bindingStatement = statementNodeVisit True Nothing
    expression value = case value of
      ELit n literal -> ELit <$> node n <*> pure literal
      EVar n name -> EVar <$> reference name n <*> pure name
      ELambda n name body -> ELambda <$> binding n <*> pure name <*> expression body
      EOperatorValue n symbol -> EOperatorValue <$> reference (operatorBindingName symbol) n <*> pure symbol
      EList n items -> EList <$> node n <*> traverse expression items
      ETuple n items -> ETuple <$> node n <*> traverse expression items
      EApply n function argument -> EApply <$> node n <*> expression function <*> expression argument
      ETypeApplication n function spanValue signature -> ETypeApplication <$> node n <*> expression function <*> pure spanValue <*> pure signature
      EIf n condition yes no -> EIf <$> node n <*> expression condition <*> expression yes <*> expression no
      EPatternCase n scrutinee arms -> EPatternCase <$> node n <*> expression scrutinee <*> traverse arm arms
      EBinary n symbol left right -> EBinary <$> reference (operatorBindingName symbol) n <*> pure symbol <*> expression left <*> expression right
      ESectionLeft n left symbol -> ESectionLeft <$> reference (operatorBindingName symbol) n <*> expression left <*> pure symbol
      ESectionRight n symbol right -> ESectionRight <$> reference (operatorBindingName symbol) n <*> pure symbol <*> expression right
      EBlock n statements -> EBlock <$> node n <*> traverse statement statements
    arm (CaseArm n p guard body) = CaseArm <$> node n <*> pattern p <*> traverse expression guard <*> expression body
    pattern value = case value of
      PWildcard n -> PWildcard <$> patternNodeVisit False Nothing n
      PVariable n name -> PVariable <$> patternNodeVisit True Nothing n <*> pure name
      PLiteral n literal -> PLiteral <$> patternNodeVisit False Nothing n <*> pure literal
      PConstructor n name nested -> PConstructor <$> patternNodeVisit False (Just name) n <*> pure name <*> traverse pattern nested
      PList n nested -> PList <$> patternNodeVisit False Nothing n <*> traverse pattern nested
      PTuple n nested -> PTuple <$> patternNodeVisit False Nothing n <*> traverse pattern nested
      PConsList n first rest -> PConsList <$> patternNodeVisit False Nothing n <*> pattern first <*> pattern rest
      PAs n name nested -> PAs <$> patternNodeVisit True Nothing n <*> pure name <*> pattern nested
      POr n alternatives -> POr <$> patternNodeVisit False Nothing n <*> traverse pattern alternatives
    statement value = case value of
      SLet n name body -> SLet <$> bindingStatement n <*> pure name <*> expression body
      SSignature n name signature -> SSignature <$> bindingStatement n <*> pure name <*> pure signature
      SData n name parameters constructors -> SData <$> plainStatement n <*> pure name <*> pure parameters <*> traverse constructor constructors
      SClass n name parameters methods -> SClass <$> plainStatement n <*> pure name <*> pure parameters <*> traverse (classMethod name) methods
      SImpl n name targets methods -> SImpl <$> plainStatement n <*> pure name <*> pure targets <*> traverse (implMethod name) methods
      SExpr n body -> SExpr <$> plainStatement n <*> expression body
      SModule n path -> SModule <$> plainStatement n <*> pure path
      SImport n path alias symbols -> SImport <$> plainStatement n <*> pure path <*> pure alias <*> pure symbols
    constructor (DataConstructor n name fields) = DataConstructor <$> bindingStatement n <*> pure name <*> pure fields
    classMethod capability (ClassMethodSignature n name signature) = ClassMethodSignature <$> statementNodeVisit True (Just (qualifiedMemberName capability name)) n <*> pure name <*> pure signature
    implMethod capability (ImplMethod n name body) = ImplMethod <$> statementNodeVisit True (Just (qualifiedMemberName capability name)) n <*> pure name <*> expression body
