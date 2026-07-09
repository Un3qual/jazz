{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.TypeInference.Capabilities
  ( applyCapabilityFacts,
    capabilityFactsFromState,
    enterModuleCapabilityScope,
    flushCurrentModuleCapabilityFacts,
    generatedEqualityCapabilityName,
    importModuleCapabilityFacts,
    mergeCapabilityFacts,
    restoreCapabilityFacts,
    seedFacts,
    seedStatementCapabilityFact,
    typeSchemeDefiningFactsFromState,
    typeSchemeReferencedCapabilityFacts,
    updateRootModuleBaselineFacts
  ) where

import Data.List (foldl' )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.AST
  ( ClassMethodSignature (..), ConstraintSignatureType, ImplMethod (..), Statement (..) )
import JazzNext.Compiler.CapabilityFacts
  ( concreteConstraintArgument, concreteImplFactClassName, concreteImplFactKey, qualifiedMethodKey, splitQualifiedMethodKey )
import JazzNext.Compiler.Identifier (identifierText)
import JazzNext.Compiler.Name
  ( GeneratedNameKind (..), Name (..), NameNamespace (..), renderName )
import JazzNext.Compiler.TypeInference.State
  ( DeclarationState (..), InferState (..), ModuleInferenceState (..), inferClassFacts, inferClassMethodSignatures, inferConcreteImplFacts, inferConcreteImplMethods, inferCurrentModuleLocalCapabilityFacts, inferCurrentModulePath, inferGeneratedEqualityClassFacts, inferModuleCapabilityFacts )
import JazzNext.Compiler.TypeInference.Types
  ( ClassMethodType (..), ImplMethodType (..), ScopeCapabilityFacts (..), TypeSchemeConstraint (..), emptyScopeCapabilityFacts )

capabilityFactsFromState :: InferState -> ScopeCapabilityFacts
capabilityFactsFromState state =
  ScopeCapabilityFacts
    { scopeClassFacts = inferClassFacts state,
      scopeGeneratedEqualityClassFacts = inferGeneratedEqualityClassFacts state,
      scopeConcreteImplFacts = inferConcreteImplFacts state,
      scopeClassMethodSignatures = inferClassMethodSignatures state,
      scopeConcreteImplMethods = inferConcreteImplMethods state
    }

typeSchemeDefiningFactsFromState :: InferState -> [TypeSchemeConstraint] -> ScopeCapabilityFacts
typeSchemeDefiningFactsFromState state schemeConstraints =
  case inferCurrentModulePath state of
    Just _ -> typeSchemeReferencedCapabilityFacts schemeConstraints (capabilityFactsFromState state)
    Nothing -> capabilityFactsFromState state

typeSchemeReferencedCapabilityFacts :: [TypeSchemeConstraint] -> ScopeCapabilityFacts -> ScopeCapabilityFacts
typeSchemeReferencedCapabilityFacts schemeConstraints facts =
  facts
    { scopeClassFacts =
        Map.filterWithKey
          (\className _ -> Set.member className referencedCapabilityNames)
          (scopeClassFacts facts),
      scopeConcreteImplFacts =
        Set.filter
          (\implKey -> Set.member (concreteImplFactClassName implKey) referencedCapabilityNames)
          (scopeConcreteImplFacts facts),
      scopeGeneratedEqualityClassFacts =
        Set.filter
          (`Set.member` referencedCapabilityNames)
          (scopeGeneratedEqualityClassFacts facts),
      scopeClassMethodSignatures =
        Map.filterWithKey
          (\methodKey _ -> methodKeyReferencesCapturedCapability methodKey)
          (scopeClassMethodSignatures facts),
      scopeConcreteImplMethods =
        Map.filterWithKey
          (\methodKey _ -> methodKeyReferencesCapturedCapability methodKey)
          (scopeConcreteImplMethods facts)
    }
  where
    referencedCapabilityNames =
      Set.fromList
        [ constraintName
          | schemeConstraint <- schemeConstraints,
            let constraintName = typeSchemeConstraintCapabilityName schemeConstraint
        ]

    methodKeyReferencesCapturedCapability methodKey =
      case splitQualifiedMethodKey methodKey of
        Just (className, _) -> Set.member className referencedCapabilityNames
        Nothing -> False

typeSchemeConstraintCapabilityName :: TypeSchemeConstraint -> Text
typeSchemeConstraintCapabilityName constraint =
  case constraint of
    TypeSchemeConstraint constraintName _ -> constraintName
    TypeSchemeInferredConstraint constraintName _ -> constraintName
    TypeSchemeMethodConstraint constraintName _ _ -> constraintName

applyCapabilityFacts :: ScopeCapabilityFacts -> InferState -> InferState
applyCapabilityFacts facts state =
  modifyDeclarationState
    ( \declarations ->
        declarations
          { declarationClassFacts = scopeClassFacts facts,
            declarationGeneratedEqualityClassFacts = scopeGeneratedEqualityClassFacts facts,
            declarationConcreteImplFacts = scopeConcreteImplFacts facts,
            declarationClassMethodSignatures = scopeClassMethodSignatures facts,
            declarationConcreteImplMethods = scopeConcreteImplMethods facts
          }
    )
    state

restoreCapabilityFacts :: InferState -> InferState -> InferState
restoreCapabilityFacts previousState nextState =
  modifyModuleInferenceState
    ( \moduleState ->
        moduleState
          { inferenceLocalCapabilities =
              inferCurrentModuleLocalCapabilityFacts previousState
          }
    )
    ( modifyDeclarationState
        ( \declarations ->
            declarations
              { declarationClassFacts = inferClassFacts previousState,
                declarationGeneratedEqualityClassFacts = inferGeneratedEqualityClassFacts previousState,
                declarationConcreteImplFacts = inferConcreteImplFacts previousState,
                declarationClassMethodSignatures = inferClassMethodSignatures previousState,
                declarationConcreteImplMethods = inferConcreteImplMethods previousState
              }
        )
        nextState
    )

mergeCapabilityFacts :: ScopeCapabilityFacts -> ScopeCapabilityFacts -> ScopeCapabilityFacts
mergeCapabilityFacts leftFacts rightFacts =
  ScopeCapabilityFacts
    { scopeClassFacts = Map.union (scopeClassFacts leftFacts) (scopeClassFacts rightFacts),
      scopeGeneratedEqualityClassFacts =
        Set.union
          (scopeGeneratedEqualityClassFacts leftFacts)
          (scopeGeneratedEqualityClassFacts rightFacts),
      scopeConcreteImplFacts =
        Set.union
          (scopeConcreteImplFacts leftFacts)
          (scopeConcreteImplFacts rightFacts),
      scopeClassMethodSignatures =
        Map.union
          (scopeClassMethodSignatures leftFacts)
          (scopeClassMethodSignatures rightFacts),
      scopeConcreteImplMethods =
        Map.unionWith
          (++)
          (scopeConcreteImplMethods leftFacts)
          (scopeConcreteImplMethods rightFacts)
    }

updateRootModuleBaselineFacts :: ScopeCapabilityFacts -> InferState -> InferState -> ScopeCapabilityFacts
updateRootModuleBaselineFacts moduleBaselineFacts previousState nextState =
  case inferCurrentModulePath previousState of
    Nothing -> capabilityFactsFromState nextState
    Just _ -> moduleBaselineFacts

flushCurrentModuleCapabilityFacts :: InferState -> InferState
flushCurrentModuleCapabilityFacts state =
  case inferCurrentModulePath state of
    Just modulePath ->
      modifyModuleInferenceState
        ( \moduleState ->
            moduleState
              { inferenceModuleCapabilities =
                  Map.insert
                    modulePath
                    (inferCurrentModuleLocalCapabilityFacts state)
                    (inferModuleCapabilityFacts state)
              }
        )
        state
    Nothing -> state

enterModuleCapabilityScope :: ScopeCapabilityFacts -> [Text] -> InferState -> InferState
enterModuleCapabilityScope baselineFacts modulePath state =
  modifyModuleInferenceState
    ( \moduleState ->
        moduleState
          { inferenceModulePath = Just modulePath,
            inferenceLocalCapabilities = emptyScopeCapabilityFacts
          }
    )
    (applyCapabilityFacts baselineFacts (flushCurrentModuleCapabilityFacts state))

importModuleCapabilityFacts :: [Text] -> Maybe Text -> Maybe [Text] -> InferState -> InferState
importModuleCapabilityFacts modulePath maybeAlias maybeSymbolNames state =
  applyCapabilityFacts
    ( mergeCapabilityFacts
        (capabilityFactsFromState state)
        (filterImportedCapabilityFacts maybeAlias maybeSymbolNames (Map.findWithDefault emptyScopeCapabilityFacts modulePath (inferModuleCapabilityFacts state)))
    )
    state

filterImportedCapabilityFacts :: Maybe Text -> Maybe [Text] -> ScopeCapabilityFacts -> ScopeCapabilityFacts
filterImportedCapabilityFacts maybeAlias maybeSymbolNames facts =
  case maybeAlias of
    Just _ -> emptyScopeCapabilityFacts
    Nothing ->
      case maybeSymbolNames of
        Nothing -> facts
        Just symbolNames ->
          facts
            { scopeClassFacts =
                Map.filterWithKey
                  (\className _ -> Set.member className visibleSymbols)
                  (scopeClassFacts facts),
              scopeGeneratedEqualityClassFacts =
                Set.filter (`Set.member` visibleSymbols) (scopeGeneratedEqualityClassFacts facts),
              scopeConcreteImplFacts =
                Set.filter
                  (\implKey -> Set.member (concreteImplFactClassName implKey) visibleSymbols)
                  (scopeConcreteImplFacts facts),
              scopeClassMethodSignatures =
                Map.filterWithKey
                  (\methodKey _ -> qualifiedMethodClassIsVisible methodKey)
                  (scopeClassMethodSignatures facts),
              scopeConcreteImplMethods =
                Map.filterWithKey
                  (\methodKey _ -> qualifiedMethodClassIsVisible methodKey)
                  (scopeConcreteImplMethods facts)
            }
          where
            visibleSymbols = Set.fromList symbolNames
            qualifiedMethodClassIsVisible methodKey =
              case splitQualifiedMethodKey methodKey of
                Just (className, _) -> Set.member className visibleSymbols
                Nothing -> False

seedStatementCapabilityFact :: InferState -> Statement -> InferState
seedStatementCapabilityFact state statement =
  let facts = seedFacts (capabilityFactsFromState state) (0, statement)
      stateWithVisibleFacts = applyCapabilityFacts facts state
   in case inferCurrentModulePath state of
        Just _ ->
          modifyModuleInferenceState
            ( \moduleState ->
                moduleState
                  { inferenceLocalCapabilities =
                      seedFacts (inferCurrentModuleLocalCapabilityFacts state) (0, statement)
                  }
            )
            stateWithVisibleFacts
        Nothing ->
          stateWithVisibleFacts

seedFacts :: ScopeCapabilityFacts -> (Int, Statement) -> ScopeCapabilityFacts
seedFacts facts (_, statement) =
  case statement of
    SClass _ capabilityName parameters methods ->
      seedClassMethodFacts
        capabilityName
        parameters
        methods
        facts
          { scopeClassFacts = Map.insert (identifierText capabilityName) (length parameters) (scopeClassFacts facts),
            scopeGeneratedEqualityClassFacts =
              if generatedEqualityCapabilityName capabilityName
                then Set.insert (renderName capabilityName) (scopeGeneratedEqualityClassFacts facts)
                else scopeGeneratedEqualityClassFacts facts
          }
    SImpl _ capabilityName arguments methods ->
      seedImplMethodFacts capabilityName arguments methods $
        case concreteImplFactKey capabilityName arguments of
          Just implFactKey ->
            facts {scopeConcreteImplFacts = Set.insert implFactKey (scopeConcreteImplFacts facts)}
          Nothing ->
            facts
    _ -> facts

seedClassMethodFacts ::
  Name ->
  [Name] ->
  [ClassMethodSignature] ->
  ScopeCapabilityFacts ->
  ScopeCapabilityFacts
seedClassMethodFacts capabilityName parameters methods facts =
  case parameters of
    [classParameter] ->
      facts
        { scopeClassMethodSignatures =
            foldl'
              insertMethodSignature
              (scopeClassMethodSignatures facts)
              methods
        }
      where
        classParameterText = identifierText classParameter
        insertMethodSignature acc (ClassMethodSignature methodName _ methodSignature) =
          Map.insert
            (qualifiedMethodKey capabilityName methodName)
            (ClassMethodType classParameterText methodSignature)
            acc
    _ -> facts

seedImplMethodFacts ::
  Name ->
  [ConstraintSignatureType] ->
  [ImplMethod] ->
  ScopeCapabilityFacts ->
  ScopeCapabilityFacts
seedImplMethodFacts capabilityName arguments methods facts =
  case arguments of
    [implTarget]
      | concreteConstraintArgument implTarget ->
          facts
            { scopeConcreteImplMethods =
                foldl'
                  insertImplMethod
                  (scopeConcreteImplMethods facts)
                  methods
            }
      where
        insertImplMethod acc (ImplMethod methodName _ _) =
          Map.insertWith
            (\newMethods existingMethods -> existingMethods ++ newMethods)
            (qualifiedMethodKey capabilityName methodName)
            [ImplMethodType implTarget]
            acc
    _ -> facts


generatedEqualityCapabilityName :: Name -> Bool
generatedEqualityCapabilityName name =
  case name of
    GeneratedName (ModuleReplayBridge _ CapabilityNamespace "Eq") -> True
    _ -> False

modifyDeclarationState :: (DeclarationState -> DeclarationState) -> InferState -> InferState
modifyDeclarationState update state =
  state {inferDeclarations = update (inferDeclarations state)}

modifyModuleInferenceState :: (ModuleInferenceState -> ModuleInferenceState) -> InferState -> InferState
modifyModuleInferenceState update state =
  state {inferModule = update (inferModule state)}
