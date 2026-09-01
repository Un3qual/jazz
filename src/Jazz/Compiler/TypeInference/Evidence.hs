{-# LANGUAGE DataKinds #-}

module Jazz.Compiler.TypeInference.Evidence
  ( implementationEvidenceCandidatesInModule,
    implementationEvidenceCandidatesInSourceUnit,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CaseArm (..),
    CoreNode (..),
    CorePhase (Resolved),
    Expr (..),
    ImplMethod (..),
    Statement (..),
  )
import Jazz.Compiler.CapabilityFacts (qualifiedMethodKey)
import Jazz.Compiler.ModuleIdentity (ModulePath)
import Jazz.Compiler.Name (identifierText, mkIdentifier)
import Jazz.Compiler.SemanticFacts (ImplId (..), MethodId (..))
import Jazz.Compiler.SourceUnitOwnership
  ( sourceUnitOwnerModulePath,
    sourceUnitStatementOwners,
  )
import Jazz.Compiler.TypeInference.State (ImplementationEvidenceCandidate (..))

implementationEvidenceCandidatesInModule :: ModulePath -> Expr 'Resolved -> Map Text [ImplementationEvidenceCandidate]
implementationEvidenceCandidatesInModule modulePath =
  candidatesFromEntries . expressionEntries modulePath

implementationEvidenceCandidatesInSourceUnit :: ModulePath -> ModulePath -> Set Int -> Expr 'Resolved -> Map Text [ImplementationEvidenceCandidate]
implementationEvidenceCandidatesInSourceUnit sourcePath preludePath preludeStatementIndices expression =
  candidatesFromEntries
    ( case expression of
        EBlock _ statements ->
          concat
            [ statementEntries
                (sourceUnitOwnerModulePath owner)
                statement
            | (owner, statement) <-
                zip
                  (sourceUnitStatementOwners sourcePath preludePath preludeStatementIndices statements)
                  statements
            ]
        _ -> expressionEntries sourcePath expression
    )

candidatesFromEntries :: [(Text, ImplementationEvidenceCandidate)] -> Map Text [ImplementationEvidenceCandidate]
candidatesFromEntries =
  Map.fromListWith (flip (<>)) . map (\(key, candidate) -> (key, [candidate]))

expressionEntries :: ModulePath -> Expr 'Resolved -> [(Text, ImplementationEvidenceCandidate)]
expressionEntries modulePath expression =
  case expression of
    ELambda _ _ body -> recur body
    EList _ elements -> foldMap recur elements
    ETuple _ elements -> foldMap recur elements
    EApply _ function argument -> recur function <> recur argument
    ETypeApplication _ function _ _ -> recur function
    EIf _ condition thenExpression elseExpression -> foldMap recur [condition, thenExpression, elseExpression]
    EPatternCase _ scrutinee arms -> recur scrutinee <> foldMap armEntries arms
    EBinary _ _ left right -> recur left <> recur right
    ESectionLeft _ left _ -> recur left
    ESectionRight _ _ right -> recur right
    EBlock _ statements -> foldMap (statementEntries modulePath) statements
    _ -> []
  where
    recur = expressionEntries modulePath
    armEntries (CaseArm _ _ guard body) = foldMap recur guard <> recur body

statementEntries :: ModulePath -> Statement 'Resolved -> [(Text, ImplementationEvidenceCandidate)]
statementEntries modulePath statement =
  case statement of
    SLet _ _ value -> expressionEntries modulePath value
    SImpl implementationNode capabilityName [target] methods ->
      foldMap methodEntry methods <> foldMap methodBodyEntries methods
      where
        implementationId = ImplId (modulePath, coreNodeId implementationNode)
        methodEntry (ImplMethod _ methodName _) =
          [ ( qualifiedMethodKey capabilityName methodName,
              ImplementationEvidenceCandidate
                { implementationCandidateCapability = capabilityName,
                  implementationCandidateTarget = target,
                  implementationCandidateId = implementationId,
                  implementationCandidateMethodId = MethodId (implementationId, mkIdentifier (identifierText methodName))
                }
            )
          ]
        methodBodyEntries (ImplMethod _ _ body) = expressionEntries modulePath body
    SImpl _ _ _ methods -> foldMap methodBodyEntries methods
      where
        methodBodyEntries (ImplMethod _ _ body) = expressionEntries modulePath body
    SExpr _ value -> expressionEntries modulePath value
    _ -> []
