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
  ( SourceUnitOwner (..),
    sourceUnitStatementOwners,
  )
import Jazz.Compiler.TypeInference.State (ImplementationEvidenceCandidate (..))

implementationEvidenceCandidatesInModule :: SourceUnitOwner -> Expr 'Resolved -> Map Text [ImplementationEvidenceCandidate]
implementationEvidenceCandidatesInModule owner =
  candidatesFromEntries . expressionEntries owner

implementationEvidenceCandidatesInSourceUnit :: ModulePath -> ModulePath -> Set Int -> Expr 'Resolved -> Map Text [ImplementationEvidenceCandidate]
implementationEvidenceCandidatesInSourceUnit sourcePath preludePath preludeStatementIndices expression =
  candidatesFromEntries
    ( case expression of
        EBlock _ statements ->
          concat
            [ statementEntries
                owner
                statement
            | (owner, statement) <-
                zip
                  (sourceUnitStatementOwners sourcePath preludePath preludeStatementIndices statements)
                  statements
            ]
        _ -> expressionEntries (StandaloneSourceUnit sourcePath) expression
    )

candidatesFromEntries :: [(Text, ImplementationEvidenceCandidate)] -> Map Text [ImplementationEvidenceCandidate]
candidatesFromEntries =
  Map.fromListWith (flip (<>)) . map (\(key, candidate) -> (key, [candidate]))

expressionEntries :: SourceUnitOwner -> Expr 'Resolved -> [(Text, ImplementationEvidenceCandidate)]
expressionEntries owner expression =
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
    EBlock _ statements -> foldMap (statementEntries owner) statements
    _ -> []
  where
    recur = expressionEntries owner
    armEntries (CaseArm _ _ guard body) = foldMap recur guard <> recur body

statementEntries :: SourceUnitOwner -> Statement 'Resolved -> [(Text, ImplementationEvidenceCandidate)]
statementEntries owner statement =
  case statement of
    SLet _ _ value -> expressionEntries owner value
    SImpl implementationNode capabilityName [target] methods ->
      foldMap methodEntry methods <> foldMap methodBodyEntries methods
      where
        implementationId = ImplId (owner, coreNodeId implementationNode)
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
        methodBodyEntries (ImplMethod _ _ body) = expressionEntries owner body
    SImpl _ _ _ methods -> foldMap methodBodyEntries methods
      where
        methodBodyEntries (ImplMethod _ _ body) = expressionEntries owner body
    SExpr _ value -> expressionEntries owner value
    _ -> []
