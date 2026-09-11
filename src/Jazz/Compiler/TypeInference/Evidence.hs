{-# LANGUAGE DataKinds #-}

module Jazz.Compiler.TypeInference.Evidence
  ( implementationEvidenceCandidatesInModule,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
import Jazz.Compiler.CoreIdentity (ResolvedNodeFacts (resolvedNodeOwner))
import Jazz.Compiler.Name (identifierText, mkIdentifier)
import Jazz.Compiler.SemanticFacts (ImplId (..), MethodId (..))
import Jazz.Compiler.TypeInference.State (ImplementationEvidenceCandidate (..))

implementationEvidenceCandidatesInModule :: Expr 'Resolved -> Map Text [ImplementationEvidenceCandidate]
implementationEvidenceCandidatesInModule = candidatesFromEntries . expressionEntries

candidatesFromEntries :: [(Text, ImplementationEvidenceCandidate)] -> Map Text [ImplementationEvidenceCandidate]
candidatesFromEntries =
  Map.fromListWith (flip (<>)) . map (\(key, candidate) -> (key, [candidate]))

expressionEntries :: Expr 'Resolved -> [(Text, ImplementationEvidenceCandidate)]
expressionEntries expression =
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
    EBlock _ statements -> foldMap (statementEntries) statements
    _ -> []
  where
    recur = expressionEntries
    armEntries (CaseArm _ _ guard body) = foldMap recur guard <> recur body

statementEntries :: Statement 'Resolved -> [(Text, ImplementationEvidenceCandidate)]
statementEntries statement =
  case statement of
    SLet _ _ value -> expressionEntries value
    SImpl implementationNode capabilityName [target] methods ->
      foldMap methodEntry methods <> foldMap methodBodyEntries methods
      where
        implementationId = ImplId (resolvedNodeOwner (coreNodeFacts implementationNode), coreNodeId implementationNode)
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
        methodBodyEntries (ImplMethod _ _ body) = expressionEntries body
    SImpl _ _ _ methods -> foldMap methodBodyEntries methods
      where
        methodBodyEntries (ImplMethod _ _ body) = expressionEntries body
    SExpr _ value -> expressionEntries value
    _ -> []
