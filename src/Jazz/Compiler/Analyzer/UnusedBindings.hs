{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Unused-binding reference accounting for one lexical block.
module Jazz.Compiler.Analyzer.UnusedBindings
  ( collectUnusedBindingWarnings,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CoreNode (coreNodeFacts, coreNodeSpan),
    CorePhase (..),
    DataConstructor (..),
    ImplMethod (..),
    Statement (..),
  )
import Jazz.Compiler.CoreIdentity (ResolvedNodeFacts (..))
import Jazz.Compiler.DiagnosticCatalog
  ( WarningCategory (..),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (..),
    SourceSpan,
    mkWarningDiagnostic,
    setDiagnosticPrimaryLabel,
    setDiagnosticSubject,
  )
import Jazz.Compiler.Name
  ( identifierText,
    resolvedValueScopeName,
  )
import Jazz.Compiler.RecursiveBindings
  ( resolvedExpressionReferences,
  )
import Jazz.Compiler.WarningConfig
  ( WarningSettings,
    isWarningEnabled,
  )

collectUnusedBindingWarnings ::
  WarningSettings ->
  Set Int ->
  [(Int, Statement 'Resolved)] ->
  Map Int [Diagnostic]
collectUnusedBindingWarnings settings hiddenStatementIndices indexedStatements
  | not (isWarningEnabled settings UnusedBinding) = Map.empty
  | otherwise =
      Map.fromList
        [ (statementIndex, [mkUnusedBindingWarning bindingNameText bindingSpan])
        | (statementIndex, SLet node bindingName _) <- indexedStatements,
          statementIndex `Set.notMember` hiddenStatementIndices,
          let bindingNameText = identifierText bindingName,
          let bindingSpan = coreNodeSpan node,
          not (Set.member statementIndex usedBindingStatementIndices),
          not
            ( isWarningEnabled settings SameScopeRebinding
                && Set.member statementIndex rebindingStatementIndices
            )
        ]
  where
    declarationsById =
      Map.fromList
        [ (binder, index)
        | (index, SLet node _ _) <- indexedStatements,
          Just binder <- [resolvedNodeBinder (coreNodeFacts node)]
        ]
    usedBindingStatementIndices =
      Set.fromList
        [ index
        | (statementIndex, statement) <- indexedStatements,
          Set.notMember statementIndex hiddenStatementIndices,
          binder <- Map.keys (statementReferences statement),
          Just index <- [Map.lookup binder declarationsById]
        ]
    rebindingStatementIndices = snd (foldl' markRebinding (Set.empty, Set.empty) indexedStatements)

    -- Preserve the warning policy that a binding's own spelling, including a
    -- rebinding initializer, does not count as an independent use.
    statementReferences statement = case statement of
      SLet _ name value ->
        Map.filter ((/= resolvedValueScopeName name) . resolvedValueScopeName) (resolvedExpressionReferences value)
      SExpr _ value -> resolvedExpressionReferences value
      SImpl _ _ _ methods -> foldMap (\(ImplMethod _ _ body) -> resolvedExpressionReferences body) methods
      _ -> Map.empty

    markRebinding current@(names, indices) (index, statement)
      | Set.member index hiddenStatementIndices = current
      | otherwise = case statement of
          SLet _ name _ ->
            let key = resolvedValueScopeName name
             in (Set.insert key names, if Set.member key names then Set.insert index indices else indices)
          SData _ _ _ constructors ->
            (foldl' (\acc (DataConstructor _ name _) -> Set.insert (resolvedValueScopeName name) acc) names constructors, indices)
          _ -> current

mkUnusedBindingWarning :: Text -> SourceSpan -> Diagnostic
mkUnusedBindingWarning variableName primarySpan =
  setDiagnosticPrimaryLabel primarySpan "binding declared here" $
    setDiagnosticSubject variableName $
      mkWarningDiagnostic
        UnusedBinding
        CompilationOrigin
        ( "unused binding: '"
            <> variableName
            <> "' is never referenced in this lexical block"
        )
