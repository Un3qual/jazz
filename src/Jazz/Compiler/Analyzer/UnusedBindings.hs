{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Unused-binding reference accounting for one lexical block.
module Jazz.Compiler.Analyzer.UnusedBindings
  ( collectUnusedBindingWarnings
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import Jazz.Compiler.AST
  ( DataConstructor (..),
    ImplMethod (..),
    Statement (..)
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (..),
    SourceSpan,
    mkWarningDiagnostic,
    setDiagnosticPrimaryLabel,
    setDiagnosticSubject
  )
import Jazz.Compiler.Name
  ( identifierText
  )
import Jazz.Compiler.RecursiveBindings
  ( freeVarsExprWithBound
  )
import Jazz.Compiler.WarningConfig
  ( WarningSettings,
    isWarningEnabled
  )
import Jazz.Compiler.DiagnosticCatalog
  ( WarningCategory (..)
  )

collectUnusedBindingWarnings ::
  WarningSettings ->
  Set Int ->
  Map Int (Set Int) ->
  [(Int, Statement)] ->
  Map Int [Diagnostic]
collectUnusedBindingWarnings settings hiddenStatementIndices recursiveGroupsByStatement indexedStatements
  | not (isWarningEnabled settings UnusedBinding) = Map.empty
  | otherwise =
      Map.fromList
        [ (statementIndex, [mkUnusedBindingWarning bindingNameText bindingSpan])
          | (statementIndex, SLet bindingName bindingSpan _) <- indexedStatements,
            statementIndex `Set.notMember` hiddenStatementIndices,
            let bindingNameText = identifierText bindingName,
            not (Set.member statementIndex usedBindingStatementIndices),
            not
              ( isWarningEnabled settings SameScopeRebinding
                  && Set.member statementIndex rebindingStatementIndices
              )
        ]
  where
    (usedBindingStatementIndices, rebindingStatementIndices) =
      collectUnusedBindingUseState
        hiddenStatementIndices
        recursiveGroupsByStatement
        indexedStatements

collectUnusedBindingUseState ::
  Set Int ->
  Map Int (Set Int) ->
  [(Int, Statement)] ->
  (Set Int, Set Int)
collectUnusedBindingUseState hiddenStatementIndices recursiveGroupsByStatement indexedStatements =
  let bindingDeclarationsByStatement =
        Map.fromList
          [ (statementIndex, bindingName)
            | (statementIndex, SLet bindingName _ _) <- indexedStatements
          ]
      (_, _, usedStatementIndices, rebindingStatementIndices) =
        foldl'
          (step bindingDeclarationsByStatement)
          (Map.empty, Set.empty, Set.empty, Set.empty)
          indexedStatements
   in (usedStatementIndices, rebindingStatementIndices)
  where
    step
      bindingDeclarationsByStatement
      (!activeBindings, !activeRebindingNames, !usedStatementIndices, !rebindingStatementIndices)
      (statementIndex, statement)
        | statementIndex `Set.member` hiddenStatementIndices =
            (activeBindings, activeRebindingNames, usedStatementIndices, rebindingStatementIndices)
        | otherwise =
            let referenceNames = statementReferenceNames statement
                visibleBindings =
                  bindingsVisibleToStatement
                    bindingDeclarationsByStatement
                    statementIndex
                    statement
                    activeBindings
                usedWithStatementReferences =
                  Set.foldl'
                    (markReferencedBinding visibleBindings)
                    usedStatementIndices
                    referenceNames
             in
              case statement of
                SLet bindingName _ _ ->
                  let rebindingStatementIndices' =
                        if Set.member bindingName activeRebindingNames
                          then Set.insert statementIndex rebindingStatementIndices
                          else rebindingStatementIndices
                   in
                    ( Map.insert bindingName statementIndex activeBindings,
                      Set.insert bindingName activeRebindingNames,
                      usedWithStatementReferences,
                      rebindingStatementIndices'
                    )
                SData _ _ _ constructors ->
                  ( foldl' removeConstructor activeBindings constructors,
                    foldl' registerConstructor activeRebindingNames constructors,
                    usedWithStatementReferences,
                    rebindingStatementIndices
                  )
                _ ->
                  ( activeBindings,
                    activeRebindingNames,
                    usedWithStatementReferences,
                    rebindingStatementIndices
                  )

    bindingsVisibleToStatement bindingDeclarationsByStatement statementIndex statement activeBindings =
      case statement of
        SLet bindingName _ _ ->
          let visibleWithCurrent = Map.insert bindingName statementIndex activeBindings
              recursivePeers =
                Set.delete
                  statementIndex
                  (Map.findWithDefault Set.empty statementIndex recursiveGroupsByStatement)
              peerBindings =
                Map.fromList
                  [ (peerName, peerStatementIndex)
                    | peerStatementIndex <- Set.toList recursivePeers,
                      Just peerName <- [Map.lookup peerStatementIndex bindingDeclarationsByStatement],
                      Map.notMember peerName visibleWithCurrent
                  ]
           in visibleWithCurrent `Map.union` peerBindings
        _ -> activeBindings

    statementReferenceNames statement =
      case statement of
        SLet bindingName _ valueExpr ->
          Set.delete
            bindingName
            (freeVarsExprWithBound Set.empty valueExpr)
        SExpr _ expr ->
          freeVarsExprWithBound Set.empty expr
        SImpl _ _ _ methods ->
          Set.unions
            [ freeVarsExprWithBound Set.empty methodExpr
              | ImplMethod _ _ methodExpr <- methods
            ]
        _ -> Set.empty

    markReferencedBinding visibleBindings usedStatementIndices referenceName =
      case Map.lookup referenceName visibleBindings of
        Nothing -> usedStatementIndices
        Just bindingStatementIndex ->
          Set.insert bindingStatementIndex usedStatementIndices

    removeConstructor activeBindings (DataConstructor constructorName _) =
      Map.delete constructorName activeBindings

    registerConstructor activeRebindingNames (DataConstructor constructorName _) =
      Set.insert constructorName activeRebindingNames

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
