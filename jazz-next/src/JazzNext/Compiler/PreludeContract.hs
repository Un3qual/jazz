{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.PreludeContract
  ( validatePreludeKernelBridges
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( kernelBridgeBindingPrefix,
    kernelBridgeTargetName
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan,
    mkDiagnostic,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject
  )
import JazzNext.Compiler.Identifier
  ( identifierText
  )

-- Validates explicit prelude bridge declarations that map prelude-visible
-- names to kernel-owned builtin symbols.
validatePreludeKernelBridges :: Expr -> [Diagnostic]
validatePreludeKernelBridges preludeExpr =
  case preludeExpr of
    EBlock statements ->
      let (diagnostics, _, _) = foldl validateStatement ([], Set.empty, Map.empty) statements
       in diagnostics
    _ -> []
  where
    validateStatement ::
      ([Diagnostic], Set Text, Map Text SourceSpan) ->
      Statement ->
      ([Diagnostic], Set Text, Map Text SourceSpan)
    validateStatement (diagnostics, seenBindings, seenBindingSpans) statement =
      case statement of
        SLet bindingName bindingSpan bindingExpr ->
          let bindingNameText = identifierText bindingName
              statementDiagnostics =
                validateBridge
                  seenBindings
                  seenBindingSpans
                  bindingNameText
                  bindingSpan
                  bindingExpr
              seenBindings' = Set.insert bindingNameText seenBindings
              seenBindingSpans' = Map.insert bindingNameText bindingSpan seenBindingSpans
           in (diagnostics <> statementDiagnostics, seenBindings', seenBindingSpans')
        _ ->
          (diagnostics, seenBindings, seenBindingSpans)

    validateBridge :: Set Text -> Map Text SourceSpan -> Text -> SourceSpan -> Expr -> [Diagnostic]
    validateBridge seenBindings seenBindingSpans bindingName bindingSpan bindingExpr =
      case kernelBridgeTargetName bindingName of
        Nothing
          | kernelBridgeBindingPrefix `Text.isPrefixOf` bindingName ->
              let suffix = Text.drop (Text.length kernelBridgeBindingPrefix) bindingName
               in
                if Text.null suffix
                  then
                    [ bridgeDiagnostic
                        bindingName
                        bindingSpan
                        ( mkDiagnostic
                            "E0005"
                            ( "prelude kernel bridge '"
                                <> bindingName
                                <> "' must include a non-empty kernel symbol suffix after '"
                                <> kernelBridgeBindingPrefix
                                <> "'"
                            )
                        )
                    ]
                  else
                    [ bridgeDiagnostic
                        bindingName
                        bindingSpan
                        ( mkDiagnostic
                            "E0004"
                            ( "prelude kernel bridge '"
                                <> bindingName
                                <> "' references unknown kernel symbol '"
                                <> bindingName
                                <> "'"
                            )
                        )
                    ]
          | otherwise -> []
        Just targetName ->
          case bindingExpr of
            EVar rhsName
              | identifierText rhsName /= targetName ->
                  [ bridgeDiagnostic
                      bindingName
                      bindingSpan
                      ( mkDiagnostic
                          "E0005"
                          ( "prelude kernel bridge '"
                              <> bindingName
                              <> "' must reference kernel symbol '"
                              <> targetName
                              <> "', found '"
                              <> identifierText rhsName
                              <> "'"
                          )
                      )
                  ]
              | targetName `Set.member` seenBindings ->
                  [ maybe
                      baseDiagnostic
                      (\previousSpan -> setDiagnosticRelatedSpan previousSpan baseDiagnostic)
                      (Map.lookup targetName seenBindingSpans)
                  ]
              | otherwise -> []
            _ ->
              [ bridgeDiagnostic
                  bindingName
                  bindingSpan
                  ( mkDiagnostic
                      "E0005"
                      ( "prelude kernel bridge '"
                          <> bindingName
                          <> "' must be a direct symbol reference to '"
                          <> targetName
                          <> "'"
                      )
                  )
              ]
          where
            baseDiagnostic =
              bridgeDiagnostic
                bindingName
                bindingSpan
                ( mkDiagnostic
                    "E0005"
                    ( "prelude kernel bridge '"
                        <> bindingName
                        <> "' must reference canonical kernel symbol '"
                        <> targetName
                        <> "', but '"
                        <> targetName
                        <> "' was rebound earlier in prelude scope"
                    )
                )

    bridgeDiagnostic :: Text -> SourceSpan -> Diagnostic -> Diagnostic
    bridgeDiagnostic bindingName bindingSpan =
      setDiagnosticSubject bindingName
        . setDiagnosticPrimarySpan bindingSpan
