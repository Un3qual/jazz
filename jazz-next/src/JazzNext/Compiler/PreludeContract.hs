{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.PreludeContract
  ( validatePreludeKernelBridges
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( isBuiltinSymbolName,
    kernelBridgeBindingPrefix,
    kernelBridgeTargetName
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    mkDiagnostic
  )

-- Validates explicit prelude bridge declarations that map prelude-visible
-- names to kernel-owned builtin symbols.
validatePreludeKernelBridges :: Expr -> [Diagnostic]
validatePreludeKernelBridges preludeExpr =
  case preludeExpr of
    EBlock statements ->
      fst (foldl validateStatement ([], Set.empty) statements)
    _ -> []
  where
    validateStatement :: ([Diagnostic], Set Text) -> Statement -> ([Diagnostic], Set Text)
    validateStatement (diagnostics, seenBindings) statement =
      case statement of
        SLet bindingName _ bindingExpr ->
          let statementDiagnostics = validateBridge seenBindings bindingName bindingExpr
              seenBindings' = Set.insert bindingName seenBindings
           in (diagnostics <> statementDiagnostics, seenBindings')
        _ -> (diagnostics, seenBindings)

    validateBridge :: Set Text -> Text -> Expr -> [Diagnostic]
    validateBridge seenBindings bindingName bindingExpr =
      case kernelBridgeTargetName bindingName of
        Nothing
          | kernelBridgeBindingPrefix `Text.isPrefixOf` bindingName ->
              [ mkDiagnostic
                  "E0005"
                  ( "prelude kernel bridge '"
                      <> bindingName
                      <> "' must include a non-empty kernel symbol suffix after '"
                      <> kernelBridgeBindingPrefix
                      <> "'"
                  )
              ]
          | otherwise -> []
        Just targetName
          | not (isBuiltinSymbolName targetName) ->
              [ mkDiagnostic
                  "E0004"
                  ( "prelude kernel bridge '"
                      <> bindingName
                      <> "' references unknown kernel symbol '"
                      <> targetName
                      <> "'"
                  )
              ]
          | otherwise ->
              case bindingExpr of
                EVar rhsName
                  | rhsName /= targetName ->
                      [ mkDiagnostic
                          "E0005"
                          ( "prelude kernel bridge '"
                              <> bindingName
                              <> "' must reference kernel symbol '"
                              <> targetName
                              <> "', found '"
                              <> rhsName
                              <> "'"
                          )
                      ]
                  | targetName `Set.member` seenBindings ->
                      [ mkDiagnostic
                          "E0005"
                          ( "prelude kernel bridge '"
                              <> bindingName
                              <> "' must reference canonical kernel symbol '"
                              <> targetName
                              <> "', but '"
                              <> targetName
                              <> "' was rebound earlier in prelude scope"
                          )
                      ]
                  | otherwise -> []
                _ ->
                  [ mkDiagnostic
                      "E0005"
                      ( "prelude kernel bridge '"
                          <> bindingName
                          <> "' must be a direct symbol reference to '"
                          <> targetName
                          <> "'"
                      )
                  ]
