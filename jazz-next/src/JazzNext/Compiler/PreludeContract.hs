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
  ( kernelBridgeBindingPrefix,
    kernelBridgeTargetName
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    mkDiagnostic
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
      fst (foldl validateStatement ([], Set.empty) statements)
    _ -> []
  where
    validateStatement :: ([Diagnostic], Set Text) -> Statement -> ([Diagnostic], Set Text)
    validateStatement (diagnostics, seenBindings) statement =
      case statement of
        SLet bindingName _ bindingExpr ->
          let bindingNameText = identifierText bindingName
              statementDiagnostics = validateBridge seenBindings bindingNameText bindingExpr
              seenBindings' = Set.insert bindingNameText seenBindings
           in (diagnostics <> statementDiagnostics, seenBindings')
        _ -> (diagnostics, seenBindings)

    validateBridge :: Set Text -> Text -> Expr -> [Diagnostic]
    validateBridge seenBindings bindingName bindingExpr =
      case kernelBridgeTargetName bindingName of
        Nothing
          | kernelBridgeBindingPrefix `Text.isPrefixOf` bindingName ->
              let suffix = Text.drop (Text.length kernelBridgeBindingPrefix) bindingName
               in
                if Text.null suffix
                  then
                    [ mkDiagnostic
                        "E0005"
                        ( "prelude kernel bridge '"
                            <> bindingName
                            <> "' must include a non-empty kernel symbol suffix after '"
                            <> kernelBridgeBindingPrefix
                            <> "'"
                        )
                    ]
                  else
                    [ mkDiagnostic
                        "E0004"
                        ( "prelude kernel bridge '"
                            <> bindingName
                            <> "' references unknown kernel symbol '"
                            <> bindingName
                            <> "'"
                        )
                    ]
          | otherwise -> []
        Just targetName ->
          case bindingExpr of
            EVar rhsName
              | identifierText rhsName /= targetName ->
                  [ mkDiagnostic
                      "E0005"
                      ( "prelude kernel bridge '"
                          <> bindingName
                          <> "' must reference kernel symbol '"
                          <> targetName
                          <> "', found '"
                          <> identifierText rhsName
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
