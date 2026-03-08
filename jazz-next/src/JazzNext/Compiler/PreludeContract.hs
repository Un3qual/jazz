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

-- Validates explicit prelude bridge declarations that map prelude-visible
-- names to kernel-owned builtin symbols.
validatePreludeKernelBridges :: Expr -> [Text]
validatePreludeKernelBridges preludeExpr =
  case preludeExpr of
    EScope statements ->
      fst (foldl validateStatement ([], Set.empty) statements)
    _ -> []
  where
    validateStatement :: ([Text], Set Text) -> Statement -> ([Text], Set Text)
    validateStatement (diagnostics, seenBindings) statement =
      case statement of
        SLet bindingName _ bindingExpr ->
          let statementDiagnostics = validateBridge seenBindings bindingName bindingExpr
              seenBindings' = Set.insert bindingName seenBindings
           in (diagnostics <> statementDiagnostics, seenBindings')
        _ -> (diagnostics, seenBindings)

    validateBridge :: Set Text -> Text -> Expr -> [Text]
    validateBridge seenBindings bindingName bindingExpr =
      case kernelBridgeTargetName bindingName of
        Nothing
          | kernelBridgeBindingPrefix `Text.isPrefixOf` bindingName ->
              let suffix = Text.drop (Text.length kernelBridgeBindingPrefix) bindingName
               in
                if Text.null suffix
                  then
                    [ "E0005: prelude kernel bridge '"
                        <> bindingName
                        <> "' must include a non-empty kernel symbol suffix after '"
                        <> kernelBridgeBindingPrefix
                        <> "'"
                    ]
                  else
                    [ "E0004: prelude kernel bridge '"
                        <> bindingName
                        <> "' references unknown kernel symbol '"
                        <> bindingName
                        <> "'"
                    ]
          | otherwise -> []
        Just targetName ->
          case bindingExpr of
            EVar rhsName
              | rhsName /= targetName ->
                  [ "E0005: prelude kernel bridge '"
                      <> bindingName
                      <> "' must reference kernel symbol '"
                      <> targetName
                      <> "', found '"
                      <> rhsName
                      <> "'"
                  ]
              | targetName `Set.member` seenBindings ->
                  [ "E0005: prelude kernel bridge '"
                      <> bindingName
                      <> "' must reference canonical kernel symbol '"
                      <> targetName
                      <> "', but '"
                      <> targetName
                      <> "' was rebound earlier in prelude scope"
                  ]
              | otherwise -> []
            _ ->
              [ "E0005: prelude kernel bridge '"
                  <> bindingName
                  <> "' must be a direct symbol reference to '"
                  <> targetName
                  <> "'"
              ]
