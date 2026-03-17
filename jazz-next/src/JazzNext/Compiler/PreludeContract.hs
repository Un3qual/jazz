{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.PreludeContract
  ( validatePreludeKernelBridges
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( builtinKernelBridgeName,
    kernelBridgeBindingPrefix,
    kernelBridgeTargetName,
    lookupBuiltinSymbol
  )

-- Validates explicit prelude bridge declarations that map prelude-visible
-- names to kernel-owned builtin symbols.
validatePreludeKernelBridges :: Expr -> [Text]
validatePreludeKernelBridges preludeExpr =
  case preludeExpr of
    EScope statements ->
      concatMap validateStatement statements
    _ -> []
  where
    validateStatement :: Statement -> [Text]
    validateStatement statement =
      case statement of
        SLet bindingName _ bindingExpr -> validateBinding bindingName bindingExpr
        _ -> []

    validateBinding :: Text -> Expr -> [Text]
    validateBinding bindingName bindingExpr
      | kernelBridgeBindingPrefix `Text.isPrefixOf` bindingName =
          [ "E0005: prelude must not redefine kernel bridge symbol '"
              <> bindingName
              <> "' directly"
          ]
      | otherwise =
          case lookupBuiltinSymbol bindingName of
            Nothing -> []
            Just builtinSymbol ->
              validateBuiltinAlias bindingName (builtinKernelBridgeName builtinSymbol) bindingExpr

    validateBuiltinAlias :: Text -> Text -> Expr -> [Text]
    validateBuiltinAlias bindingName expectedKernelBridge bindingExpr =
      case bindingExpr of
        EVar rhsName
          | rhsName == expectedKernelBridge ->
              []
          | otherwise ->
              validateKernelReference bindingName expectedKernelBridge rhsName
        _ ->
          [ "E0005: prelude builtin alias '"
              <> bindingName
              <> "' must be a direct kernel reference to '"
              <> expectedKernelBridge
              <> "'"
          ]

    validateKernelReference :: Text -> Text -> Text -> [Text]
    validateKernelReference bindingName expectedKernelBridge rhsName =
      case kernelBridgeTargetName rhsName of
        Nothing
          | kernelBridgeBindingPrefix `Text.isPrefixOf` rhsName ->
              [ "E0005: prelude kernel bridge reference '"
                  <> rhsName
                  <> "' must include a non-empty kernel symbol suffix after '"
                  <> kernelBridgeBindingPrefix
                  <> "'"
              ]
          | otherwise ->
              [ "E0005: prelude builtin alias '"
                  <> bindingName
                  <> "' must be a direct kernel reference to '"
                  <> expectedKernelBridge
                  <> "'"
              ]
        Just targetName ->
          case lookupBuiltinSymbol targetName of
            Nothing ->
              [ "E0004: prelude builtin alias '"
                  <> bindingName
                  <> "' references unknown kernel symbol '"
                  <> targetName
                  <> "'"
              ]
            Just _
              | rhsName /= expectedKernelBridge ->
                  [ "E0005: prelude builtin alias '"
                      <> bindingName
                      <> "' must reference matching kernel symbol '"
                      <> expectedKernelBridge
                      <> "', found '"
                      <> rhsName
                      <> "'"
                  ]
              | otherwise ->
                  []
