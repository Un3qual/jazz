{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Validation rules for prelude-owned kernel bridge bindings. This keeps the
-- bundled/explicit prelude contract auditable while builtin ownership is still
-- in transition.
module Jazz.Compiler.PreludeContract
  ( validatePreludeKernelBridges,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CoreNode (coreNodeSpan),
    CorePhase (Lowered),
    Expr (..),
    Statement (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( kernelBridgeBindingPrefix,
    kernelBridgeTargetName,
  )
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (..),
    SourceSpan,
    mkErrorDiagnostic,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject,
  )
import Jazz.Compiler.Name
  ( renderName,
  )

-- | Validate explicit prelude bridge declarations that map prelude-visible
-- names directly onto canonical kernel symbols.
validatePreludeKernelBridges :: Expr 'Lowered -> [Diagnostic]
validatePreludeKernelBridges preludeExpr =
  case preludeExpr of
    EBlock _ statements ->
      let (diagnostics, _, _) = foldl validateStatement ([], Set.empty, Map.empty) statements
       in diagnostics
    _ -> []
  where
    validateStatement ::
      ([Diagnostic], Set Text, Map Text SourceSpan) ->
      Statement 'Lowered ->
      ([Diagnostic], Set Text, Map Text SourceSpan)
    validateStatement (diagnostics, seenBindings, seenBindingSpans) statement =
      case statement of
        SLet node bindingName bindingExpr ->
          let bindingSpan = coreNodeSpan node
              bindingNameText = renderName bindingName
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

    -- Only names with the reserved bridge prefix participate in this contract;
    -- ordinary prelude aliases are validated by the normal analyzer/type path.
    validateBridge :: Set Text -> Map Text SourceSpan -> Text -> SourceSpan -> Expr 'Lowered -> [Diagnostic]
    validateBridge seenBindings seenBindingSpans bindingName bindingSpan bindingExpr =
      case kernelBridgeTargetName bindingName of
        Nothing
          | kernelBridgeBindingPrefix `Text.isPrefixOf` bindingName ->
              let suffix = Text.drop (Text.length kernelBridgeBindingPrefix) bindingName
               in if Text.null suffix
                    then
                      [ bridgeDiagnostic
                          bindingName
                          bindingSpan
                          ( mkErrorDiagnostic
                              E0005
                              CompilationOrigin
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
                          ( mkErrorDiagnostic
                              E0004
                              CompilationOrigin
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
            EVar _ rhsName
              | renderName rhsName /= targetName ->
                  [ bridgeDiagnostic
                      bindingName
                      bindingSpan
                      ( mkErrorDiagnostic
                          E0005
                          CompilationOrigin
                          ( "prelude kernel bridge '"
                              <> bindingName
                              <> "' must reference kernel symbol '"
                              <> targetName
                              <> "', found '"
                              <> renderName rhsName
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
                  ( mkErrorDiagnostic
                      E0005
                      CompilationOrigin
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
                ( mkErrorDiagnostic
                    E0005
                    CompilationOrigin
                    ( "prelude kernel bridge '"
                        <> bindingName
                        <> "' must reference canonical kernel symbol '"
                        <> targetName
                        <> "', but '"
                        <> targetName
                        <> "' was rebound earlier in prelude scope"
                    )
                )

    -- Route all bridge diagnostics through one helper so subject/span metadata
    -- stays consistent across the different validation failures above.
    bridgeDiagnostic :: Text -> SourceSpan -> Diagnostic -> Diagnostic
    bridgeDiagnostic bindingName bindingSpan =
      setDiagnosticSubject bindingName
        . setDiagnosticPrimarySpan bindingSpan
