{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Semantics.Runtime.ScopeTests
  ( scopeTests,
  )
where

import Control.Exception
  ( SomeException,
    evaluate,
    try,
  )
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CorePhase (Resolved),
    Expr,
    Literal (..),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.Name
  ( UnresolvedName,
    mkIdentifier,
    qualifiedName,
    sourceName,
  )
import Jazz.Compiler.Runtime
  ( evaluateRuntimeExpr,
    renderRuntimeValue,
  )
import Jazz.Compiler.Semantics.Runtime.Fixtures
import Jazz.Compiler.TypeRepresentation
  ( SignaturePayload (..),
    SignatureType (..),
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
  )
import System.Timeout (timeout)

scopeTests :: [NamedTest]
scopeTests =
  [ ("large flat binding scopes complete within the runtime budget", testLargeFlatBindingScopeCompletes),
    ("large nested block alias scopes complete within the runtime budget", testLargeNestedBlockAliasScopeCompletes)
  ]

testLargeFlatBindingScopeCompletes :: IO ()
testLargeFlatBindingScopeCompletes = do
  let bindingCount = 100000
      binding index =
        statementLet
          (indexedBindingName index)
          (SourceSpan index 1)
          (expressionLiteral (LInt (fromIntegral index)))
      expression =
        expressionBlock
          ( map binding [1 .. bindingCount]
              <> [statementExpression (SourceSpan (bindingCount + 1) 1) (expressionVariable (indexedBindingName bindingCount))]
          )
  assertRuntimeCompletesWithin
    10000000
    "100,000-binding flat scope"
    expression
    (Text.pack (show bindingCount))

testLargeNestedBlockAliasScopeCompletes :: IO ()
testLargeNestedBlockAliasScopeCompletes =
  assertRuntimeCompletesWithin
    10000000
    "50,000-binding nested block alias scope"
    (nestedBlockAliasScope 50000)
    "True"

nestedBlockAliasScope :: Int -> Expr 'Resolved
nestedBlockAliasScope bindingCount =
  expressionBlock
    [ statementClass
        (SourceSpan 1 1)
        "RuntimeFlag"
        ["a"]
        [ classMethodSignature "enabled" (SourceSpan 2 1) (ConstrainedSignature [] TypeBool),
          classMethodSignature "on" (SourceSpan 3 1) (ConstrainedSignature [] TypeBool),
          classMethodSignature "off" (SourceSpan 4 1) (ConstrainedSignature [] TypeBool)
        ],
      statementImpl
        (SourceSpan 5 1)
        "RuntimeFlag"
        [TypeInt]
        [ implMethod "enabled" (SourceSpan 6 1) enabledBody,
          implMethod "on" (SourceSpan 7 1) (expressionLiteral (LBool True)),
          implMethod "off" (SourceSpan 8 1) (expressionLiteral (LBool False))
        ],
      statementExpression (SourceSpan 9 1) (expressionVariable (qualifiedName "RuntimeFlag" "enabled"))
    ]
  where
    enabledBody =
      expressionBlock
        ( firstAlias
            : remainingAliases
              <> [ statementLet
                     "target"
                     (SourceSpan (bindingCount + 1) 3)
                     ( expressionIf
                         (expressionVariable (indexedBindingName bindingCount))
                         (expressionVariable (qualifiedName "RuntimeFlag" "on"))
                         (expressionVariable (qualifiedName "RuntimeFlag" "off"))
                     ),
                   statementExpression (SourceSpan (bindingCount + 2) 3) (expressionVariable "target")
                 ]
        )
    firstAlias = statementLet (indexedBindingName 1) (SourceSpan 1 3) (expressionLiteral (LBool True))
    remainingAliases =
      [ statementLet
          (indexedBindingName index)
          (SourceSpan index 3)
          (expressionVariable (indexedBindingName (index - 1)))
      | index <- [2 .. bindingCount]
      ]

indexedBindingName :: Int -> UnresolvedName
indexedBindingName index =
  sourceName (mkIdentifier ("binding" <> Text.pack (show index)))

assertRuntimeCompletesWithin :: Int -> Text -> Expr 'Resolved -> Text -> IO ()
assertRuntimeCompletesWithin timeoutMicros label expression expectedRendering = do
  outcome <-
    try
      ( timeout
          timeoutMicros
          ( case evaluateRuntimeExpr expression of
              Left diagnostic ->
                failTest (label <> " failed: " <> renderDiagnostic diagnostic)
              Right Nothing ->
                failTest (label <> " produced no result")
              Right (Just runtimeValue) -> do
                let rendered = renderRuntimeValue runtimeValue
                _ <- evaluate (Text.length rendered)
                pure rendered
          )
      ) ::
      IO (Either SomeException (Maybe Text))
  case outcome of
    Right Nothing ->
      failTest (label <> " timed out")
    Left err ->
      failTest (label <> " leaked host exception: " <> Text.pack (show err))
    Right (Just rendered) ->
      assertEqual (label <> " result") expectedRendering rendered
