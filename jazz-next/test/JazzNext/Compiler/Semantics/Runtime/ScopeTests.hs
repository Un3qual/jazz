{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.Runtime.ScopeTests
  ( scopeTests
  ) where

import Control.Exception
  ( SomeException,
    evaluate,
    try
  )
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( ClassMethodSignature (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    renderDiagnostic
  )
import JazzNext.Compiler.Runtime
  ( evaluateRuntimeExpr,
    renderRuntimeValue
  )
import JazzNext.Compiler.Name
  ( Name,
    mkIdentifier,
    qualifiedName,
    sourceName
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    failTest
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
        SLet
          (indexedBindingName index)
          (SourceSpan index 1)
          (ELit (LInt (fromIntegral index)))
      expression =
        EBlock
          ( map binding [1 .. bindingCount]
              <> [SExpr (SourceSpan (bindingCount + 1) 1) (EVar (indexedBindingName bindingCount))]
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

nestedBlockAliasScope :: Int -> Expr
nestedBlockAliasScope bindingCount =
  EBlock
    [ SClass
        (SourceSpan 1 1)
        "RuntimeFlag"
        ["a"]
        [ ClassMethodSignature "enabled" (SourceSpan 2 1) (ConstrainedSignature [] TypeBool),
          ClassMethodSignature "on" (SourceSpan 3 1) (ConstrainedSignature [] TypeBool),
          ClassMethodSignature "off" (SourceSpan 4 1) (ConstrainedSignature [] TypeBool)
        ],
      SImpl
        (SourceSpan 5 1)
        "RuntimeFlag"
        [TypeInt]
        [ ImplMethod "enabled" (SourceSpan 6 1) enabledBody,
          ImplMethod "on" (SourceSpan 7 1) (ELit (LBool True)),
          ImplMethod "off" (SourceSpan 8 1) (ELit (LBool False))
        ],
      SExpr (SourceSpan 9 1) (EVar (qualifiedName "RuntimeFlag" "enabled"))
    ]
  where
    enabledBody =
      EBlock
        ( firstAlias
            : remainingAliases
              <> [ SLet
                     "target"
                     (SourceSpan (bindingCount + 1) 3)
                     ( EIf
                         (EVar (indexedBindingName bindingCount))
                         (EVar (qualifiedName "RuntimeFlag" "on"))
                         (EVar (qualifiedName "RuntimeFlag" "off"))
                     ),
                   SExpr (SourceSpan (bindingCount + 2) 3) (EVar "target")
                 ]
        )
    firstAlias = SLet (indexedBindingName 1) (SourceSpan 1 3) (ELit (LBool True))
    remainingAliases =
      [ SLet
          (indexedBindingName index)
          (SourceSpan index 3)
          (EVar (indexedBindingName (index - 1)))
        | index <- [2 .. bindingCount]
      ]

indexedBindingName :: Int -> Name
indexedBindingName index =
  sourceName (mkIdentifier ("binding" <> Text.pack (show index)))

assertRuntimeCompletesWithin :: Int -> Text -> Expr -> Text -> IO ()
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
      )
      :: IO (Either SomeException (Maybe Text))
  case outcome of
    Right Nothing ->
      failTest (label <> " timed out")
    Left err ->
      failTest (label <> " leaked host exception: " <> Text.pack (show err))
    Right (Just rendered) ->
      assertEqual (label <> " result") expectedRendering rendered
