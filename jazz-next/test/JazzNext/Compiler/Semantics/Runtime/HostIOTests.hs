{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.Runtime.HostIOTests
  ( hostIOTests
  ) where

import Data.Functor.Identity (Identity (..))
import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics (SourceSpan (..))
import JazzNext.Compiler.Runtime
  ( evaluateRuntimeExpr,
    evaluateRuntimeExprWithHost
  )
import JazzNext.Compiler.RuntimeHost
  ( RuntimeHost (..)
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual
  )

hostIOTests :: [NamedTest]
hostIOTests =
  [ ("host-aware evaluator preserves pure expressions", testHostAwareEvaluatorPreservesPureExpressions)
  ]

testHostAwareEvaluatorPreservesPureExpressions :: IO ()
testHostAwareEvaluatorPreservesPureExpressions = do
  mapM_ assertPreserved expressions
  where
    expressions =
      [ EBinary "+" (ELit (LInt 20)) (ELit (LInt 22)),
        EApply (ELambda "value" (EBinary "+" (EVar "value") (ELit (LInt 2)))) (ELit (LInt 40)),
        EBlock
          [ SLet "value" (SourceSpan 1 1) (ELit (LInt 40)),
            SExpr (SourceSpan 2 1) (EBinary "+" (EVar "value") (ELit (LInt 2)))
          ]
      ]

    assertPreserved expression = do
      let expected = evaluateRuntimeExpr expression
          actual = runIdentity (evaluateRuntimeExprWithHost deterministicHost expression)
      assertEqual "host-aware pure result" expected actual

deterministicHost :: RuntimeHost Identity
deterministicHost =
  RuntimeHost
    { runtimeHostReadText = \_ -> pure (Right "unused"),
      runtimeHostWriteText = \_ _ -> pure (Right ()),
      runtimeHostReadStdin = pure (Right "unused"),
      runtimeHostWriteStdout = \_ -> pure (Right ()),
      runtimeHostWriteStderr = \_ -> pure (Right ()),
      runtimeHostArguments = pure [],
      runtimeHostExit = \_ -> pure ()
    }
