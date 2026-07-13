{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.BindingSignature.Shared
  ( validSignatureProgram,
    separatedSignatureProgram,
    mismatchedSignatureProgram,
    useBeforeDefinitionProgram,
    nestedScopeProgram,
    selfRecursiveProgram,
    mutualRecursionProgram,
    threeNodeMutualRecursionProgram,
    nonRecursiveForwardReferenceProgram,
    retroactiveRebindingProgram,
    signatureTypeMismatchProgram,
    assertSourceOk,
    assertSourceOkWithoutPrelude,
    assertSourceErrorContains,
    assertSourceSingleErrorContains,
    assertSourceSingleErrorContainsWithoutPrelude,
    assertSourceSingleErrorCodeAndPrimarySpan,
    qualifiedEqSource,
    importedQualifiedMethodFactsProgram,
    aliasOnlyImportedCapabilityFactsProgram,
    speculativePreviewDeferredConstraintProgram,
    speculativePreviewDeferredConstraintBlock,
    speculativePreviewInferredConstraintProgram
  ) where

import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( ClassMethodSignature (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileSource,
    compileSourceWithPrelude
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( assertContains,
    assertEqual,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains,
    assertSingleDiagnosticPrimarySpan
  )

validSignatureProgram :: Expr
validSignatureProgram =
  EBlock
    [ SSignature "x" (SourceSpan 1 1) (SignatureType TypeInt),
      SLet "x" (SourceSpan 2 1) (ELit (LInt 1)),
      SExpr (SourceSpan 3 1) (EVar "x")
    ]

separatedSignatureProgram :: Expr
separatedSignatureProgram =
  EBlock
    [ SSignature "x" (SourceSpan 1 1) (SignatureType TypeInt),
      SExpr (SourceSpan 2 1) (ELit (LInt 1)),
      SLet "x" (SourceSpan 3 1) (ELit (LInt 2))
    ]

mismatchedSignatureProgram :: Expr
mismatchedSignatureProgram =
  EBlock
    [ SSignature "x" (SourceSpan 1 1) (SignatureType TypeInt),
      SLet "y" (SourceSpan 2 1) (ELit (LInt 2))
    ]

useBeforeDefinitionProgram :: Expr
useBeforeDefinitionProgram =
  EBlock
    [ SExpr (SourceSpan 1 1) (EVar "x"),
      SLet "x" (SourceSpan 2 1) (ELit (LInt 1))
    ]

nestedScopeProgram :: Expr
nestedScopeProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (ELit (LInt 1)),
      SExpr
        (SourceSpan 2 1)
        ( EBlock
            [ SExpr (SourceSpan 3 1) (EVar "x")
            ]
        )
    ]

selfRecursiveProgram :: Expr
selfRecursiveProgram =
  EBlock
    [ SLet "f" (SourceSpan 1 1) (EVar "f")
    ]

mutualRecursionProgram :: Expr
mutualRecursionProgram =
  EBlock
    [ SLet "even" (SourceSpan 1 1) (EVar "odd"),
      SLet "odd" (SourceSpan 2 1) (EVar "even"),
      SExpr (SourceSpan 3 1) (EVar "even")
    ]

threeNodeMutualRecursionProgram :: Expr
threeNodeMutualRecursionProgram =
  EBlock
    [ SLet "a" (SourceSpan 1 1) (EVar "b"),
      SLet "b" (SourceSpan 2 1) (EVar "c"),
      SLet "c" (SourceSpan 3 1) (EVar "a"),
      SExpr (SourceSpan 4 1) (EVar "a")
    ]

nonRecursiveForwardReferenceProgram :: Expr
nonRecursiveForwardReferenceProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (EVar "y"),
      SLet "y" (SourceSpan 2 1) (ELit (LInt 1)),
      SExpr (SourceSpan 3 1) (EVar "x")
    ]

retroactiveRebindingProgram :: Expr
retroactiveRebindingProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (EVar "y"),
      SLet "y" (SourceSpan 2 1) (ELit (LInt 1)),
      SLet "y" (SourceSpan 3 1) (EVar "x"),
      SExpr (SourceSpan 4 1) (EVar "x")
    ]

signatureTypeMismatchProgram :: Expr
signatureTypeMismatchProgram =
  EBlock
    [ SSignature "x" (SourceSpan 1 1) (SignatureType TypeInt),
      SLet "x" (SourceSpan 2 1) (ELit (LBool True))
    ]

assertSourceOk :: Text.Text -> IO ()
assertSourceOk src = do
  result <- compileSource defaultWarningSettings src
  assertEqual "compile errors" [] (compileErrors result)

assertSourceOkWithoutPrelude :: Text.Text -> IO ()
assertSourceOkWithoutPrelude src = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing src
  assertEqual "compile errors" [] (compileErrors result)

assertSourceErrorContains :: Text.Text -> Text.Text -> IO ()
assertSourceErrorContains src needle = do
  result <- compileSource defaultWarningSettings src
  assertContains "source error" needle (Text.unlines (map renderDiagnostic (compileErrors result)))

assertSourceSingleErrorContains :: Text.Text -> Text.Text -> IO ()
assertSourceSingleErrorContains src needle = do
  result <- compileSource defaultWarningSettings src
  assertSingleDiagnosticContains "source error" needle (compileErrors result)

assertSourceSingleErrorContainsWithoutPrelude :: Text.Text -> Text.Text -> IO ()
assertSourceSingleErrorContainsWithoutPrelude src needle = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing src
  assertSingleDiagnosticContains "source error" needle (compileErrors result)

assertSourceSingleErrorCodeAndPrimarySpan :: Text.Text -> Text.Text -> SourceSpan -> IO ()
assertSourceSingleErrorCodeAndPrimarySpan src expectedCode expectedSpan = do
  result <- compileSource defaultWarningSettings src
  assertSingleDiagnosticCode "source error code" expectedCode (compileErrors result)
  assertSingleDiagnosticPrimarySpan "source error primary span" expectedSpan (compileErrors result)

qualifiedEqSource :: Text.Text
qualifiedEqSource =
  "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\nimpl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"

importedQualifiedMethodFactsProgram :: Expr
importedQualifiedMethodFactsProgram =
  EBlock
    [ SModule (SourceSpan 1 1) ["Lib"],
      SClass
        (SourceSpan 2 1)
        "RemoteEq"
        ["a"]
        [ ClassMethodSignature
            "equals"
            (SourceSpan 3 1)
            ( ConstrainedSignature
                []
                ( TypeFunction
                    (TypeVariable "a")
                    (TypeFunction (TypeVariable "a") (TypeBool))
                )
            )
        ],
      SImpl
        (SourceSpan 4 1)
        "RemoteEq"
        [TypeInt]
        [ ImplMethod
            "equals"
            (SourceSpan 5 1)
            (ELambda "left" (ELambda "right" (EBinary "==" (EVar "left") (EVar "right"))))
        ],
      SModule (SourceSpan 6 1) ["App"],
      SImport (SourceSpan 7 1) ["Lib"] Nothing Nothing,
      SExpr
        (SourceSpan 9 1)
        ( EApply
            (EApply (EVar "RemoteEq::equals") (ELit (LInt 1)))
            (ELit (LInt 1))
        )
    ]

aliasOnlyImportedCapabilityFactsProgram :: Expr
aliasOnlyImportedCapabilityFactsProgram =
  EBlock
    [ SModule (SourceSpan 1 1) ["Lib"],
      SClass (SourceSpan 2 1) "RemoteEq" ["a"] [],
      SImpl (SourceSpan 3 1) "RemoteEq" [TypeInt] [],
      SModule (SourceSpan 4 1) ["App"],
      SImport (SourceSpan 5 1) ["Lib"] (Just "Lib") Nothing,
      SSignature
        "x"
        (SourceSpan 6 1)
        (ConstrainedSignature [SignatureConstraint "RemoteEq" [TypeInt]] (TypeInt)),
      SLet "x" (SourceSpan 7 1) (ELit (LInt 1))
    ]

speculativePreviewDeferredConstraintProgram :: Expr
speculativePreviewDeferredConstraintProgram =
  EBlock
    [ SModule (SourceSpan 1 1) ["Base"],
      SClass (SourceSpan 2 1) "Eq" ["a"] [],
      SImpl (SourceSpan 3 1) "Eq" [TypeInt] [],
      SModule (SourceSpan 4 1) ["Facts"],
      SImport (SourceSpan 5 1) ["Base"] Nothing Nothing,
      SImpl (SourceSpan 6 1) "Eq" [TypeBool] [],
      SModule (SourceSpan 7 1) ["Main"],
      SImport (SourceSpan 8 1) ["Base"] Nothing Nothing,
      SSignature
        "id"
        (SourceSpan 9 1)
        ( ConstrainedSignature
            [SignatureConstraint "Eq" [TypeVariable "a"]]
            (TypeFunction (TypeVariable "a") (TypeVariable "a"))
        ),
      SLet "id" (SourceSpan 10 1) (ELambda "x" (EVar "x")),
      SLet "value" (SourceSpan 11 1) speculativePreviewDeferredConstraintBlock,
      SExpr (SourceSpan 18 1) (EVar "value")
    ]

speculativePreviewDeferredConstraintBlock :: Expr
speculativePreviewDeferredConstraintBlock =
  EBlock
    [ SLet
        "left"
        (SourceSpan 12 1)
        ( EIf
            (ELit (LBool True))
            (ELambda "x" (EVar "x"))
            (ELambda "x" (EVar "right"))
        ),
      SLet "early" (SourceSpan 13 1) (EApply (EVar "left") (ELit (LBool True))),
      SImport (SourceSpan 14 1) ["Facts"] Nothing Nothing,
      SLet
        "right"
        (SourceSpan 15 1)
        ( EIf
            (ELit (LBool False))
            (EApply (EVar "left") (ELit (LBool True)))
            (EApply (EVar "id") (ELit (LBool True)))
        ),
      SExpr (SourceSpan 16 1) (EVar "early")
    ]

speculativePreviewInferredConstraintProgram :: Expr
speculativePreviewInferredConstraintProgram =
  EBlock
    [ SModule (SourceSpan 1 1) ["Base"],
      SClass
        (SourceSpan 2 1)
        "C"
        ["a"]
        [ ClassMethodSignature
            "m"
            (SourceSpan 3 1)
            (SignatureType (TypeFunction (TypeVariable "a") TypeBool))
        ],
      SModule (SourceSpan 4 1) ["Facts"],
      SImport (SourceSpan 5 1) ["Base"] Nothing Nothing,
      SImpl
        (SourceSpan 6 1)
        "C"
        [TypeBool]
        [ImplMethod "m" (SourceSpan 7 1) (ELambda "value" (ELit (LBool True)))],
      SModule (SourceSpan 8 1) ["Main"],
      SImport (SourceSpan 9 1) ["Base"] Nothing Nothing,
      SExpr
        (SourceSpan 10 1)
        ( EBlock
            [ SLet
                "left"
                (SourceSpan 11 1)
                (EIf (ELit (LBool True)) (ELambda "x" (EVar "x")) (EVar "right")),
              SLet "early" (SourceSpan 12 1) (EApply (EVar "left") (ELit (LBool True))),
              SImport (SourceSpan 13 1) ["Facts"] Nothing Nothing,
              SLet
                "right"
                (SourceSpan 14 1)
                ( EIf
                    (ELit (LBool False))
                    (ELambda "x" (EApply (EVar "C::m") (EVar "x")))
                    (EVar "left")
                ),
              SExpr (SourceSpan 15 1) (EVar "early")
            ]
        )
    ]
