{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Semantics.PrimitiveSemantics.ScalarCollection
  ( arithmeticPrimitiveTests,
    scalarPrimitiveTests,
    arithmeticMismatchTests,
    collectionTests,
    mixedCollectionTests
  )
where

import Jazz.Compiler.AST
  ( Expr (..),
    Literal (..)
  )
import Jazz.Compiler.Driver
  ( compileErrors,
    compileExpr
  )
import Jazz.Compiler.Semantics.PrimitiveSemantics.Shared
  ( assertCompileError,
    assertCompileErrorWithBundledPrelude,
    assertCompiles,
    assertCompilesWithBundledPrelude,
    mkProgram
  )
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertSingleDiagnosticContains
  )

arithmeticPrimitiveTests :: [NamedTest]
arithmeticPrimitiveTests =
  [ ("arithmetic primitives accept Int operands", testAcceptsArithmeticIntOperands)
  ]

scalarPrimitiveTests :: [NamedTest]
scalarPrimitiveTests =
  [ ("source pipeline types private text traversal primitives", testSourcePipelineTypesPrivateTextTraversalPrimitives),
    ("source pipeline rejects non-Text traversal arguments", testSourcePipelineRejectsNonTextTraversalArguments),
    ("source pipeline types bootstrap collection and scalar primitives", testSourcePipelineTypesBootstrapCollectionScalarPrimitives),
    ("source pipeline rejects invalid bootstrap scalar arguments", testSourcePipelineRejectsInvalidBootstrapScalarArguments),
    ("source pipeline types private host IO primitives", testSourcePipelineTypesPrivateHostIOPrimitives),
    ("source pipeline rejects invalid host IO arguments", testSourcePipelineRejectsInvalidHostIOArguments)
  ]

arithmeticMismatchTests :: [NamedTest]
arithmeticMismatchTests =
  [ ("arithmetic primitives reject mismatched operand types", testRejectsArithmeticTypeMismatch)
  ]

collectionTests :: [NamedTest]
collectionTests =
  [ ("source pipeline accepts hd with list literal argument", testSourcePipelineAcceptsHdListLiteral),
    ("source pipeline accepts map over nested list literals", testSourcePipelineAcceptsMapHdNestedLists),
    ("source pipeline accepts filter over list literals", testSourcePipelineAcceptsFilterListLiteral),
    ("source pipeline rejects hd with non-list argument", testSourcePipelineRejectsHdNonListArgument),
    ("source pipeline rejects tl with non-list argument", testSourcePipelineRejectsTlNonListArgument),
    ("source pipeline rejects map with non-function mapper", testSourcePipelineRejectsMapNonFunctionMapper),
    ("source pipeline rejects map with non-list collection", testSourcePipelineRejectsMapNonListCollection),
    ("source pipeline rejects filter with non-function predicate", testSourcePipelineRejectsFilterNonFunctionPredicate),
    ("source pipeline rejects filter with non-list collection", testSourcePipelineRejectsFilterNonListCollection),
    ("source pipeline rejects filter predicate with non-Bool result", testSourcePipelineRejectsFilterPredicateNonBoolResult)
  ]

mixedCollectionTests :: [NamedTest]
mixedCollectionTests =
  [ ("source pipeline rejects mixed-type list literals", testSourcePipelineRejectsMixedTypeListLiteral)
  ]

testAcceptsArithmeticIntOperands :: IO ()
testAcceptsArithmeticIntOperands = do
  result <- compileExpr defaultWarningSettings arithmeticProgram
  assertEqual "compile errors" [] (compileErrors result)

testSourcePipelineTypesPrivateTextTraversalPrimitives :: IO ()
testSourcePipelineTypesPrivateTextTraversalPrimitives =
  assertCompiles
    """
    length :: Int.
    length = __kernel_textLength \"a\\u{1F642}\".
    parts :: [(Char, Text)].
    parts = __kernel_textUnconsRaw \"a\\u{1F642}\".
    """

testSourcePipelineRejectsNonTextTraversalArguments :: IO ()
testSourcePipelineRejectsNonTextTraversalArguments = do
  assertCompileError
    "bad = __kernel_textLength 1."
    "textLength argument type mismatch"
    "E2006"
  assertCompileError
    "bad = __kernel_textUnconsRaw True."
    "textUnconsRaw argument type mismatch"
    "E2006"

testSourcePipelineTypesBootstrapCollectionScalarPrimitives :: IO ()
testSourcePipelineTypesBootstrapCollectionScalarPrimitives =
  assertCompiles
    ( """
    items :: [Text].
    items = __kernel_listPrependRaw \"first\" [\"second\"].
    reversed :: [Text].
    reversed = __kernel_listReverseRaw items.
    scalar :: UInt32.
    scalar = __kernel_charToUInt32 '\\u{1F642}'.
    decoded :: [Char].
    decoded = __kernel_charFromUInt32Raw scalar.
    classes :: (Bool, Bool, Bool, Bool, Bool, Bool, Bool).
    classes = (__kernel_charIsAlpha 'é', __kernel_charIsAlphaNum '9', __kernel_charIsDigit '9', __kernel_charIsSpace '\\t', __kernel_charIsHexDigit 'F', __kernel_charIsLower 'é', __kernel_charIsUpper 'É').
    lower :: Char.
    lower = __kernel_charToLower 'É'.
    upper :: Char.
    upper = __kernel_charToUpper 'é'.
    built :: Text.
    built = __kernel_textAppendChar (__kernel_textAppend \"Ja\" \"z\") 'z'.
    fromChars :: Text.
    fromChars = __kernel_textFromChars ['J', 'a', 'z', 'z'].
    concatenated :: Text.
    concatenated = __kernel_textConcat ["Ja", "zz"].
    """
    )

testSourcePipelineRejectsInvalidBootstrapScalarArguments :: IO ()
testSourcePipelineRejectsInvalidBootstrapScalarArguments = do
  assertCompileError "bad = __kernel_charToUInt32 1." "charToUInt32 argument type mismatch" "E2006"
  assertCompileError "bad = __kernel_charFromUInt32Raw 'a'." "charFromUInt32Raw argument type mismatch" "E2006"
  assertCompileError "bad = __kernel_charIsAlpha \"a\"." "charIsAlpha argument type mismatch" "E2006"
  assertCompileError
    """
    bad = __kernel_charToLower "a".
    """
    "charToLower argument type mismatch"
    "E2006"
  assertCompileError "bad = __kernel_textAppend \"a\" True." "textAppend argument type mismatch" "E2006"
  assertCompileError "bad = __kernel_textAppendChar \"a\" 1." "textAppendChar argument type mismatch" "E2006"
  assertCompileError "bad = __kernel_listReverseRaw 1." "listReverseRaw argument type mismatch" "E2006"
  assertCompileError "bad = __kernel_textFromChars \"Jazz\"." "textFromChars argument type mismatch" "E2006"
  assertCompileError "bad = __kernel_textFromChars [1]." "textFromChars argument type mismatch" "E2006"
  assertCompileError
    """
    bad = __kernel_textConcat [1].
    """
    "textConcat argument type mismatch"
    "E2006"

testSourcePipelineTypesPrivateHostIOPrimitives :: IO ()
testSourcePipelineTypesPrivateHostIOPrimitives =
  assertCompiles
    ( """
    read! :: (Bool, Text, Text, Text).
    read! = __kernel_readTextRaw! \"source.jz\".
    write! :: (Bool, Text, Text, Text).
    write! = __kernel_writeTextRaw! \"output.txt\" \"Jazz\".
    stdin! :: (Bool, Text, Text, Text).
    stdin! = __kernel_readStdinRaw! ().
    stdout! :: (Bool, Text, Text, Text).
    stdout! = __kernel_writeStdoutRaw! \"out\".
    stderr! :: (Bool, Text, Text, Text).
    stderr! = __kernel_writeStderrRaw! \"err\".
    args! :: [Text].
    args! = __kernel_arguments! ().
    terminated! :: ().
    terminated! = __kernel_exit! 0.
    """
    )

testSourcePipelineRejectsInvalidHostIOArguments :: IO ()
testSourcePipelineRejectsInvalidHostIOArguments =
  mapM_ assertInvalidArgument invalidCalls
  where
    invalidCalls =
      [ ("__kernel_readTextRaw! 1", "readTextRaw!"),
        ("__kernel_writeTextRaw! \"path\" True", "writeTextRaw!"),
        ("__kernel_readStdinRaw! True", "readStdinRaw!"),
        ("__kernel_writeStdoutRaw! 1", "writeStdoutRaw!"),
        ("__kernel_writeStderrRaw! False", "writeStderrRaw!"),
        ("__kernel_arguments! 1", "arguments!"),
        ("__kernel_exit! \"zero\"", "exit!")
      ]

    assertInvalidArgument (call, name) =
      assertCompileError
        ("bad! = " <> call <> ".")
        (name <> " argument type mismatch")
        "E2006"

testRejectsArithmeticTypeMismatch :: IO ()
testRejectsArithmeticTypeMismatch = do
  result <- compileExpr defaultWarningSettings arithmeticTypeMismatchProgram
  assertSingleDiagnosticContains
    "arithmetic type error"
    "E2003"
    (compileErrors result)

testSourcePipelineAcceptsHdListLiteral :: IO ()
testSourcePipelineAcceptsHdListLiteral =
  assertCompilesWithBundledPrelude "x = hd [1, 2, 3]."

testSourcePipelineAcceptsMapHdNestedLists :: IO ()
testSourcePipelineAcceptsMapHdNestedLists =
  assertCompilesWithBundledPrelude "x = map hd [[1, 2], [3], [4, 5]]."

testSourcePipelineAcceptsFilterListLiteral :: IO ()
testSourcePipelineAcceptsFilterListLiteral =
  assertCompilesWithBundledPrelude "x = filter (> 1) [1, 2, 3]."

testSourcePipelineRejectsHdNonListArgument :: IO ()
testSourcePipelineRejectsHdNonListArgument =
  assertCompileErrorWithBundledPrelude
    "x = hd 1."
    "hd argument type mismatch"
    "E2006"

testSourcePipelineRejectsTlNonListArgument :: IO ()
testSourcePipelineRejectsTlNonListArgument =
  assertCompileErrorWithBundledPrelude
    "x = tl 1."
    "tl argument type mismatch"
    "E2006"

testSourcePipelineRejectsMapNonFunctionMapper :: IO ()
testSourcePipelineRejectsMapNonFunctionMapper =
  assertCompileErrorWithBundledPrelude
    "x = map 1 [1, 2]."
    "map mapper type mismatch"
    "E2006"

testSourcePipelineRejectsMapNonListCollection :: IO ()
testSourcePipelineRejectsMapNonListCollection =
  assertCompileErrorWithBundledPrelude
    "x = map hd 1."
    "map collection type mismatch"
    "E2006"

testSourcePipelineRejectsFilterNonFunctionPredicate :: IO ()
testSourcePipelineRejectsFilterNonFunctionPredicate =
  assertCompileErrorWithBundledPrelude
    "x = filter 1 [1, 2]."
    "filter predicate type mismatch"
    "E2006"

testSourcePipelineRejectsFilterNonListCollection :: IO ()
testSourcePipelineRejectsFilterNonListCollection =
  assertCompileErrorWithBundledPrelude
    "x = filter (> 1) 1."
    "filter collection type mismatch"
    "E2006"

testSourcePipelineRejectsFilterPredicateNonBoolResult :: IO ()
testSourcePipelineRejectsFilterPredicateNonBoolResult =
  assertCompileErrorWithBundledPrelude
    "x = filter (+ 1) [1, 2]."
    "filter predicate non-bool mismatch"
    "E2006"

testSourcePipelineRejectsMixedTypeListLiteral :: IO ()
testSourcePipelineRejectsMixedTypeListLiteral =
  assertCompileError
    "x = [1, True]."
    "list literal element mismatch"
    "E2007"

arithmeticProgram :: Expr
arithmeticProgram =
  mkProgram
    ( EBinary
        "+"
        (EBinary "*" (ELit (LInt 7)) (ELit (LInt 6)))
        (EBinary "/" (ELit (LInt 8)) (ELit (LInt 2)))
    )

arithmeticTypeMismatchProgram :: Expr
arithmeticTypeMismatchProgram =
  mkProgram (EBinary "+" (ELit (LInt 1)) (ELit (LBool True)))
